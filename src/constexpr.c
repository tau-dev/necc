#include "lex_preproc.h"
#include "common.h"
#include "parse.h"
#include "checked_arith.h"

typedef struct {
	Tokenization *tok;
	const Token *cause_token;
	const Token *pos;
	const Options *opt;
	const char *cause;
	// This is false when only parsing should happen and the result will
	// be discarded, e.g. in `false && <expr>`.
	bool active;
} Parse;

typedef ConstInt CValue;

typedef CValue (*Evaluator)(Parse *);

static CValue evalExprConditional(Parse *);
static CValue evalExprOr(Parse *);
static CValue evalExprAnd(Parse *);
static CValue evalExprBitOr(Parse *);
static CValue evalExprBitXor(Parse *);
static CValue evalExprBitAnd(Parse *);
static CValue evalExprEquality (Parse *);
static CValue evalExprComparison(Parse *);
static CValue evalExprShift(Parse *);
static CValue evalExprAddition(Parse *);
static CValue evalExprMultiplication(Parse *);
static CValue evalExprPrefix(Parse *);
static CValue evalExprBase(Parse *);


static void truncate (Parse *p, CValue *v);
static void toBooleanInt(CValue *v);
static i64 signedVal(Parse *parse, CValue v);
static void joinIntTypes(const Parse *parse, CValue *a, CValue b);

CValue evalIntegerConstantExpression (Tokenization *tokens, const char *cause, const Options *opt, const Token **pos) {
	Parse parse = {
		.tok = tokens,
		.cause_token = *pos,
		.pos = *pos,
		.opt = opt,
		.cause = cause,
		.active = true,
	};
	CValue v = evalExprConditional(&parse);
	*pos = parse.pos;

	return v;
}

static void msgAt (Log log, const Parse *parse, const Token *pos) {
	Tokenization *t = parse->tok;
	u32 idx = pos - t->list.tokens;
	TokenLocation loc = t->list.positions[idx];
	SourceFile source = *t->files.ptr[loc.source.file_id];
    printMsg(log, source, loc.source);
}

static _Noreturn void cevalerror (const Parse *parse, const char *msg, ...) {
	msgAt(Log_Err, parse, parse->tok->list.tokens);
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, ".\n");

	msgAt(Log_Info, parse, parse->cause_token);
    fprintf(stderr, "%s.\n", parse->cause);

	exit(1);
}

_Noreturn static void unexpectedToken (const Parse *p, TokenKind expected) {
	cevalerror(p, "expected %s before the %s token", tokenName(expected), tokenName(p->pos->kind));
}

static inline Token expect (Parse *parse, TokenKind expected) {
	if (parse->pos->kind != expected)
		unexpectedToken(parse, expected);
	Token t = *parse->pos;
	parse->pos++;
	return t;
}

static inline bool tryEat (Parse *parse, TokenKind kind) {
	if (parse->pos->kind == kind) {
		parse->pos++;
		return true;
	}
	return false;
}

static PrimitiveSize size (const Parse *p, BasicType t) {
	return p->opt->target.typesizes[t & ~Int_unsigned];
}

static void discard (Parse *parse, Evaluator ev) {
	bool prev = parse->active;
	parse->active = false;
	ev(parse);
	parse->active = prev;
}


static CValue evalExprConditional (Parse *parse) {
	CValue res = evalExprOr(parse);

	if (tryEat(parse, Tok_Question)) {
		CValue other;
		if (res.val) {
			res = evalExprConditional(parse);
			expect(parse, Tok_Colon);
			other = evalExprConditional(parse);
		} else {
			other = evalExprConditional(parse);
			expect(parse, Tok_Colon);
			res = evalExprConditional(parse);
		}
		joinIntTypes(parse, &res, other);
	}
	return res;
}

static CValue evalExprOr (Parse *parse) {
	CValue res = evalExprAnd(parse);

	if (tryEat(parse, Tok_DoubleAmpersand)) {
		if (res.val)
			discard(parse, evalExprOr);
		else
			res = evalExprOr(parse);
		toBooleanInt(&res);
	}
	return res;
}

static CValue evalExprAnd (Parse *parse) {
	CValue res = evalExprBitOr(parse);

	if (tryEat(parse, Tok_DoubleAmpersand)) {
		if (res.val)
			res = evalExprAnd(parse);
		else
			discard(parse, evalExprAnd);
		toBooleanInt(&res);
	}
	return res;
}

// TODO All of these should associate left-to-right instead of
// right-to-left; most of them are commutative tho.
static CValue evalExprBitOr (Parse *parse) {
	CValue lhs = evalExprBitXor(parse);

	if (tryEat(parse, Tok_Pipe)) {
		CValue rhs = evalExprBitOr(parse);
		joinIntTypes(parse, &lhs, rhs);
		lhs.val |= rhs.val;
	}
	return lhs;
}

static CValue evalExprBitXor (Parse *parse) {
	CValue lhs = evalExprBitAnd(parse);

	if (tryEat(parse, Tok_Hat)) {
		CValue rhs = evalExprBitXor(parse);
		joinIntTypes(parse, &lhs, rhs);
		lhs.val ^= rhs.val;
	}
	return lhs;
}

static CValue evalExprBitAnd (Parse *parse) {
	CValue lhs = evalExprEquality(parse);

	if (tryEat(parse, Tok_Ampersand)) {
		CValue rhs = evalExprBitAnd(parse);
		joinIntTypes(parse, &lhs, rhs);
		lhs.val &= rhs.val;
	}
	return lhs;
}


static CValue evalExprEquality (Parse *parse) {
	CValue lhs = evalExprComparison(parse);

	if (tryEat(parse, Tok_EqualEqual)) {
		CValue rhs = evalExprComparison(parse);
		lhs.val = lhs.val == rhs.val;
		lhs.type = Int_int;
	} else if (tryEat(parse, Tok_BangEqual)) {
		CValue rhs = evalExprComparison(parse);
		lhs.val = lhs.val != rhs.val;
		lhs.type = Int_int;
	}
	return lhs;
}

static CValue evalExprComparison (Parse *parse) {
	CValue lhs = evalExprShift(parse);

	while (true) {
		switch (parse->pos->kind) {
		case Tok_Less:
		case Tok_LessEqual:
		case Tok_Greater:
		case Tok_GreaterEqual: {
			const Token *primary = parse->pos;
			parse->pos++;
			CValue rhs = evalExprShift(parse);
			joinIntTypes(parse, &lhs, rhs);

			if (lhs.type & Int_unsigned) {
				switch ((int) primary->kind) {
				case Tok_Less:
					lhs.val = lhs.val < rhs.val; break;
				case Tok_LessEqual:
					lhs.val = lhs.val <= rhs.val; break;
				case Tok_Greater:
					lhs.val = lhs.val > rhs.val; break;
				case Tok_GreaterEqual:
					lhs.val = lhs.val >= rhs.val; break;
				}
			} else {
				i64 a = signedVal(parse, lhs);
				i64 b = signedVal(parse, rhs);

				switch ((int) primary->kind) {
				case Tok_Less:
					lhs.val = a < b; break;
				case Tok_LessEqual:
					lhs.val = a <= b; break;
				case Tok_Greater:
					lhs.val = a > b; break;
				case Tok_GreaterEqual:
					lhs.val = a >= b; break;
				}
			}

			lhs.type = Int_int;
		} break;
		default:
			return lhs;
		}
	}
}

static CValue evalExprShift (Parse *parse) {
	CValue lhs = evalExprAddition(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_DoubleLess && primary->kind != Tok_DoubleGreater)
			return lhs;
		parse->pos++;
		CValue rhs = evalExprAddition(parse);
		joinIntTypes(parse, &lhs, rhs);

		if (parse->active) {
			if (rhs.val >= 8*size(parse, lhs.type))
				cevalerror(parse, "shift too wide");

			if (primary->kind == Tok_DoubleLess)
				lhs.val = lhs.val << rhs.val;
			else
				lhs.val = lhs.val >> rhs.val;
		}

		joinIntTypes(parse, &lhs, rhs);
	}
}

static CValue evalExprAddition (Parse *parse) {
	CValue lhs = evalExprMultiplication(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_Plus && primary->kind != Tok_Minus)
			return lhs;
		parse->pos++;
		CValue rhs = evalExprMultiplication(parse);
		joinIntTypes(parse, &lhs, rhs);

		if (parse->active) {
			PrimitiveSize sz = size(parse, lhs.type);
			if (lhs.type & Int_unsigned) {
				if (primary->kind == Tok_Plus)
					lhs.val = lhs.val + rhs.val;
				else
					lhs.val = lhs.val - rhs.val;
			} else {
				i64 a = signedVal(parse, lhs);
				i64 b = signedVal(parse, rhs);
				if (primary->kind == Tok_Plus) {
					if (addSignedOverflow(a, b, sz))
						cevalerror(parse, "signed integer overflow");
					lhs.val = a + b;
				} else {
					if (subSignedOverflow(a, b, sz))
						cevalerror(parse, "signed integer overflow");
					lhs.val = a - b;
				}
			}
			truncate(parse, &lhs);
		}
	}
}

static CValue evalExprMultiplication (Parse *parse) {
	CValue lhs = evalExprPrefix(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_Asterisk && primary->kind != Tok_Slash && primary->kind != Tok_Percent)
			return lhs;
		parse->pos++;
		CValue rhs = evalExprMultiplication(parse);
		joinIntTypes(parse, &lhs, rhs);

		if (parse->active) {
			PrimitiveSize sz = size(parse, lhs.type);
			if (lhs.type & Int_unsigned) {
				if (primary->kind == Tok_Asterisk) {
					lhs.val = lhs.val * rhs.val;
				} else if (primary->kind == Tok_Slash) {
					if (rhs.val == 0)
						cevalerror(parse, "division by 0");
					lhs.val = lhs.val / rhs.val;
				} else {
					if (rhs.val == 0)
						cevalerror(parse, "division by 0");
					lhs.val = lhs.val % rhs.val;
				}
			} else {
				i64 a = signedVal(parse, lhs);
				i64 b = signedVal(parse, rhs);
				if (primary->kind == Tok_Asterisk) {
					if (mulSignedOverflow(a, b, sz))
						cevalerror(parse, "signed integer overflow");
					lhs.val = a * b;
				} else if (primary->kind == Tok_Slash) {
					if (b == 0)
						cevalerror(parse, "division by 0");
					if (a == -1 && (u64) b == signBit(sz))
						cevalerror(parse, "signed integer overflow");
					lhs.val = a / b;
				} else {
					if (b == 0)
						cevalerror(parse, "division by 0");
					if (a == -1 && (u64) b == signBit(sz))
						cevalerror(parse, "signed integer overflow");
					lhs.val = a % b;
				}
			}
			truncate(parse, &lhs);
		}
	}
}

_Noreturn static void dynamicerr(Parse *p) {
	cevalerror(p, "this is not a constant integer operation");
}


static CValue evalExprPrefix (Parse *parse) {
	const Token *primary = parse->pos;

	parse->pos++;
	switch (primary->kind) {
	case Tok_DoublePlus:
	case Tok_DoubleMinus:
	case Tok_Asterisk:
	case Tok_Ampersand:
		dynamicerr(parse);

	case Tok_Bang: {
		CValue v = evalExprPrefix(parse);
		return (CValue) { .type = Int_int, .val = !v.val };
	}
	case Tok_Tilde: {
		CValue v = evalExprPrefix(parse);
		v.val = ~v.val;
		truncate(parse, &v);
		return v;
	}
	case Tok_Key_Sizeof: {
		Type typ = {0};
		bool openparen = tryEat(parse, Tok_OpenParen);
// 		if (!openparen || !tryParseTypeName(parse, &typ, NULL))
// 			typ = parseCValueType(parse, openparen ? parseExpression : evalExprPrefix);
		if (openparen)
			expect(parse, Tok_CloseParen);
		if (typ.kind == Kind_Function)
			cevalerror(parse, "the operand of a sizeof may not have a function type");

		BasicType res_type = parse->opt->target.ptrdiff.basic;
		res_type |= Int_unsigned;
		return (CValue) { .type = res_type, .val = typeSize(typ, &parse->opt->target) };
	}
	case Tok_Plus:
		return evalExprPrefix(parse);
	case Tok_Minus: {
		CValue v = evalExprPrefix(parse);
		if (!(v.type & Int_unsigned) && (v.val == signBit(size(parse, v.type))))
			cevalerror(parse, "signed integer overflow");
		v.val = -v.val;
		return v;
	}
// 	case Tok_OpenParen: {
// 		Type cast_target;
// 		if (false) { //tryParseTypeName(parse, &cast_target, NULL)) {
// 			cast_target.qualifiers = 0;
// 			expect(parse, Tok_CloseParen);
// 			if (parse->pos->kind == Tok_OpenBrace) {
// 				// Compound literal.
// 				// TODO Parse right unary operators!
// 				IrRef stack = genStackAllocFixed(build, typeSize(cast_target, &parse->opt->target));
// 				// TODO Generate Ir_StackDealloc at end of block!
// 				InitializationDest dest = {
// 					.address = stack,
// 				};
// 				parseInitializer(parse, dest, 0, cast_target);
// 				return (CValue) {cast_target, stack, Ref_LCValue};
// 			} else {
// 				// Cast operator
// 				CValue v = rvalue(evalExprPrefix(parse), parse);
// 				return (CValue) {cast_target, coercerval(v, cast_target, parse, primary, true)};
// 			}
// 		} else {
// 			parse->pos--;
// 			return evalExprBase(parse);
// 		}
// 	} break;
	default:
		parse->pos--;
		return evalExprBase(parse);
	}
}


static CValue evalExprBase (Parse *parse) {
	Token t = *parse->pos;
	CValue res;

	parse->pos++;
	switch (t.kind) {
	case Tok_OpenParen: {
		// TODO Need ExprAssignment here?
		res = evalExprConditional(parse);
		expect(parse, Tok_CloseParen);
	} break;
	case Tok_Char:
		return charLiteralValue(parse->tok, parse->pos - 1);
	case Tok_Integer:
		return intLiteralValue(parse->pos - 1);
	case Tok_IntegerReplaced:
		return (CValue) { .type = t.literal_type, .val = t.val.integer };

	case Tok_Key_True:
	case Tok_Key_False:
		return (CValue) { .type = t.literal_type, .val = t.kind == Tok_Key_True };

	case Tok_Key_Alignof: {
		expect(parse, Tok_OpenParen);
		Type t; // TODO = parseTypeName(parse, NULL);
		expect(parse, Tok_CloseParen);

		BasicType res_type = parse->opt->target.ptrdiff.basic;
		res_type |= Int_unsigned;
		res = (CValue) { .type = res_type, .val = typeAlignment(t, &parse->opt->target) };
	} break;

	case Tok_Identifier: {
		OrdinaryIdentifier *ident = t.val.symbol->ordinary;
		if (ident == NULL)
			cevalerror(parse, "undefined identifier ‘%.*s’", STRING_PRINTAGE(t.val.symbol->name));

		switch (ident->kind) {
		case Sym_Typedef:
			cevalerror(parse, "expected a value, found a typedef name");
		case Sym_EnumConstant:
			// TODO Store enum-ness in the integer type, e.g. for switch completeness checking.
			return (CValue) { .type = Int_int, ident->enum_constant };
		case Sym_Value_Static:
		case Sym_Value_Auto:
			dynamicerr(parse);
		}

	} unreachable;
	case Tok_Real:
	case Tok_String:
	case Tok_Intrinsic:
	case Tok_Key_Nullptr:
	case Tok_Key_Generic:
		dynamicerr(parse);

	default:
		cevalerror(parse, "expected an expression");
	}

	parse->pos++;
	switch ((int) parse->pos->kind) {
	case Tok_OpenParen:
	case Tok_OpenBracket:
	case Tok_DoublePlus:
	case Tok_DoubleMinus:
	case Tok_Dot:
	case Tok_Arrow:
		dynamicerr(parse);
	}

	return res;
}





// ======== Helpers ========


static void truncate (Parse *p, CValue *v) {
	v->val &= bitsBelow(size(p, v->type));
}

static void toBooleanInt (CValue *v) {
	*v = (CValue) {
		.type = Int_int,
		.val = v->val ? 1 : 0,
	};
}

static i64 signedVal (Parse *parse, CValue v) {
	u32 sz = size(parse, v.type);
	if (sz == 8)
		return toSigned(v.val);
	return sign(v.val, sz) * bitsAbove(sz) + (i64) v.val;
}


static bool isNegative (const Parse *p, CValue v) {
	return !(v.type & Int_unsigned) && sign(v.val, size(p, v.type));
}

static void joinIntTypes (const Parse *parse, CValue *a, CValue b) {
	// STYLE Copypasta from arithmeticConversions.

	i32 rank_diff = rankDiff(a->type, b.type);
	// a shall have the type rank of lower than or equal to b, or be the
	// signed one if the ranks are equal.
	BasicType lower = a->type;
	BasicType higher = b.type;
	if (rank_diff > 0 || (rank_diff == 0 && (higher & Int_unsigned))) {
		BasicType tmp = lower;
		lower = higher;
		higher = tmp;
		rank_diff = -rank_diff;
	}
	a->type = higher;

	if ((lower & Int_unsigned) != (higher & Int_unsigned)) {
		bool signed_cannot_represent = size(parse, lower) == size(parse, higher);

		if ((lower & Int_unsigned) && signed_cannot_represent)
			a->type |= Int_unsigned;

		if ((a->type & Int_unsigned) && parse->opt->warn_on_wrapping) {
// 			CValue *signed_val = higher & Int_unsigned ? a : &b;
// 			u64 constval;
			if (isNegative(parse, *a) || isNegative(parse, b)) {
				cevalerror(parse, "wrapping negative value range when converting operands to common type %s", "???");
// 						printTypeHighlighted(parse->arena, common));
			}
		}
	}
}
