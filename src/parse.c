#include "parse.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#include "ir_gen.h"
#include "ansii.h"

#define MAX_BACKTRACE 64

// TODO Move this into Parse
StringMap symbols = {0};

typedef LIST(String) Scope;

typedef struct {
	Arena *arena;
	const Tokenization tokens;
	const Token *pos;
	const Target target;

	// At the top level, these may be null.
	Scope current_scope;
	Function *current_function;
	IrBuild ir;
} Parse;


// void printBacktrace(void **addrs, int count) {
// 	char **syms = backtrace_symbols(addrs, count);
// 	for (int i = 0; i < count; i++) {
// 		printf("%s\n", syms[i]);
// 	}
// 	free(syms);
// }

_Noreturn void comperror (const Tokenization *t, const Token *tok, const char *msg, ...) {
	u32 idx = tok - t->tokens;
	TokenPosition pos = t->positions[idx];
	SourceFile source = *t->files.ptr[pos.source_file_ref];

    va_list args;
    va_start(args, msg);
    vprintErr(source, pos.source_file_offset, msg, args);
    va_end(args);
// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}

_Noreturn void parseerror (const Parse *p, const char *msg, ...) {
	u32 idx = p->pos - p->tokens.tokens;
	TokenPosition pos = p->tokens.positions[idx];
	SourceFile source = *p->tokens.files.ptr[pos.source_file_ref];

    va_list args;
    va_start(args, msg);
    vprintErr(source, pos.source_file_offset, msg, args);
    va_end(args);
// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}


_Noreturn void unexpectedToken (const Parse *p, TokenKind expected) {
	parseerror(p, "expected %s before the %s token", tokenName(expected), tokenName(p->pos->kind));
}

Token expect (Parse *parse, TokenKind expected) {
	if (parse->pos->kind != expected)
		unexpectedToken(parse, expected);
	Token t = *parse->pos;
	parse->pos++;
	return t;
}

bool tryEat (Parse *parse, TokenKind kind) {
	if (parse->pos->kind == kind) {
		parse->pos++;
		return true;
	}
	return false;
}


static Declaration parseDeclarator(Parse *parse, Type base_type);
// static Declaration parseDeclarator (Parse* parse, const Token **tok, Type base_type);
static Type parseValueType (Parse *parse, Value (*operator)(Parse *parse));
static Type parseTypeBase(Parse *parse, u8 *storage);
static Type parseTypeName(Parse *parse, u8 *storage);
static bool tryParseTypeBase(Parse *parse, Type *dest, u8 *storage);
static bool tryParseTypeName(Parse *parse, Type *dest, u8 *storage);
static void parseFunction(Parse *parse);
static Value rvalue(Value v, Parse *parse);
static Value dereference (Parse *parse, Value v);
static IrRef coerce(Value v, Type t, Parse *p);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse);
static void parseTypedefDecls(Parse *parse, Type base_type);
static Attributes parseAttributes(Parse *parse);


void parse (Arena *arena, Tokenization tokens, Target target) {
	(void) arena;
// 	const Token *t = tokens.tokens;
	IrBuild static_initializations = {0};
	(void) static_initializations;

	Parse parse = {
		.arena = arena,
		.tokens = tokens,
		.target = target,
		.pos = tokens.tokens,
	};


	while (parse.pos->kind != Tok_EOF) {
		u8 storage;
		Type base_type = parseTypeBase(&parse, &storage);

		if (storage == Storage_Typedef) {
			parseTypedefDecls(&parse, base_type);
			continue;
		}
		// TODO Accept type base without declarations

		Declaration decl = parseDeclarator(&parse, base_type);

		void **slot = mapGetOrCreate(&symbols, decl.name);
		bool existing = *slot != NULL;
		if (existing) {
			Symbol *existing_sym = *slot;
			if (existing_sym->kind == Sym_Value) {
				if (existing_sym->storage != Storage_Extern)
					parseerror(&parse, "redefinition of identifier %s", decl.name);
			} else {
				parseerror(&parse, "cannot redeclare a type as a variable or function", decl.name);
			}

			if (!typeEqual(existing_sym->value.typ, decl.type))
				parseerror(&parse,
						"mismatched declaration: previously declared with type %s",
						printTypeHighlighted(parse.arena, existing_sym->type)); // TODO Add location of original declaration
		} else {
			*slot = ALLOC(arena, Symbol);
			*(Symbol *) *slot = (Symbol) {decl.name};
		}

		Symbol *sym = *slot;

		if (decl.type.kind == Kind_Function) {
			sym->kind = Sym_Value;
			sym->value.function = ALLOC(arena, Function);
			*sym->value.function = (Function) {decl.name, decl.type.function};
			sym->value.typ = decl.type;

			if (parse.pos->kind == Tok_OpenBrace) {
				Function *func = sym->value.function;
				if (func->entry != NULL)
					parseerror(&parse, "redefinition of existing function");

				Declaration *old_parameters = func->type.parameters.ptr;
				Declaration *new_parameters = decl.type.function.parameters.ptr;
				size_t param_count = func->type.parameters.len;


				assert(func->type.parameters.len == decl.type.function.parameters.len);

				for (u32 i = 0; i < param_count; i++) {
					if (new_parameters[i].name.len == 0)
						parseerror(&parse, "missing parameter name");

					if (existing && old_parameters[i].name.len != 0 &&
						!SPAN_EQL(old_parameters[i].name, new_parameters[i].name))
					{
						parseerror(&parse, "parameter redeclared to different name");
					}
				}

				func->type = decl.type.function;
				sym->value.typ = decl.type;

				parse.current_function = func;
				parseFunction(&parse);
				func->ir = parse.ir.ir;
				parse.current_function = 0;
				parse.ir = (IrBuild) {0};
				parse.current_scope = (Scope) {0};
			} else if (parse.pos->kind == Tok_Semicolon) {

			} else
				parseerror(&parse, "expected %s or function body", tokenName(Tok_Semicolon));
		} else {

			parseerror(&parse, "TODO globals");
		}
	}
}


Symbol *defSymbol (Parse *parse, String name) {
	PUSH_A(parse->arena, parse->current_scope, name);
	Symbol *sym = malloc(sizeof(Symbol)); // TODO Use arena
	*sym = (Symbol) {name};
	void **slot = mapGetOrCreate(&symbols, name);
	sym->shadowed = *slot;
	*slot = sym;
	return sym;
}

void removeScope (Scope names) {
	for (u32 i = 0; i < names.len; i++) {
		void **slot = mapGetOrCreate(&symbols, names.ptr[i]);
		Symbol *s = *slot;
		*slot = s->shadowed;
		free(s);
	}
}

void popScope (Parse *parse, Scope replacement) {
	removeScope(parse->current_scope);
	parse->current_scope = replacement;
}

static void parseCompound(Parse *parse);
static void parseStatement(Parse *parse);
static Value parseExpression(Parse *parse);
static Value parseExprAssignment(Parse *parse);
static Value parseExprBitOr(Parse *parse);
static Value parseExprBitXor(Parse *parse);
static Value parseExprBitAnd(Parse *parse);
static Value parseExprGreaterLess(Parse *parse);
static Value parseExprAddition(Parse *parse);
static Value parseExprMultiplication(Parse *parse);
static Value parseExprLeftUnary(Parse *parse);
static Value parseExprRightUnary(Parse *parse);
static Value parseExprBase(Parse *parse);
static void parseInitializer(Parse *parse, Value dest);
static void parseStructInitializer(Parse *parse, Value dest);
static void requiresVersion (Parse *parse, const char *desc, Version v);

void parseFunction (Parse *parse) {
	Function *func = parse->current_function;
	u32 param_count = func->type.parameters.len;

	func->entry = ALLOC(parse->arena, Block);
	func->entry = genNewBlockLabeled(parse->arena, &parse->ir, STRING("__entry"));
	parse->current_scope = (Scope) {0};

	for (u32 i = 0; i < param_count; i++) {
		Declaration param = func->type.parameters.ptr[i];
		Symbol *sym = defSymbol(parse, param.name);
		sym->kind = Sym_Value;
		IrRef slot = genStackAllocFixed(&parse->ir, typeSizeBytes(param.type, &parse->target));
		sym->value = (Value) {param.type, {{slot, Ref_LValue}}};
		IrRef paramval = genParameter(&parse->ir, typeSize(param.type, &parse->target));
		genStore(&parse->ir, slot, paramval);
	}

	parseCompound(parse);

	// TODO Warning if this is reachable and return type is not void.
	genReturnVal(&parse->ir, IR_REF_NONE);

	removeScope(parse->current_scope);
	// TODO Find and report undefined labels
	for (u32 i = 0; i < func->labels.capacity; i++) {
		Block *b = func->labels.content[i];
		if (b && b->exit.kind == Exit_None)
			parseerror(parse, "a `goto` references label `%s`, which is not declared in this function", b->label.ptr);
	}
}

static void parseCompound (Parse *parse) {
	expect(parse, Tok_OpenBrace);
	const Token *block_begin = parse->pos;
	Scope enclosing_scope = parse->current_scope;
	parse->current_scope = (Scope) {0};

	while (true) {
		Token t = *parse->pos;
		if (t.kind == Tok_CloseBrace) {
			parse->pos++;
			return;
		} else if (t.kind == Tok_EOF) {
			parse->pos = block_begin;
			parseerror(parse, "unclosed %s", tokenName(Tok_OpenBrace));
		} else {
			parseStatement(parse);
		}
	}

	popScope(parse, enclosing_scope);
}

static Block *getLabeledBlock (Parse *parse, String label) {
	void **slot = mapGetOrCreate(&parse->current_function->labels, label);
	if (*slot == NULL) {
		*slot = ALLOC(parse->arena, Block);
		// null-terminate the label
		char *s = aalloc(parse->arena, label.len + 1);
		memcpy(s, label.ptr, label.len);
		s[label.len] = 0;
		*(Block *)*slot = (Block) {
			.label = {label.len, s},
		};
	}
	return *slot;
}

static void parseStatement (Parse *parse) {
	IrBuild *build = &parse->ir;
	bool labeled = false;
	Token t = *parse->pos;

	Attributes attr = parseAttributes(parse);
	(void) attr;

	if (t.kind == Tok_Identifier && parse->pos[1].kind == Tok_Colon) {
		void **slot = mapGetOrCreate(&parse->current_function->labels, t.val.identifier);
		Block *blk = *slot;
		Block *previous = parse->ir.insertion_block;
		genJump(build, blk);

		if (blk) {
			if (blk->exit.kind != Exit_None)
				parseerror(parse, "redefinition of label `%s`", blk->label.ptr);
			blk->first_inst = parse->ir.ir.len;
			parse->ir.insertion_block = blk;
		} else {
			*slot = genNewBlockLabeled(parse->arena, build, t.val.identifier);
			previous->exit.unconditional = *slot;
		}

		labeled = true;
		parse->pos += 2;
		t = *parse->pos;
	}

	switch (t.kind) {
		case Tok_OpenBrace:
			parseCompound(parse);
			return;
		case Tok_Key_Return: {
			parse->pos++;
			// TODO Check against function return type
			if (parse->pos->kind == Tok_Semicolon) {
				parse->pos++;
				genReturnVal(build, IR_REF_NONE);
			} else {
// 				Parse start = *parse;
				// TODO We cannot retain the parse as a value (parse.ir may get invalidated), so take a Token * for locating error messages.
				IrRef val = coerce(rvalue(parseExpression(parse), parse),
						*parse->current_function->type.rettype,
						parse);
				genReturnVal(build, val);
			}

			// Unreferenced dummy block for further instructions, will be ignored
			genNewBlock(parse->arena, build);
			expect(parse, Tok_Semicolon);
		} return;
		case Tok_Key_While: {
			parse->pos++;

			genJump(build, NULL);
			Block *before = parse->ir.insertion_block;
			Block *head = genNewBlock(parse->arena, build);
			before->exit.unconditional = head;

			expect(parse, Tok_OpenParen);
// 			Parse start = *parse;
			Value condition = rvalue(parseExpression(parse), parse);
			expect(parse, Tok_CloseParen);
			genBranch(build, coerce(condition, BASIC_INT, parse));

			Block *body = genNewBlock(parse->arena, build);
			head->exit.branch.on_true = body;
			parseStatement(parse);
			genJump(build, NULL);

			Block *join = genNewBlock(parse->arena, build);
			head->exit.branch.on_false = join;
			body->exit.unconditional = join;
		} return;
		case Tok_Key_If: {
			parse->pos++;

			expect(parse, Tok_OpenParen);
			Value condition = rvalue(parseExpression(parse), parse);
			expect(parse, Tok_CloseParen);
			genBranch(build, coerce(condition, BASIC_INT, parse));

			Block *head = parse->ir.insertion_block;
			head->exit.branch.on_true = genNewBlock(parse->arena, build);
			parseStatement(parse);
			Block *body = parse->ir.insertion_block;
			genJump(build, NULL);
			Block *join = genNewBlock(parse->arena, build);
			body->exit.unconditional = join;

			if (parse->pos->kind == Tok_Key_Else) {
				parse->pos++;
				head->exit.branch.on_false = genNewBlock(parse->arena, build);
				parseStatement(parse);
				genJump(build, join);
			} else {
				head->exit.branch.on_false = join;
			}
			parse->ir.insertion_block = join;
		} return;
		case Tok_Key_Goto: {
			parse->pos++;
			String label = expect(parse, Tok_Identifier).val.identifier;
			expect(parse, Tok_Semicolon);

			genJump(build, getLabeledBlock(parse, label));
			// Unreferenced dummy block for further instructions, will be ignored
			genNewBlock(parse->arena, build);
		} return;
		case Tok_Semicolon:
			return;
		default:;
			Type base_type;
			u8 storage;
			if (tryParseTypeBase(parse, &base_type, &storage)) {
				if (labeled && parse->target.version < Version_C23)
					parseerror(parse, "for arbitrary reasons, a label may not appear directly before a declaration; you will want to add a semicolon between the two or put the declaration into a braced block");

				if (storage == Storage_Typedef) {
					parseTypedefDecls(parse, base_type);
					return;
				}

				if (tryEat(parse, Tok_Semicolon))
					return;

				do {
					Declaration decl = parseDeclarator(parse, base_type);
					Symbol *sym = defSymbol(parse, decl.name);
					if (decl.type.kind == Kind_Function) {
						parseerror(parse, "TODO Support function declarations in functions");
					} else if (decl.type.kind == Kind_Void) {
						parseerror(parse, "Variables can not have $svoid$s type", BOLD, RESET);
					} else {
						sym->kind = Sym_Value;
						sym->value = (Value) {decl.type, {{
							genStackAllocFixed(build, typeSizeBytes(decl.type, &parse->target)),
							storage == Storage_Register ? Ref_LValue_Register : Ref_LValue,
						}}};

						if (tryEat(parse, Tok_Equals))
							parseInitializer(parse, sym->value);
					}
				} while (tryEat(parse, Tok_Comma));
				expect(parse, Tok_Semicolon);

				return;
			}
			parseExpression(parse);
			expect(parse, Tok_Semicolon);
			return;
	}
}

static Value parseExpression (Parse *parse) {
	Attributes attr = parseAttributes(parse);
	(void) attr;

	Value v = parseExprAssignment(parse);

	while (tryEat(parse, Tok_Comma))
		v = parseExprAssignment(parse);

	return v;
}

static Value parseExprAssignment (Parse *parse) {
	Value v = parseExprBitOr(parse);

	switch (parse->pos->kind) {
	case Tok_Equals:
		if (v.typ.kind == Kind_Function)
			parseerror(parse, "cannot assign to a function");
		if (v.byref == Ref_RValue)
			parseerror(parse, "cannot assign to rvalue of type %s", printType(parse->arena, v.typ));
		if (v.typ.qualifiers & Qualifier_Const)
			parseerror(parse, "cannot assign to %sconst%s value", BOLD, RESET);

		parse->pos++;
		Value assigned_val = parseExprAssignment(parse);

		genStore(&parse->ir, v.ir, coerce(rvalue(assigned_val, parse), v.typ, parse));
		return assigned_val;
	// TODO Tok_PlusEquals, Tok_MinusEquals etc.
	default:
		return v;
	}
}


// TODO All of these should associate left-to-right instead of
// right-to-left; most of them are commutative tho.
static Value parseExprBitOr (Parse *parse) {
	Value lhs = parseExprBitXor(parse);
	if (tryEat(parse, Tok_Pipe)) {
// 		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitOr(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, {{genOr(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprBitXor (Parse *parse) {
	Value lhs = parseExprBitAnd(parse);
	if (tryEat(parse, Tok_Hat)) {
// 		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitXor(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, {{genXor(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprBitAnd (Parse *parse) {
	Value lhs = parseExprGreaterLess(parse);
	if (tryEat(parse, Tok_Ampersand)) {
// 		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitAnd(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, {{genAnd(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprGreaterLess (Parse *parse) {
	IrBuild *build = &parse->ir;
	Value lhs = parseExprAddition(parse);
// 	Parse start = *parse;

	switch (parse->pos->kind) {
	case Tok_Less: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThan(build,
				coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))}}};
	}
	case Tok_LessEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(build,
				coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))}}};
	}
	case Tok_Greater: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThan(build,
				coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))}}};
	}
	case Tok_GreaterEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(build,
				coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))}}};
	}
	default:
		return lhs;
	}
}

static Value parseExprAddition (Parse *parse) {
	IrBuild *build = &parse->ir;
	Value lhs = parseExprMultiplication(parse);
// 	Parse start = *parse;
	Token t = *parse->pos;
	if (t.kind != Tok_Plus && t.kind != Tok_Minus)
		return lhs;
	parse->pos++;
	lhs = rvalue(lhs, parse);
	Value rhs = rvalue(parseExprMultiplication(parse), parse);


	// TODO Type checking
	if (t.kind == Tok_Plus) {
		if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
			return pointerAdd(build, lhs, rhs, parse);
		} else {
			return (Value) {BASIC_INT, {{genAdd(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))}}};
		}
	} else {
		if (lhs.typ.kind == Kind_Pointer) {
			IrRef stride = genImmediateInt(build,
					typeSize(*lhs.typ.pointer, &parse->target), parse->target.ptr_size);

			if (rhs.typ.kind == Kind_Pointer) {
				IrRef diff = genSub(build, lhs.ir, rhs.ir);
				return (Value) {BASIC_INT, {{genDiv(build, diff, stride)}}};
			} else {
				IrRef idx = genMul(build, coerce(rhs, BASIC_INT, parse), stride);
				return (Value) {lhs.typ, {{genSub(build, lhs.ir, idx)}}};
			}
		} else {
			if (rhs.typ.kind == Kind_Pointer)
				parseerror(parse, "cannot subtract pointer from %s", printType(parse->arena, rhs.typ));
			return (Value) {BASIC_INT, {{genSub(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))}} };
		}
	}
}

static Value parseExprMultiplication (Parse *parse) {
	Value lhs = parseExprLeftUnary(parse);
	Token t = *parse->pos;
	if (t.kind != Tok_Asterisk && t.kind != Tok_Slash)
		return lhs;
	parse->pos++;
	lhs = rvalue(lhs, parse);
	Value rhs = rvalue(parseExprLeftUnary(parse), parse);

	if (t.kind == Tok_Asterisk)
		return (Value) { BASIC_INT, {{genMul(&parse->ir, lhs.ir, rhs.ir)}} };
	else
		return (Value) { BASIC_INT, {{genDiv(&parse->ir, lhs.ir, rhs.ir)}} };
}

static Value parseExprLeftUnary (Parse *parse) {
	IrBuild *build = &parse->ir;
	/* TODO:

	+
	-
	_Alignof

	*/
	TokenKind kind = parse->pos->kind;
	parse->pos++;
	switch (kind) {
	case Tok_DoublePlus:
	case Tok_DoubleMinus: {
		Value v = parseExprLeftUnary(parse);
		if (v.typ.kind == Kind_Function)
			parseerror(parse, "cannot modify a function");
		if (v.byref == Ref_RValue)
			parseerror(parse, "cannot modify a %s rvalue", printTypeHighlighted(parse->arena, v.typ), RESET);
		Value rval = (Value) {v.typ, {{genLoad(build, v.ir, typeSize(v.typ, &parse->target))}}};

		const int delta = parse->pos->kind == Tok_DoublePlus ? 1 : -1;
		Value one = {v.typ, {{genImmediateInt(build, delta, typeSize(v.typ, &parse->target))}}};

		Value result;
		if (v.typ.kind == Kind_Pointer)
			result = pointerAdd(build, rval, one, parse);
		else
			result = (Value) {v.typ, {{genAdd(build, rval.ir, one.ir)}}};

		genStore(build, v.ir, result.ir);
		return result;
	} break;
	case Tok_Asterisk:
		return dereference(parse, parseExprLeftUnary(parse));
	case Tok_Bang: {
		Value v = parseExprLeftUnary(parse);
		IrRef zero = genImmediateInt(build, 0, typeSize(v.typ, &parse->target));
		return (Value) {BASIC_INT, {{genEquals(build, v.ir, zero, typeSize(BASIC_INT, &parse->target))}}};
	} break;
	case Tok_Tilde: {
		Value v = parseExprLeftUnary(parse);
		return (Value) {BASIC_INT, {{genNot(build, v.ir)}}};
	} break;
	case Tok_Ampersand: {
		Value v = parseExprLeftUnary(parse);
		// TODO Structs and unions will be handeled byref even if they
		// are not lvalues; mark this somehow.
		if (v.typ.kind == Kind_Function) {
			v.ir = genFunctionRef(build, v.function);
			v.typ.kind = Kind_FunctionPtr;
		} else {
			if (v.byref == Ref_RValue)
				parseerror(parse, "cannot take address of a %s rvalue", printTypeHighlighted(parse->arena, v.typ));
			if (v.byref == Ref_LValue_Register)
				parseerror(parse, "cannot take address of a value declared %sregister%s", BOLD, RESET);

			Type *pointee = ALLOC(parse->arena, Type);
			*pointee = v.typ;
			v.typ = (Type) {Kind_Pointer, .pointer = pointee};
		}
		v.byref = Ref_RValue;
		return v;
	} break;
	case Tok_Key_Sizeof: {
		Type typ;
		bool openparen = tryEat(parse, Tok_OpenParen);
		if (!openparen || !tryParseTypeBase(parse, &typ, NULL))
			typ = parseValueType(parse, openparen ? parseExpression : parseExprLeftUnary);
		if (openparen)
			expect(parse, Tok_CloseParen);

		Type sizetype = parse->target.ptrdiff;
		sizetype.basic |= Int_unsigned;
		return (Value) {sizetype, {{genImmediateInt(build, typeSize(typ, &parse->target), typeSize(sizetype, &parse->target))}}};
	} break;
	case Tok_OpenParen: {
		Type cast_target;
		if (tryParseTypeName(parse, &cast_target, NULL)) {
			expect(parse, Tok_CloseParen);
			// TODO Compound literals
			Value v = rvalue(parseExprLeftUnary(parse), parse);
			// TODO Actually cast
			return (Value) {cast_target, {{coerce(v, cast_target, parse)}}};
		} else {
			Value v = parseExpression(parse);
			expect(parse, Tok_CloseParen);
			return v;
		}
	} break;
	default:
		parse->pos--;
		return parseExprRightUnary(parse);
	}
}

static Value parseExprRightUnary (Parse *parse) {
	IrBuild *build = &parse->ir;
	Value v = parseExprBase(parse);

	while (true) {
		if (tryEat(parse, Tok_OpenParen)) {
			Value func = rvalue(v, parse);
			if (func.typ.kind != Kind_FunctionPtr)
				parseerror(parse, "expected a function type, got an expression of type %s", printTypeHighlighted(parse->arena, func.typ));

			IrRefList arguments = {0};
			DeclList params = func.typ.function.parameters;
			if (parse->pos->kind != Tok_CloseParen) {
				do {
// 					Parse start = *parse;
					Value arg = parseExprAssignment(parse);
					arg = rvalue(arg, parse);
					if (arguments.len == params.len)
						parseerror(parse, "too many arguments to function call");
					PUSH_A(parse->arena, arguments, coerce(arg, params.ptr[arguments.len].type, parse));
				} while (tryEat(parse, Tok_Comma));
			}
			expect(parse, Tok_CloseParen);

			if (arguments.len < params.len)
				parseerror(parse, "too few arguments to function call");
			else if (arguments.len > params.len)
				parseerror(parse, "too many arguments to function call");

			ValuesSpan args = {arguments.len, arguments.ptr};
			v = (Value) {
				*func.typ.function.rettype,
				{{genCall(build, func.ir, args)}}
			};
		} else if (tryEat(parse, Tok_OpenBracket)) {
			v = rvalue(v, parse);
// 			Parse start = *parse;
			Value index = rvalue(parseExpression(parse), parse);
			expect(parse, Tok_CloseBracket);
			if (!(v.typ.kind == Kind_Pointer || index.typ.kind == Kind_Pointer))
				parseerror(parse, "either the subscript or the subuscripted value must be a pointer");
			v = dereference(parse, pointerAdd(build, v, index, parse));
		} else if (parse->pos[0].kind == Tok_DoublePlus || parse->pos[0].kind == Tok_DoubleMinus) {
			int delta = parse->pos[0].kind == Tok_DoublePlus ? 1 : -1;
			parse->pos++;

			Value v = parseExprLeftUnary(parse);
			if (v.typ.kind == Kind_Function)
				parseerror(parse, "cannot modify a function");
			if (v.byref == Ref_RValue)
				parseerror(parse, "cannot modify a %s rvalue", printTypeHighlighted(parse->arena, v.typ));
			Value rval = (Value) {v.typ, {{genLoad(build, v.ir, typeSize(v.typ, &parse->target))}}};

			Value one = {v.typ, {{genImmediateInt(build, delta, typeSize(v.typ, &parse->target))}}};

			IrRef result;
			if (v.typ.kind == Kind_Pointer)
				result = pointerAdd(build, rval, one, parse).ir;
			else
				result = genAdd(build, rval.ir, one.ir);

			genStore(build, v.ir, result);
			v = rval;
		} else if (parse->pos[0].kind == Tok_Dot || parse->pos[0].kind == Tok_Arrow) {
			bool arrow = parse->pos[0].kind == Tok_Arrow;
			parse->pos++;

			if (arrow) {
				if (v.typ.kind != Kind_Pointer)
					parseerror(parse, "the arrow %s->%s operator expects a pointer value. You may want to use a regular dot", BOLD, RESET);
				v = dereference(parse, v);
			}
			Token member = expect(parse, Tok_Identifier);
			String member_name = member.val.identifier;

			if (v.typ.kind == Kind_Struct) {
				assert(v.byref != Ref_RValue);
				bool found = false;
				for (u32 i = 0; i < v.typ.structure.len; i++) {
					StructMember member = v.typ.structure.ptr[i];
					if (SPAN_EQL(member.name, member_name)) {
						IrRef offset = genImmediateInt(build, member.offset, typeSize(parse->target.intptr, &parse->target));
						v.typ = member.type;
						v.ir = genAdd(build, v.ir, offset);
						found = true;
						break;
					}
				}
				if (!found)
					parseerror(parse, "type %s does not have a member named %.*s", printTypeHighlighted(parse->arena, v.typ), member_name.len, member_name.ptr);
			} else if (v.typ.kind == Kind_Union) {
				parseerror(parse, "TODO Implement unions", printTypeHighlighted(parse->arena, v.typ), member_name.len, member_name.ptr);
			} else {
				parseerror(parse, "member access only works on %sstruct%ss and %sunion%s [unions not yet implemented]", BOLD, RESET, BOLD, RESET);
			}
		} else {
			break;
		}
	}
	return v;
}

static void skipOverCommaOrToCloseParen (Parse *parse) {
	// TODO Check well-formedness.
	for (u32 depth = 1; depth > 0; parse->pos++) {
		switch (parse->pos->kind) {
		case Tok_OpenParen:
		case Tok_OpenBrace:
		case Tok_OpenBracket:
			depth++;
			break;
		case Tok_CloseParen:
		case Tok_CloseBrace:
		case Tok_CloseBracket:
			depth--;
			break;
		case Tok_Comma:
			if (depth == 1)
				depth = 0;
			break;
		case Tok_EOF:
			parseerror(parse, "missing close paren");
			break;
		default:
			break;
		}
	}
	if (parse->pos[-1].kind == Tok_CloseParen)
		parse->pos--;
}

static Value parseExprBase (Parse *parse) {
	IrBuild *build = &parse->ir;
	Token t = *parse->pos;
	parse->pos++;
	// TODO
	switch (t.kind) {
	case Tok_OpenParen: {
		Value v = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		return v;
	}
	case Tok_Integer:
		return (Value) {BASIC_INT, {{genImmediateInt(build, t.val.integer, typeSize(BASIC_INT, &parse->target))}}};
// 	case Tok_Real:
// 		return (Value) {{Kind_Basic, {Basic_double}}, genImmediateReal(t.val.real)};
	case Tok_Identifier: {
		Symbol *sym = mapGet(&symbols, t.val.string);
		if (sym == NULL)
			parseerror(parse, "undefined identifier");
		if (sym->kind != Sym_Value)
			parseerror(parse, "expected value, found type");
		return sym->value;
	}
	case Tok_Key_Generic: {
		requiresVersion(parse, "generic selections", Version_C17);

		expect(parse, Tok_OpenParen);
		Type t = parseValueType(parse, parseExprAssignment);
		expect(parse, Tok_Comma);
		bool got = false;
		const Token *default_case = NULL;
		Value result;

		while (!tryEat(parse, Tok_CloseParen)) {
			if (tryEat(parse, Tok_Key_Default)) {
				if (default_case)
					parseerror(parse, "generic selection may not have more than one default association");
				expect(parse, Tok_Colon);
				default_case = parse->pos;
				skipOverCommaOrToCloseParen(parse);
			} else {
				Type typecase = parseTypeName(parse, NULL);
				expect(parse, Tok_Colon);
				if (typeEqual(t, typecase)) {
					// TODO Disallow any two typecases being compatible
					// (not just with the selecting type).
					if (got)
						parseerror(parse, "no two generic assiciations may specify compatible types");
					got = true;
					result = parseExprAssignment(parse);
				} else {
					skipOverCommaOrToCloseParen(parse);
				}
			};
		}
		if (!got) {
			if (!default_case)
				parseerror(parse, "the controlling expression's type %s matched none of the generic associations, and no default case was provided", printTypeHighlighted(parse->arena, t));
			const Token *end = parse->pos;
			parse->pos = default_case;
			result = parseExprAssignment(parse);
			if (!tryEat(parse, Tok_Comma))
				expect(parse, Tok_CloseParen);
			parse->pos = end;
		}
		return result;
	} break;
	default:
		parseerror(parse, "expected an expression");
	}
}

static Attributes parseAttributes (Parse *parse) {
	Attributes result = 0;
	while (parse->pos[0].kind == Tok_OpenBracket && parse->pos[1].kind == Tok_OpenBracket) {
		requiresVersion(parse, "attributes", Version_C23);
		parse->pos += 2;
		parseerror(parse, "TODO Implement attributes");
	}
	return result;
}

static void parseInitializer(Parse *parse, Value dest) {
	// TODO Values must be constants when initializing static/_Thread_local values.
	assert(dest.byref != Ref_RValue);
	if (tryEat(parse, Tok_OpenBrace)) {
		switch (dest.typ.kind) {
		case Kind_Struct:
			parseStructInitializer(parse, dest);
			return;
		case Kind_Union:
			parseerror(parse, "TODO Union initializers");
		case Kind_Array:
			parseerror(parse, "TODO Array initializers");
		default:
			parseerror(parse, "initializer lists only apply to structs, unions and arrays, not to %s", printTypeHighlighted(parse->arena, dest.typ));
		}
	} else {
		Value got = parseExprAssignment(parse);
		if (parse->ir.ir.ptr[got.ir].kind != Ir_Constant)
			requiresVersion(parse, "non-constant initializers", Version_C99);
		genStore(&parse->ir, dest.ir, coerce(rvalue(got, parse), dest.typ, parse));
	}
}

static void parseStructInitializer(Parse *parse, Value dest) {
	IrBuild *build = &parse->ir;
	assert(dest.typ.kind == Kind_Struct);
	assert(parse->pos->kind == Tok_OpenBrace);
	parse->pos++;
	u32 len = dest.typ.structure.len;
	StructMember *members = dest.typ.structure.ptr;

	// TODO Only zero out the gaps after initialization.
// 	genMemSet();


	u32 member_idx = 0;
	while (!tryEat(parse, Tok_CloseBrace)) {
		if (tryEat(parse, Tok_Dot)) {
			requiresVersion(parse, "dot-designated initializers", Version_C99);
			String member = expect(parse, Tok_Identifier).val.identifier;
			expect(parse, Tok_Equals);

			member_idx = (u32) -1;
			for (u32 i = 0; i < len; i++) {
				if (SPAN_EQL(member, members[i].name)) {
					member_idx = i;
					break;
				}
			}
			if (member_idx == (u32) -1)
				parseerror(parse, "%s does not have a member named %.*s", printTypeHighlighted(parse->arena, dest.typ), member.len, member.ptr);
		}
		if (member_idx >= len)
			parseerror(parse, "initializers beyond the end of the struct");

		IrRef off = genImmediateInt(build, members[member_idx].offset, parse->target.ptr_size);
		IrRef pos = genAdd(build, dest.ir, off);
		parseInitializer(parse, (Value) {members[member_idx].type, {{pos, dest.byref}}});

		member_idx++;

		expect(parse, Tok_Comma);
	}
}

static Type parseValueType (Parse *parse, Value (*operator)(Parse *parse)) {
	Block *current = parse->ir.insertion_block;
	// Emit the expression into a block which is then discarded. Pretty hacky.
	genNewBlock(parse->arena, &parse->ir);
	Type type = operator(parse).typ;
	parse->ir.insertion_block = current;
	return type;
}


static Type parseTypeName (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeName(parse, &type, storage))
		parseerror(parse, "invalid type");
	return type;
}

static bool tryParseTypeName (Parse *parse, Type *type, u8 *storage_dest) {
	return tryParseTypeBase(parse, type, storage_dest);
}

static Type parseTypeBase (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeBase(parse, &type, storage))
		parseerror(parse, "invalid type");
	return type;
}

static bool tryParseTypeBase (Parse *parse, Type *type, u8 *storage_dest) {
	// TODO Type names (e.g. arguments to sizeof) and parameters cannot have storage class.
	const Token *begin = parse->pos;

	u32 bases = 0;
	bool modifiable = true;
	Type base = BASIC_INT;
	u8 storage = Storage_Unspecified;
	u32 storages = 0;

	bool is_unsigned = false;
	const Token *signedness = NULL;
	const Token *shortness = NULL;
	const Token *longness[2] = {NULL};

	bool is_type_token = true;
	while (is_type_token) {
		switch (parse->pos->kind) {
		case Tok_Key_Int:
			base.kind = Kind_Basic;
			base.basic = Int_int;
			bases++;
			break;
		case Tok_Key_Char:
			base.kind = Kind_Basic;
			base.basic = Int_char;
			bases++;
			break;
		case Tok_Key_Void:
			base.kind = Kind_Void;
			modifiable = false;
			bases++;
			break;
		case Tok_Key_Bool:
			requiresVersion(parse, "boolean types", Version_C99);
			parseerror(parse, "TODO Booleans");
			modifiable = false;
			break;
		case Tok_Key_Struct: {
			parse->pos++;
			Attributes attr = parseAttributes(parse);
			(void) attr;
			if (tryEat(parse, Tok_Identifier)) {
				// TODO
			}
			expect(parse, Tok_OpenBrace);

			LIST(StructMember) members = {0};
			u32 current_offset = 0;
			while (!tryEat(parse, Tok_CloseBrace)) {
				Type base = parseTypeBase(parse, NULL);
				Declaration decl = parseDeclarator(parse, base);
				// TODO Support VLAs
				StructMember member = {decl.type, decl.name, addMemberOffset(&current_offset, decl.type, &parse->target)};

				PUSH_A(parse->arena, members, member);
				expect(parse, Tok_Semicolon);
			}

			base.kind = Kind_Struct;
			base.structure.ptr = members.ptr;
			base.structure.len = members.len;
			modifiable = false;
			parse->pos--;
		} break;
		case Tok_Key_Union:
			parseerror(parse, "TODO Support unions");
			break;
		case Tok_Key_Enum:
			parseerror(parse, "TODO Support enums");
			break;
		case Tok_Key_Long:
			if (longness[0]) {
				if (longness[1])
					parseerror(parse, "The Type Is Too Damn Long");
				requiresVersion(parse, "long long types", Version_C99);
				longness[1] = parse->pos;
			} else {
				longness[0] = parse->pos;
			}
			break;
		case Tok_Key_Short:
			if (shortness)
				parseerror(parse, "duplicate %sshort%s", BOLD, RESET);
			shortness = parse->pos;
			break;
		case Tok_Key_Const: base.qualifiers |= Qualifier_Const; break;
		case Tok_Key_Volatile: base.qualifiers |= Qualifier_Volatile; break;
		case Tok_Key_Restrict: base.qualifiers |= Qualifier_Restrict; break;
		case Tok_Key_Atomic:
			requiresVersion(parse, "atomic types", Version_C17);
			base.qualifiers |= Qualifier_Atomic;
			break;
		// TODO Parse _Atomic() type operator
		case Tok_Key_Unsigned:
			if (signedness) {
				if (is_unsigned)
					parseerror(parse, "duplicate %sunsigned%s", BOLD, RESET);
				else
					parseerror(parse, "conflicting %ssigned%s and %sunsigned%s", BOLD, RESET, BOLD, RESET);
			}
			signedness = parse->pos;
			is_unsigned = true;
			break;
		case Tok_Key_Signed:
			if (signedness) {
				if (is_unsigned)
					parseerror(parse, "conflicting %sunsigned%s and %ssigned%s", BOLD, RESET, BOLD, RESET);
				else
					parseerror(parse, "duplicate %ssigned%s", BOLD, RESET);
			}
			signedness = parse->pos;
			break;
		case Tok_Key_Typedef:
			storage = Storage_Typedef;
			storages++;
			break;
		case Tok_Key_Auto:
			storage = Storage_Auto;
			storages++;
			break;
		case Tok_Key_Register:
			storage = Storage_Register;
			storages++;
			break;
		case Tok_Key_Static:
			storage = Storage_Static;
			storages++;
			break;
		case Tok_Key_Extern:
			storage = Storage_Extern;
			storages++;
			break;
		case Tok_Key_Threadlocal:
			requiresVersion(parse, "thread-local types", Version_C17);
			parseerror(parse, "TODO Support _Thread_local");
// 			base.storage = Storage_Threadlocal;
// 			storages++;
			break;
			// TODO Parse function specifiers inline and _Noreturn
		case Tok_Identifier: {
			Symbol *sym = mapGet(&symbols, parse->pos->val.identifier);
			if (sym && sym->kind == Sym_Type) {
				base = sym->type;
				bases++;
				modifiable = false;
			} else {
				is_type_token = false;
				continue;
			}
		} break;
		case Tok_Key_TypeofUnqual:
		case Tok_Key_Typeof: {
			bool unqual = parse->pos->kind == Tok_Key_TypeofUnqual;
			expect(parse, Tok_OpenParen);
			u8 storage_dummy;
			if (!tryParseTypeName(parse, &base, &storage_dummy))
				base = parseValueType(parse, parseExpression);
			expect(parse, Tok_CloseParen);
			bases++;
			modifiable = false;
			if (unqual)
				base.qualifiers = 0;

			if (storage_dummy != Storage_Unspecified) {
				storage = storage_dummy;
				storages++;
			}
		} break;
		default:
			is_type_token = false;
			continue;
		}

		if (bases > 1)
			parseerror(parse, "cannot have multiple base types"); // TODO Clarify
		if (storages > 1)
			parseerror(parse, "cannot have multiple storage modifiers"); // TODO Clarify
		if (storage_dest) {
			*storage_dest = storage;
		} else if (storages) {
			if (storage == Storage_Typedef)
				parseerror(parse, "a typedef is not allowed in this context");
			else
				parseerror(parse, "a storage specifier is not allowed in this context");
		}
		parse->pos++;
	}
	if (parse->pos == begin)
		return false;

	if (modifiable) {
		if (base.basic == Int_int) {
			if (longness[0] && shortness)
				comperror(&parse->tokens, longness[0], "integer cannot be %sshort%s and %long%s at the same time", BOLD, RESET, BOLD, RESET);
			if (longness[0]) {
				if (longness[1])
					base.basic = Int_longlong;
				else
					base.basic = Int_long;
			}
			if (shortness)
				base.basic = Int_short;
			if (is_unsigned)
				base.basic |= Int_unsigned;
			*type = base;
			if (!longness[0] && !shortness && !is_unsigned && bases == 0) {
				if (!storage)
					parseerror(parse, "missing type");

				Version v = parse->target.version;
				if (v > Version_C89 && v < Version_GNU)
					parseerror(parse, "support for implicit %s in declarations was removed in C99", printTypeHighlighted(parse->arena, BASIC_INT));
			}
			return true;
		} else if (base.basic == Int_char) {
			if (longness[0])
				comperror(&parse->tokens, longness[0], "char type cannot be %slong%s-modified", BOLD, RESET);
			if (shortness)
				comperror(&parse->tokens, shortness, "char type cannot be %sshort%s-modified", BOLD, RESET);
			if (signedness) {
				base.basic = Int_suchar;
				if (is_unsigned)
					base.basic |= Int_unsigned;
			}
			*type = base;
			return true;
		} else
			unreachable;
	}

	if (signedness)
		comperror(&parse->tokens, signedness, "type cannot be %s%s%s-modified", BOLD, is_unsigned ? "unsigned" : "signed", RESET);
	if (longness[0])
		comperror(&parse->tokens, longness[0], "type cannot be %slong%s-modified", BOLD, RESET);
	if (shortness)
		comperror(&parse->tokens, shortness, "type cannot be %sshort%s-modified", BOLD, RESET);
	*type = base;

	return true;
}

u8 parseQualifiers(Parse *parse) {
	u8 qual = 0;
	while (true) {
		u8 new = 0;
		TokenKind tok = parse->pos->kind;
		if (tok == Tok_Key_Const)
			new = Qualifier_Const;
		else if (tok == Tok_Key_Atomic)
			new = Qualifier_Atomic;
		else if (tok == Tok_Key_Volatile)
			new = Qualifier_Volatile;
		else if (tok == Tok_Key_Restrict)
			new = Qualifier_Restrict;
		else
			break;

		if (qual & new)
			parseerror(parse, "duplicate %s", tokenName(tok));
		qual |= new;
		parse->pos++;
	}
	return qual;
}


//
static Declaration parseDeclarator (Parse *parse, Type base_type) {
	Type base = base_type;

	while (parse->pos->kind == Tok_Asterisk) {
		parse->pos++;
		Attributes attr = parseAttributes(parse);
		(void) attr;

		Type *ptr = ALLOC(parse->arena, Type);
		*ptr = base;

		base = (Type) {Kind_Pointer,
			.qualifiers = parseQualifiers(parse),
			.pointer = ptr,
		};
	}

	Declaration decl = {0};
	Type *inner = &decl.type;
	Type *enclosing = NULL;

	if (tryEat(parse, Tok_OpenParen)) {
		decl = parseDeclarator(parse, BASIC_VOID);
		expect(parse, Tok_CloseParen);

		while (inner->kind != Kind_Void) {
			switch (inner->kind) {
			case Kind_Pointer:
				enclosing = inner;
				inner = inner->pointer;
				break;
			case Kind_Array:
				enclosing = inner;
				inner = inner->array.inner;
				break;
			case Kind_Function:
				enclosing = inner;
				inner = inner->function.rettype;
				break;
			case Kind_Void:
				break;
			default:
				unreachable;
			}
		}
		*inner = base;
	} else {
		decl.type = base;
		decl.name = expect(parse, Tok_Identifier).val.identifier;
	}

	while (true) {
		if (tryEat(parse, Tok_OpenParen)) {
			if (enclosing) {
				if (enclosing->kind == Kind_Function)
					parseerror(parse, "a function cannot return a function. You may want to return a function pointer instead");
				else if (enclosing->kind == Kind_Array)
					parseerror(parse, "an array cannot contain a function. You may want to store function pointers instead");
			}

			Type *rettype = ALLOC(parse->arena, Type);
			*rettype = *inner;

			inner->kind = Kind_Function;
			inner->function = (FunctionType) { rettype };

			if (parse->pos->kind != Tok_CloseParen) {
				do {
					u8 storage;
					Type param_type = parseTypeBase(parse, &storage);
					Declaration param_decl = parseDeclarator(parse, param_type);
					if (param_decl.type.kind == Kind_Array) {
						param_decl.type.kind = Kind_Pointer;
						param_decl.type.pointer = param_decl.type.array.inner;
					}
					PUSH_A(parse->arena, inner->function.parameters, param_decl);
				} while (tryEat(parse, Tok_Comma));
			}

			expect(parse, Tok_CloseParen);

			enclosing = inner;
			inner = rettype;
		} else if (tryEat(parse, Tok_OpenBracket)) {
			if (enclosing && enclosing->kind == Kind_Function)
				parseerror(parse, "a function cannot return an array. You may want to return a pointer to an array instead");

			Type *content = ALLOC(parse->arena, Type);
			*content = *inner;
			bool is_static = tryEat(parse, Tok_Key_Static);

			inner->kind = Kind_Array;
			inner->array = (ArrayType) { content };
			inner->qualifiers = parseQualifiers(parse);
			if (tryEat(parse, Tok_Key_Static)) {
				if (is_static)
					parseerror(parse, "static may not appear twice"); // TODO Nice formatting
				is_static = true;
			}
			if (is_static) {
				// TODO Reject a `static` everywhere except on a function parameter
				requiresVersion(parse, "static specifiers on array parameters", Version_C99);
			}

			if (tryEat(parse, Tok_Asterisk)) {
				parseerror(parse, "TODO Support ‘variable length array of unspecified size’, whatever that means");
				inner->array.count = 0;
			} else {
				inner->array.count = coerce(rvalue(parseExprAssignment(parse), parse), parse->target.ptrdiff, parse);
				if (parse->ir.ir.ptr[inner->array.count].kind != Ir_Constant) {
					requiresVersion(parse, "variable length arrays", Version_C99);
					parseerror(parse, "TODO Support");
				}
			}

			expect(parse, Tok_CloseBracket);
			enclosing = inner;
			inner = content;
		} else {
			break;
		}
	}

	return decl;
}

static void parseTypedefDecls(Parse *parse, Type base_type) {
	do {
		Declaration decl = parseDeclarator(parse, base_type);
		// TODO Error on overwriting
		Symbol *sym = defSymbol(parse, decl.name);
		sym->kind = Sym_Type;
		sym->type = decl.type;
	} while (tryEat(parse, Tok_Comma));

	expect(parse, Tok_Semicolon);
}

static Value pointerAdd (IrBuild *ir, Value lhs, Value rhs, Parse *op_parse) {
	assert(lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer);
	if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer)
		parseerror(op_parse, "cannot add two pointers");
	Value ptr;
	IrRef integer;
	if (lhs.typ.kind == Kind_Pointer) {
		ptr = lhs;
		integer = coerce(rhs, op_parse->target.ptrdiff, op_parse);
	} else {
		integer = coerce(lhs, op_parse->target.ptrdiff, op_parse);
		ptr = rhs;
	}
	IrRef stride = genImmediateInt(ir,
			typeSize(*ptr.typ.pointer, &op_parse->target), op_parse->target.ptr_size);
	IrRef diff = genMul(ir, stride, integer);
	return (Value) {ptr.typ, {{genAdd(ir, ptr.ir, diff)}}};
}

static Value dereference (Parse *parse, Value v) {
	assert(v.typ.kind == Kind_Pointer);
	if (v.byref == Ref_RValue)
		v.ir = genLoad(&parse->ir, v.ir, typeSize(v.typ, &parse->target));
	v.typ = *v.typ.pointer;
	v.byref = Ref_RValue;
	return v;
}


Value rvalue (Value v, Parse *parse) {
	// TODO Handle arrays
	if (v.typ.kind == Kind_Function) {
		v.ir = genFunctionRef(&parse->ir, v.function);
		v.typ.kind = Kind_FunctionPtr;
	} else if (v.byref != Ref_RValue) {
		assert(v.typ.kind == Kind_Pointer || v.typ.kind == Kind_Basic);

		v.ir = genLoad(&parse->ir, v.ir, typeSize(v.typ, &parse->target));
		v.typ.qualifiers = 0;
		v.byref = Ref_RValue;
	}
	return v;
}

IrRef coerce (Value v, Type t, Parse *p) {
	assert(v.byref == Ref_RValue); // FIXME Why???
	if (typeEqual(v.typ, t))
		return v.ir;
	if (t.basic == Int_bool)
		parseerror(p, "TODO Cast booleans");

	// Integer conversions
	if (v.typ.kind == Kind_Basic && t.kind == Kind_Basic) {
		Size target = p->target.typesizes[t.basic & ~Int_unsigned];
		Size source = p->target.typesizes[v.typ.basic & ~Int_unsigned];
		// TODO Is it reasonable to omit the truncation on signed
		// destination? This could cause a UB overflow to potentially
		// screw with much later calculations.
		if (target < source) {
			return genTrunc(&p->ir, v.ir, target);
		} else if (target == source) {
			return v.ir;
		} else {
			if (t.basic & Int_unsigned)
				return genZeroExt(&p->ir, v.ir, target);
			else
				return genSignExt(&p->ir, v.ir, target);
		}
	}

	// Conversions to and from void*
	if (v.typ.kind == Kind_Pointer && t.kind == Kind_Pointer
			&& (v.typ.pointer->kind == Kind_Void || t.pointer->kind == Kind_Void))
	{
		return v.ir;
	}
	parseerror(p, "could not convert type %s to type %s",
		printTypeHighlighted(p->arena, v.typ), printTypeHighlighted(p->arena, t));
}

static void requiresVersion (Parse *parse, const char *desc, Version v) {
	if (parse->target.version < v)
		parseerror(parse, "%s are only supported with %s, but the current target uses %s",
				desc, version_names[v], version_names[parse->target.version]);
}
