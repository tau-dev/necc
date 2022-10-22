#include "parse.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#include "ir_gen.h"
#include "ansii.h"
#include "backtrace.h"

#define MAX_BACKTRACE 64

// TODO Move this into Parse
StringMap symbols = {0};

typedef LIST(String) Scope;

typedef struct {
	Arena *arena;
	IrBuild ir;
	Function *current_function;
	Scope current_scope;
	const Tokenization *tokens;
	const Token *pos;
	const Target *target;
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
	u32 idx = p->pos - p->tokens->tokens;
	TokenPosition pos = p->tokens->positions[idx];
	SourceFile source = *p->tokens->files.ptr[pos.source_file_ref];

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


static Declaration parseDeclaration(Arena *arena, const Tokenization *tokens, const Token **tok, Type base_type);
// static Declaration parseDeclaration (Parse* parse, const Token **tok, Type base_type);
static Type parseTypeBase (const Tokenization *tokens, const Token **tok);
static bool tryParseTypeBase (const Token **pos, Type *dest);
static void parseFunction(Parse *parse);
static Value rvalue(Value v, Parse *parse);
static Value dereference (Parse *parse, Value v);
static IrRef coerce(Value v, Type t, Parse *p);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse);


void parse (Arena *arena, Tokenization tokens, Target target) {
	(void) arena;
	const Token *t = tokens.tokens;

	while (t->kind != Tok_EOF) {
		Type base_type = parseTypeBase(&tokens, &t);
		Declaration decl = parseDeclaration(arena, &tokens, &t, base_type);

		void **slot = mapGetOrCreate(&symbols, decl.name);
		bool existing = *slot != NULL;
		if (!existing) {
			*slot = ALLOC(arena, Symbol);
			*(Symbol *) *slot = (Symbol) {decl.name};
		}
		Symbol *sym = *slot;

		if (decl.type.kind == Kind_Function) {
			if (existing) {
				if (sym->kind != Sym_Value || sym->value.typ.kind != Kind_Function)
					comperror(&tokens, t, "previous declaration of identifier was not a function");

				if (!fnTypeEqual(sym->value.typ.function, decl.type.function))
					comperror(&tokens, t, "mismatched declaration");
			} else {
				sym->kind = Sym_Value;
				sym->value.function = ALLOC (arena, Function);
				*sym->value.function = (Function) {decl.name, decl.type.function};
				sym->value.typ = decl.type;
			}

			if (t->kind == Tok_OpenBrace) {
				Function *func = sym->value.function;
				if (func->entry != NULL)
					comperror(&tokens, t, "redefinition of existing function");

				Declaration *old_parameters = func->type.parameters.ptr;
				Declaration *new_parameters = decl.type.function.parameters.ptr;
				size_t param_count = func->type.parameters.len;


				assert(!existing || func->type.parameters.len == decl.type.function.parameters.len);
				for (u32 i = 0; i < param_count; i++) {
					if (new_parameters[i].name.len == 0)
						comperror(&tokens, t, "missing parameter name");

					if (existing && old_parameters[i].name.len != 0 &&
						!SPAN_EQL(old_parameters[i].name, new_parameters[i].name))
					{
						comperror(&tokens, t, "parameter redeclared to different name");
					}
				}

				func->type = decl.type.function;
				sym->value.typ = decl.type;

				Parse parse = {
					.arena = arena,
					.current_function = func,
					.tokens = &tokens,
					.pos = t,
					.target = &target,
				};
				parseFunction(&parse);
				func->ir = parse.ir.ir;
				t = parse.pos;
			} else if (t->kind == Tok_Semicolon) {

			} else
				comperror(&tokens, t, "expected %s or function body", tokenName(Tok_Semicolon));
		} else
			comperror(&tokens, t, "TODO globals");
	}
}


Symbol *defSymbol (Parse *parse, String name) {
	PUSH_A(parse->arena, parse->current_scope, name);
	Symbol *sym = malloc(sizeof(Symbol));
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
		IrRef slot = genStackAlloc(&parse->ir, typeSize(param.type, parse->target));
		sym->value = (Value) {param.type, {{slot, true}}};
		IrRef paramval = genParameter(&parse->ir, typeSize(param.type, parse->target));
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
			if (tryParseTypeBase(&parse->pos, &base_type)) {
				// PERMIT This is a silly rule.
				if (labeled)
					parseerror(parse, "a label may not appear directly before a declaration; you will want to add a semicolon between the two or put the declaration into a braced block");
				// TODO Structs may be declared without introducing any identifiers.
				do {
					Declaration decl = parseDeclaration(parse->arena, parse->tokens, &parse->pos, base_type);
					Symbol *sym = defSymbol(parse, decl.name);
					sym->kind = Sym_Value;
					sym->value = (Value) {decl.type, {{genStackAlloc(build, typeSize(decl.type, parse->target)), true}}};

					if (parse->pos->kind == Tok_Equals) {
						parse->pos++;
						IrRef val = coerce(rvalue(parseExprAssignment(parse), parse), decl.type, parse);
						genStore(build, sym->value.ir, val);
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
	Value v = parseExprAssignment(parse);

	while (parse->pos->kind == Tok_Comma) {
		parse->pos++;
		v = parseExprAssignment(parse);
	}
	return v;
}

static Value parseExprAssignment (Parse *parse) {
	Value v = parseExprBitOr(parse);

	switch (parse->pos->kind) {
	case Tok_Equals:
		if (v.typ.kind == Kind_Function)
			parseerror(parse, "cannot assign to a function");
		if (!v.byref)
			parseerror(parse, "cannot assign to rvalue of type %s", printType(parse->arena, v.typ));

		parse->pos++;
		Value assigned_val = parseExprAssignment(parse);

		genStore(&parse->ir, v.ir, coerce(rvalue(assigned_val, parse), v.typ, parse));
		return assigned_val;
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
		return (Value) {BASIC_INT, {{genLessThan(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))}}};
	}
	case Tok_LessEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))}}};
	}
	case Tok_Greater: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThan(build, coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse))}}};
	}
	case Tok_GreaterEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(build, coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse))}}};
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
					typeSize(*lhs.typ.pointer, parse->target),
				typeSize(parse->target->ptrdiff, parse->target));

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
	/* TODO
	++
	--
	+
	-
	!
	~
	sizeof		[before cast: https://en.cppreference.com/w/c/language/operator_precedence#cite_note-2]
	(type)
	*
	&
	_Alignof
	*/
	switch (parse->pos->kind) {
	case Tok_DoublePlus:
	case Tok_DoubleMinus: {
		parse->pos++;
		Value v = parseExprLeftUnary(parse);
		if (v.typ.kind == Kind_Function)
			parseerror(parse, "cannot modify a function");
		if (!v.byref)
			parseerror(parse, "cannot modify a %s%s%s rvalue", BOLD, printType(parse->arena, v.typ), RESET);
		Value rval = (Value) {v.typ, {{genLoad(build, v.ir, typeSize(v.typ, parse->target))}}};

		const int delta = parse->pos->kind == Tok_DoublePlus ? 1 : -1;
		Value one = {v.typ, {{genImmediateInt(build, delta, typeSize(v.typ, parse->target))}}};

		Value result;
		if (v.typ.kind == Kind_Pointer)
			result = pointerAdd(build, rval, one, parse);
		else
			result = (Value) {v.typ, {{genAdd(build, rval.ir, one.ir)}}};

		genStore(build, v.ir, result.ir);
		return result;
	} break;
	case Tok_Asterisk:
		parse->pos++;
		return dereference(parse, parseExprLeftUnary(parse));
	case Tok_Ampersand:
		parse->pos++;
		Value v = parseExprLeftUnary(parse);
		// TODO Structs and unions will be handeled byref even if they
		// are not lvalues; mark this somehow.
		if (v.typ.kind == Kind_Function) {
			v.ir = genFunctionRef(build, v.function);
			v.typ.kind = Kind_FunctionPtr;
		} else {
			if (!v.byref)
				parseerror(parse, "cannot take address of a %s%s%s rvalue", BOLD, printType(parse->arena, v.typ), RESET);
			Type *pointee = ALLOC(parse->arena, Type);
			*pointee = v.typ;
			v.typ = (Type) {Kind_Pointer, .pointer = pointee};
		}
		v.byref = false;
		return v;
	default:
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
				parseerror(parse, "expected a function type, got an expression of type %s%s%s", BOLD, printType(parse->arena, func.typ), RESET);

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
			if (!v.byref)
				parseerror(parse, "cannot modify a %s%s%s rvalue", BOLD, printType(parse->arena, v.typ), RESET);
			Value rval = (Value) {v.typ, {{genLoad(build, v.ir, typeSize(v.typ, parse->target))}}};

			Value one = {v.typ, {{genImmediateInt(build, delta, typeSize(v.typ, parse->target))}}};

			IrRef result;
			if (v.typ.kind == Kind_Pointer)
				result = pointerAdd(build, rval, one, parse).ir;
			else
				result = genAdd(build, rval.ir, one.ir);

			genStore(build, v.ir, result);
			v = rval;
		} else {
			break;
		}
	}
	return v;
}

static Value parseExprBase (Parse *parse) {
	IrBuild *build = &parse->ir;
	Token t = *parse->pos;
	parse->pos++;
	switch (t.kind) {
	case Tok_OpenParen: {
		Value v = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		return v;
	}
	case Tok_Integer:
		return (Value) {BASIC_INT, {{genImmediateInt(build, t.val.integer, typeSize(BASIC_INT, parse->target))}}};
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
	case Tok_Key_Sizeof: {
		Type typ;
		if (!tryParseTypeBase(&parse->pos, &typ)) {
			Block *current = parse->ir.insertion_block;
			// Emit the expression into a block which is then discarded. Pretty hacky.
			genNewBlock(parse->arena, build);
			typ = parseExprBase(parse).typ;
			parse->ir.insertion_block = current;
		}
		return (Value) {BASIC_INT, {{genImmediateInt(build, typeSize(typ, parse->target), typeSize(BASIC_INT, parse->target))}}};
	}
	default:
		parseerror(parse, "expected an expression");
	}
}

static Type parseTypeBase (const Tokenization *tokens, const Token **tok) {
	Type type;
	if (!tryParseTypeBase(tok, &type))
		comperror(tokens, *tok, "invalid type");
	return type;
}

static bool tryParseTypeBase (const Token **tok, Type *type) {
	const Token *t = *tok;
	if (t->kind == Tok_Identifier) {
		if (eql("int", t->val.identifier)) {
			*type = BASIC_INT;
			(*tok)++;
			return true;
		}
		if (eql("char", t->val.identifier)) {
			*type = (Type) {Kind_Basic, .basic = Int_char};
			(*tok)++;
			return true;
		}
	}
	return false;
}

u8 parseQualifiers (const Tokenization *tokens, const Token **tok) {
	(void) tokens;
	(void) tok;
	return 0;
}

static Declaration parseDeclaration (Arena *arena, const Tokenization *tokens, const Token **pos, Type base_type) {
	Declaration decl = {.type = base_type};
	const Token *t = *pos;

	while (t->kind == Tok_Asterisk) {
		decl.type.qualifiers |= parseQualifiers(tokens, &t);
		Type *ptr = ALLOC(arena, Type);
		*ptr = decl.type;
		decl.type.kind = Kind_Pointer;
		decl.type.pointer = ptr;
		t++;
	}

	Type *innermost_nested = &decl.type;
	if (t->kind != Tok_Identifier)
		comperror(tokens, t, "expected an identifier");
	decl.name = t->val.identifier;
	t++;

	while (true) {
		if (t->kind == Tok_OpenBracket) {
			t++;
			Type *new_innermost = ALLOC(arena, Type);
			*new_innermost = *innermost_nested;
			innermost_nested->kind = Kind_Array;
			ArrayType *array = &innermost_nested->array;
			innermost_nested = new_innermost;

			array->inner = new_innermost;
			if (t->kind != Tok_Integer)
				comperror(tokens, t, "array length");
			array->count = t->val.integer;
			t++;
			if (t->kind != Tok_CloseBracket)
				comperror(tokens, t, "closing bracket [TODO]");
			t++;
		} else if (t->kind == Tok_OpenParen) {
			t++;
			Type *new_innermost = ALLOC(arena, Type);
			*new_innermost = *innermost_nested;
			innermost_nested->kind = Kind_Function;
			FunctionType *func = &innermost_nested->function;
			innermost_nested = new_innermost;

			func->rettype = new_innermost;
			func->parameters = (DeclList) MAKE_LIST(Declaration);

			if (t->kind != Tok_CloseParen) {
				while (true) {
					Type param_type = parseTypeBase(tokens, &t);
					PUSH(func->parameters, parseDeclaration(arena, tokens, &t, param_type));
					if (t->kind == Tok_Comma)
						t++;
					else
						break;
				}
			}
			if (t->kind != Tok_CloseParen)
				comperror(tokens, t, "expected closing paren");
			t++;
		} else {
			break;
		}
	}
// 	SourceLocation p = findSourcePos(code, *pos);
// 	printf("declaration ended on %lu:%lu\n", (unsigned long) p.line, (unsigned long) p.col);
	*pos = t;
	return decl;
}


static Value pointerAdd (IrBuild *ir, Value lhs, Value rhs, Parse *op_parse) {
	assert(lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer);
	if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer)
		parseerror(op_parse, "cannot add two pointers");
	Value ptr;
	IrRef integer;
	if (lhs.typ.kind == Kind_Pointer) {
		ptr = lhs;
		integer = coerce(rhs, op_parse->target->ptrdiff, op_parse);
	} else {
		integer = coerce(lhs, op_parse->target->ptrdiff, op_parse);
		ptr = rhs;
	}
	IrRef stride = genImmediateInt(ir,
			typeSize(*ptr.typ.pointer, op_parse->target),
		typeSize(op_parse->target->ptrdiff, op_parse->target));
	IrRef diff = genMul(ir, stride, integer);
	return (Value) {ptr.typ, {{genAdd(ir, ptr.ir, diff)}}};
}

static Value dereference (Parse *parse, Value v) {
	assert(v.typ.kind == Kind_Pointer);
	if (v.byref)
		v.ir = genLoad(&parse->ir, v.ir, typeSize(v.typ, parse->target));
	v.typ = *v.typ.pointer;
	v.byref = true;
	return v;
}

Value rvalue (Value v, Parse *parse) {
	if (v.typ.kind == Kind_Function) {
		v.ir = genFunctionRef(&parse->ir, v.function);
		v.typ.kind = Kind_FunctionPtr;
	} else if (v.byref) {
		v.ir = genLoad(&parse->ir, v.ir, typeSize(v.typ, parse->target));
		v.byref = false;
	}
	return v;
}

IrRef coerce (Value v, Type t, Parse *p) {
	if (!typeEqual(v.typ, t)) {
		parseerror(p, "could not convert type %s%s%s to type %s%s%s",
			BOLD, printType(p->arena, v.typ), RESET, BOLD, printType(p->arena, t), RESET);
	}
	return v.ir;
}


