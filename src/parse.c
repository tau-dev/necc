#include "parse.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#include "lex.h"
#include "ir_gen.h"
#include "ansii.h"
#include "backtrace.h"

#define MAX_BACKTRACE 64

StringMap symbols = {0};

typedef LIST(String) Scope;

typedef struct {
	Arena *arena;
	IrBuild ir;
	Function *current_function;
	Scope current_scope;
	const char *code;
	const char *pos;
} Parse;

typedef struct {
	u32 line;
	u32 col;
} SourceLocation;

SourceLocation findSourcePos(const char *source, const char *pos) {
	u32 line = 1;
	u32 col = 1;
	while (source < pos) {
		if (*source == '\n') {
			line++;
			col = 1;
		} else {
			col++;
		}
		source++;
	}
	return (SourceLocation) {line, col};
}

// void printBacktrace(void **addrs, int count) {
// 	char **syms = backtrace_symbols(addrs, count);
// 	for (int i = 0; i < count; i++) {
// 		printf("%s\n", syms[i]);
// 	}
// 	free(syms);
// }

_Noreturn void comperror(const char *source, const char *pos, const char *msg, ...) {
	SourceLocation p = findSourcePos(source, pos);

	printf("%lu:%lu: " RED "error: " RESET,
			(unsigned long) p.line, (unsigned long) p.col);

    va_list args;
    va_start(args, msg);
    vprintf(msg, args);
    va_end(args);
    printf(".\n");
// #ifndef DNDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}

_Noreturn void unexpectedToken(const char *source, const char *pos, Token got, const char *expected) {
	SourceLocation p = findSourcePos(source, pos);

	printf("%lu:%lu: " RED "error: " RESET "expected %s, found %s.\n",
			(unsigned long) p.line, (unsigned long) p.col, expected, tokenName(got.kind));
// #ifndef DNDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}

_Noreturn void parseerror(Parse *parse, const char *msg, ...) {
	SourceLocation p = findSourcePos(parse->code, parse->pos);

	printf("%lu:%lu: " RED "error: " RESET,
			(unsigned long) p.line, (unsigned long) p.col);

    va_list args;
    va_start(args, msg);
    vprintf(msg, args);
    va_end(args);
    printf(".\n");
// #ifndef DNDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}


static Declaration parseDeclaration(Arena *arena, const char *code, const char **pos, Type base_type);
static Type parseTypeBase (const char *code, const char **pos);
static bool tryParseTypeBase (const char **pos, Type *dest);
static void parseFunction(Parse *parse);
static Value rvalue(Value v, Parse *parse);
static Value dereference(IrBuild *ir, Value v);
static IrRef coerce(Value v, Type t, Parse *p);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse);


void parseFile (Arena *arena, const char *code) {
	const char *c = code;
	(void) arena;

	while (peekToken(c).kind != Tok_EOF) {
		Type base_type = parseTypeBase(code, &c);
		Declaration decl = parseDeclaration(arena, code, &c, base_type);

		void **slot = mapGetOrCreate(&symbols, decl.name);
		bool existing = *slot != NULL;
		if (!existing) {
			*slot = ALLOC(arena, Symbol);
			*(Symbol *) *slot = (Symbol) {decl.name};
		}
		Symbol *sym = *slot;

		Token t = getToken(&c);
		if (decl.type.kind == Kind_Function) {
			if (existing) {
				if (sym->kind != Sym_Value || sym->value.typ.kind != Kind_Function)
					comperror(code, c, "previous declaration of identifier was not a function");

				if (!fnTypeEqual(sym->value.typ.function, decl.type.function))
					comperror(code, c, "mismatched declaration");
			} else {
				sym->kind = Sym_Value;
				sym->value.function = ALLOC (arena, Function);
				*sym->value.function = (Function) {decl.name, decl.type.function};
				sym->value.typ = decl.type;
			}

			if (t.kind == Tok_OpenBrace) {
				Function *func = sym->value.function;
				if (func->entry != NULL)
					comperror(code, c, "redefinition of existing function");

				Declaration *old_parameters = func->type.parameters.ptr;
				Declaration *new_parameters = decl.type.function.parameters.ptr;
				size_t param_count = func->type.parameters.len;


				assert(!existing || func->type.parameters.len == decl.type.function.parameters.len);
				for (u32 i = 0; i < param_count; i++) {
					if (new_parameters[i].name.len == 0)
						comperror(code, c, "missing parameter name");

					if (existing && old_parameters[i].name.len != 0 &&
						!SPAN_EQL(old_parameters[i].name, new_parameters[i].name))
					{
						comperror(code, c, "parameter redeclared to different name");
					}
				}

				func->type = decl.type.function;
				sym->value.typ = decl.type;

				Parse parse = {
					.arena = arena,
					.current_function = func,
					.code = code,
					.pos = c,
				};
				parseFunction(&parse);
				func->ir = parse.ir.ir;
				c = parse.pos;
			} else if (t.kind == Tok_Semicolon) {

			} else
				unexpectedToken(code, c, t, "semicolon or function body");
		} else
			comperror(code, c, "TODO globals");
	}
}


Symbol *defSymbol(Parse *parse, String name) {
	PUSH_A(parse->arena, parse->current_scope, name);
	Symbol *sym = malloc(sizeof(Symbol));
	*sym = (Symbol) {name};
	void **slot = mapGetOrCreate(&symbols, name);
	sym->shadowed = *slot;
	*slot = sym;
	return sym;
}

void popScope(Scope names) {
	for (u32 i = 0; i < names.len; i++) {
		void **slot = mapGetOrCreate(&symbols, names.ptr[i]);
		Symbol *s = *slot;
		*slot = s->shadowed;
		free(s);
	}
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
	const char *start = parse->pos;
	Function *func = parse->current_function;
	u32 count = func->type.parameters.len;

	func->entry = ALLOC(parse->arena, Block);
	func->entry = genNewBlockLabeled(parse->arena, &parse->ir, STRING("__entry"));
	parse->current_scope = (Scope) {0};

	for (u32 i = 0; i < count; i++) {
		Declaration param = func->type.parameters.ptr[i];
		Symbol *sym = defSymbol(parse, param.name);
		sym->kind = Sym_Value;
		IrRef slot = genStackAlloc(&parse->ir, param.type);
		sym->value = (Value) {param.type, {{slot, true}}};
		IrRef paramval = genParameter(&parse->ir, param.type);
		genStore(&parse->ir, slot, paramval);
	}

	parseCompound(parse);

	// TODO Warning if this is reachable and return type is not void.
	genReturnVal(&parse->ir, IR_REF_NONE);

	popScope(parse->current_scope);
	// TODO Find and report undefined labels
	for (u32 i = 0; i < func->labels.capacity; i++) {
		Block *b = func->labels.content[i];
		if (b && b->exit.kind == Exit_None)
			comperror(parse->code, start, "a goto references label `%s`, which is not declared in this function", b->label.ptr);
	}
}

static void parseCompound (Parse *parse) {
	const char *block_begin = parse->pos;
	const char **pos = &parse->pos;
	Scope enclosing_scope = parse->current_scope;
	parse->current_scope = (Scope) {0};

	while (true) {
		Token t = peekToken(*pos);
		if (t.kind == Tok_CloseBrace) {
			getToken(pos);
			return;
		} else if (t.kind == Tok_EOF) {
			comperror(parse->code, block_begin, "unclosed " BOLD "{" RESET);
		} else {
			parseStatement(parse);
		}
	}

	popScope(parse->current_scope);
	parse->current_scope = enclosing_scope;
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
	const char **pos = &parse->pos;
// 	const char *begin = *pos;
	const char *peek = parse->pos;
	Token t = getToken(&peek);
	bool labeled = false;
	if (t.kind == Tok_Identifier && getToken(&peek).kind == Tok_Colon) {
		void **slot = mapGetOrCreate(&parse->current_function->labels, t.val.identifier);
		Block *blk = *slot;
		Block *previous = parse->ir.insertion_block;
		genJump(&parse->ir, blk);

		if (blk) {
			if (blk->exit.kind != Exit_None)
				comperror(parse->code, *pos, "redefinition of label `%s`", blk->label.ptr);
			blk->first_inst = parse->ir.ir.len;
			parse->ir.insertion_block = blk;
		} else {
			*slot = genNewBlockLabeled(parse->arena, &parse->ir, t.val.identifier);
			previous->exit.unconditional = *slot;
		}

		labeled = true;
		parse->pos = peek;
	}
	t = peekToken(parse->pos);
	switch (t.kind) {
		case Tok_OpenBrace:
			getToken(pos);
			parseCompound(parse);
			return;
		case Tok_Key_Return:
			getToken(pos);
			// TODO Check against function return type
			if (peekToken(*pos).kind == Tok_Semicolon) {
				getToken(pos);
				genReturnVal(&parse->ir, IR_REF_NONE);
			} else {
				Parse start = *parse;
				genReturnVal(&parse->ir, coerce(
						rvalue(parseExpression(parse), parse),
						*parse->current_function->type.rettype,
						&start));
			}
			// Unreferenced dummy block for further instructions, will be ignored
			genNewBlock(parse->arena, &parse->ir);
			t = getToken(pos);
			if (t.kind != Tok_Semicolon)
				unexpectedToken(parse->code, *pos, t, "semicolon");
			return;

		case Tok_Key_If:
			getToken(pos);
			t = getToken(pos);
			if (t.kind != Tok_OpenParen)
				unexpectedToken(parse->code, *pos, t, "opening paren");
			Parse start = *parse;
			Value condition = rvalue(parseExpression(parse), parse);
			t = getToken(pos);
			if (t.kind != Tok_CloseParen)
				unexpectedToken(parse->code, *pos, t, "closing paren");

			genBranch(&parse->ir, coerce(condition, BASIC_INT, &start));
			Block *head = parse->ir.insertion_block;
			head->exit.branch.on_true = genNewBlock(parse->arena, &parse->ir);
			parseStatement(parse);
			Block *body = parse->ir.insertion_block;
			genJump(&parse->ir, NULL);
			Block *join = genNewBlock(parse->arena, &parse->ir);
			body->exit.unconditional = join;

			if (peekToken(*pos).kind == Tok_Key_Else) {
				getToken(pos);
				head->exit.branch.on_false = genNewBlock(parse->arena, &parse->ir);
				parseStatement(parse);
				genJump(&parse->ir, join);
			} else {
				head->exit.branch.on_false = join;
			}
			parse->ir.insertion_block = join;
			break;
		case Tok_Key_Goto:
			getToken(pos);
			t = getToken(pos);
			if (t.kind != Tok_Identifier)
				unexpectedToken(parse->code, parse->pos, t, "label name");
			String label = t.val.identifier;
			t = getToken(pos);
			if (t.kind != Tok_Semicolon)
				unexpectedToken(parse->code, parse->pos, t, "semicolon");
			genJump(&parse->ir, getLabeledBlock(parse, label));
			// Unreferenced dummy block for further instructions, will be ignored
			genNewBlock(parse->arena, &parse->ir);
			return;
		case Tok_Semicolon:
			return;
		default:;
			Type base_type;
			if (tryParseTypeBase(pos, &base_type)) {
				// PERMIT This is a silly rule.
				if (labeled)
					comperror(parse->code, *pos, "a label may not appear directly before a declaration; you will want to add a semicolon or open a braced block between the two");
				// TODO Structs may be declared without introducing any identifiers.
				while (true) {
					Declaration decl = parseDeclaration(parse->arena, parse->code, pos, base_type);
					Symbol *sym = defSymbol(parse, decl.name);
					sym->kind = Sym_Value;
					sym->value = (Value) {decl.type, {{genStackAlloc(&parse->ir, decl.type), true}}};
					t = getToken(pos);
					if (t.kind == Tok_Equals) {
						Parse start = *parse;
						IrRef val = coerce(rvalue(parseExprAssignment(parse), parse), decl.type, &start);
						genStore(&parse->ir, sym->value.ir, val);
						t = getToken(pos);
					}
					if (t.kind == Tok_Semicolon)
						break;
					else if (t.kind != Tok_Comma)
						unexpectedToken(parse->code, *pos, t, "comma or semicolon");
				}

				return;
			}
			parseExpression(parse);
			Token t = getToken(pos);
			if (t.kind != Tok_Semicolon)
				unexpectedToken(parse->code, *pos, t, "semicolon");
			return;
	}
}

static Value parseExpression (Parse *parse) {
	Value v = parseExprAssignment(parse);

	while (peekToken(parse->pos).kind == Tok_Comma) {
		getToken(&parse->pos);
		v = parseExprAssignment(parse);
	}
	return v;
}

static Value parseExprAssignment (Parse *parse) {
	const char *start = parse->pos;
	Value v = parseExprBitOr(parse);

	switch (peekToken(parse->pos).kind) {
	case Tok_Equals:
		getToken(&parse->pos);
		if (v.typ.kind == Kind_Function)
			comperror(parse->code, start, "cannot assign to a function");
		if (!v.byref)
			comperror(parse->code, start, "cannot assign to rvalue of type %s", printType(parse->arena, v.typ));
		Value assignment = parseExprAssignment(parse);

		genStore(&parse->ir, v.ir, coerce(rvalue(assignment, parse), v.typ, parse));
		return assignment;
	default:
		return v;
	}
}


// TODO All of these should associate left-to-right instead of
// right-to-left; most of them are commutative tho.
static Value parseExprBitOr(Parse *parse) {
	Value lhs = parseExprBitXor(parse);
	if (peekToken(parse->pos).kind == Tok_Pipe) {
		getToken(&parse->pos);
		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, &start);
		IrRef b = coerce(rvalue(parseExprBitOr(parse), parse), BASIC_INT, &start);
		return (Value) {BASIC_INT, {{genOr(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprBitXor(Parse *parse) {
	Value lhs = parseExprBitAnd(parse);
	if (peekToken(parse->pos).kind == Tok_Hat) {
		getToken(&parse->pos);
		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, &start);
		IrRef b = coerce(rvalue(parseExprBitXor(parse), parse), BASIC_INT, &start);
		return (Value) {BASIC_INT, {{genXor(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprBitAnd(Parse *parse) {
	Value lhs = parseExprGreaterLess(parse);
	if (peekToken(parse->pos).kind == Tok_Ampersand) {
		getToken(&parse->pos);
		Parse start = *parse;
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, &start);
		IrRef b = coerce(rvalue(parseExprBitAnd(parse), parse), BASIC_INT, &start);
		return (Value) {BASIC_INT, {{genAnd(&parse->ir, a, b)}}};
	}
	return lhs;
}

static Value parseExprGreaterLess(Parse *parse) {
	Value lhs = parseExprAddition(parse);
	Token t = peekToken(parse->pos);
	Parse start = *parse;
	switch (t.kind) {
	case Tok_Less: {
		getToken(&parse->pos);
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThan(&parse->ir, coerce(lhs, BASIC_INT, &start), coerce(rhs, BASIC_INT, &start))}}};
	}
	case Tok_LessEquals: {
		getToken(&parse->pos);
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(&parse->ir, coerce(lhs, BASIC_INT, &start), coerce(rhs, BASIC_INT, &start))}}};
	}
	case Tok_Greater: {
		getToken(&parse->pos);
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThan(&parse->ir, coerce(rhs, BASIC_INT, &start), coerce(lhs, BASIC_INT, &start))}}};
	}
	case Tok_GreaterEquals: {
		getToken(&parse->pos);
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, {{genLessThanOrEquals(&parse->ir, coerce(rhs, BASIC_INT, &start), coerce(lhs, BASIC_INT, &start))}}};
	}
	default:
		return lhs;
	}
}

static Value parseExprAddition (Parse *parse) {
	const char **pos = &parse->pos;

	Value lhs = parseExprMultiplication(parse);
	Parse start = *parse;
	Token t = peekToken(*pos);
	if (t.kind != Tok_Plus && t.kind != Tok_Minus)
		return lhs;
	lhs = rvalue(lhs, parse);
	getToken(pos);
	Value rhs = rvalue(parseExprMultiplication(parse), parse);


	// TODO Type checking
	if (t.kind == Tok_Plus) {
		if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
			return pointerAdd(&parse->ir, lhs, rhs, &start);
		} else {
			return (Value) {BASIC_INT, {{genAdd(&parse->ir, coerce(lhs, BASIC_INT, &start), coerce(rhs, BASIC_INT, &start))}}};
		}
	} else {
		if (lhs.typ.kind == Kind_Pointer) {
			IrRef stride = genImmediateInt(&parse->ir, typeSize(*lhs.typ.pointer));

			if (rhs.typ.kind == Kind_Pointer) {
				IrRef diff = genSub(&parse->ir, lhs.ir, rhs.ir);
				return (Value) {BASIC_INT, {{genDiv(&parse->ir, diff, stride)}}};
			} else {
				IrRef idx = genMul(&parse->ir, coerce(rhs, BASIC_INT, &start), stride);
				return (Value) {lhs.typ, {{genSub(&parse->ir, lhs.ir, idx)}}};
			}
		} else {
			if (rhs.typ.kind == Kind_Pointer)
				parseerror(&start, "cannot subtract pointer from %s", printType(parse->arena, rhs.typ));
			return (Value) {BASIC_INT, {{genSub(&parse->ir, coerce(lhs, BASIC_INT, &start), coerce(rhs, BASIC_INT, &start))}}};
		}
	}
}

static Value parseExprMultiplication (Parse *parse) {
	const char **pos = &parse->pos;

	Value lhs = parseExprLeftUnary(parse);
	Token t = peekToken(*pos);
	if (t.kind != Tok_Asterisk && t.kind != Tok_Slash)
		return lhs;
	lhs = rvalue(lhs, parse);
	getToken(pos);
	Value rhs = rvalue(parseExprLeftUnary(parse), parse);

	if (t.kind == Tok_Asterisk)
		return (Value) {BASIC_INT, {{genMul(&parse->ir, lhs.ir, rhs.ir)}}};
	else
		return (Value) {BASIC_INT, {{genDiv(&parse->ir, lhs.ir, rhs.ir)}}};
}

static Value parseExprLeftUnary (Parse *parse) {
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
	switch (peekToken(parse->pos).kind) {
	case Tok_Asterisk:
		getToken(&parse->pos);
		return dereference(&parse->ir, parseExprLeftUnary(parse));
	case Tok_Ampersand:
		getToken(&parse->pos);
		const char *start = parse->pos;
		Value v = parseExprLeftUnary(parse);
		// TODO Structs and unions will be handeled byref even if they
		// are not lvalues; mark this somehow.
		if (v.typ.kind == Kind_Function) {
			v.ir = genFunctionRef(&parse->ir, v.function);
			v.typ.kind = Kind_FunctionPtr;
		} else {
			if (!v.byref)
				comperror(parse->code, start, "cannot take address of rvalue of type %s", printType(parse->arena, v.typ));
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
	Value v = parseExprBase(parse);

	while (true) {
		Token t = peekToken(parse->pos);
		if (t.kind == Tok_OpenParen) {
			getToken(&parse->pos);
			Value func = rvalue(v, parse);
			if (func.typ.kind != Kind_FunctionPtr)
				comperror(parse->code, parse->pos, "expected a function type, got an expression of type %s", printType(parse->arena, func.typ));

			IrRefList arguments = {0};
			DeclList params = func.typ.function.parameters;
			if (peekToken(parse->pos).kind != Tok_CloseParen) {
				do {
					Parse start = *parse;
					Value arg = parseExprAssignment(parse);
					arg = rvalue(arg, parse);
					if (arguments.len == params.len)
						parseerror(parse, "too many arguments to function call");
					PUSH_A(parse->arena, arguments, coerce(arg, params.ptr[arguments.len].type, &start));

					t = getToken(&parse->pos);
				} while (t.kind == Tok_Comma);
			}

			if (arguments.len < params.len)
				parseerror(parse, "too few arguments to function call");
			else if (arguments.len > params.len)
				parseerror(parse, "too many arguments to function call");

			if (t.kind != Tok_CloseParen)
				unexpectedToken(parse->code, parse->pos, t, "closing paren");

			ValuesSpan args = {arguments.len, arguments.ptr};
			v = (Value) {
				*func.typ.function.rettype,
				{{genCall(&parse->ir, func.ir, args)}}
			};
		} else if (t.kind == Tok_OpenBracket) {
			getToken(&parse->pos);
			v = rvalue(v, parse);
			Parse start = *parse;
			Value index = rvalue(parseExpression(parse), &start);
			v = dereference(&parse->ir, pointerAdd(&parse->ir, v, index, &start));
		} else {
			break;
		}
	}
	return v;
}

static Value parseExprBase (Parse *parse) {
	Token t = getToken(&parse->pos);
	switch (t.kind) {
	case Tok_OpenParen: {
		Value v = parseExpression(parse);
		if (getToken(&parse->pos).kind != Tok_CloseParen)
			parseerror(parse, "missing closing paren");
		return v;
	}
	case Tok_Integer:
		return (Value) {BASIC_INT, {{genImmediateInt(&parse->ir, t.val.integer)}}};
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
	default:
		unexpectedToken(parse->code, parse->pos, t, "identifier or equivalent inner expression");
	}
}

static Type parseTypeBase (const char *code, const char **pos) {
	Type type;
	const char *start = *pos;
	if (!tryParseTypeBase(pos, &type))
		comperror(code, start, "invalid type");
	return type;
}

static bool tryParseTypeBase(const char **p, Type *type) {
	const char *pos = *p;
	Token t = getToken(&pos);
	if (t.kind != Tok_Identifier || !eql("int", t.val.identifier))
		return false;
	*type = BASIC_INT;
	*p = pos;
	return true;
}


u8 parseQualifiers (const char *code, const char **pos) {
	(void) code;
	(void) pos;
	return 0;
}

Declaration parseDeclaration (Arena *arena, const char *code, const char **pos, Type base_type) {
	Declaration decl = {.type = base_type};
	Token t = getToken(pos);

	while (t.kind == Tok_Asterisk) {
		decl.type.qualifiers |= parseQualifiers(code, pos);
		Type *ptr = ALLOC(arena, Type);
		*ptr = decl.type;
		decl.type.kind = Kind_Pointer;
		decl.type.pointer = ptr;
		t = getToken(pos);
	}

	Type *innermost_nested = &decl.type;
	if (t.kind != Tok_Identifier)
		unexpectedToken(code, *pos, t, "identifier");
	decl.name = t.val.identifier;
	t = peekToken(*pos);

	while (true) {
		if (t.kind == Tok_OpenBracket) {
			getToken(pos);
			Type *new_innermost = ALLOC(arena, Type);
			*new_innermost = *innermost_nested;
			innermost_nested->kind = Kind_Array;
			ArrayType *array = &innermost_nested->array;
			innermost_nested = new_innermost;

			array->inner = new_innermost;
			t = getToken(pos);
			if (t.kind != Tok_Integer)
				unexpectedToken(code, *pos, t, "array length");
			array->count = t.val.integer;
		} else if (t.kind == Tok_OpenParen) {
			getToken(pos);

			Type *new_innermost = ALLOC(arena, Type);
			*new_innermost = *innermost_nested;
			innermost_nested->kind = Kind_Function;
			FunctionType *func = &innermost_nested->function;
			innermost_nested = new_innermost;

			func->rettype = new_innermost;
			func->parameters = (DeclList) MAKE_LIST(Declaration);

			if (peekToken(*pos).kind != Tok_CloseParen) {
				do {
					Type param_type = parseTypeBase(code, pos);
					PUSH(func->parameters, parseDeclaration(arena, code, pos, param_type));
					t = getToken(pos);
				} while (t.kind == Tok_Comma);
			}
			if (t.kind != Tok_CloseParen)
				unexpectedToken(code, *pos, t, "closing paren");
		} else {
			break;
		}
		t = peekToken(*pos);
	}
// 	SourceLocation p = findSourcePos(code, *pos);
// 	printf("declaration ended on %lu:%lu\n", (unsigned long) p.line, (unsigned long) p.col);

	return decl;
}


static Value pointerAdd(IrBuild *ir, Value lhs, Value rhs, Parse *op_parse) {
	assert(lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer);
	if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer)
		parseerror(op_parse, "cannot add two pointers");
	Value ptr;
	IrRef integer;
	if (lhs.typ.kind == Kind_Pointer) {
		ptr = lhs;
		integer = coerce(rhs, BASIC_INT, op_parse);
	} else {
		integer = coerce(lhs, BASIC_INT, op_parse);
		ptr = rhs;
	}
	IrRef stride = genImmediateInt(ir, typeSize(*ptr.typ.pointer));
	IrRef diff = genMul(ir, stride, integer);
	return (Value) {ptr.typ, {{genAdd(ir, ptr.ir, diff)}}};
}

static Value dereference(IrBuild *ir, Value v) {
	assert(v.typ.kind == Kind_Pointer);
	if (v.byref)
		v.ir = genLoad(ir, v.ir, typeSize(v.typ));
	v.typ = *v.typ.pointer;
	v.byref = true;
	return v;
}

Value rvalue(Value v, Parse *parse) {
	if (v.typ.kind == Kind_Function) {
		v.ir = genFunctionRef(&parse->ir, v.function);
		v.typ.kind = Kind_FunctionPtr;
	} else if (v.byref) {
		v.ir = genLoad(&parse->ir, v.ir, typeSize(v.typ));
		v.byref = false;
	}
	return v;
}

IrRef coerce(Value v, Type t, Parse *p) {
	if (!typeEqual(v.typ, t)) {
		comperror(p->code, p->pos, "could not convert type %s to type %s.", printType(p->arena, v.typ), printType(p->arena, t));
	}
	return v.ir;
}


