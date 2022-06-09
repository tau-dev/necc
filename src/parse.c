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

typedef struct {
	Arena *arena;
	IrBuild ir;
	Function *current_function;
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
#ifndef DNDEBUG
	PRINT_STACK_TRACE;
#endif
	exit(1);
}

_Noreturn void unexpectedToken(const char *source, const char *pos, Token got, const char *expected) {
	SourceLocation p = findSourcePos(source, pos);

	printf("%lu:%lu: " RED "error: " RESET "expected %s, found %s.\n",
			(unsigned long) p.line, (unsigned long) p.col, expected, tokenName(got.kind));
#ifndef DNDEBUG
	PRINT_STACK_TRACE;
#endif
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
#ifndef DNDEBUG
	PRINT_STACK_TRACE;
#endif
	exit(1);
}


Declaration parseDeclaration(Arena *arena, const char *code, const char **pos, Type base_type);
Type parseType (const char *code, const char **pos);
void parseFunction(Parse *parse);
IrRef lvalue(Value v);
Value rvalue(Value v, Parse *parse);
IrRef coerce(Value v, Type t, Parse *p);
bool fnTypeEqual(FunctionType a, FunctionType b);
bool typeEqual(Type a, Type b);
char *printType(Arena *arena, Type t);


void parseFile (Arena *arena, const char *code) {
	const char *c = code;
	(void) arena;

	while (peekToken(c).kind != Tok_EOF) {
		Type base_type = parseType(code, &c);
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
				*sym->value.function = (Function) {decl.type.function, .name = decl.name};
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

typedef LIST(String) Scope;

Symbol *defSymbol(Scope *scope, String name) {
	PUSH(*scope, name);
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

void parseCompound(Parse *parse);

void parseFunction (Parse *parse) {
	Function *func = parse->current_function;
	u32 count = func->type.parameters.len;
	Scope param_names = {0};

	func->entry = ALLOC(parse->arena, Block);
	*func->entry = (Block) {
		.exit.kind = Exit_None,
	};
	parse->ir.insertion_block = func->entry;

	for (u32 i = 0; i < count; i++) {
		Declaration param = func->type.parameters.ptr[i];
		Symbol *sym = defSymbol(&param_names, param.name);
		sym->kind = Sym_Value;
		sym->value = (Value) {param.type, {{genParameter(&parse->ir, param.type), false}}};
	}

	parseCompound(parse);

	// TODO Find and report undefined labels
	popScope(param_names);
	free(param_names.ptr);
// 	return block;
}

void parseStatement(Parse *parse);

void parseCompound (Parse *parse) {
	const char *block_begin = parse->pos;
	const char **pos = &parse->pos;
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
}

static Value parseExpression(Parse *parse);
Value parseExprAddition(Parse *parse);
Value parseExprMultiplication(Parse *parse);
Value parseExprLeftUnary(Parse *parse);
Value parseExprRightUnary(Parse *parse);
Value parseExprBase(Parse *parse);


void parseStatement (Parse *parse) {
	const char **pos = &parse->pos;
// 	const char *begin = *pos;
	Token t = peekToken(*pos);
	switch (t.kind) {
		case Tok_Key_Return:
			getToken(pos);
			// TODO Check against function return type
			if (peekToken(*pos).kind == Tok_Semicolon) {
				getToken(pos);
				genReturnVal(&parse->ir, IR_REF_NONE);
			} else {
				genReturnVal(&parse->ir, parseExpression(parse).ir);
			}
			t = getToken(pos);
			if (t.kind != Tok_Semicolon)
				unexpectedToken(parse->code, parse->pos, t, "semicolon");
			return;

// 		case Tok_Identifier:
// 			String identifier = t.val.identifier:
// 			t = getToken(pos);
// 			switch (t.kind) {
// 				case Tok_Equals:

// 					return;
// 			}
// 			return;
		case Tok_Key_If:
			getToken(pos);
			t = getToken(pos);
			Value v = parseExpression(parse);
			(void) v;
			// ...
			break;
		default:
			parseExpression(parse);
			Token t = getToken(pos);
			if (t.kind != Tok_Semicolon)
				unexpectedToken(parse->code, parse->pos, t, "semicolon");
			return;
	}
}

static Value parseExpression (Parse *parse) {
	return parseExprAddition(parse);
}

Value parseExprAddition (Parse *parse) {
	const char **pos = &parse->pos;

	Value lhs = parseExprMultiplication(parse);
	Token t = peekToken(*pos);
	if (t.kind != Tok_Plus && t.kind != Tok_Minus)
		return lhs;
	lhs = rvalue(lhs, parse);
	getToken(pos);
	Value rhs = rvalue(parseExprMultiplication(parse), parse);

	// TODO Type checking
	if (t.kind == Tok_Plus)
		return (Value) {BASIC_INT, {{genAdd(&parse->ir, lhs.ir, rhs.ir)}}};
	else
		return (Value) {BASIC_INT, {{genSub(&parse->ir, lhs.ir, rhs.ir)}}};
}

Value parseExprMultiplication (Parse *parse) {
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

Value parseExprLeftUnary (Parse *parse) {
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
	return parseExprRightUnary(parse);
}

Value parseExprRightUnary (Parse *parse) {
	Value v = parseExprBase(parse);
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
				Value arg = parseExpression(parse);
				arg = rvalue(arg, parse);
				if (arguments.len == params.len)
					parseerror(parse, "too many arguments to function call");
				PUSH_A(parse->arena, arguments, coerce(arg, params.ptr[arguments.len].type, parse));

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
		return (Value) {
			*func.typ.function.rettype,
			{{genCall(&parse->ir, func.ir, args)}}
		};
	}
	return v;
}

Value parseExprBase (Parse *parse) {
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
			parseerror(parse, "expected value, got type");
		return sym->value;
	}
	default:
		unexpectedToken(parse->code, parse->pos, t, "identifier or equivalent inner expression");
	}
}

Type parseType (const char *code, const char **pos) {
	Token t = getToken(pos);
	if (t.kind != Tok_Identifier || !eql("int", t.val.identifier))
		comperror(code, *pos, "unknown type");
	return BASIC_INT;
}

u8 parseQualifiers (const char *code, const char **pos) {
	(void) code;
	(void) pos;
	return 0;
}
Type parseTypeSpecifiersQualifiers (const char *code, const char **pos) {
	Type base = parseType (code, pos);
	// TODO extern etc.
	return base;
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
					Type param_type = parseType(code, pos);
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

bool fnTypeEqual(FunctionType a, FunctionType b) {
	if (!typeEqual(*a.rettype, *b.rettype))
		return false;
	if (a.parameters.len != b.parameters.len)
		return false;

	for (u32 u = 0; u < a.parameters.len; u++)
		if (!typeEqual(a.parameters.ptr[u].type, b.parameters.ptr[u].type))
			return false;
	return true;
}

bool typeEqual(Type a, Type b) {
	if (a.kind != b.kind)
		return false;

	switch (a.kind) {
	case Kind_Function:
	case Kind_FunctionPtr:
		return fnTypeEqual(a.function, b.function);
	case Kind_Pointer:
		return typeEqual(*a.pointer, *b.pointer);
	case Kind_Basic:
		return a.basic == b.basic;
	default:
		printf("could not compare types, assuming equal\n");
		return true;
	}
}

IrRef lvalue(Value v) {
	if (v.typ.kind == Kind_Function || !v.byref)
		return IR_REF_NONE;
	return v.ir;
}

Value rvalue(Value v, Parse *parse) {
	if (v.typ.kind == Kind_Function) {
		v.ir = genFunctionRef(&parse->ir, v.function);
		v.typ.kind = Kind_FunctionPtr;
	} else if (v.byref) {
		v.ir = genLoad(&parse->ir, v.ir);
		v.byref = false;
	}
	return v;
}

IrRef coerce(Value v, Type t, Parse *p) {
	if (!typeEqual(v.typ, t)) {
		parseerror(p, "could not convert type %s to type %s.", printType(p->arena, v.typ), printType(p->arena, t));
	}
	return v.ir;
}


void printto(char **insert, const char *end, char *fmt, ...) {
	if (*insert >= end)
		return;
	va_list args;
    va_start(args, fmt);
    int count = vsnprintf(*insert, end-*insert, fmt, args);
    va_end(args);
    if (count < 0) {
    	perror(NULL);
    	exit(1);
    }
    *insert += count;
}

#define MAX_TYPE_STRING_LENGTH 1024

char *printDeclaration(Arena *arena, Type t, String name) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	char *end = string + MAX_TYPE_STRING_LENGTH;

	printTypeBase(t, &pos, end);
	printto(&pos, end, " ");
	printTypeHead(t, &pos, end);
	if (name.len > 0 && pos + name.len + 1 < end) {
		memcpy(pos, name.ptr, name.len);
		pos += name.len;
	}
	printTypeTail(t, &pos, end);

	return string;
}
char *printType(Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	char *end = string + MAX_TYPE_STRING_LENGTH;

	printTypeBase(t, &pos, end);
	if (t.kind != Kind_Basic && t.kind != Kind_Void && t.kind != Kind_Struct && t.kind != Kind_Struct) {
		printto(&pos, end, " ");
		printTypeHead(t, &pos, end);
		printTypeTail(t, &pos, end);
	}

	return string;
}

void printTypeBase(Type t, char **pos, char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		printTypeBase(*t.pointer, pos, end); break;
	case Kind_Array:
		printTypeBase(*t.array.inner, pos, end); break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printTypeBase(*t.function.rettype, pos, end); break;
	case Kind_Void:
		printto(pos, end, "void");
		break;
	case Kind_Basic:
		switch (t.basic) {
		case Basic_int:
			printto(pos, end, "int");
			break;
		}
		break;
	case Kind_Struct:
		printto(pos, end, "struct {???}");
		break;
	case Kind_Enum:
		printto(pos, end, "enum {???}");
		break;
	}
}

void printTypeHead(Type t, char **pos, char *end) {
	// Is this quite right?
	if (t.storage == Storage_Extern)
		printto(pos, end, "static ");
	if (t.storage == Storage_Static)
		printto(pos, end, "static ");

	if (t.qualifiers & Qualifier_Const)
		printto(pos, end, "const ");
	if (t.qualifiers & Qualifier_Atomic)
		printto(pos, end, "atomic ");
	if (t.qualifiers & Qualifier_Volatile)
		printto(pos, end, "volatile ");
	if (t.qualifiers & Qualifier_Restrict)
		printto(pos, end, "restrict ");

	switch (t.kind) {
	case Kind_Pointer:
		printTypeHead(*t.pointer, pos, end);
		if (t.pointer->kind == Kind_Function || t.pointer->kind == Kind_Pointer)
			printto(pos, end, "(");
		printto(pos, end, "*");
		break;
	case Kind_Array:
		printTypeHead(*t.array.inner, pos, end);
		break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printTypeHead(*t.function.rettype, pos, end);
		break;
	default: {}
	}
}

void printTypeTail(Type t, char **pos, char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		if (t.pointer->kind == Kind_Function || t.pointer->kind == Kind_Pointer)
			printto(pos, end, ")");
		printTypeTail(*t.pointer, pos, end);
		break;
	case Kind_Array:
		printto(pos, end, "[%lu]", (unsigned long) t.array.count);
		printTypeTail(*t.array.inner, pos, end);
		break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printto(pos, end, "(");
		for (size_t i = 0; i < t.function.parameters.len; i++) {
			printTypeBase(t.function.parameters.ptr[i].type, pos, end);
			String name = t.function.parameters.ptr[i].name;
			if (name.len > 0 && *pos + name.len + 1 < end) {
				printto(pos, end, " ");
				printTypeHead(t.function.parameters.ptr[i].type, pos, end);
				memcpy(*pos, name.ptr, name.len);
				*pos += name.len;
			} else {
				printTypeHead(t.function.parameters.ptr[i].type, pos, end);
			}
			printTypeTail(t.function.parameters.ptr[i].type, pos, end);
			if (i + 1 < t.function.parameters.len)
				printto(pos, end, ", ");
		}
		printto(pos, end, ")");
		printTypeTail(*t.function.rettype, pos, end);
		break;
	default: {}
	}
}

