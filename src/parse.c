#include "parse.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <signal.h>

#include "ir_gen.h"
#include "ansii.h"
#include "dbgutils.h"


// TODO Move this into Parse
StringMap symbols = {0};

typedef LIST(Symbol*) Scope;

typedef struct {
	Arena *arena;
	const Tokenization tokens;
	const Token *pos;
	const Target target;

	u32 scope_depth;
	// At the top level, these may be null.
	Scope current_scope;

	FunctionType current_func_type;
	IrBuild build;
	Scope func_goto_labels;
	Block *current_loop_head;
	Block *current_loop_switch_exit;

	Module *module;
} Parse;


const char *plxz(const Parse *parse) {
	return lexz(parse->tokens, NULL, parse->pos, 12);
}


typedef LIST(Reference) RefsList;
typedef struct {
	u32 offset;
	Type type;

	IrRef address;

	RefsList *reloc_references;
	SPAN(char) reloc_data;
} InitializationDest;

static void vcomperror (Log level, const Tokenization *t, const Token *tok, const Token *parse_pos, const char *msg, va_list args) {
	(void) parse_pos;
	u32 idx = tok - t->tokens;
	TokenPosition pos = t->positions[idx];
	SourceFile source = *t->files.ptr[pos.source_file_ref];

    printMsg(level, source, pos.source_file_offset);

    vfprintf(stderr, msg, args);
    fprintf(stderr, ".\n");
	if (pos.macro_file_offset) {
    	printInfo(*t->files.ptr[pos.macro_file_ref], pos.macro_file_offset);
    	fprintf(stderr, "(macro-expanded from here)\n");
	}

// #ifndef NDEBUG
// 	printf("TOKEN STREAM:\n\t");
// 	puts(lexz(*t, tok, parse_pos, 12));
// #endif

	if (level & Log_Fatal) {
#ifndef NDEBUG
		raise(SIGTRAP);
#endif
		exit(1);
	}
}

_Noreturn void comperror (const Tokenization *t, const Token *tok, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vcomperror(Log_Err | Log_Fatal, t, tok, NULL, msg, args);
    va_end(args);
    exit(1);
}

_Noreturn void parseerror (const Parse *p, const Token *main, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vcomperror(Log_Err | Log_Fatal, &p->tokens, main ? main : p->pos-1, p->pos, msg, args);
    va_end(args);
    exit(1);
}


void parsemsg (Log level, const Parse *p, const Token *main, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vcomperror(level, &p->tokens, main ? main : p->pos, p->pos, msg, args);
    va_end(args);
}


_Noreturn void unexpectedToken (const Parse *p, TokenKind expected) {
	parseerror(p, p->pos, "expected %s before the %s token", tokenName(expected), tokenName(p->pos->kind));
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



static Symbol *getSymbol (Parse *, String name);
static Declaration parseDeclarator(Parse *, Type base_type);
static nodiscard bool allowedNoDeclarator(Parse *, Type base_type);
// static Declaration parseDeclarator (Parse* parse, const Token **tok, Type base_type);
static void parseInitializer(Parse *, InitializationDest);
static Type parseValueType (Parse *, Value (*operator)(Parse *));
static Type parseTypeBase(Parse *, u8 *storage);
static Type parseTypeName(Parse *, u8 *storage);
static nodiscard bool tryParseTypeBase(Parse *, Type *dest, u8 *storage);
static nodiscard bool tryParseTypeName(Parse *, Type *dest, u8 *storage);
static void parseFunction(Parse *);
static Value rvalue(Value v, Parse *);
static Value dereference (Parse *, Value v);
static IrRef coerce(Value v, Type t, Parse *);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse, const Token *);
static void parseTypedefDecls(Parse *, Type base_type);
static Attributes parseAttributes(Parse *);
static void popScope(Parse *, Scope);
static nodiscard Scope pushScope(Parse *);

// Parses all top level declarations into a Module.
void parse (Arena *arena, Tokenization tokens, Target target, Module *module) {
	// Used only as a scratch buffer for constructing constants at the
	// top level. Kind of inefficient, but very straightforward.
	IrBuild global_ir = {0};
	startNewBlock(&global_ir, arena, zString("dummy"));

	Parse parse = {
		.arena = arena,
		.tokens = tokens,
		.target = target,
		.pos = tokens.tokens,
		.module = module,
	};

	while (parse.pos->kind != Tok_EOF) {
		u8 storage;
		const Token *type_token = parse.pos;
		Type base_type = parseTypeBase(&parse, &storage);

		if (storage == Storage_Typedef) {
			parseTypedefDecls(&parse, base_type);
			continue;
		}

		if (storage == Storage_Auto)
			parseerror(&parse, type_token, "top-level symbol can not have automatic storage duration");
		bool public_linkage = storage != Storage_Static;

		if (allowedNoDeclarator(&parse, base_type))
			continue;

		u32 declarators = 0;
		while (true) {
			const Token *primary = parse.pos;
			Declaration decl = parseDeclarator(&parse, base_type);

			Symbol *sym = getSymbol(&parse, decl.name);
			OrdinaryIdentifier *existing = sym->ordinary;

			if (existing) {
				assert(existing->scope_depth == 0);
				if (existing->kind != Sym_Value_Static) {
					assert(existing->kind != Sym_Value_Auto);
					parseerror(&parse, primary, "cannot redeclare a type as a variable or function", decl.name);
				}

				StaticValue *existing_val = &module->ptr[existing->static_id];
				if (!typeCompatible(existing_val->type, decl.type)) {
					parsemsg(Log_Err, &parse, primary,
							"type %s does not match previous declaration",
							printTypeHighlighted(parse.arena, decl.type));
					parsemsg(Log_Info | Log_Fatal, &parse, existing_val->decl_location,
							"‘%.*s’ was originally declared with type %s",
							STRING_PRINTAGE(decl.name),
							printTypeHighlighted(parse.arena, existing_val->type));
				}
			} else {
				PUSH(*module, ((StaticValue) {
					.name = decl.name,
					.type = decl.type,
					.is_public = public_linkage,
					.decl_location = primary,
					.def_kind = decl.type.kind == Kind_Function ? Static_Function : Static_Variable,
				}));

				sym->ordinary = ALLOC(arena, OrdinaryIdentifier);
				*sym->ordinary = (OrdinaryIdentifier) {
					.kind = Sym_Value_Static,
					.static_id = module->len-1,
				};
			}
			StaticValue *val = &module->ptr[sym->ordinary->static_id];

			parse.build = (IrBuild) {0};
			Type argument_type;
			if (decl.type.kind == Kind_Function && declarators == 0 &&
				(parse.pos->kind == Tok_OpenBrace || tryParseTypeBase(&parse, &argument_type, NULL)))
			{
				// TODO Disallow old-style definitions in C23
				if (val->def_state == Def_Defined) {
					parsemsg(Log_Err, &parse, NULL, "redefinition of identifier ‘%.*s’", STRING_PRINTAGE(val->name));
					parsemsg(Log_Info | Log_Fatal, &parse, val->def_location, "previously defined here");
				}

				val->def_state = Def_Defined;
				val->def_location = primary;

				parse.current_func_type = decl.type.function;
				parseFunction(&parse);
				parse.scope_depth = 0;

				val = &module->ptr[sym->ordinary->static_id];
				val->function_ir = parse.build.ir;
				val->function_entry = parse.build.entry;
				break;
			} else if (decl.type.kind != Kind_Function) {
				if (existing) {
					if (val->is_public && storage == Storage_Static) {
						parseerror(&parse, primary,
								"‘%.*s’ cannot be declared static after it was declared extern",
								STRING_PRINTAGE(decl.name));
						parsemsg(Log_Info | Log_Fatal, &parse, val->decl_location, "previously declared here");
					}
					if (!val->is_public && storage == Storage_Unspecified) {
						parseerror(&parse, primary,
								"‘%.*s’ cannot be declared extern after it was declared static",
								STRING_PRINTAGE(decl.name));
						parsemsg(Log_Info | Log_Fatal, &parse, val->decl_location, "previously declared here");
					}
				} else {
					val->is_public = storage != Storage_Static;
				}

				if (tryEat(&parse, Tok_Equals)) {
					// STYLE Copypasta
					if (val->def_state == Def_Defined) {
						parsemsg(Log_Err, &parse, NULL, "redefinition of identifier ‘%.*s’", STRING_PRINTAGE(val->name));
						parsemsg(Log_Info | Log_Fatal, &parse, val->def_location, "previously defined here");
					}

					val->def_state = Def_Defined;
					val->def_location = primary;

					RefsList refs = {0};
					InitializationDest init = {
						.type = decl.type,
						.reloc_references = &refs,
						.reloc_data = ALLOCN(parse.arena, char, typeSize(val->type, &parse.target)),
					};
					parse.build = global_ir;
					parseInitializer(&parse, init);
					global_ir = parse.build;

					val = &module->ptr[sym->ordinary->static_id];
					val->value_references = (References) {refs.len, refs.ptr};
					val->value_data = (String) {init.reloc_data.len, init.reloc_data.ptr};
				} else if ((storage == Storage_Static || storage == Storage_Unspecified)
					&& val->def_state == Def_Undefined)
				{
					val->def_state = Def_Tentative;
				}

			}

			if (!tryEat(&parse, Tok_Comma)) {
				expect(&parse, Tok_Semicolon);
				break;
			}

			declarators++;
		}
	}

	for (u32 i = 0; i < module->len; i++) {
		StaticValue *val = &module->ptr[i];
		if (val->def_state == Def_Tentative)
			val->value_data = (String) ALLOCN(parse.arena, char, typeSize(val->type, &parse.target));
		else if (val->is_used && val->def_state == Def_Undefined && !val->is_public)
			parseerror(&parse, val->decl_location, "TODO(phrasing) static identifier was never defined");
	}

	discardIrBuilder(&global_ir);
	popScope(&parse, (Scope) {0});
}

static Symbol *getSymbol (Parse *parse, String name) {
	void **slot = mapGetOrCreate(&symbols, name);
	if (!*slot) {
		// TODO Make the symbol map hold Symbol structs by value instead of by pointer.
		Symbol *sym = ALLOC(parse->arena, Symbol);
		*sym = (Symbol) {name};
		*slot = sym;
	}
	return *slot;
}

OrdinaryIdentifier *genOrdSymbol (Parse *parse, String name, bool *new) {
	Symbol *sym = getSymbol(parse, name);
	bool is_new = !sym->ordinary || sym->ordinary->scope_depth < parse->scope_depth;
	if (is_new) {
		OrdinaryIdentifier *created = ALLOC(parse->arena, OrdinaryIdentifier);
		*created = (OrdinaryIdentifier) { .shadowed = sym->ordinary, .scope_depth = parse->scope_depth };
		sym->ordinary = created;
		PUSH(parse->current_scope, sym);
	}
	if (new)
		*new = is_new;
	return sym->ordinary;
}


Scope pushScope (Parse *parse) {
	Scope prev = parse->current_scope;
	parse->current_scope = (Scope) {0};
	return prev;
}

void popScope (Parse *parse, Scope replacement) {
	Scope def = parse->current_scope;
	for (u32 i = 0; i < def.len; i++) {
		assert(def.ptr[i]->ordinary->scope_depth == parse->scope_depth);
		def.ptr[i]->ordinary = def.ptr[i]->ordinary->shadowed;
	}
	free(def.ptr);
	parse->current_scope = replacement;
	parse->scope_depth--;
}

static void parseCompound(Parse *);
static void parseStatement(Parse *, bool *had_non_declaration);
static Value parseExpression(Parse *);
static Value parseExprAssignment(Parse *);
static Value parseExprBitOr(Parse *);
static Value parseExprBitXor(Parse *);
static Value parseExprBitAnd(Parse *);
static Value parseExprGreaterLess(Parse *);
static Value parseExprAddition(Parse *);
static Value parseExprMultiplication(Parse *);
static Value parseExprLeftUnary(Parse *);
static Value parseExprRightUnary(Parse *);
static Value parseExprBase(Parse *);
static void parseStructInitializer(Parse *, InitializationDest);
static void requiresVersion(Parse *, const char *desc, Version);
static void requiresVersionJust(Parse *, const char *desc, Version);

void parseFunction (Parse *parse) {
	IrBuild *build = &parse->build;
	*build = (IrBuild) {0};

	build->entry = startNewBlock(build, parse->arena, zString("__entry"));

	assert(parse->scope_depth == 0);
	parse->scope_depth = 1;
	Scope enclosing = pushScope(parse);

	u32 param_count = parse->current_func_type.parameters.len;
	for (u32 i = 0; i < param_count; i++) {
		Declaration param = parse->current_func_type.parameters.ptr[i];

		IrRef slot = genStackAllocFixed(&parse->build, typeSize(param.type, &parse->target));
		IrRef paramval = genParameter(&parse->build, typeSize(param.type, &parse->target));
		genStore(&parse->build, slot, paramval);

		OrdinaryIdentifier *sym = genOrdSymbol(parse, param.name, NULL);
		sym->kind = Sym_Value_Auto;
		sym->value = (Value) {param.type, slot, Ref_LValue};
	}

	parseCompound(parse);

	// TODO Warning if this is reachable and return type is not void.
	genReturnVal(&parse->build, IR_REF_NONE);

	popScope(parse, enclosing);

	for (u32 i = 0; i < parse->func_goto_labels.capacity; i++) {
		Symbol *sym = parse->func_goto_labels.ptr[i];
		Block *b = sym->label.block;
		if (b && b->exit.kind == Exit_None)
			parseerror(parse, sym->label.first_appearance, "a `goto` references label `%.*s`, which is not declared in this function", STRING_PRINTAGE(b->label));
	}
}

static void parseCompound (Parse *parse) {
	const Token *block_begin = parse->pos;
	expect(parse, Tok_OpenBrace);
	Scope enclosing_scope = pushScope(parse);

	// For C89, disallow declarators after the beginning of the block.
	bool had_non_declaration = false;

	while (true) {
		Token t = *parse->pos;
		if (t.kind == Tok_CloseBrace) {
			parse->pos++;
			return;
		} else if (t.kind == Tok_EOF) {
			parseerror(parse, block_begin, "unclosed %s", tokenName(Tok_OpenBrace));
		} else {
			parseStatement(parse, &had_non_declaration);
		}
	}

	popScope(parse, enclosing_scope);
}

static Block *getLabeledBlock (Parse *parse, String label) {
	Symbol *sym = getSymbol(parse, label);

	if (sym->label.block == NULL) {
		sym->label.block = newBlock(parse->arena, label);
		PUSH(parse->func_goto_labels, sym);
	}
	return sym->label.block;
}

static void parseStatement (Parse *parse, bool *had_non_declaration) {
	IrBuild *build = &parse->build;
	bool labeled = false;
	Token t = *parse->pos;
	bool is_declaration = false;

	Attributes attr = parseAttributes(parse);
	(void) attr;

	if (t.kind == Tok_Identifier && parse->pos[1].kind == Tok_Colon) {
		Symbol *sym = getSymbol(parse, t.val.identifier);

		if (sym->label.block) {
			if (sym->label.block->exit.kind != Exit_None)
				parseerror(parse, parse->pos, "redefinition of label `%.*s`", STRING_PRINTAGE(t.val.identifier));
		} else {
			sym->label.first_appearance = parse->pos;
			sym->label.block = newBlock(parse->arena, t.val.identifier);
		}

		genJump(build, sym->label.block);

		labeled = true;
		parse->pos += 2;
		t = *parse->pos;
	}

	switch (t.kind) {
	case Tok_OpenBrace:
		parseCompound(parse);
		break;
	case Tok_Key_Return: {
		parse->pos++;
		// TODO Check against function return type
		if (tryEat(parse, Tok_Semicolon)) {
			genReturnVal(build, IR_REF_NONE);
		} else {
			IrRef val = coerce(rvalue(parseExpression(parse), parse),
					*parse->current_func_type.rettype,
					parse);
			expect(parse, Tok_Semicolon);

			genReturnVal(build, val);
		}

		// Unreferenced dummy block for further instructions, will be ignored
		startNewBlock(build, parse->arena, STRING_EMPTY);
	} break;
	case Tok_Key_While: {
		parse->pos++;

		Block *head = newBlock(parse->arena, zString("__while_head"));
		genJump(build, head);
		expect(parse, Tok_OpenParen);
		Value condition = rvalue(parseExpression(parse), parse);
		expect(parse, Tok_CloseParen);
		genBranch(build, coerce(condition, BASIC_INT, parse));

		head->exit.branch.on_true = startNewBlock(build, parse->arena, zString("__while_body"));
		parseStatement(parse, had_non_declaration);

		Block *join = newBlock(parse->arena, zString("__while_join"));
		genJump(build, join);
		head->exit.branch.on_false = join;
	} break;
	case Tok_Key_If: {
		parse->pos++;

		expect(parse, Tok_OpenParen);
		Value condition = rvalue(parseExpression(parse), parse);
		expect(parse, Tok_CloseParen);
		genBranch(build, coerce(condition, BASIC_INT, parse));

		Block *head = build->insertion_block;
		head->exit.branch.on_true = startNewBlock(build, parse->arena, zString("__if_true"));
		parseStatement(parse, had_non_declaration);

		Block *join = newBlock(parse->arena, zString("__if_join"));

		if (parse->pos->kind == Tok_Key_Else) {
			parse->pos++;
			head->exit.branch.on_false = startNewBlock(build, parse->arena, zString("__if_else"));
			parseStatement(parse, had_non_declaration);
			genJump(build, join);
		} else {
			head->exit.branch.on_false = join;
			startBlock(build, join);
		}
	} break;
	case Tok_Key_Goto: {
		// TODO Check: "A goto statement shall not jump from outside the
		// scope of an identifier having a variably modified type to
		// inside the scope of that identifier." I think this is a
		// run-time constarint, so we can only emit a warning.
		parse->pos++;
		String label = expect(parse, Tok_Identifier).val.identifier;
		expect(parse, Tok_Semicolon);

		genJump(build, getLabeledBlock(parse, label));

		// Unreferenced dummy block for further instructions, will be ignored
		startNewBlock(build, parse->arena, STRING_EMPTY);
	} break;
	case Tok_Semicolon:
		break;
	default: {
		Type base_type;
		u8 storage;
		if (!tryParseTypeBase(parse, &base_type, &storage)) {
			parseExpression(parse);
			expect(parse, Tok_Semicolon);
			break;
		}
		is_declaration = true;

		if (labeled)
			requiresVersion(parse, "for arbitrary reasons, labels before declarations", Version_C23);

		if (storage == Storage_Typedef) {
			parseTypedefDecls(parse, base_type);
			break;
		}

		if (allowedNoDeclarator(parse, base_type))
			break;

		do {
			const Token *decl_token = parse->pos;
			Declaration decl = parseDeclarator(parse, base_type);
			bool is_new;
			OrdinaryIdentifier *sym = genOrdSymbol(parse, decl.name, &is_new);

			if (decl.type.kind == Kind_Function) {
				parseerror(parse, decl_token, "TODO Support function declarations in functions");
			} else if (decl.type.kind == Kind_Void) {
				parseerror(parse, decl_token, "variables can not have $svoid$s type", BOLD, RESET);
			} else {
				sym->kind = Sym_Value_Auto;
				sym->value = (Value) {decl.type,
					genStackAllocFixed(build, typeSize(decl.type, &parse->target)),
					storage == Storage_Register ? Ref_LValue_Register : Ref_LValue,
				};

				if (tryEat(parse, Tok_Equals))
					parseInitializer(parse, (InitializationDest) {0, sym->value.typ, .address = sym->value.inst});
			}
		} while (tryEat(parse, Tok_Comma));

		expect(parse, Tok_Semicolon);
	} break;
	}

	if (is_declaration && *had_non_declaration)
		requiresVersion(parse, "declarations after the begnning of the block", Version_C99);
	else
		*had_non_declaration = true;
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
	const Token *primary = parse->pos;
	switch (primary->kind) {
	case Tok_Equals:
		parse->pos++;
		if (v.typ.kind == Kind_Function)
			parseerror(parse, primary, "cannot assign to a function");
		if (!isLvalue(v))
			parseerror(parse, primary, "cannot assign to an rvalue");
		if (v.typ.qualifiers & Qualifier_Const)
			parseerror(parse, primary, "cannot assign to a %sconst%s-qualified value", BOLD, RESET);

		Value assigned_val = parseExprAssignment(parse);

		genStore(&parse->build, v.inst, coerce(rvalue(assigned_val, parse), v.typ, parse));
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
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitOr(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, genOr(&parse->build, a, b)};
	}
	return lhs;
}

static Value parseExprBitXor (Parse *parse) {
	Value lhs = parseExprBitAnd(parse);
	if (tryEat(parse, Tok_Hat)) {
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitXor(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, genXor(&parse->build, a, b)};
	}
	return lhs;
}

static Value parseExprBitAnd (Parse *parse) {
	Value lhs = parseExprGreaterLess(parse);
	if (tryEat(parse, Tok_Ampersand)) {
		IrRef a = coerce(rvalue(lhs, parse), BASIC_INT, parse);
		IrRef b = coerce(rvalue(parseExprBitAnd(parse), parse), BASIC_INT, parse);
		return (Value) {BASIC_INT, genAnd(&parse->build, a, b)};
	}
	return lhs;
}

static Value parseExprGreaterLess (Parse *parse) {
	IrBuild *build = &parse->build;
	Value lhs = parseExprAddition(parse);

	switch (parse->pos->kind) {
	case Tok_Less: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, genLessThan(build,
				coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))};
	}
	case Tok_LessEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, genLessThanOrEquals(build,
				coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))};
	}
	case Tok_Greater: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, genLessThan(build,
				coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))};
	}
	case Tok_GreaterEquals: {
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprAddition(parse), parse);
		return (Value) {BASIC_INT, genLessThanOrEquals(build,
				coerce(rhs, BASIC_INT, parse), coerce(lhs, BASIC_INT, parse),
			typeSize(BASIC_INT, &parse->target))};
	}
	default:
		return lhs;
	}
}

static Value parseExprAddition (Parse *parse) {
	IrBuild *build = &parse->build;
	Value lhs = parseExprMultiplication(parse);
	const Token *primary = parse->pos;
	Token t = *primary;
	if (t.kind != Tok_Plus && t.kind != Tok_Minus)
		return lhs;
	parse->pos++;
	lhs = rvalue(lhs, parse);
	Value rhs = rvalue(parseExprMultiplication(parse), parse);


	// TODO Type checking
	if (t.kind == Tok_Plus) {
		if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
			return pointerAdd(build, lhs, rhs, parse, primary);
		} else {
			return (Value) {BASIC_INT, genAdd(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))};
		}
	} else {
		if (lhs.typ.kind == Kind_Pointer) {
			IrRef stride = genImmediateInt(build,
					typeSize(*lhs.typ.pointer, &parse->target), parse->target.ptr_size);

			if (rhs.typ.kind == Kind_Pointer) {
				IrRef diff = genSub(build, lhs.inst, rhs.inst);
				return (Value) {BASIC_INT, genDiv(build, diff, stride)};
			} else {
				IrRef idx = genMul(build, coerce(rhs, BASIC_INT, parse), stride);
				return (Value) {lhs.typ, genSub(build, lhs.inst, idx)};
			}
		} else {
			if (rhs.typ.kind == Kind_Pointer)
				parseerror(parse, primary, "cannot subtract pointer from %s", printType(parse->arena, rhs.typ));
			return (Value) {BASIC_INT, genSub(build, coerce(lhs, BASIC_INT, parse), coerce(rhs, BASIC_INT, parse))};
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
		return (Value) { BASIC_INT, genMul(&parse->build, lhs.inst, rhs.inst) };
	else
		return (Value) { BASIC_INT, genDiv(&parse->build, lhs.inst, rhs.inst) };
}

static Value parseExprLeftUnary (Parse *parse) {
	IrBuild *build = &parse->build;

	const Token *primary = parse->pos;
	parse->pos++;
	switch (primary->kind) {
	case Tok_DoublePlus:
	case Tok_DoubleMinus: {
		Value v = parseExprLeftUnary(parse);
		if (v.typ.kind == Kind_Function)
			parseerror(parse, primary, "cannot modify a function");
		if (!isLvalue(v))
			parseerror(parse, primary, "cannot modify an rvalue");
		Value rval = (Value) {v.typ, genLoad(build, v.inst, typeSize(v.typ, &parse->target))};

		const int delta = parse->pos->kind == Tok_DoublePlus ? 1 : -1;
		Value one = {v.typ, genImmediateInt(build, delta, typeSize(v.typ, &parse->target))};

		Value result;
		if (v.typ.kind == Kind_Pointer)
			result = pointerAdd(build, rval, one, parse, primary);
		else
			result = (Value) {v.typ, genAdd(build, rval.inst, one.inst)};

		genStore(build, v.inst, result.inst);
		return result;
	} break;
	case Tok_Asterisk:
		return dereference(parse, parseExprLeftUnary(parse));
	case Tok_Bang: {
		Value v = rvalue(parseExprLeftUnary(parse), parse);
		IrRef zero = genImmediateInt(build, 0, typeSize(v.typ, &parse->target));
		return (Value) {BASIC_INT, genEquals(build, v.inst, zero, parse->target.typesizes[Int_int])};
	} break;
	case Tok_Tilde: {
		Value v = parseExprLeftUnary(parse);
		return (Value) {BASIC_INT, genNot(build, v.inst)};
	} break;
	case Tok_Ampersand: {
		Value v = parseExprLeftUnary(parse);
		// TODO Structs and unions will be handeled byref even if they
		// are not lvalues; mark this somehow.

		if (v.typ.kind == Kind_Function) {
			v.typ.kind = Kind_FunctionPtr;
		} else {
			Type *pointee = ALLOC(parse->arena, Type);
			*pointee = v.typ;
			if (v.category == Ref_LValue_Register)
				parseerror(parse, primary, "cannot take the address of a value declared %sregister%s", BOLD, RESET);
			if (v.category != Ref_LValue)
				parseerror(parse, primary, "cannot take the address of an rvalue");
			v.typ = (Type) {Kind_Pointer, .pointer = pointee};
		}

		v.category = Ref_RValue;
		return v;
	} break;
	case Tok_Key_Sizeof: {
		Type typ;
		bool openparen = tryEat(parse, Tok_OpenParen);
		if (!openparen || !tryParseTypeBase(parse, &typ, NULL))
			typ = parseValueType(parse, openparen ? parseExpression : parseExprLeftUnary);
		if (openparen)
			expect(parse, Tok_CloseParen);
		if (typ.kind == Kind_Function)
			parseerror(parse, primary, "the operand of a sizeof may not have a function type");

		Type sizetype = parse->target.ptrdiff;
		sizetype.basic |= Int_unsigned;
		return (Value) {sizetype, genImmediateInt(build, typeSize(typ, &parse->target), typeSize(sizetype, &parse->target))};
	} break;
	case Tok_Plus:
	case Tok_Minus:
		parseerror(parse, primary, "TODO unary +-");
		break;
	case Tok_Key_Alignof: {
		parseerror(parse, primary, "TODO _Alignof");
	} break;
	case Tok_OpenParen: {
		Type cast_target;
		if (tryParseTypeName(parse, &cast_target, NULL)) {
			expect(parse, Tok_CloseParen);
			if (tryEat(parse, Tok_OpenBrace)) {
				parseerror(parse, primary, "TODO Implement compound literals");
			}
			Value v = rvalue(parseExprLeftUnary(parse), parse);
			// TODO Actually cast
			// TODO In MSVC, a cast can produce an lvalue. I assume a
			// store to that will reverse-cast ...?
			return (Value) {cast_target, coerce(v, cast_target, parse)};
		} else {
			parse->pos--;
			return parseExprRightUnary(parse);
		}
	} break;
	default:
		parse->pos--;
		return parseExprRightUnary(parse);
	}
}

static Value parseExprRightUnary (Parse *parse) {
	IrBuild *build = &parse->build;
	Value v = parseExprBase(parse);

	while (true) {
		const Token *const primary = parse->pos;
		if (tryEat(parse, Tok_OpenParen)) {
			Value func = rvalue(v, parse);
			if (func.typ.kind != Kind_FunctionPtr)
				parseerror(parse, primary, "expected a function type, got an expression of type %s", printTypeHighlighted(parse->arena, func.typ));

			IrRefList arguments = {0};
			DeclList params = func.typ.function.parameters;
			if (parse->pos->kind != Tok_CloseParen) {
				do {
					Value arg = parseExprAssignment(parse);
					arg = rvalue(arg, parse);
					if (arguments.len == params.len)
						parseerror(parse, primary, "too many arguments to function call");
					PUSH_A(parse->arena, arguments, coerce(arg, params.ptr[arguments.len].type, parse));
				} while (tryEat(parse, Tok_Comma));
			}
			expect(parse, Tok_CloseParen);

			if (arguments.len < params.len)
				parseerror(parse, primary, "too few arguments to function call");
			else if (arguments.len > params.len)
				parseerror(parse, primary, "too many arguments to function call");

			ValuesSpan args = {arguments.len, arguments.ptr};
			v = (Value) {
				*func.typ.function.rettype,
				genCall(build, func.inst, args)
			};
		} else if (tryEat(parse, Tok_OpenBracket)) {
			v = rvalue(v, parse);
			Value index = rvalue(parseExpression(parse), parse);
			expect(parse, Tok_CloseBracket);
			if (!(v.typ.kind == Kind_Pointer || index.typ.kind == Kind_Pointer))
				parseerror(parse, primary, "either the subscript or the subuscripted value must be a pointer");
			v = dereference(parse, pointerAdd(build, v, index, parse, primary));
		} else if (tryEat(parse, Tok_DoublePlus) || tryEat(parse, Tok_DoubleMinus)) {
			int delta = primary->kind == Tok_DoublePlus ? 1 : -1;

			Value v = parseExprLeftUnary(parse);
			if (v.typ.kind == Kind_Function)
				parseerror(parse, primary, "cannot modify a function");
			if (!isLvalue(v))
				parseerror(parse, primary, "cannot modify an rvalue");
			Value rval = (Value) {v.typ, genLoad(build, v.inst, typeSize(v.typ, &parse->target))};

			Value one = {v.typ, genImmediateInt(build, delta, typeSize(v.typ, &parse->target))};

			IrRef result;
			if (v.typ.kind == Kind_Pointer)
				result = pointerAdd(build, rval, one, parse, primary).inst;
			else
				result = genAdd(build, rval.inst, one.inst);

			genStore(build, v.inst, result);
			v = rval;
		} else if (parse->pos[0].kind == Tok_Dot || parse->pos[0].kind == Tok_Arrow) {
			bool arrow = parse->pos[0].kind == Tok_Arrow;
			parse->pos++;

			if (arrow) {
				if (v.typ.kind != Kind_Pointer)
					parseerror(parse, NULL, "the arrow %s->%s operator expects a pointer value. You may want to use a regular dot", BOLD, RESET);
				v = dereference(parse, v);
			}
			Token member = expect(parse, Tok_Identifier);
			String member_name = member.val.identifier;

			if (v.typ.kind == Kind_Struct) {
				bool found = false;
				for (u32 i = 0; i < v.typ.struct_members.len; i++) {
					StructMember member = v.typ.struct_members.ptr[i];
					if (SPAN_EQL(member.name, member_name)) {
						IrRef offset = genImmediateInt(build, member.offset, typeSize(parse->target.intptr, &parse->target));
						v.typ = member.type;
						v.inst = genAdd(build, v.inst, offset);
						found = true;
						break;
					}
				}
				if (!found)
					parseerror(parse, NULL, "type %s does not have a member named %.*s", printTypeHighlighted(parse->arena, v.typ), member_name.len, member_name.ptr);
			} else if (v.typ.kind == Kind_Union) {
				parseerror(parse, parse->pos-2, "TODO Implement unions", printTypeHighlighted(parse->arena, v.typ), member_name.len, member_name.ptr);
			} else {
				parseerror(parse, NULL, "member access only works on %sstruct%ss and %sunion%s [unions not yet implemented]", BOLD, RESET, BOLD, RESET);
			}
		} else {
			break;
		}
	}
	return v;
}

static void skipOverCommaOrToCloseParen (Parse *parse, const Token *opening_paren) {
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
			parseerror(parse, opening_paren, "missing closing parenthesis");
			break;
		default:
			break;
		}
	}
	if (parse->pos[-1].kind == Tok_CloseParen)
		parse->pos--;
}

static Value parseExprBase (Parse *parse) {
	IrBuild *build = &parse->build;
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
		return (Value) {BASIC_INT, genImmediateInt(build, t.val.integer, typeSize(BASIC_INT, &parse->target))};
// 	case Tok_Real:
// 		return (Value) {{Kind_Basic, {Basic_double}}, genImmediateReal(t.val.real)};
	case Tok_String: {
		Type strtype = {
			.kind = Kind_Pointer,
			.qualifiers = Qualifier_Const,
			.pointer = ALLOC(parse->arena, Type),
		};
		*strtype.pointer = BASIC_CHAR;

		PUSH(*parse->module, ((StaticValue) {
			.type = strtype,
			.decl_location = parse->pos-1,
			.def_location = parse->pos-1,
			.def_kind = Static_Variable,
			.def_state = Def_Defined,
			.value_data = t.val.string,
		}));
		u32 id = parse->module->len - 1;

		return (Value) { strtype, genGlobal(build, id) };
	}
	case Tok_Identifier: {
		Symbol *sym = mapGet(&symbols, t.val.string);
		if (sym == NULL || sym->ordinary == NULL)
			parseerror(parse, NULL, "undefined identifier ‘%.*s’", STRING_PRINTAGE(t.val.string));
		OrdinaryIdentifier *ident = sym->ordinary;
		switch (ident->kind) {
		case Sym_Typedef:
			parseerror(parse, NULL, "expected a value, found a typedef name");
		case Sym_EnumConstant:
			return (Value) {
				BASIC_INT, // TODO Store parent type for Sym_EnumConstant.
				genImmediateInt(build, ident->enum_constant, typeSize(BASIC_INT, &parse->target)),
				Ref_RValue,
			};
		case Sym_Value_Static:
			return (Value) {
				parse->module->ptr[ident->static_id].type,
				genGlobal(build, ident->static_id),
				Ref_LValue,
			};
		case Sym_Value_Auto:
			return ident->value;
		}
	} unreachable;
	case Tok_Key_Generic: {
		requiresVersion(parse, "generic selections", Version_C17);

		const Token *primary = parse->pos;
		expect(parse, Tok_OpenParen);
		Type t = parseValueType(parse, parseExprAssignment);
		expect(parse, Tok_Comma);
		bool got = false;
		const Token *default_case = NULL;
		Value result;

		while (!tryEat(parse, Tok_CloseParen)) {
			if (tryEat(parse, Tok_Key_Default)) {
				if (default_case) {
					parsemsg(Log_Err, parse, NULL, "generic selection may not have more than one default association");
					parsemsg(Log_Info | Log_Fatal, parse, default_case, "previous default association appeared here");
				}
				expect(parse, Tok_Colon);
				default_case = parse->pos;
				skipOverCommaOrToCloseParen(parse, primary);
			} else {
				const Token *const type_token = parse->pos;
				Type typecase = parseTypeName(parse, NULL);
				expect(parse, Tok_Colon);
				if (typeCompatible(t, typecase)) {
					// TODO Disallow any two typecases being compatible
					// (not just with the selecting type).
					if (got) {
						parsemsg(Log_Err, parse, type_token, "no two generic assiciations may specify compatible types");
						parsemsg(Log_Info, parse, NULL, "TODO Show previous label");
					}
					got = true;
					result = parseExprAssignment(parse);
				} else {
					skipOverCommaOrToCloseParen(parse, primary);
				}
			};
		}
		if (!got) {
			if (!default_case)
				parseerror(parse, primary, "the controlling expression's type %s matched none of the generic associations, and no default case was provided", printTypeHighlighted(parse->arena, t));
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
		parseerror(parse, NULL, "expected an expression");
	}
}

static Attributes parseAttributes (Parse *parse) {
	Attributes result = 0;
	while (parse->pos[0].kind == Tok_OpenBracket && parse->pos[1].kind == Tok_OpenBracket) {
		requiresVersion(parse, "attributes", Version_C23);
		parse->pos += 2;
		parseerror(parse, NULL, "TODO Implement attributes");
	}
	return result;
}


static void parseInitializer (Parse *parse, InitializationDest dest) {
	// TODO Values must be constants when initializing static/_Thread_local values.
	if (tryEat(parse, Tok_OpenBrace)) {
		switch (dest.type.kind) {
		case Kind_Struct:
			parseStructInitializer(parse, dest);
			return;
		case Kind_Union: {
			u32 member_idx = 0;
			DeclList members = dest.type.union_members;
			if (tryEat(parse, Tok_Dot)) {
				requiresVersion(parse, "dot-designated initializers", Version_C99);
				const Token *name = parse->pos;
				String member = expect(parse, Tok_Identifier).val.identifier;

				while (member_idx < members.len && !SPAN_EQL(member, members.ptr[member_idx].name))
					member_idx++;

				if (member_idx == members.len)
					parseerror(parse, name, "%s does not have a member named %.*s", printTypeHighlighted(parse->arena, dest.type), member.len, member.ptr);

				expect(parse, Tok_Equals);
			}
			dest.type = members.ptr[member_idx].type;
			parseInitializer(parse, dest);
			expect(parse, Tok_CloseBrace);
		} return;
		case Kind_Array:
			parseerror(parse, NULL, "TODO Array initializers");
		default:
			parseerror(parse, NULL, "initializer can only be used for structs, unions and arrays, not for %s", printTypeHighlighted(parse->arena, dest.type));
		}
	} else {
		const Token *begin = parse->pos;
		Value got = parseExprAssignment(parse);
		Inst inst = parse->build.ir.ptr[got.inst];
		if (inst.kind != Ir_Constant && inst.kind != Ir_Reloc) {
			if (dest.reloc_data.ptr)
				parseerror(parse, begin, "expected a static initializer (TODO print the non-static culprit)");

			requiresVersion(parse, "non-constant initializers", Version_C99);
		}

		IrBuild *build = &parse->build;
		if (dest.reloc_data.ptr) {
			if (inst.kind == Ir_Constant) {
				memcpy(
					dest.reloc_data.ptr + dest.offset,
					&inst.constant,
					inst.size
				);
			} else {
				assert(inst.kind == Ir_Reloc);
				StaticValue *val = &parse->module->ptr[inst.reloc.id];

				if (isByref(got)) {
					// Load value from a constant static value
					if (val->def_kind != Static_Variable || !(val->type.qualifiers & Qualifier_Const))
						parseerror(parse, NULL, "TODO: ???");
					// TODO This does not yet copy references from the static value.
					StaticValue *src = &parse->module->ptr[inst.reloc.id];
					memcpy(
						dest.reloc_data.ptr + dest.offset,
						src->value_data.ptr + inst.reloc.offset,
						inst.size
					);
				} else {
					Reference ref = {dest.offset, inst.reloc.id, inst.reloc.offset};
					PUSH(*dest.reloc_references, ref);
				}
			}
		} else {
			IrRef offset = genImmediateInt(build, dest.offset, parse->target.ptr_size);
			IrRef dest_addr = genAdd(build, dest.address, offset);
			genStore(build, dest_addr, coerce(rvalue(got, parse), dest.type, parse));
		}
	}
}

static void parseStructInitializer (Parse *parse, InitializationDest dest) {
	assert(dest.type.kind == Kind_Struct);

	u32 len = dest.type.struct_members.len;
	StructMember *members = dest.type.struct_members.ptr;

	if (tryEat(parse, Tok_CloseBrace)) {
		requiresVersionJust(parse, "empty initializers", Version_C23);
		parseerror(parse, NULL, "TODO Implement empty initializers");
		return;
	}

	// TODO Zero out the gaps after initialization.

	u32 member_idx = 0;
	while (true) {
		if (tryEat(parse, Tok_Dot)) {
			requiresVersion(parse, "dot-designated initializers", Version_C99);
			const Token *name = parse->pos;
			String member = expect(parse, Tok_Identifier).val.identifier;

			member_idx = 0;
			while (member_idx < len && !SPAN_EQL(member, members[member_idx].name))
				member_idx++;

			if (member_idx == len)
				parseerror(parse, name, "%s does not have a member named %.*s", printTypeHighlighted(parse->arena, dest.type), member.len, member.ptr);

			expect(parse, Tok_Equals);
		} else if (member_idx >= len) {
			parseerror(parse, NULL, "initializers went beyond the end of the struct");
		}

		InitializationDest sub_dest = dest;
		sub_dest.offset += members[member_idx].offset;
		sub_dest.type = members[member_idx].type;
		parseInitializer(parse, sub_dest);

		member_idx++;

		if (tryEat(parse, Tok_Comma)) {
			if (tryEat(parse, Tok_CloseBrace))
				break;
		} else {
			expect(parse, Tok_CloseBrace);
			break;
		}
	}
}

static Type parseValueType (Parse *parse, Value (*operator)(Parse *parse)) {
	Block *current = parse->build.insertion_block;
	// Emit the expression into a block which is then discarded. Pretty hacky.
	startNewBlock(&parse->build, parse->arena, STRING_EMPTY);
	Type type = operator(parse).typ;
	parse->build.insertion_block = current;
	return type;
}


static Type parseTypeName (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeName(parse, &type, storage))
		parseerror(parse, parse->pos, "invalid type");
	return type;
}

static bool tryParseTypeName (Parse *parse, Type *type, u8 *storage_dest) {
	if (!tryParseTypeBase(parse, type, storage_dest))
		return false;

	if (tryEat(parse, Tok_Asterisk)) {
		Type ptr = {
			.kind = Kind_Pointer,
			.pointer = ALLOC(parse->arena, Type),
		};
		*ptr.pointer = *type;
		*type = ptr;
	}
	// TODO Rest of the type name.
	// Note: empty parentheses in a type name are interpreted as
	// “function with no parameter specification”, rather than
	// redundant parentheses around the omitted identifier.
	return true;
}

static Type parseTypeBase (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeBase(parse, &type, storage))
		parseerror(parse, parse->pos, "invalid type");
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
			parseerror(parse, parse->pos, "TODO Booleans");
			modifiable = false;
			break;
		case Tok_Key_Struct: {
			parse->pos++;
			Attributes attr = parseAttributes(parse);
			(void) attr;
			if (tryEat(parse, Tok_Identifier)) {
				// TODO
				// A tagged struct or union type followed by its definition
				// or by a semicolon *declares* that type tag into the
				// scope; other usage may refer to an existing type.
				// A tagged struct or union without definition which
				// does not refer to a previous declaration declares its
				// tag as an incomplete type.
				// A tagged struct or union with definition must match
				// all previous defintions.
			}
			expect(parse, Tok_OpenBrace);

			LIST(StructMember) members = {0};
			u32 current_offset = 0;
			while (!tryEat(parse, Tok_CloseBrace)) {
				// May not be function type, struct or union ending
				// with an incomplete array, or incomplete type except
				// that the last item may be an incomplete array.
				// TODO Check this.
				Type base = parseTypeBase(parse, NULL);
				Declaration decl = parseDeclarator(parse, base);
				// TODO Support VLAs
				StructMember member = {decl.type, decl.name, addMemberOffset(&current_offset, decl.type, &parse->target)};

				PUSH_A(parse->arena, members, member);
				expect(parse, Tok_Semicolon);
			}

			base.kind = Kind_Struct;
			base.struct_members.ptr = members.ptr;
			base.struct_members.len = members.len;
			modifiable = false;
			parse->pos--;
		} break;
		case Tok_Key_Union:
			parseerror(parse, parse->pos, "TODO Support unions");
			break;
		case Tok_Key_Enum: {
			parse->pos++;
			Attributes attr = parseAttributes(parse);
			(void) attr;
			if (tryEat(parse, Tok_Identifier)) {
				// TODO
			}
			expect(parse, Tok_OpenBrace);

			for (i32 value = 0;; value++) {
				String name = expect(parse, Tok_Identifier).val.identifier;

				if (tryEat(parse, Tok_Equals)) {
					const Token *expr_token = parse->pos;
					Value val = parseExprAssignment(parse);
					Inst inst = parse->build.ir.ptr[val.inst];
					if (inst.kind != Ir_Constant)
						parseerror(parse, expr_token, "enumeration values must be constant expressions");
					// TODO Range check this downcast.
					value = inst.constant;
				}
				bool new;
				OrdinaryIdentifier *ident = genOrdSymbol(parse, name, &new);
				if (!new)
					parseerror(parse, NULL, "redefinition of enumerator %.*s", STRING_PRINTAGE(name));

				ident->kind = Sym_EnumConstant;
				ident->enum_constant = value;

				if (tryEat(parse, Tok_Comma)) {
					if (tryEat(parse, Tok_CloseBrace)) {
						requiresVersion(parse, "trailing comma in enumerator list", Version_C99);
						break;
					}
				} else {
					expect(parse, Tok_CloseBrace);
					break;
				}
			}
			base.kind = Kind_Enum;
			modifiable = false;
			parse->pos--;
		} break;
		case Tok_Key_Long:
			if (longness[0]) {
				if (longness[1])
					parseerror(parse, parse->pos, "The Type Is Too Damn Long");
				requiresVersion(parse, "long long types", Version_C99);
				longness[1] = parse->pos;
			} else {
				longness[0] = parse->pos;
			}
			break;
		case Tok_Key_Short:
			if (shortness)
				parseerror(parse, parse->pos, "duplicate %sshort%s", BOLD, RESET);
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
					parseerror(parse, parse->pos, "duplicate %sunsigned%s", BOLD, RESET);
				else
					parseerror(parse, parse->pos, "conflicting %ssigned%s and %sunsigned%s", BOLD, RESET, BOLD, RESET);
			}
			signedness = parse->pos;
			is_unsigned = true;
			break;
		case Tok_Key_Signed:
			if (signedness) {
				if (is_unsigned)
					parseerror(parse, parse->pos, "conflicting %sunsigned%s and %ssigned%s", BOLD, RESET, BOLD, RESET);
				else
					parseerror(parse, parse->pos, "duplicate %ssigned%s", BOLD, RESET);
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
			parseerror(parse, parse->pos, "TODO Support _Thread_local");
// 			base.storage = Storage_Threadlocal;
// 			storages++;
			break;
			// TODO Parse function specifiers inline and _Noreturn
		case Tok_Identifier: {
			Symbol *sym = mapGet(&symbols, parse->pos->val.identifier);

			if (sym && sym->ordinary && sym->ordinary->kind == Sym_Typedef) {
				base = sym->ordinary->typedef_type;
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
			parseerror(parse, parse->pos, "cannot have multiple base types"); // TODO Clarify
		if (storages > 1)
			parseerror(parse, parse->pos, "cannot have multiple storage modifiers"); // TODO Clarify
		if (storage_dest) {
			*storage_dest = storage;
		} else if (storages) {
			if (storage == Storage_Typedef)
				parseerror(parse, parse->pos, "a typedef is not allowed in this context");
			else
				parseerror(parse, parse->pos, "a storage specifier is not allowed in this context");
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
					parseerror(parse, begin, "missing type");

				Version v = parse->target.version;
				if (v > Version_C89 && v < Version_GNU)
					parseerror(parse, begin, "support for implicit %s in declarations was removed in C99", printTypeHighlighted(parse->arena, BASIC_INT));
			}
			return true;
		} else if (base.basic == Int_char) {
			if (longness[0])
				parseerror(parse, longness[0], "char type cannot be %slong%s-modified", BOLD, RESET);
			if (shortness)
				parseerror(parse, shortness, "char type cannot be %sshort%s-modified", BOLD, RESET);
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
		parseerror(parse, signedness, "type cannot be %s%s%s-modified", BOLD, is_unsigned ? "unsigned" : "signed", RESET);
	if (longness[0])
		parseerror(parse, longness[0], "type cannot be %slong%s-modified", BOLD, RESET);
	if (shortness)
		parseerror(parse, shortness, "type cannot be %sshort%s-modified", BOLD, RESET);
	*type = base;

	return true;
}

u8 parseQualifiers (Parse *parse) {
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
			parseerror(parse, parse->pos, "duplicate %s", tokenName(tok));
		qual |= new;
		parse->pos++;
	}
	return qual;
}


static bool allowedNoDeclarator (Parse *parse, Type base_type) {
	(void) base_type;
	// TODO This is not quite accurate.
	if (base_type.kind != Kind_Struct && base_type.kind != Kind_Enum && base_type.kind != Kind_Union)
		return false;
	if (!tryEat(parse, Tok_Semicolon))
		return false;
	return true;
}

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
					parseerror(parse, NULL, "a function cannot return a function. You may want to return a function pointer instead");
				else if (enclosing->kind == Kind_Array)
					parseerror(parse, NULL, "an array cannot contain a function. You may want to store function pointers instead");
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
				parseerror(parse, NULL, "a function cannot return an array. You may want to return a pointer to an array instead");

			Type *content = ALLOC(parse->arena, Type);
			*content = *inner;
			bool is_static = tryEat(parse, Tok_Key_Static);

			inner->kind = Kind_Array;
			inner->array = (ArrayType) { content };
			inner->qualifiers = parseQualifiers(parse);
			if (tryEat(parse, Tok_Key_Static)) {
				if (is_static)
					parseerror(parse, NULL, "static may not appear twice"); // TODO Nice formatting
				is_static = true;
			}
			if (is_static) {
				// TODO Reject a `static` everywhere except on a function parameter
				requiresVersion(parse, "static specifiers on array parameters", Version_C99);
			}

			if (tryEat(parse, Tok_Asterisk)) {
				parseerror(parse, NULL, "TODO Support ‘variable length array of unspecified size’, whatever that may be");
				inner->array.count = 0;
			} else {
				inner->array.count = coerce(rvalue(parseExprAssignment(parse), parse), parse->target.ptrdiff, parse);
				if (parse->build.ir.ptr[inner->array.count].kind != Ir_Constant) {
					requiresVersion(parse, "variable length arrays", Version_C99);
					parseerror(parse,NULL, "TODO Support VLA");
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
		const Token *begin = parse->pos;
		Declaration decl = parseDeclarator(parse, base_type);
		// TODO Error on overwriting
		bool new;
		OrdinaryIdentifier *ident = genOrdSymbol(parse, decl.name, &new);
		if (!new) {
			if (ident->kind != Sym_Typedef)
				parseerror(parse, begin, "TODO(phrasing) cannot typedef a value to a type");
			if (!typeCompatible(ident->typedef_type, decl.type))
				parseerror(parse, begin, "TODO(phrasing) identifier was already typedef'd to incompatible type %s",
						printTypeHighlighted(parse->arena, ident->typedef_type));
		} else {
			ident->kind = Sym_Typedef;
			ident->typedef_type = decl.type;
		}
	} while (tryEat(parse, Tok_Comma));

	expect(parse, Tok_Semicolon);
}

static Value pointerAdd (IrBuild *ir, Value lhs, Value rhs, Parse *op_parse, const Token *token) {
	assert(lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer);
	if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer)
		parseerror(op_parse, token, "cannot add two pointers");
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
	return (Value) {ptr.typ, genAdd(ir, ptr.inst, diff)};
}

static Value dereference (Parse *parse, Value v) {
	assert(v.typ.kind == Kind_Pointer);

	if (isLvalue(v))
		v.inst = genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target));
	v.typ = *v.typ.pointer;
	v.category = Ref_LValue;
	return v;
}


// Performs lvalue conversion, array to pointer conversion and function
// to pointer conversion as necessary.
Value rvalue (Value v, Parse *parse) {
	// TODO Handle arrays, adapt to new byref categories
	if (v.typ.kind == Kind_Function) {
		v.typ.kind = Kind_FunctionPtr;
	} else if (isLvalue(v)) {
// 		assert(v.typ.kind == Kind_Pointer || v.typ.kind == Kind_Basic);

		v.inst = genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target));
		v.typ.qualifiers = 0;
		v.category = Ref_RValue;
	}
	return v;
}


IrRef coerce (Value v, Type t, Parse *p) {
	assert(!isByref(v)); // FIXME Why???

	if (t.kind == Kind_Void)
		return 0;
	if (typeCompatible(v.typ, t))
		return v.inst;
	if (t.basic == Int_bool)
		parseerror(p, NULL, "TODO Cast booleans");

	// Integer conversions
	if (v.typ.kind == Kind_Basic && t.kind == Kind_Basic) {
		u16 target = p->target.typesizes[t.basic & ~Int_unsigned];
		u16 source = p->target.typesizes[v.typ.basic & ~Int_unsigned];

		IrBuild *build = &p->build;
		// TODO Is it reasonable to omit the truncation on signed
		// destination? This could cause a UB overflow to potentially
		// screw with much later calculations.
		if (target < source) {
			return genTrunc(build, v.inst, target);
		} else if (target == source) {
			return v.inst;
		} else {
			if (t.basic & Int_unsigned)
				return genZeroExt(build, v.inst, target);
			else
				return genSignExt(build, v.inst, target);
		}
	}

	// Conversions to and from void*
	if (v.typ.kind == Kind_Pointer && t.kind == Kind_Pointer
			&& (v.typ.pointer->kind == Kind_Void || t.pointer->kind == Kind_Void))
	{
		return v.inst;
	}
	parseerror(p, NULL, "TODO(location) could not convert type %s to type %s",
		printTypeHighlighted(p->arena, v.typ), printTypeHighlighted(p->arena, t));
}

static void requiresVersionJust (Parse *parse, const char *desc, Version v) {
	if (parse->target.version != v && parse->target.version != Version_Lax)
		parseerror(parse, NULL, "%s are only supported with %s, but the current target uses %s",
				desc, version_names[v], version_names[parse->target.version]);
}

static void requiresVersion (Parse *parse, const char *desc, Version v) {
	if (parse->target.version < v)
		parseerror(parse, NULL, "%s are only supported with %s, but the current target uses %s",
				desc, version_names[v], version_names[parse->target.version]);
}
