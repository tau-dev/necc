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


/*

Parses and typechecks tokens into IR in a single pass.

*/



static Type chartype = {Kind_Basic, .basic = Int_char};
static Type const_chartype = {Kind_Basic, .qualifiers = Qualifier_Const, .basic = Int_char};



// TODO Move this into Parse
StringMap symbols = {0};

typedef LIST(Symbol*) Scope;

typedef struct {
	Arena arena;
	Arena *code_arena;
	const Tokenization tokens;
	const Token *pos;
	const Target target;
	Options *opt;

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
	return lexz(parse->tokens, parse->pos, parse->pos, 12);
}


typedef LIST(Reference) RefsList;
typedef struct {
	Type type;
	u32 offset;

	IrRef address;

	RefsList *reloc_references;
	MutableString reloc_data;
} InitializationDest;

static void vcomperror (Log level, bool crash_on_error, const Tokenization *t, const Token *tok, const Token *parse_pos, const char *msg, va_list args) {
	(void) parse_pos;
	u32 idx = tok - t->tokens;
	TokenPosition pos = t->positions[idx];
	SourceFile source = *t->files.ptr[pos.source_file_ref];

    printMsg(level, source, pos.source_file_offset);

    vfprintf(stderr, msg, args);
    fprintf(stderr, ".\n");
	if (pos.macro_file_offset && !(level & Log_Noexpand)) {
    	printInfo(*t->files.ptr[pos.macro_file_ref], pos.macro_file_offset);
    	fprintf(stderr, "(macro-expanded from here)\n");
	}

	if (level & Log_Fatal) {
		if (crash_on_error) {
			printf("TOKEN STREAM:\n\t");
			puts(lexz(*t, tok, parse_pos, 12));
			puts("");
			int *c = NULL;
			*c = 1;
		}
		exit(1);
	}
}


_Noreturn static void parseerror (const Parse *p, const Token *main, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vcomperror(Log_Err | Log_Fatal, p->opt->crash_on_error, &p->tokens, main ? main : p->pos-1, p->pos, msg, args);
    va_end(args);
    exit(1);
}

static void parsemsg (Log level, const Parse *p, const Token *main, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vcomperror(level, p->opt->crash_on_error, &p->tokens, main ? main : p->pos, p->pos, msg, args);
    va_end(args);
    if ((level & ~Log_Fatal & ~Log_Noexpand) == Log_Warn)
    	p->opt->emitted_warnings = true;
}

static void redefinition (const Parse *p, const Token *main, const Token *previous, String name) {
    parsemsg(Log_Err, p, main, "redefinition of %.*s", name);
    parsemsg(Log_Info | Log_Fatal, p, previous, "previous definition was here");
}

_Noreturn static void unexpectedToken (const Parse *p, TokenKind expected) {
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

bool fromStandardHeader (Parse *parse, const Token *primary) {
	assert(primary);
	const Tokenization *t = &parse->tokens;
	u32 p = primary - t->tokens;
	return t->files.ptr[t->positions[p].source_file_ref]->is_standard_header;
}

typedef enum {
	Decl_Named,
	Decl_PossiblyAbstract,
	Decl_Abstract,
} Namedness;

static Symbol *getSymbol (Parse *, String name);
static Declaration parseDeclarator(Parse *, Type base_type, Namedness);
static nodiscard bool allowedNoDeclarator(Parse *, Type base_type);
// static Declaration parseDeclarator (Parse* parse, const Token **tok, Type base_type);
static void initializeDefinition(Parse *, OrdinaryIdentifier *, Type);
static void parseInitializer(Parse *, InitializationDest);
static bool tryIntConstant (Parse *, Value, u64 *result);
static Value arithAdd(Parse *parse, const Token *primary, Value lhs, Value rhs);
static Value arithSub(Parse *parse, const Token *primary, Value lhs, Value rhs);
static Type arithmeticConversions (Parse *parse, const Token *primary, Value *lhs, Value *rhs);
static Type parseValueType (Parse *, Value (*operator)(Parse *));
static Type parseTypeBase(Parse *, u8 *storage);
static Type parseTypeName(Parse *, u8 *storage);
static nodiscard bool tryParseTypeBase(Parse *, Type *dest, u8 *storage);
static nodiscard bool tryParseTypeName(Parse *, Type *dest, u8 *storage);
static void parseFunction(Parse *, String func_name);
static Value rvalue(Value v, Parse *);
static Value dereference(Parse *, Value v);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse, const Token *);
static void parseTypedefDecls(Parse *, Type base_type);
static Attributes parseAttributes(Parse *);
static void popScope(Parse *, Scope);
static nodiscard Scope pushScope(Parse *);
static void requires(Parse *, const char *desc, Features);
static OrdinaryIdentifier *define(Parse *, Declaration, u8 storage, const Token *type_token, const Token *decl_token, const Token *def_token, bool file_scope);

// Parses all top level declarations into a Module.
void parse (Arena *code_arena, Tokenization tokens, Options *opt, Module *module) {
	// Used only as a scratch buffer for constructing constants at the
	// top level. Kind of inefficient, but very straightforward.
	IrBuild global_ir = {0};
	startNewBlock(&global_ir, code_arena, zString("dummy"));

	Parse parse = {
		.arena = create_arena(16 * 1024),
		.build = global_ir,
		.code_arena = code_arena,
		.tokens = tokens,
		.target = opt->target,
		.opt = opt,
		.pos = tokens.tokens,
		.module = module,
	};

	while (parse.pos->kind != Tok_EOF) {
		u8 storage;
		while (parse.pos->kind == Tok_Semicolon) {
			parse.pos++;
			requires(&parse, "semicolons at file scope", Features_GNU_Extensions);
		}
		if (parse.pos->kind == Tok_EOF) break;

		const Token *type_token = parse.pos;
		Type base_type = parseTypeBase(&parse, &storage);

		if (storage == Storage_Typedef) {
			parseTypedefDecls(&parse, base_type);
			continue;
		}

		if (storage == Storage_Auto)
			parseerror(&parse, type_token, "top-level symbol can not have automatic (stack-allocated) storage duration");

		if (allowedNoDeclarator(&parse, base_type))
			continue;

		u32 declarators = 0;
		while (true) {
			const Token *primary = parse.pos;
			Declaration decl = parseDeclarator(&parse, base_type, Decl_Named);
			Type first_argument_type;

			bool function_def = decl.type.kind == Kind_Function && declarators == 0 &&
				(parse.pos->kind == Tok_OpenBrace || tryParseTypeBase(&parse, &first_argument_type, NULL));
			bool object_def = decl.type.kind != Kind_Function && parse.pos->kind == Tok_Equals;

			OrdinaryIdentifier *ord = define(&parse, decl, storage, type_token, primary,
					function_def | object_def ? parse.pos : NULL, /* file_scope */ true);


			if (function_def) {
				// TODO Allow old-style definitions before C23

				parse.current_func_type = decl.type.function;
				parseFunction(&parse, decl.name);
				parse.scope_depth = 0;

				StaticValue *val = &parse.module->ptr[ord->static_id];
				val->function_ir = parse.build.ir;
				val->function_entry = parse.build.entry;

				parse.build = global_ir;
				break;
			} else if (object_def) {
				initializeDefinition(&parse, ord, decl.type);
			}

			if (!tryEat(&parse, Tok_Comma)) {
				expect(&parse, Tok_Semicolon);
				break;
			}

			declarators++;
		}
	}

	for (u32 i = 0; i < module->len; i++) {
		StaticValue *val = &parse.module->ptr[i];
		if (val->def_state == Def_Tentative) {
			u32 amount = typeSize(val->type, &parse.target);
			char *data = aalloc(parse.code_arena, amount);
			memset(data, 0, amount);
			val->value_data = (String) {amount, data};
		} else if (val->is_used && val->def_state == Def_Undefined && !val->is_public)
			parseerror(&parse, val->decl_location, "TODO(phrasing) static identifier was never defined");
	}

	discardIrBuilder(&global_ir);
	popScope(&parse, (Scope) {0});
	free_arena(&parse.arena);
}


static Symbol *getSymbol (Parse *parse, String name) {
	void **slot = mapGetOrCreate(&symbols, name);
	if (!*slot) {
		// TODO Make the symbol map hold Symbol structs by value instead of by pointer.
		Symbol *sym = ALLOC(&parse->arena, Symbol);
		*sym = (Symbol) {name};
		*slot = sym;
	}
	return *slot;
}

OrdinaryIdentifier *genOrdSymbol (Parse *parse, String name, bool *new) {
	Symbol *sym = getSymbol(parse, name);
	bool is_new = !sym->ordinary || sym->ordinary->scope_depth < parse->scope_depth;
	if (is_new) {
		OrdinaryIdentifier *created = ALLOC(&parse->arena, OrdinaryIdentifier);
		*created = (OrdinaryIdentifier) {
			.shadowed = sym->ordinary,
			.scope_depth = parse->scope_depth
		};
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
	parse->scope_depth++;
	return prev;
}

void popScope (Parse *parse, Scope replacement) {
	Scope def = parse->current_scope;
	for (u32 i = 0; i < def.len; i++) {
		if (def.ptr[i]->ordinary && def.ptr[i]->ordinary->scope_depth == parse->scope_depth)
			def.ptr[i]->ordinary = def.ptr[i]->ordinary->shadowed;
		if (def.ptr[i]->nametagged && def.ptr[i]->nametagged->scope_depth == parse->scope_depth)
			def.ptr[i]->nametagged = def.ptr[i]->nametagged->shadowed;
	}
	free(def.ptr);
	parse->current_scope = replacement;
	parse->scope_depth--;
}

static void parseCompound(Parse *);
static void parseStatement(Parse *, bool *had_non_declaration);
static Value parseExpression(Parse *);
static Value parseExprAssignment(Parse *);
static Value parseExprElvis(Parse *);
static Value parseExprOr(Parse *);
static Value parseExprAnd(Parse *);
static Value parseExprBitOr(Parse *);
static Value parseExprBitXor(Parse *);
static Value parseExprBitAnd(Parse *);
static Value parseExprEquality (Parse *);
static Value parseExprComparison(Parse *);
static Value parseExprShift(Parse *);
static Value parseExprAddition(Parse *);
static Value parseExprMultiplication(Parse *);
static Value parseExprLeftUnary(Parse *);
static Value parseExprRightUnary(Parse *);
static Value parseExprBase(Parse *);
static void parseStructInitializer(Parse *, InitializationDest);
static void parseArrayInitializer(Parse *, InitializationDest);
// TODO The order of parameters here is inconsistent.
static Value intPromote(Value v, Parse *, const Token*);
static void discardValue(Parse *, const Token *, Value v);
static IrRef coerce(Value v, Type t, Parse *, const Token *);
static IrRef toBoolean(Parse *, const Token *, Value v);
static IrRef coercerval(Value v, Type t, Parse *, const Token *, bool allow_casts);
static Value immediateIntVal(Parse *, Type typ, u64 val);

void parseFunction (Parse *parse, String func_name) {
	IrBuild *build = &parse->build;
	*build = (IrBuild) {0};

	build->entry = startNewBlock(build, parse->code_arena, zString("entry"));

	assert(parse->scope_depth == 0);
	Scope enclosing = pushScope(parse);

	char *terminated = aalloc(parse->code_arena, func_name.len + 1);
	memcpy(terminated, func_name.ptr, func_name.len);
	terminated[func_name.len] = 0;
	PUSH(*parse->module, ((StaticValue) {
		.type = {
			.kind = Kind_Array,
			.array = { .inner = &const_chartype, .count = func_name.len + 1 },
		},
		.decl_location = parse->pos,
		.def_kind = Static_Variable,
		.def_state = Def_Defined,
		.value_data = {func_name.len+1, terminated},
	}));
	bool is_new;
	OrdinaryIdentifier *func_name_sym = genOrdSymbol(parse, zString("__func__"), &is_new);
	assert(is_new);
	func_name_sym->kind = Sym_Value_Static;
	func_name_sym->decl_location = func_name_sym->def_location = parse->pos;
	func_name_sym->static_id = parse->module->len-1;


	u32 param_count = parse->current_func_type.parameters.len;

	for (u32 i = 0; i < param_count; i++) {
		Declaration param = parse->current_func_type.parameters.ptr[i];

		IrRef slot = genStackAllocFixed(&parse->build, typeSize(param.type, &parse->target));
		IrRef paramval = genParameter(&parse->build, typeSize(param.type, &parse->target));
		genStore(&parse->build, slot, paramval);

		bool is_new;
		OrdinaryIdentifier *sym = genOrdSymbol(parse, param.name, &is_new);
		assert(is_new);
		sym->kind = Sym_Value_Auto;
		sym->decl_location = sym->def_location = parse->pos;
		sym->value = (Value) {param.type, slot, Ref_LValue};
	}

	parseCompound(parse);

	// TODO Warning if this is reachable and return type is not void.
	genReturnVal(&parse->build, IR_REF_NONE);

	popScope(parse, enclosing);

	for (u32 i = 0; i < parse->func_goto_labels.len; i++) {
		Symbol *sym = parse->func_goto_labels.ptr[i];
		Block *b = sym->label.block;
		if (b && b->exit.kind == Exit_None)
			parseerror(parse, sym->label.def_location, "a `goto` references label `%.*s`, which is not declared in this function", STRING_PRINTAGE(b->label));
		sym->label.block = NULL;
		sym->label.def_location = NULL;
	}
	parse->func_goto_labels.len = 0;
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
			break;
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
		sym->label.block = newBlock(&parse->build, parse->code_arena, label);
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
				redefinition(parse, parse->pos, sym->label.def_location, t.val.identifier);
		} else {
			sym->label.def_location = parse->pos;
			sym->label.block = newBlock(&parse->build, parse->code_arena, t.val.identifier);
		}

		genJump(build, sym->label.block);

		labeled = true;
		parse->pos += 2;
		t = *parse->pos;
	}

	const Token *primary = parse->pos;
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
			IrRef val = coerce(parseExpression(parse),
					*parse->current_func_type.rettype,
					parse, primary);
			expect(parse, Tok_Semicolon);

			genReturnVal(build, val);
		}

		// Unreferenced dummy block for further instructions, will be ignored
		startNewBlock(build, parse->code_arena, STRING_EMPTY);
	} break;
	case Tok_Key_Do: {
		parse->pos++;
		*had_non_declaration = true;

		Block *body = newBlock(&parse->build, parse->code_arena, zString("do_while_body_"));
		genJump(build, body);
		parseStatement(parse, had_non_declaration);
		expect(parse, Tok_Key_While);
		expect(parse, Tok_OpenParen);
		Value condition = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		expect(parse, Tok_Semicolon);

		genBranch(build, toBoolean(parse, primary, condition));
		build->insertion_block->exit.branch.on_true = body;
		build->insertion_block->exit.branch.on_false = startNewBlock(build, parse->code_arena, zString("do_while_join"));
	} break;
	case Tok_Key_While: {
		parse->pos++;
		*had_non_declaration = true;

		Block *head = newBlock(build, parse->code_arena, zString("while_head_"));
		genJump(build, head);
		expect(parse, Tok_OpenParen);
		Value condition = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		Block *head_end = build->insertion_block;
		genBranch(build, toBoolean(parse, primary, condition));

		head_end->exit.branch.on_true = startNewBlock(build, parse->code_arena, zString("while_body_"));
		parseStatement(parse, had_non_declaration);
		genJump(build, head);

		head_end->exit.branch.on_false = startNewBlock(&parse->build, parse->code_arena, zString("while_join_"));
	} break;
	case Tok_Key_For: {
		*had_non_declaration = true;
		parse->pos++;
		Scope s = pushScope(parse);

		expect(parse, Tok_OpenParen);

		// TODO Limit this to declaration or expression
		parseStatement(parse, had_non_declaration);

		Block *enclosing_head = parse->current_loop_head;
		Block *enclosing_exit = parse->current_loop_switch_exit;
		parse->current_loop_switch_exit = newBlock(build, parse->code_arena, zString("for_join_"));
		parse->current_loop_head = newBlock(build, parse->code_arena, zString("for_head_"));

		genJump(build, parse->current_loop_head);
		Block *to_tail = parse->current_loop_head;

		if (!tryEat(parse, Tok_Semicolon)) {
			IrRef cond = toBoolean(parse, primary, parseExpression(parse));
			expect(parse, Tok_Semicolon);

			genBranch(build, cond);
			build->insertion_block->exit.branch.on_false = parse->current_loop_switch_exit;
			build->insertion_block->exit.branch.on_true = startNewBlock(build, parse->code_arena, zString("for_join_"));
		}
		if (!tryEat(parse, Tok_CloseParen)) {
			Block *current = build->insertion_block;
			to_tail = startNewBlock(build, parse->code_arena, zString("for_tail_"));
			discardValue(parse, primary, parseExpression(parse));
			expect(parse, Tok_CloseParen);

			// TODO This is just one example of how the basic block
			// construction interfacees are badly structured.
			// It takes a lot of code to achieve something simple, and
			// in the end, blocks are not even activated in the right
			// order, setting wrong first_instruction values. (Those are
			// currently fixed up by analysis passes, but I don't want
			// to do those for lightspeed codegen).
			genJump(build, parse->current_loop_head);
			build->insertion_block = current;
		}

		parseStatement(parse, had_non_declaration);
		genJump(build, NULL);
		build->insertion_block->exit.unconditional = to_tail;
		build->insertion_block = parse->current_loop_switch_exit;

		parse->current_loop_head = enclosing_head;
		parse->current_loop_switch_exit = enclosing_exit;
		popScope(parse, s);
	} break;
	case Tok_Key_If: {
		parse->pos++;
		*had_non_declaration = true;

		expect(parse, Tok_OpenParen);
		Value condition = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		genBranch(build, toBoolean(parse, primary, condition));
		Block *head = build->insertion_block;

		head->exit.branch.on_true = startNewBlock(build, parse->code_arena, zString("if_true_"));
		parseStatement(parse, had_non_declaration);
		Block *on_true = build->insertion_block;
		genJump(build, NULL);

		Block *join = newBlock(&parse->build, parse->code_arena, zString("if_join_"));

		if (parse->pos->kind == Tok_Key_Else) {
			parse->pos++;
			head->exit.branch.on_false = startNewBlock(build, parse->code_arena, zString("if_else_"));
			parseStatement(parse, had_non_declaration);
			genJump(build, join);
		} else {
			head->exit.branch.on_false = join;
			startBlock(build, join);
		}
		on_true->exit.unconditional = join;
	} break;
	case Tok_Key_Switch: {
		parse->pos++;
		expect(parse, Tok_OpenParen);
		Value v = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		if (v.typ.kind == Kind_Enum)
			v.typ = BASIC_INT;
		if (v.typ.kind != Kind_Basic)
			parseerror(parse, primary, "can only switch on expressions of integer type, not on an %s",
					printTypeHighlighted(&parse->arena, v.typ));
		v = intPromote(v, parse, primary);

	} break;
	case Tok_Key_Goto: {
		// TODO Check: "A goto statement shall not jump from outside the
		// scope of an identifier having a variably modified type to
		// inside the scope of that identifier." I think this is a
		// run-time constraint, so we can only emit a warning.
		parse->pos++;
		String label = expect(parse, Tok_Identifier).val.identifier;
		expect(parse, Tok_Semicolon);

		genJump(build, getLabeledBlock(parse, label));

		// Unreferenced dummy block for further instructions, will be ignored
		startNewBlock(build, parse->code_arena, STRING_EMPTY);
	} break;
	case Tok_Semicolon:
		parse->pos++;
		break; // TODO Does this count as a non-declaration??
	default: {
		Type base_type;
		u8 storage;
		if (!tryParseTypeBase(parse, &base_type, &storage)) {
			discardValue(parse, primary, parseExpression(parse));
			expect(parse, Tok_Semicolon);
			break;
		}
		is_declaration = true;

		if (labeled)
			requires(parse, "for arbitrary reasons, labels before declarations", Features_C23);

		if (storage == Storage_Typedef) {
			parseTypedefDecls(parse, base_type);
			break;
		}

		if (allowedNoDeclarator(parse, base_type))
			break;

		do {
			const Token *decl_token = parse->pos;
			Declaration decl = parseDeclarator(parse, base_type, Decl_Named);

			const Token *definer = parse->pos->kind == Tok_Equals ? parse->pos : NULL;
			OrdinaryIdentifier *ord = define(parse, decl, storage, primary, decl_token,
					definer, /* file_scope */ false);

			initializeDefinition(parse, ord, decl.type);
		} while (tryEat(parse, Tok_Comma));

		expect(parse, Tok_Semicolon);
	} break;
	}

	// TODO This is broken.
	if (is_declaration && *had_non_declaration)
		requires(parse, "declarations after the begnning of the block", Features_C99);
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
	Value v = parseExprElvis(parse);
	const Token *primary = parse->pos;

	if (!(primary->kind == Tok_Equals || primary->kind >= Tok_Assignment_Start))
		return v;

	parse->pos++;
	if (v.typ.kind == Kind_Function)
		parseerror(parse, primary, "cannot assign to a function");
	if (!isLvalue(v))
		parseerror(parse, primary, "cannot assign to an rvalue");
	if (v.typ.qualifiers & Qualifier_Const)
		parseerror(parse, primary, "cannot assign to a %sconst%s-qualified value", BOLD, RESET);

	Value assigned = parseExprAssignment(parse);

	if (primary->kind != Tok_Equals) {
		Value loaded = {v.typ, genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target))};
		assigned = rvalue(assigned, parse);
		switch (primary->kind) {
		case Tok_PlusEquals:
			assigned = arithAdd(parse, primary, loaded, assigned);
			break;
		case Tok_MinusEquals:
			assigned = arithSub(parse, primary, loaded, assigned);
			break;
		case Tok_AmpersandEquals: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genAnd(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_PipeEquals: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genOr(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_HatEquals: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genXor(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_DoubleLessEquals:
		case Tok_DoubleGreaterEquals: {
			loaded = intPromote(loaded, parse, primary);
			assigned = intPromote(assigned, parse, primary);

			if (primary->kind == (Tok_DoubleLessEquals))
				assigned = (Value) {loaded.typ, genShiftLeft(&parse->build, loaded.inst, assigned.inst)};
			else
				assigned = (Value) {loaded.typ, genShiftRight(&parse->build, loaded.inst, assigned.inst)};
		} break;
		default:
			parseerror(parse, primary, "TODO Compound assignments");
		}
	}

	genStore(&parse->build, v.inst, coerce(assigned, v.typ, parse, primary));
	return assigned;
}

static Value parseExprElvis (Parse *parse) {
	Value cond = parseExprOr(parse);

	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_Question)) {
		IrBuild *build = &parse->build;
		Block *head = build->insertion_block;

		IrRef ir = toBoolean(parse, primary, cond);

		// Some hacks to allow constant expressions.
		// TODO Figure out a way to fold this purely in the IR
		Inst inst = build->ir.ptr[ir];
		if (inst.kind == Ir_Constant) {
			Value res;
			if (inst.constant) {
				res = parseExprAssignment(parse);
				expect(parse, Tok_Colon);
				Block *b = build->insertion_block;
				startNewBlock(build, parse->code_arena, zString("dummy"));
				parseExprElvis(parse);
				build->insertion_block = b;
			} else {
				Block *b = build->insertion_block;
				startNewBlock(build, parse->code_arena, zString("dummy"));
				parseExprAssignment(parse);
				expect(parse, Tok_Colon);
				build->insertion_block = b;
				res = parseExprElvis(parse);
			}
			return res;
		}

		// This has to generate instructions out of block
		// order—because the two expressions need to be joined after
		// both types are known. Maybe the reordering pass really is
		// necessary.

		genBranch(build, ir);
		head->exit.branch.on_true = startNewBlock(build, parse->code_arena, STRING_EMPTY);
		Value lhs = rvalue(parseExprAssignment(parse), parse);
		Block *true_branch = build->insertion_block;

		const Token *colon = parse->pos;
		expect(parse, Tok_Colon);

		head->exit.branch.on_false = startNewBlock(build, parse->code_arena, STRING_EMPTY);
		Value rhs = rvalue(parseExprElvis(parse), parse);

		Type common;
		if (typeCompatible(lhs.typ, rhs.typ))
			common = lhs.typ;
		else if (lhs.typ.kind == Kind_Pointer && lhs.typ.kind == Kind_Pointer)
			if (lhs.typ.pointer->kind == Kind_Void)
				common = rhs.typ;
			else if (rhs.typ.pointer->kind == Kind_Void)
				common = lhs.typ;
			else
				parseerror(parse, colon, "incompatible pointer types");
		else // This can only generate an extension instruction, which will be reordered into the right block.
			common = arithmeticConversions(parse, colon, &rhs, &lhs);

		IrRef src_r = genPhiOut(build, coerce(rhs, common, parse, colon));
		Block *join = newBlock(&parse->build, parse->code_arena, STRING_EMPTY);
		genJump(build, join);

		build->insertion_block = true_branch;
		IrRef src_l = genPhiOut(build, coerce(lhs, common, parse, colon));
		genJump(build, join);

		IrRef dest = genPhiIn(build, typeSize(common, &parse->target));
		setPhiOut(build, src_l, dest, IR_REF_NONE);
		setPhiOut(build, src_r, dest, IR_REF_NONE);
		cond = (Value) {common, dest};
	}
	return cond;
}

static Value parseExprOr (Parse *parse) {
	Value lhs = parseExprAnd(parse);
	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_DoublePipe)) {
		const u16 int_size = typeSize(BASIC_INT, &parse->target);
		IrBuild *build = &parse->build;
		Block *head = build->insertion_block;

		IrRef constant = genPhiOut(build, genImmediateInt(build, 1, int_size));
		genBranch(build, toBoolean(parse, primary, lhs));
		head->exit.branch.on_false = startNewBlock(build, parse->code_arena, STRING_EMPTY);

		Value rhs = parseExprOr(parse);

		IrRef rhs_val = genPhiOut(build, toBoolean(parse, primary, rhs));
		Block *join = newBlock(&parse->build, parse->code_arena, STRING_EMPTY);
		genJump(build, join);
		head->exit.branch.on_true = join;
		IrRef res = genPhiIn(build, int_size);

		setPhiOut(build, constant, res, IR_REF_NONE);
		setPhiOut(build, rhs_val, res, IR_REF_NONE);
		lhs = (Value) {BASIC_INT, res};
	}
	return lhs;
}

// STYLE Copypasta, should probably be merged with parseExprOr.
static Value parseExprAnd (Parse *parse) {
	Value lhs = parseExprBitOr(parse);
	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_DoubleAmpersand)) {
		const u16 int_size = typeSize(BASIC_INT, &parse->target);
		IrBuild *build = &parse->build;
		Block *head = build->insertion_block;

		IrRef constant = genPhiOut(build, genImmediateInt(build, 0, int_size));
		genBranch(build, toBoolean(parse, primary, lhs));
		head->exit.branch.on_true = startNewBlock(build, parse->code_arena, STRING_EMPTY);

		Value rhs = parseExprOr(parse);

		IrRef rhs_val = genPhiOut(build, toBoolean(parse, primary, rhs));
		Block *join = newBlock(&parse->build, parse->code_arena, STRING_EMPTY);
		genJump(build, join);
		head->exit.branch.on_false = join;
		IrRef res = genPhiIn(build, int_size);

		setPhiOut(build, constant, IR_REF_NONE, res);
		setPhiOut(build, rhs_val, res, IR_REF_NONE);
		lhs = (Value) {BASIC_INT, res};
	}
	return lhs;
}


// TODO All of these should associate left-to-right instead of
// right-to-left; most of them are commutative tho.
static Value parseExprBitOr (Parse *parse) {
	Value lhs = parseExprBitXor(parse);
	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_Pipe)) {
		Value rhs = parseExprBitOr(parse);
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		return (Value) {common, genOr(&parse->build, lhs.inst, rhs.inst)};
	}
	return lhs;
}

static Value parseExprBitXor (Parse *parse) {
	Value lhs = parseExprBitAnd(parse);
	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_Hat)) {
		Value rhs = parseExprBitXor(parse);
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		return (Value) {common, genXor(&parse->build, lhs.inst, rhs.inst)};
	}
	return lhs;
}

static Value parseExprBitAnd (Parse *parse) {
	Value lhs = parseExprEquality(parse);
	const Token *primary = parse->pos;
	if (tryEat(parse, Tok_Ampersand)) {
		Value rhs = parseExprBitAnd(parse);
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		return (Value) {common, genAnd(&parse->build, lhs.inst, rhs.inst)};
	}
	return lhs;
}


static Value parseExprEquality (Parse *parse) {
	IrBuild *build = &parse->build;
	Value lhs = parseExprComparison(parse);

	switch ((int) parse->pos->kind) {
	case Tok_EqualsEquals:
	case Tok_BangEquals: {
		const Token *primary = parse->pos;
		parse->pos++;
		Value rhs = parseExprComparison(parse);

		if (lhs.inst == rhs.inst && lhs.category == rhs.category && parse->opt->warn_compare)
			parsemsg(Log_Warn, parse, primary, "comparison to self is always %s",
					primary->kind == Tok_BangEquals ? "false" : "true");

		if (lhs.typ.kind == Kind_Pointer && lhs.typ.kind == Kind_Pointer) {
			if (!lhs.typ.pointer->kind == Kind_Void &&
				!rhs.typ.pointer->kind == Kind_Void &&
				!typeCompatible(lhs.typ, rhs.typ))
			{
				parseerror(parse, primary, "can not compare types %s and %s",
						printTypeHighlighted(&parse->arena, lhs.typ), printTypeHighlighted(&parse->arena, rhs.typ));
			}
			lhs = rvalue(lhs, parse);
			rhs = rvalue(rhs, parse);
		} else {
			arithmeticConversions(parse, primary, &lhs, &rhs);
		}

		IrRef eql = genEquals(build, lhs.inst, rhs.inst, typeSize(BASIC_INT, &parse->target));
		if (primary->kind == (Tok_BangEquals))
			eql = genNot(build, eql);
		return (Value) { BASIC_INT, eql };
	}
	default:
		return lhs;
	}
}

static Value parseExprComparison (Parse *parse) {
	IrBuild *build = &parse->build;
	Value lhs = parseExprShift(parse);
	const Token *primary = parse->pos;
	switch (primary->kind) {
	case Tok_Less:
	case Tok_LessEquals:
	case Tok_Greater:
	case Tok_GreaterEquals: {
		parse->pos++;
		Value rhs = parseExprShift(parse);

		if (lhs.inst == rhs.inst && lhs.category == rhs.category && parse->opt->warn_compare)
			parsemsg(Log_Warn, parse, primary, "comparison to self is always %s",
					primary->kind == Tok_Less || primary->kind == Tok_Greater ? "false" : "true");

		if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer) {
			lhs = rvalue(lhs, parse);
			rhs = rvalue(rhs, parse);
		} else if (lhs.typ.kind == Kind_Pointer) {
			parseerror(parse, primary, "can not compare a pointer with an %s", printTypeHighlighted(&parse->arena, rhs.typ));
		} else if (rhs.typ.kind == Kind_Pointer) {
			parseerror(parse, primary, "can not compare a pointer with an %s", printTypeHighlighted(&parse->arena, lhs.typ));
		} else {
			arithmeticConversions(parse, primary, &lhs, &rhs);
		}
		u16 size = typeSize(BASIC_INT, &parse->target);
		IrRef res;
		switch (primary->kind) {
		case Tok_Less: res = genLessThan(build, lhs.inst, rhs.inst, size); break;
		case Tok_LessEquals: res = genLessThanOrEquals(build, lhs.inst, rhs.inst, size); break;
		case Tok_Greater: res = genLessThan(build, rhs.inst, lhs.inst, size); break;
		case Tok_GreaterEquals: res = genLessThanOrEquals(build, rhs.inst, lhs.inst, size); break;
		default: unreachable;
		}
		return (Value) { BASIC_INT, res };
	}
	default:
		return lhs;
	}
}

static Value parseExprShift (Parse *parse) {
	Value lhs = parseExprAddition(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_DoubleLess && primary->kind != Tok_DoubleGreater)
			return lhs;
		parse->pos++;
		lhs = intPromote(lhs, parse, primary);
		Value rhs = intPromote(parseExprAddition(parse), parse, primary);

		// TODO Type checking
		if (primary->kind == Tok_DoubleLess) {
			lhs.inst = genShiftLeft(&parse->build, lhs.inst, rhs.inst);
		} else {
			lhs.inst = genShiftRight(&parse->build, lhs.inst, rhs.inst);
		}
	}
}

static Value parseExprAddition (Parse *parse) {
	Value lhs = parseExprMultiplication(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_Plus && primary->kind != Tok_Minus)
			return lhs;
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprMultiplication(parse), parse);

		// TODO Type checking
		if (primary->kind == Tok_Plus)
			lhs = arithAdd(parse, primary, lhs, rhs);
		else
			lhs = arithSub(parse, primary, lhs, rhs);
	}
}

static Value parseExprMultiplication (Parse *parse) {
	Value lhs = parseExprLeftUnary(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_Asterisk && primary->kind != Tok_Slash && primary->kind != Tok_Percent)
			return lhs;
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprLeftUnary(parse), parse);

		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		if (primary->kind == Tok_Asterisk)
			lhs = (Value) {common, genMul(&parse->build, lhs.inst, rhs.inst)};
		else if (primary->kind == Tok_Percent)
			lhs = (Value) {common, genMod(&parse->build, lhs.inst, rhs.inst)};
		else
			lhs = (Value) {common, genDiv(&parse->build, lhs.inst, rhs.inst)};
	}
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
		Value one = immediateIntVal(parse, v.typ, delta);

		Value result;
		if (v.typ.kind == Kind_Pointer)
			result = pointerAdd(build, rval, one, parse, primary);
		else
			result = (Value) {v.typ, genAdd(build, rval.inst, one.inst)};

		genStore(build, v.inst, result.inst);
		return result;
	}
	case Tok_Asterisk: {
		Value v = parseExprLeftUnary(parse);
		if (v.typ.kind == Kind_FunctionPtr) {
			v.typ.kind = Kind_Function;
			return v;
		}
		if (v.typ.kind != Kind_Pointer)
			parseerror(parse, primary, "cannot dereference value of type %s; expected a pointer type", printTypeHighlighted(&parse->arena, v.typ));
		return dereference(parse, v);
	}
	case Tok_Bang: {
		Value v = rvalue(parseExprLeftUnary(parse), parse);
		IrRef zero = genImmediateInt(build, 0, typeSize(v.typ, &parse->target));
		return (Value) {BASIC_INT, genEquals(build, v.inst, zero, parse->target.typesizes[Int_int])};
	}
	case Tok_Tilde: {
		Value v = intPromote(parseExprLeftUnary(parse), parse, primary);
		return (Value) {v.typ, genNot(build, v.inst)};
	}
	case Tok_Ampersand: {
		Value v = parseExprLeftUnary(parse);
		// TODO Structs and unions will be handeled byref even if they
		// are not lvalues; mark this somehow.

		if (v.typ.kind == Kind_Function) {
			v.typ.kind = Kind_FunctionPtr;
		} else {
			Type *pointee = ALLOC(parse->code_arena, Type);
			*pointee = v.typ;
			if (v.category == Ref_LValue_Register)
				parseerror(parse, primary, "cannot take the address of a value declared %sregister%s", BOLD, RESET);
			if (v.category != Ref_LValue)
				parseerror(parse, primary, "cannot take the address of an rvalue");
			v.typ = (Type) {Kind_Pointer, .pointer = pointee};
		}

		v.category = Ref_RValue;
		return v;
	}
	case Tok_Key_Sizeof: {
		Type typ;
		bool openparen = tryEat(parse, Tok_OpenParen);
		if (!openparen || !tryParseTypeName(parse, &typ, NULL))
			typ = parseValueType(parse, openparen ? parseExpression : parseExprLeftUnary);
		if (openparen)
			expect(parse, Tok_CloseParen);
		if (typ.kind == Kind_Function)
			parseerror(parse, primary, "the operand of a sizeof may not have a function type");

		return immediateIntVal(parse, parse->target.ptrdiff, typeSize(typ, &parse->target));
	}
	case Tok_Plus:
	case Tok_Minus: {
		// TODO Promote
		Value v = intPromote(parseExprLeftUnary(parse), parse, primary);

		if (primary->kind == Tok_Minus) {
			u32 zero = genImmediateInt(build, 0, typeSize(v.typ, &parse->target));
			v.inst = genSub(build, zero, v.inst);
		}
		return v;
	}
	case Tok_OpenParen: {
		Type cast_target;
		if (tryParseTypeName(parse, &cast_target, NULL)) {
			cast_target.qualifiers = 0;
			expect(parse, Tok_CloseParen);
			if (parse->pos->kind == Tok_OpenBrace) {
				// Compound literal.
				// TODO Parse right unary operators!
				IrRef stack = genStackAllocFixed(build, typeSize(cast_target, &parse->target));
				// TODO Generate Ir_StackDealloc at end of block!
				InitializationDest dest = {
					.type = cast_target,
					.address = stack,
				};
				parseInitializer(parse, dest);
				return (Value) {cast_target, stack, Ref_LValue};
			} else {
				// Cast operator
				Value v = rvalue(parseExprLeftUnary(parse), parse);
				return (Value) {cast_target, coercerval(v, cast_target, parse, primary, true)};
			}
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
				parseerror(parse, primary, "expected a function type, got an expression of type %s", printTypeHighlighted(&parse->arena, func.typ));

			IrRefList arguments = {0};
			DeclList params = func.typ.function.parameters;

			bool have_prototype = !func.typ.function.missing_prototype;
			bool is_vararg = func.typ.function.is_vararg;
			if (parse->pos->kind != Tok_CloseParen) {
				do {
					const Token *primary = parse->pos;
					Value arg = parseExprAssignment(parse);
					if (have_prototype && !is_vararg && arguments.len == params.len)
						parseerror(parse, primary, "too many arguments to function call");

					IrRef inst;
					if (have_prototype && arguments.len < params.len) {
						inst = coerce(arg, params.ptr[arguments.len].type, parse, primary);
					} else {
						inst = intPromote(arg, parse, primary).inst;
					}
					PUSH_A(parse->code_arena, arguments, inst);
				} while (tryEat(parse, Tok_Comma));
			}

			expect(parse, Tok_CloseParen);

			if (have_prototype) {
				if (params.len && arguments.len < params.len)
					parseerror(parse, primary, "too few arguments to function call");
				else if (params.len && arguments.len > params.len && !is_vararg)
					parseerror(parse, primary, "too many arguments to function call");
			}

			ValuesSpan args = {arguments.len, arguments.ptr};
			v = (Value) {
				*func.typ.function.rettype,
				genCall(build, func.inst, args, typeSize(*func.typ.function.rettype, &parse->target))
			};
		} else if (tryEat(parse, Tok_OpenBracket)) {
			v = rvalue(v, parse);
			Value index = rvalue(parseExpression(parse), parse);
			expect(parse, Tok_CloseBracket);
			if (parse->opt->warn_char_subscript && index.typ.kind == Kind_Basic && index.typ.basic == Int_char) {
				parsemsg(Log_Warn | Log_Noexpand, parse, primary, "the character type of the index may platform-dependently be signed");
				parsemsg(Log_Info, parse, primary, "you may want to cast it to a char of explicit signedness");
			}

			if (!(v.typ.kind == Kind_Pointer || index.typ.kind == Kind_Pointer))
				parseerror(parse, primary, "either the subscript or the subscripted value must be a pointer");
			v = dereference(parse, pointerAdd(build, v, index, parse, primary));
		} else if (tryEat(parse, Tok_DoublePlus) || tryEat(parse, Tok_DoubleMinus)) {
			int delta = primary->kind == Tok_DoublePlus ? 1 : -1;

			if (v.typ.kind == Kind_Function)
				parseerror(parse, primary, "cannot modify a function");
			if (!isLvalue(v))
				parseerror(parse, primary, "cannot modify an rvalue");
			Value rval = (Value) {v.typ, genLoad(build, v.inst, typeSize(v.typ, &parse->target))};

			Value one = immediateIntVal(parse, v.typ, delta);

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

			String member_name = expect(parse, Tok_Identifier).val.identifier;
			Type resolved = resolveType(v.typ);

			if (resolved.kind == Kind_Struct || resolved.kind == Kind_Union) {
				u32 i = 0;
				for (; i < resolved.members.len; i++) {
					if (SPAN_EQL(resolved.members.ptr[i].name, member_name))
						break;
				}
				if (i == resolved.members.len)
					parseerror(parse, NULL, "type %s does not have a member named %.*s", printTypeHighlighted(&parse->arena, v.typ), member_name.len, member_name.ptr);
				CompoundMember member = resolved.members.ptr[i];
				bool is_flex = isFlexibleArrayMember(resolved.members, i);

				if (isLvalue(v)) {
					IrRef offset = genImmediateInt(build, member.offset, typeSize(parse->target.intptr, &parse->target));
					v.typ = member.type;
					v.inst = genAdd(build, v.inst, offset);

					if (is_flex) {
						v.typ = (Type) {Kind_Pointer, .pointer = &resolved.members.ptr[i].type};
						v.category = Ref_RValue;
					}
				} else {
					parseerror(parse, NULL, "TODO rvalue member access");
				}
			} else {
				parseerror(parse, NULL, "member access only works on %sstruct%ss and %sunion%ss, not on %s",
						BOLD, RESET, BOLD, RESET, printTypeHighlighted(&parse->arena, resolved));
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
		return immediateIntVal(parse, (Type) {Kind_Basic, .basic = t.val.literal.int_type}, t.val.literal.integer);
// 	case Tok_Real:
// 		return (Value) {{Kind_Basic, {Basic_double}}, genImmediateReal(t.val.real)};
	case Tok_String: {
		const Token *scan = parse->pos - 1;

		u32 len = t.val.string.len;
		while (parse->pos->kind == Tok_String) {
			len += parse->pos->val.string.len;
			parse->pos++;
		}
		char *data = aalloc(parse->code_arena, len + 1);
		char *insert = data;
		while (scan->kind == Tok_String) {
			String str = scan->val.string;
			memcpy(insert, str.ptr, str.len);
			insert += str.len;
			scan++;
		}
		insert[0] = 0;
		Type strtype = {
			.kind = Kind_Array,
			.array = {.inner = &chartype, .count = len + 1},
		};
		PUSH(*parse->module, ((StaticValue) {
			.type = strtype,
			.decl_location = parse->pos-1,
			.def_kind = Static_Variable,
			.def_state = Def_Defined,
			.value_data = {len + 1, data},
		}));
		u32 id = parse->module->len - 1;

		return (Value) { strtype, genGlobal(build, id), Ref_LValue };
	}
	case Tok_Key_Alignof: {
		expect(parse, Tok_OpenParen);
		Type t = parseTypeName(parse, NULL);
		expect(parse, Tok_CloseParen);
		return immediateIntVal(parse, BASIC_INT, typeAlignment(t, &parse->target));
	}
	case Tok_Identifier: {
		Symbol *sym = mapGet(&symbols, t.val.identifier);
		if (sym == NULL || sym->ordinary == NULL)
			parseerror(parse, NULL, "undefined identifier ‘%.*s’", STRING_PRINTAGE(t.val.identifier));
		OrdinaryIdentifier *ident = sym->ordinary;
		switch (ident->kind) {
		case Sym_Typedef:
			parseerror(parse, NULL, "expected a value, found a typedef name");
		case Sym_EnumConstant:
			// TODO Store enum-ness in the integer type, e.g. for switch completeness checking.
			return immediateIntVal(parse, BASIC_INT, ident->enum_constant);
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
		requires(parse, "generic selections", Features_C23);

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
						parsemsg(Log_Info | Log_Fatal, parse, NULL, "TODO Show previous label");
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
				parseerror(parse, primary, "the controlling expression's type %s matched none of the generic associations, and no default case was provided", printTypeHighlighted(&parse->arena, t));
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
		requires(parse, "attributes", Features_C23);
		parse->pos += 2;
		parseerror(parse, NULL, "TODO Implement attributes");
	}
	return result;
}


// More redundant state. See the comment on define().
static void initializeDefinition (Parse *parse, OrdinaryIdentifier *ord, Type type) {
	const Token *init_token = NULL;
	if (parse->pos->kind == Tok_Equals) {
		init_token = parse->pos;
		parse->pos++;
	}

	bool unspecified_size = false;
	if (type.kind == Kind_VLArray) {
		if (type.array.count == IR_REF_NONE) {
			unspecified_size = true;
			if (!init_token)
				parseerror(parse, NULL, "cannot infer size of array without initializer");
		} else if (init_token) {
			requires(parse, "initialization of variable-length arrays", Features_C23);

			expect(parse, Tok_OpenBrace);
			expect(parse, Tok_CloseBrace);

			parseerror(parse, NULL, "TODO Implement VLA initializers");
		}
	}

	StaticValue *static_val = NULL;
	RefsList refs = { 0 };
	InitializationDest dest = { .type = type };

	if (ord->kind == Sym_Value_Auto) {
		ord->value = (Value) {type, genStackAllocFixed(&parse->build, typeSize(type, &parse->target)), Ref_LValue};
		dest.address = ord->value.inst;
	} else {
		assert(ord->kind == Sym_Value_Static);
		assert(init_token);
		static_val = &parse->module->ptr[ord->static_id];
// 		assert(type == static_val->type);
		dest.reloc_references = &refs;
	}

	if (unspecified_size) {
		assert(init_token);
		expect(parse, Tok_OpenBrace);
		dest.type = *dest.type.array.inner;

		u32 member_size = typeSize(dest.type, &parse->target);
		u32 count = 0;
		while (true) {
			if (tryEat(parse, Tok_CloseBrace))
				break;
			count++;
			if (static_val && count * member_size > dest.reloc_data.len) {
				dest.reloc_data = (MutableString) ALLOCN(parse->code_arena, char,
						dest.reloc_data.len + count * member_size);
			}

			parseInitializer(parse, dest);

			if (!tryEat(parse, Tok_Comma)) {
				expect(parse, Tok_CloseBrace);
				break;
			}
		}

		if (static_val) {
			static_val->type.kind = Kind_Array;
			static_val->type.array.count = count;
			static_val->value_data = (String) {count * member_size, dest.reloc_data.ptr};
			static_val->value_references = (References) {refs.len, refs.ptr};
		} else {
			ord->value.typ.kind = Kind_Array;
			ord->value.typ.array.count = count;
			Inst *size = &parse->build.ir.ptr[parse->build.ir.ptr[dest.address].alloc.size];
			assert(size->kind == Ir_Constant);
			size->constant = count * member_size;
		}
	}

	if (init_token) {
		if (static_val) {
			dest.reloc_data = (MutableString) ALLOCN(parse->code_arena, char,
					typeSize(type, &parse->target));
			parseInitializer(parse, dest);
			static_val->value_data = (String) {dest.reloc_data.len, dest.reloc_data.ptr};
			static_val->value_references = (References) {refs.len, refs.ptr};
		} else {
			parseInitializer(parse, dest);
		}
	}
}

static void parseInitializer (Parse *parse, InitializationDest dest) {
	// TODO Values must be constants when initializing static/_Thread_local values.
	if (tryEat(parse, Tok_OpenBrace)) {
		switch (dest.type.kind) {
		case Kind_Struct:
		case Kind_Struct_Named:
			parseStructInitializer(parse, dest);
			return;
		case Kind_Union:
		case Kind_Union_Named: {
			u32 member_idx = 0;
			Members members = resolveType(dest.type).members;
			if (tryEat(parse, Tok_Dot)) {
				requires(parse, "dot-designated initializers", Features_C99);
				const Token *name = parse->pos;
				String member = expect(parse, Tok_Identifier).val.identifier;

				while (member_idx < members.len && !SPAN_EQL(member, members.ptr[member_idx].name))
					member_idx++;

				if (member_idx == members.len)
					parseerror(parse, name, "%s does not have a member named %.*s",
							printTypeHighlighted(&parse->arena, dest.type), STRING_PRINTAGE(member));

				expect(parse, Tok_Equals);
			}
			dest.type = members.ptr[member_idx].type;
			parseInitializer(parse, dest);
			expect(parse, Tok_CloseBrace);
		} return;
		case Kind_Array:
			parseArrayInitializer(parse, dest);
			return;
		default:
			parseerror(parse, NULL, "initializer can only be used for structs, unions and arrays, not for %s", printTypeHighlighted(&parse->arena, dest.type));
		}
	} else {
		const Token *begin = parse->pos;
		Value got = parseExprAssignment(parse);
		Inst inst = parse->build.ir.ptr[got.inst];
		if (inst.kind != Ir_Constant && inst.kind != Ir_Reloc) {
			if (dest.reloc_data.ptr)
				parseerror(parse, begin, "expected a static initializer (TODO print the non-static culprit)");

			requires(parse, "non-constant initializers", Features_C99);
		}

		IrBuild *build = &parse->build;
		if (dest.reloc_data.ptr) {
			if (inst.kind == Ir_Constant) {
				memcpy(
					dest.reloc_data.ptr + dest.offset,
					&inst.constant,
					inst.size);
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
			genStore(build, dest_addr, coerce(got, dest.type, parse, begin));
		}
	}
}

static void parseStructInitializer (Parse *parse, InitializationDest dest) {
	Type resolved = resolveType(dest.type);
	u32 len = resolved.members.len;
	CompoundMember *members = resolved.members.ptr;

	if (dest.reloc_data.ptr) {
		memset(dest.reloc_data.ptr + dest.offset, 0, typeSize(resolved, &parse->target));
	} else {
		parsemsg(Log_Warn, parse, parse->pos - 1, "TODO zero-init values");
	}

	if (tryEat(parse, Tok_CloseBrace)) {
		requires(parse, "empty initializers", Features_C23);
		parseerror(parse, parse->pos - 2, "TODO Implement empty initializers");
		return;
	}

	// TODO Zero out the gaps after initialization.

	u32 member_idx = 0;
	while (true) {
		if (tryEat(parse, Tok_Dot)) {
			requires(parse, "dot-designated initializers", Features_C99);
			const Token *name = parse->pos;
			String member = expect(parse, Tok_Identifier).val.identifier;

			member_idx = 0;
			while (member_idx < len && !SPAN_EQL(member, members[member_idx].name))
				member_idx++;

			if (member_idx == len)
				parseerror(parse, name, "%s does not have a member named %.*s", printTypeHighlighted(&parse->arena, dest.type), member.len, member.ptr);

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

static void parseArrayInitializer(Parse *parse, InitializationDest dest) {
	assert(dest.type.kind == Kind_Array);
	u32 count = dest.type.array.count;
	Type inner = *dest.type.array.inner;
	u32 member_size = typeSize(inner, &parse->target);

	if (dest.reloc_data.ptr) {
		memset(dest.reloc_data.ptr + dest.offset, 0, typeSize(dest.type, &parse->target));
	} else {
		parsemsg(Log_Warn, parse, NULL, "TODO zero-init values");
	}

	if (tryEat(parse, Tok_CloseBrace)) {
		requires(parse, "empty initializers", Features_C23);
		parseerror(parse, NULL, "TODO Implement empty initializers");
		return;
	}

	u32 member_idx = 0;
	while (true) {
		if (tryEat(parse, Tok_OpenBracket)) {
			const Token *designator = parse->pos - 1;
			requires(parse, "bracket-designated initializers", Features_C99);

			u64 pos;
			if (!tryIntConstant(parse, parseExpression(parse), &pos))
				parseerror(parse, designator, "bracket designators must be constant integer expressions");

			if (pos >= count)
				parseerror(parse, designator, "index %lld is out of range of the array", (long long) member_idx);

			member_idx = pos;

			expect(parse, Tok_CloseBracket);
			expect(parse, Tok_Equals);
		} else if (member_idx >= count) {
			parseerror(parse, NULL, "initializers went beyond the end of the array");
		}

		InitializationDest sub_dest = dest;
		sub_dest.offset += member_idx * member_size;
		sub_dest.type = inner;
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
	startNewBlock(&parse->build, parse->code_arena, STRING_EMPTY);
	Type type = operator(parse).typ;
	parse->build.insertion_block = current;
	return type;
}


static Type parseTypeName (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeName(parse, &type, storage))
		parseerror(parse, parse->pos, "expected a type name");
	return type;
}

static bool tryParseTypeName (Parse *parse, Type *type, u8 *storage_dest) {
	Type base;
	if (!tryParseTypeBase(parse, &base, storage_dest))
		return false;

	Declaration decl = parseDeclarator(parse, base, Decl_Abstract);
	*type = decl.type;
	return true;
}

static Type parseTypeBase (Parse *parse, u8 *storage) {
	Type type;
	if (!tryParseTypeBase(parse, &type, storage)) {
		if (parse->pos->kind == Tok_Identifier) {
			requires(parse, "default types of int", Features_DefaultInt);
			*storage = Storage_Unspecified;
			return BASIC_INT;
		}
		parseerror(parse, parse->pos, "expected a type name");
	}
	return type;
}

static nodiscard Type parseStructUnionBody(Parse *parse, bool is_struct);

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
		case Tok_Key_Double:
			// TODO
			base.kind = Kind_Basic;
			base.basic = Int_int;
			bases++;
			break;
		case Tok_Key_Float:
			// TODO
			base.kind = Kind_Basic;
			base.basic = Int_int;
			modifiable = false;
			bases++;
			break;
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
		case Tok_Key_VaList: // TODO
			base.kind = Kind_Void;
			modifiable = false;
			bases++;
			break;
		case Tok_Key_Bool:
			base.kind = Kind_Basic;
			base.basic = Int_bool;
			modifiable = false;
			bases++;
			break;
		case Tok_Key_Union:
		case Tok_Key_Struct: {
			const Token *primary = parse->pos;
			bool is_struct = primary->kind == Tok_Key_Struct;
			parse->pos++;
			Attributes attr = parseAttributes(parse);
			(void) attr;

			Symbol *named = NULL;
			if (parse->pos->kind == Tok_Identifier) {
				named = getSymbol(parse, parse->pos->val.identifier);
				parse->pos++;
			}
			// A tagged struct or union type followed by its definition
			// or by a semicolon declares that type tag into the
			// scope; other usage may refer to an existing type.
			bool can_declare = parse->pos->kind == Tok_Semicolon
					|| parse->pos->kind  == Tok_OpenBrace;

			// A tagged struct or union without definition which
			// does not refer to a previous declaration declares its
			// tag as an incomplete type.
			Type body_type = BASIC_VOID;
			if (tryEat(parse, Tok_OpenBrace)) {
				body_type = parseStructUnionBody(parse, is_struct);
			} else if (!named) {
				parseerror(parse, primary, "name or %s required after %s", tokenName(Tok_OpenBrace), tokenName(Tok_Key_Struct));
			}


			if (named) {
				NameTaggedType *existing = named->nametagged;
				if (!existing ||
					(can_declare && existing->scope_depth < parse->scope_depth))
				{
					NameTaggedType *new = ALLOC(parse->code_arena, NameTaggedType);
					*new = (NameTaggedType) {
						.name = named->name,
						.shadowed = named->nametagged,
						.scope_depth = parse->scope_depth,
						.type = body_type,
					};
					named->nametagged = new;

				} else {
					if (existing->type.kind == Kind_Void) {
						existing->type = body_type;
					} else if (body_type.kind != Kind_Void) {
						// A tagged struct or union with definition must match
						// all previous defintions.
						bool equal = false;
						Type prev = existing->type;

						if (prev.kind == Kind_Struct &&
							prev.members.len == body_type.members.len)
						{
							equal = true;
							for (u32 i = 0; i < prev.members.len; i++) {
								if (!typeCompatible(prev.members.ptr[i].type, body_type.members.ptr[i].type)) {
									equal = false;
									break;
								}
							}
						}
						if (!equal)
							redefinition(parse, primary, existing->def_location, named->name);
					}
				}
				base.kind = Kind_Struct_Named;
				base.nametagged = named->nametagged;
			} else {
				base = body_type;
			}

			modifiable = false;
			parse->pos--;
		} break;
		case Tok_Key_Enum: {
			parse->pos++;
			Attributes attr = parseAttributes(parse);
			(void) attr;
			if (tryEat(parse, Tok_Identifier)) {
				// TODO
			}
			expect(parse, Tok_OpenBrace);

			for (i32 value = 0;; value++) {
				const Token *primary = parse->pos;
				String name = expect(parse, Tok_Identifier).val.identifier;

				if (tryEat(parse, Tok_Equals)) {
					const Token *expr_token = parse->pos;
					Value val = parseExprAssignment(parse);

					u64 int_val;
					if (!tryIntConstant(parse, val, &int_val))
						parseerror(parse, expr_token, "enumeration values must be constant expressions");
					// TODO Range check this downcast.
					value = int_val;
				}
				bool new;
				OrdinaryIdentifier *ident = genOrdSymbol(parse, name, &new);
				if (!new)
					redefinition(parse, NULL, ident->def_location, name);

				ident->kind = Sym_EnumConstant;
				ident->decl_location = ident->def_location = primary;
				ident->enum_constant = value;

				if (tryEat(parse, Tok_Comma)) {
					if (tryEat(parse, Tok_CloseBrace)) {
						requires(parse, "trailing comma in enumerator list", Features_C99);
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
				requires(parse, "long long types", Features_C99);
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
			requires(parse, "atomic types", Features_C11);
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
		case Tok_Key_Inline:
		case Tok_Key_Noreturn:
			// TODO
			break;
		case Tok_Key_Threadlocal:
			requires(parse, "thread-local types", Features_C11);
			parseerror(parse, parse->pos, "TODO Support _Thread_local");
// 			base.storage = Storage_Threadlocal;
// 			storages++;
			break;
			// TODO Parse function specifiers inline and _Noreturn
		case Tok_Identifier: {
			Symbol *sym = mapGet(&symbols, parse->pos->val.identifier);

			// Hacky way to correctly accept re-typedefs.
			// TODO Find a correct parser.
			if (sym && sym->ordinary && sym->ordinary->kind == Sym_Typedef
				&& !(storage == Storage_Typedef &&
					(parse->pos[1].kind == Tok_Semicolon || parse->pos[1].kind == Tok_Comma)))
			{
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
				parseerror(parse, longness[0], "integer cannot be %sshort%s and %long%s at the same time", BOLD, RESET, BOLD, RESET);
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
					parseerror(parse, begin, "support for implicit %s in declarations was removed in C99", printTypeHighlighted(&parse->arena, BASIC_INT));
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

static Type parseStructUnionBody(Parse *parse, bool is_struct) {
	LIST(CompoundMember) members = {0};
	u32 current_offset = 0;
	while (!tryEat(parse, Tok_CloseBrace)) {
		// May not be function type, struct or union ending
		// with an incomplete array, or incomplete type except
		// that the last item may be an incomplete array.
		// TODO Check this.

		const Token *begin = parse->pos;
		Type base = parseTypeBase(parse, NULL);

		if (tryEat(parse, Tok_Semicolon)) {
			Type t = resolveType(base);
			if (t.kind != Kind_Struct && t.kind != Kind_Union)
				parseerror(parse, begin, "missing");
			requires(parse, "anonymous members", Features_C11);

			u32 offset = 0;
			if (is_struct)
				offset = addMemberOffset(&current_offset, t, &parse->target);
			for (u32 i = 0; i < t.members.len; i++) {
				// TODO Check that the last member is not a flexible array.
				CompoundMember m = t.members.ptr[i];
				m.offset += offset;
				PUSH_A(parse->code_arena, members, m);
			}
			continue;
		}

		do {
			const Token *begin = parse->pos;
			bool bare_colon = tryEat(parse, Tok_Colon);

			Declaration decl;
			if (!bare_colon)
				decl = parseDeclarator(parse, base, Decl_Named);

			if (bare_colon || tryEat(parse, Tok_Colon)) {
				const Token *colon = parse->pos - 1;
				Type basetype = bare_colon ? base : decl.type;
				if (basetype.kind != Kind_Basic)
					parseerror(parse, colon, "a bit field must have integer type, %s does not work",
							printTypeHighlighted(&parse->arena, basetype));
				if (basetype.basic != Int_bool && (basetype.basic & ~Int_unsigned) != Int_int)
					requires(parse, "bit fields of types other than int", Features_GNU_Extensions);

				u64 bitsize;
				if (!tryIntConstant(parse, parseExpression(parse), &bitsize))
					parseerror(parse, colon, "the width of a bit field must be a constant integer expression");
				// TODO
			}

			// TODO Support VLAs
			u32 member_offset = 0;
			if (is_struct)
				member_offset = addMemberOffset(&current_offset, decl.type, &parse->target);

			if (arrayUnknownSize(decl.type)) {
				if (!(parse->pos[0].kind == Tok_Semicolon && parse->pos[1].kind == Tok_CloseBrace)) {
					// TOOD "such a structure (and any union containing,
					// possibly recursively, a member that is such a
					// structure) shall not be a member of a structure or an
					// element of an array."
					parseerror(parse, begin, "only the last member of a struct member may have incomplete array type");
				}
				requires(parse, "flexible array members", Features_C99);
				if (members.len == 0)
					requires(parse, "flexible array members without preceding members", Features_GNU_Extensions);
			}

			PUSH_A(parse->code_arena, members, ((CompoundMember) {decl.type, decl.name, member_offset}));
		} while (tryEat(parse, Tok_Comma));
		expect(parse, Tok_Semicolon);
	}
	return (Type) {
		.kind = Kind_Struct,
		.members = {
			.ptr = members.ptr,
			.len = members.len,
		},
	};
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
	switch (base_type.kind) {
	case Kind_Struct:
	case Kind_Struct_Named:
	case Kind_Union:
	case Kind_Union_Named:
	case Kind_Enum:
		return tryEat(parse, Tok_Semicolon);
	default:
		return false;
	}
}

static Declaration parseDeclarator (Parse *parse, Type base_type, Namedness named) {
	Type base = base_type;

	while (parse->pos->kind == Tok_Asterisk) {
		parse->pos++;
		Attributes attr = parseAttributes(parse);
		(void) attr;

		Type *ptr = ALLOC(parse->code_arena, Type);
		*ptr = base;

		base = (Type) {Kind_Pointer,
			.qualifiers = parseQualifiers(parse),
			.pointer = ptr,
		};
	}

	Declaration decl = {0};
	Type *inner = &decl.type;
	Type *enclosing = NULL;

	// For abstract declarators, empty parentheses in a type name are
	// “interpreted as function with no parameter specification”,
	// rather than redundant parentheses around the omitted identifier.
	if (parse->pos[0].kind == Tok_OpenParen && parse->pos[1].kind != Tok_CloseParen) {
		parse->pos++;
		decl = parseDeclarator(parse, BASIC_VOID, named);
		expect(parse, Tok_CloseParen);

		while (inner->kind != Kind_Void) {
			switch (inner->kind) {
			case Kind_Pointer:
				enclosing = inner;
				inner = inner->pointer;
				break;
			case Kind_Array:
			case Kind_VLArray:
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
		if (parse->pos->kind == Tok_Identifier && named != Decl_Abstract)
			decl.name = expect(parse, Tok_Identifier).val.identifier;
		else if (named == Decl_Named)
			parseerror(parse, parse->pos, "expected an identifier");
	}

	while (true) {
		if (tryEat(parse, Tok_OpenParen)) {
			if (enclosing) {
				if (enclosing->kind == Kind_Function)
					parseerror(parse, NULL, "a function cannot return a function. You may want to return a function pointer instead");
				else if (enclosing->kind == Kind_Array)
					parseerror(parse, NULL, "an array cannot contain a function. You may want to store function pointers instead");
			}

			if (enclosing && enclosing->kind == Kind_Pointer) {
				enclosing->kind = Kind_FunctionPtr;
				enclosing->function = (FunctionType) { enclosing->pointer };
				inner = enclosing;
			} else {
				Type *rettype = ALLOC(parse->code_arena, Type);
				*rettype = *inner;

				inner->kind = Kind_Function;
				inner->function = (FunctionType) { rettype };
			}

			if (parse->pos->kind == Tok_CloseParen) {
				// TODO C23
				inner->function.missing_prototype = true;
			} else if (parse->pos[0].kind == Tok_Key_Void && parse->pos[1].kind == Tok_CloseParen) {
				parse->pos++;
			} else {
				do {
					if (tryEat(parse, Tok_TripleDot)) {
						inner->function.is_vararg = true;
						break;
					}
					u8 storage;
					Type param_type = parseTypeBase(parse, &storage);
					Declaration param_decl = parseDeclarator(parse, param_type, Decl_PossiblyAbstract);
					if (param_decl.type.kind == Kind_Array) {
						param_decl.type.kind = Kind_Pointer;
						param_decl.type.pointer = param_decl.type.array.inner;
					} else if (param_decl.type.kind == Kind_Function) {
						param_decl.type.kind = Kind_FunctionPtr;
					}
					PUSH_A(parse->code_arena, inner->function.parameters, param_decl);
				} while (tryEat(parse, Tok_Comma));
			}

			expect(parse, Tok_CloseParen);

			enclosing = inner;
			inner = inner->function.rettype;
		} else if (tryEat(parse, Tok_OpenBracket)) {
			const Token *primary = parse->pos - 1;
			if (enclosing && enclosing->kind == Kind_Function)
				parseerror(parse, NULL, "a function cannot return an array. You may want to return a pointer to an array instead");

			Type *content = ALLOC(parse->code_arena, Type);
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
				requires(parse, "static specifiers on array parameters", Features_C99);
			}

			if (tryEat(parse, Tok_Asterisk)) {
				parseerror(parse, NULL, "TODO Support ‘variable length array of unspecified size’, whatever that may be");
				inner->array.count = 0;
			} else if (parse->pos->kind == Tok_CloseBracket) {
				inner->kind = Kind_VLArray;
				inner->array.count = IR_REF_NONE;
			} else {
				inner->array.count = coerce(parseExprAssignment(parse), parse->target.ptrdiff, parse, primary);
				if (parse->build.ir.ptr[inner->array.count].kind != Ir_Constant) {
					requires(parse, "variable length arrays", Features_C99);
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
		Declaration decl = parseDeclarator(parse, base_type, Decl_Named);
		// TODO Error on overwriting
		bool new;
		OrdinaryIdentifier *ident = genOrdSymbol(parse, decl.name, &new);
		if (!new) {
			if (ident->kind != Sym_Typedef || !typeCompatible(ident->typedef_type, decl.type))
				redefinition(parse, begin, ident->def_location, decl.name);
		} else {
			ident->kind = Sym_Typedef;
			ident->decl_location = ident->def_location = begin;
			ident->typedef_type = decl.type;
		}
	} while (tryEat(parse, Tok_Comma));

	expect(parse, Tok_Semicolon);
}


// ===> Type operations and semantic analysis


/*
The semantics of declarations and linkages in C are the most
aesthetically offensive piece of logic I have ever read. And this does
not even cover the bizarre MSVC modifications to scope, like function
declarations always being at file scope.

This code tries to express some finnicky bits in the standard's wording
of linkage, resulting in some abstractness and redundant representations
of data. I usually like to compress that kind of stuff down, but here it
looks like explaining the correctness of the result would be more effort
than it's worth.
*/

static OrdinaryIdentifier *define (
	Parse *parse,
	Declaration decl,
	u8 storage,
	const Token *type_token,
	const Token *decl_token,
	const Token *defining_token,
	bool file_scope)
{
	typedef enum {
		Link_None,
		Link_External,
		Link_Internal,
	} Linkage;
	bool msvc = parse->target.version & Features_MSVC_Extensions;

	Symbol *sym = getSymbol(parse, decl.name);
	OrdinaryIdentifier *existing = sym->ordinary;
	Linkage existing_link = Link_None;
	u32 scope_depth = parse->scope_depth;

	if (msvc && decl.type.kind == Kind_Function)
		scope_depth = 0;

	if (existing && existing->kind == Sym_Value_Static) {
		existing_link = parse->module->ptr[existing->static_id].is_public ?
				Link_External : Link_Internal;
	}

	// If the declaration of an identifier for a function has no
	// storage-class specifier, its linkage is determined exactly as if
	// it were declared with the storage-class specifier extern.
	if (decl.type.kind == Kind_Function && storage == Storage_Unspecified)
		storage = Storage_Extern;


	Linkage link = Link_None;
	if (storage == Storage_Static || storage == Storage_Constexpr) {
		link = Link_Internal;
		if (msvc && existing && existing->kind == Sym_Value_Static) {
			parse->module->ptr[existing->static_id].is_public = false;
			existing_link = Link_Internal;
		}
	} else if (storage == Storage_Extern) {
		if (defining_token) {
			// GCC reports the initialization of a non-file-scope extern
			// identifier as an error, which matches my intuition, but I
			// have not yet found the relevant section in the standard.
			if (!file_scope)
				parseerror(parse, defining_token, "cannot initialize an %s%s%s declaration", BOLD, "extern", RESET);
		}
		// For an identifier declared with the storage-class specifier
		// extern in a scope in which a prior dec- laration of that
		// identifier is visible, if the prior declaration specifies
		// internal or external linkage, the linkage of the identifier at
		// the later declaration is the same as the linkage specified at the
		// prior declaration. If no prior declaration is visible, or if the
		// prior declaration specifies no linkage, then the identifier has
		// external linkage.
		if (existing && existing_link != Link_None)
			link = existing_link; // TOOD The later declaration may hide the former declaration...
		else
			link = Link_External;
	} else if (file_scope) {
		// If the declaration of an identifier for an object has file
		// scope and no storage-class specifier, its linkage is
		// external.
		link = Link_External;
	}

	if (decl.type.kind == Kind_Function && !file_scope && storage != Storage_Extern)
		parseerror(parse, type_token, "a function at block scope may only be specified extern");

	// (6.7.1.9)
	bool tentative = file_scope && decl.type.kind != Kind_Function && !defining_token && (storage == Storage_Static || storage == Storage_Unspecified);
	if (tentative && storage == Storage_Static && typeSize(decl.type, &parse->target) == 0)
		parseerror(parse, decl_token, "tentative definition with internal linkage may not have incomplete type");

	if (existing && existing->scope_depth == parse->scope_depth) {
		Type existing_type;
		StaticValue *staticval = NULL;

		if (existing->kind == Sym_Value_Auto) {
			existing_type = existing->value.typ;
		} else if (existing->kind == Sym_Value_Static) {
			staticval = &parse->module->ptr[existing->static_id];
			existing_type = staticval->type;
		} else {
			// Actually: Redeclaration as different kind of symbol.
			redefinition(parse, decl_token, existing->def_location, decl.name);
		}

		if (existing_link != link || !typeCompatible(decl.type, existing_type))
			// Actually: Redeclaration with incompatible linkage/type.
			redefinition(parse, decl_token, existing->def_location, decl.name);


		if (defining_token) {
			if (!staticval || staticval->def_state == Def_Defined)
				redefinition(parse, defining_token, existing->def_location, decl.name);
			staticval->def_state = Def_Defined;
		}
		return existing;
	}

	sym->ordinary = ALLOC(&parse->arena, OrdinaryIdentifier);
	*sym->ordinary = (OrdinaryIdentifier) {
		.kind = link == Link_None ? Sym_Value_Auto : Sym_Value_Static,
		.shadowed = existing,
		.scope_depth = scope_depth,
		.decl_location = decl_token,
		.def_location = defining_token,
	};

	if (link != Link_None) {
		sym->ordinary->static_id = parse->module->len;
		PUSH(*parse->module, ((StaticValue) {
			.name = decl.name,
			.type = decl.type,
			.is_public = link == Link_External,
			.decl_location = decl_token,
			.def_kind = decl.type.kind == Kind_Function ? Static_Function : Static_Variable,
			.def_state = defining_token ? Def_Defined :
					tentative ? Def_Tentative : Def_Undefined,
		}));
	}

	PUSH(parse->current_scope, sym);
	return sym->ordinary;
}


static bool tryIntConstant (Parse *parse, Value v, u64 *result) {
	if (isLvalue(v)) return false;

	Inst inst = parse->build.ir.ptr[v.inst];
	if (inst.kind != Ir_Constant) return false;

	*result = inst.constant;
	return true;
}

static Value arithAdd (Parse *parse, const Token *primary, Value lhs, Value rhs) {
	assert(!isLvalue(lhs));
	assert(!isLvalue(rhs));
	if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
		return pointerAdd(&parse->build, lhs, rhs, parse, primary);
	} else {
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		return (Value) {common, genAdd(&parse->build, lhs.inst, rhs.inst)};
	}
}

static Value arithSub (Parse *parse, const Token *primary, Value lhs, Value rhs) {
	assert(!isLvalue(lhs));
	assert(!isLvalue(rhs));
	IrBuild *build = &parse->build;
	if (lhs.typ.kind == Kind_Pointer) {
		IrRef stride = genImmediateInt(build,
				typeSize(*lhs.typ.pointer, &parse->target), parse->target.ptr_size);

		if (rhs.typ.kind == Kind_Pointer) {
			IrRef diff = genSub(build, lhs.inst, rhs.inst);
			return (Value) {parse->target.ptrdiff, genDiv(build, diff, stride)};
		} else {
			IrRef idx = genMul(build, coercerval(rhs, parse->target.ptrdiff, parse, primary, false), stride);
			return (Value) {lhs.typ, genSub(build, lhs.inst, idx)};
		}
	} else {
		if (rhs.typ.kind == Kind_Pointer)
			parseerror(parse, primary, "cannot subtract pointer from %s", printType(&parse->arena, rhs.typ));
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		return (Value) {common, genSub(build, lhs.inst, rhs.inst)};
	}
}


static Type arithmeticConversions (Parse *parse, const Token *primary, Value *a, Value *b) {
	*a = intPromote(*a, parse, primary);
	*b = intPromote(*b, parse, primary);

	i32 rank_diff = rankDiff(a->typ.basic, b->typ.basic);
	// a shall have the type rank of lower than or equal to b, or be the
	// signed one if the ranks are equal.
	if (rank_diff > 0 || (rank_diff == 0 && (b->typ.basic & Int_unsigned))) {
		Value *tmp = a;
		a = b;
		b = tmp;
		rank_diff = -rank_diff;
	}
	Type lower = a->typ;
	Type higher = b->typ;


	/*
	From https://en.cppreference.com/w/c/language/conversion:
	If the types have the same signedness (both signed or both unsigned), the operand whose type has the lesser conversion rank1 is implicitly converted to the other type.
	Else, the operands have different signedness:
		If the unsigned type has conversion rank greater than or equal to the rank of the signed type, then the operand with the signed type is implicitly converted to the unsigned type.
		Else, the unsigned type has conversion rank less than the signed type:
			If the signed type can represent all values of the unsigned type, then the operand with the unsigned type is implicitly converted to the signed type.
			Else, both operands undergo implicit conversion to the unsigned type counterpart of the signed operand's type.

    So the logic is:

	if (lhs.typ.basic & Int_unsigned == higher.basic & Int_unsigned) {
		common = higher;
	} else {
		if (higher.basic & Int_unsigned) {
			common = higher; // Warn
		} else { // lower.basic & Int_unsigned, rank(lower) < rank(higher
			if (can_represent_all) {
				common = higher;
			} else {
				common = higher;
				common.basic |= Int_unsigned;
				// Warn
			}
		}
	}

	Therefore, unsigned->signed is only performed if it the signed can hold the
	unsigned's value range (as other strategies could cause undefined behavior).
	*/

	Type common = higher;
	if ((lower.basic & Int_unsigned) != (higher.basic & Int_unsigned)) {
		bool signed_cannot_represent = typeSize(lower, &parse->target) == typeSize(higher, &parse->target);
		if ((lower.basic & Int_unsigned) && signed_cannot_represent) {
			common.basic |= Int_unsigned;
			b->typ.basic |= Int_unsigned;
		}

		if ((common.basic & Int_unsigned) && parse->opt->warn_on_wrapping) {
			Value *signed_val = higher.basic & Int_unsigned ? a : b;
			u64 constval;
			if (!tryIntConstant(parse, *signed_val, &constval) || (i64) constval < 0) {
				parsemsg(Log_Warn, parse, primary, "wrapping negative value range when converting operands to common type %s",
						printTypeHighlighted(&parse->arena, common));
				parsemsg(Log_Info | Log_Noexpand, parse, primary, "explicitly cast operands to an unsigned type to suppress this warning");
			}
		}
	}
	*a = (Value) {common, coercerval(*a, common, parse, primary, false)};

	return common;
}


static Value pointerAdd (IrBuild *ir, Value lhs, Value rhs, Parse *op_parse, const Token *token) {
	assert(!isLvalue(lhs));
	assert(!isLvalue(rhs));
	assert(lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer);

	if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer)
		parseerror(op_parse, token, "cannot add two pointers");
	Value ptr;
	IrRef integer;
	if (lhs.typ.kind == Kind_Pointer) {
		ptr = lhs;
		integer = coercerval(rhs, op_parse->target.ptrdiff, op_parse, token, false);
	} else {
		integer = coercerval(lhs, op_parse->target.ptrdiff, op_parse, token, false);
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


static Value intPromote (Value val, Parse *p, const Token *primary) {
	val = rvalue(val, p);
	if (val.typ.kind == Kind_Enum) {
		val.typ = BASIC_INT;
		return val;
	}

	if (val.typ.kind != Kind_Basic)
		parseerror(p, primary, "expected a scalar type");

	if (rankDiff(val.typ.basic, Int_int) >= 0)
		return val;
	const Type unsignedint = {Kind_Basic, .basic = Int_int | Int_unsigned};

	if (val.typ.basic & Int_unsigned)
		return (Value) {unsignedint, coerce(val, unsignedint, p, primary)};
	else
		return (Value) {BASIC_INT, coerce(val, BASIC_INT, p, primary)};
}

static void discardValue(Parse *parse, const Token *token, Value v) {
	(void) parse;
	(void) token;
	(void) v;
}

// Performs lvalue conversion, array to pointer conversion and function
// to pointer conversion as necessary. This is idempotent, but I still
// want to avoid just sprinkling it everywhere.
Value rvalue (Value v, Parse *parse) {
	if (v.typ.kind == Kind_Function) {
		v.typ.kind = Kind_FunctionPtr;
	} else if (v.typ.kind == Kind_Array) {
		assert(isLvalue(v));
		v.typ.kind = Kind_Pointer;
		v.typ.pointer = v.typ.array.inner;
	} else if (isLvalue(v)) {
		v.inst = genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target));
		v.typ.qualifiers = 0;
	}
	v.category = Ref_RValue;
	return v;
}

static IrRef coerce (Value v, Type t, Parse *p, const Token *primary) {
	return coercerval(rvalue(v, p), t, p, primary, false);
}

static IrRef toBoolean (Parse *p, const Token *primary, Value v) {
	IrBuild *build = &p->build;

	if (v.typ.kind != Kind_Basic && v.typ.kind != Kind_Enum && v.typ.kind != Kind_Pointer)
		parseerror(p, primary, "(TODO Explain this better) expected an expression of scalar type");

	u32 size = typeSize(v.typ, &p->target);
	IrRef zero = genImmediateInt(build, 0, size);
	return genNot(build, genEquals(build, rvalue(v, p).inst, zero, typeSize(BASIC_INT, &p->target)));
}

// Performs implicit conversions on rvalues.
static IrRef coercerval (Value v, Type t, Parse *p, const Token *primary, bool allow_casts) {
	assert(!isLvalue(v));
	IrBuild *build = &p->build;

	if (t.kind == Kind_Void)
		return IR_REF_NONE;
	if (typeCompatible(v.typ, t))
		return v.inst;
	if (t.basic == Int_bool)
		return toBoolean(p, primary, v);

	// Integer conversions
	if (v.typ.kind == Kind_Basic && t.kind == Kind_Basic) {
		u16 target = p->target.typesizes[t.basic & ~Int_unsigned];
		u16 source = p->target.typesizes[v.typ.basic & ~Int_unsigned];
		assert(source == build->ir.ptr[v.inst].size);

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
			&& (v.typ.pointer->kind == Kind_Void || t.pointer->kind == Kind_Void || allow_casts))
	{
		return v.inst;
	}

	if (allow_casts) {
		// NOTE This relies on pointers being the largest types.
		if (t.kind == Kind_Basic && v.typ.kind == Kind_Pointer)
			return genTrunc(build, v.inst, p->target.typesizes[t.basic & ~Int_unsigned]);
		if (t.kind == Kind_Pointer && v.typ.kind == Kind_Basic)
			return genZeroExt(build, v.inst, p->target.ptr_size);
		if (t.kind == Kind_FunctionPtr && v.typ.kind == Kind_FunctionPtr)
			return v.inst;
	}
	parseerror(p, primary, "could not convert type %s to type %s",
		printTypeHighlighted(&p->arena, v.typ), printTypeHighlighted(&p->arena, t));
}

static Value immediateIntVal (Parse *p, Type typ, u64 val) {
	return (Value) { typ,
		genImmediateInt(&p->build, val, typeSize(typ, &p->target))
	};
}

static void requires (Parse *parse, const char *desc, Features required) {
	if (!(parse->target.version & required))
		parseerror(parse, NULL, "%s are not supported under the current target (%s)",
				desc, versionName(parse->target.version));
}
