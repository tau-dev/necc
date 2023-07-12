#include "parse.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>

#include "ir_gen.h"
#include "ansi.h"
#include "dbgutils.h"


/*

Parses and typechecks tokens into IR in a single pass.

*/



static Type chartype = {Kind_Basic, .basic = Int_char};
static Type const_chartype = {Kind_Basic, .qualifiers = Qualifier_Const, .basic = Int_char};
static Value no_value = { .inst = IDX_NONE };


typedef LIST(Symbol*) Scope;

typedef struct {
	Arena *arena;
	Arena *code_arena;
	const Tokenization tokens;
	const Token *pos;
	const Target target;
	Options *opt;

	u32 scope_depth;
	// At the top level, these may be null.
	Scope current_scope;

	u32 current_func_id;
	FunctionType current_func_type;
	IrBuild build;
	Scope func_goto_labels;

	Block *switch_block;
	Block *current_loop_switch_exit;
	Block *current_loop_head;

	Module *module;
} Parse;


const char *plxz(const Parse *parse) {
	return lexz(parse->tokens.list, parse->pos, NULL, 12);
}


typedef LIST(Reference) RefsList;
typedef struct {
	IrRef address;

	RefsList *reloc_references;
	MutableString reloc_data;
} InitializationDest;

static void vcomperror (Log level, bool crash_on_error, const Tokenization *t, const Token *tok, const Token *parse_pos, const char *msg, va_list args) {
	(void) parse_pos;
	u32 idx = tok - t->list.tokens;
	TokenLocation pos = t->list.positions[idx];
	SourceFile source = *t->files.ptr[pos.source.file_id];

	printMsg(level, source, pos.source);

	vfprintf(stderr, msg, args);
	fprintf(stderr, ".\n");
	if (pos.macro.line && !(level & Log_Noexpand)) {
		printInfo(*t->files.ptr[pos.macro.file_id], pos.macro);
		fprintf(stderr, "(macro-expanded from here)\n");
	}

	if (level & Log_Fatal) {
		if (crash_on_error) {
			puts(lexz(t->list, tok, parse_pos, 12));
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
	vcomperror(Log_Err | Log_Fatal, p->opt->crash_on_error, &p->tokens, main ? main : p->pos - 1, p->pos, msg, args);
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



static void redefinitionError (const Parse *p, const Token *main, const Token *previous, Symbol *name) {
	parsemsg(Log_Err, p, main, "redefinition of %.*s", name->name);
	parsemsg(Log_Info | Log_Fatal, p, previous, "previous definition was here");
}

_Noreturn static void unexpectedToken (const Parse *p, TokenKind expected) {
	parseerror(p, p->pos, "expected %s before the %s token", tokenName(expected), tokenName(p->pos->kind));
}

static Token expect (Parse *parse, TokenKind expected) {
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
static inline void eatMatchingParens (Parse *parse) {
	const Token *opening_paren = parse->pos-1;
	assert(opening_paren->kind == Tok_OpenParen);

	u32 parens = 1;
	while (parens > 0) {
		switch (parse->pos->kind) {
		case Tok_OpenParen: parens++; break;
		case Tok_CloseParen: parens--; break;
		case Tok_EOF:
			parseerror(parse, opening_paren, "missing closing parenthesis");
			break;
		default: break;
		}
		parse->pos++;
	}
}

static inline void setLoc (Parse *parse, const Token *tok) {
	parse->build.loc = parse->tokens.list.positions[tok - parse->tokens.list.tokens].source;
}

// Whether an identifier is expected to be on a declarator.
typedef enum {
	Decl_Named,            // E.g. variables
	Decl_PossiblyAbstract, // E.g. parameters
	Decl_Abstract,         // E.g. type names
} Namedness;



typedef enum {
	DeclKind_Declaration,
	DeclKind_Definition,
	DeclKind_Typedef,
} DeclKind;

static Declaration parseDeclarator(Parse *, Type base_type, Namedness);
static inline void markDecl(const Parse *, const Token *, Declaration, DeclKind);
static inline void markDeclScopeBegin(String name);
static inline void markDeclScopeEnd();

static nodiscard bool allowedNoDeclarator(Parse *, Type base_type);
// static Declaration parseDeclarator (Parse* parse, const Token **tok, Type base_type);
static void initializeStaticToZero(Parse *, StaticValue *);
static void initializeStaticDefinition(Parse *, OrdinaryIdentifier *, Type, const Token *init_token);
static void initializeAutoDefinition(Parse *, OrdinaryIdentifier *, Type, const Token *init_token);
static IrRef checkVaListValue(Parse *parse, Value v, const Token *pos);
static void parseInitializer(Parse *, InitializationDest, u32 offset, Type type);
static void maybeBracedInitializer(Parse *, InitializationDest, u32 offset, Type type, Value current_value);
static bool tryIntConstant(Parse *, Value, u64 *result);
static bool tryEatStaticAssert(Parse *);
static Value arithAdd(Parse *parse, const Token *primary, Value lhs, Value rhs);
static Value arithSub(Parse *parse, const Token *primary, Value lhs, Value rhs);
static Value arithMultiplicativeOp(Parse *parse, const Token *primary, TokenKind op, Value lhs, Value rhs);
static Type arithmeticConversions(Parse *parse, const Token *primary, Value *lhs, Value *rhs);
static Type parseValueType(Parse *, Value (*operator)(Parse *));
static Type parseTypeBase(Parse *, u8 *storage);
static Type parseTypeName(Parse *, u8 *storage);
static nodiscard bool tryParseTypeBase(Parse *, Type *dest, u8 *storage);
static nodiscard bool tryParseTypeName(Parse *, Type *dest, u8 *storage);
static void parseFunction(Parse *, Symbol *name);
static Value rvalue(Value v, Parse *);
static bool isStringType(Type typ);
static Value dereference(Parse *, Value v);
static Value pointerAdd(IrBuild *, Value lhs, Value rhs, Parse *op_parse, const Token *);
static bool comparablePointers(Parse *, Value a, Value b);
static void parseTypedefDecls(Parse *, Type base_type);
static u32 parseStringLiteral(Parse *);
static Attributes parseAttributes(Parse *);
static void popScope(Parse *, Scope);
static nodiscard Scope pushScope(Parse *);
static void requires(Parse *, const char *desc, Features);
static OrdinaryIdentifier *define(Parse *, Declaration, u8 storage, const Token *type_token, const Token *decl_token, const Token *def_token, u32 parent_decl_id);



// Parses all top level declarations into a Module.
void parse (Arena *code_arena, Tokenization tokens, Options *opt, Module *module) {
	Arena parse_arena = create_arena(256 * 1024L);
	Parse parse = {
		.arena = &parse_arena,
		.code_arena = code_arena,
		// Used only as a scratch buffer for constructing constants at the
		// top level. Kind of inefficient, but very straightforward.
		.build = {.block_arena = code_arena},
		.tokens = tokens,
		.target = opt->target,
		.opt = opt,
		.pos = tokens.list.tokens,
		.module = module,
		.current_func_id = IDX_NONE,
	};
	startNewBlock(&parse.build, zstr("dummy"));

	while (parse.pos->kind != Tok_EOF) {
		u8 storage;
		while (parse.pos->kind == Tok_Semicolon) {
			parse.pos++;
			requires(&parse, "semicolons at file scope", Features_GNU_Extensions);
		}
		if (parse.pos->kind == Tok_EOF) break;
		if (tryEatStaticAssert(&parse))
			continue;

		const Token *type_token = parse.pos;
		Attributes attr = parseAttributes(&parse);
		(void) attr;
		Type base_type = parseTypeBase(&parse, &storage);

		if (allowedNoDeclarator(&parse, base_type))
			continue;

		if (storage == Storage_Typedef) {
			parseTypedefDecls(&parse, base_type);
			continue;
		}

		if (storage == Storage_Auto || storage == Storage_Register)
			parseerror(&parse, type_token, "identifier at file scope can not have automatic (stack-allocated) storage duration");


		u32 declarators = 0;
		while (true) {
			const Token *primary = parse.pos;
			Declaration decl = parseDeclarator(&parse, base_type, Decl_Named);
			Type first_argument_type;


			bool function_def = decl.type.kind == Kind_Function && declarators == 0 &&
				(parse.pos->kind == Tok_OpenBrace || tryParseTypeBase(&parse, &first_argument_type, NULL));
			bool object_def = decl.type.kind != Kind_Function && parse.pos->kind == Tok_Equal;

			OrdinaryIdentifier *ord = define(&parse, decl, storage, type_token, primary,
					function_def || object_def ? parse.pos : NULL, IDX_NONE);


			if (function_def) {
				// TODO Allow old-style definitions before C23

				IrBuild global_ir = parse.build;
				parse.current_func_type = decl.type.function;
				parse.current_func_id = ord->static_id;
				markDeclScopeBegin(decl.name->name);

				parseFunction(&parse, decl.name);

				markDeclScopeEnd();
				parse.current_func_id = IDX_NONE;
				parse.scope_depth = 0;

				StaticValue *val = &parse.module->ptr[ord->static_id];
				val->def_state = Def_Defined;
				val->function_ir = parse.build.ir;

				parse.build = global_ir;
				break;
			} else if (object_def) {
				const Token *eq = parse.pos++;
				initializeStaticDefinition(&parse, ord, decl.type, eq);
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
			initializeStaticToZero(&parse, val);
		} else if (val->is_used && val->def_state == Def_Undefined && !val->is_public)
			parseerror(&parse, val->decl_location, "TODO(phrasing) static identifier was never defined");
	}

	discardIrBuilder(&parse.build);
	popScope(&parse, (Scope) {0});
	free_arena(parse.arena, "parse temporaries");
}


OrdinaryIdentifier *genOrdSymbol (Parse *parse, Symbol *sym, bool *new) {
	bool is_new = !sym->ordinary || sym->ordinary->scope_depth < parse->scope_depth;
	if (is_new) {
		OrdinaryIdentifier *created = ALLOC(parse->arena, OrdinaryIdentifier);
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
		OrdinaryIdentifier *ord = def.ptr[i]->ordinary;
		assert(!ord || ord->scope_depth <= parse->scope_depth);
		if (ord && ord->scope_depth == parse->scope_depth) {
			if (!ord->is_used && parse->opt->warn_unused) {
				if (ord->kind == Sym_Value_Auto) {
					parsemsg(Log_Warn, parse, ord->decl_location, "‘%.*s’ is never used", STRING_PRINTAGE(def.ptr[i]->name));
				} else if (ord->kind == Sym_Value_Static) {
					StaticValue *val = &parse->module->ptr[ord->static_id];
					if (!val->is_public && val->name.len && val->name.ptr[0] != '_')
						parsemsg(Log_Warn, parse, ord->decl_location, "‘%.*s’ is never used", STRING_PRINTAGE(def.ptr[i]->name));
				}
			}
			def.ptr[i]->ordinary = ord->shadowed;
		}
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
static Value parseExprConditional(Parse *);
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
static Value parseExprPrefix(Parse *);
static Value parseExprPostfix(Parse *);
static Value parseExprBase(Parse *);
static Value parseIntrinsic(Parse *, Intrinsic);
// TODO The order of parameters here is inconsistent.
static Value intPromote(Value v, Parse *, const Token*);
static void needAssignableLval(Parse *, const Token *, Value v);
static void needScalar(Parse *, const Token *, Type t);
static void discardValue(Parse *, const Token *, Value v);
static IrRef coerce(Value v, Type t, Parse *, const Token *);
static IrRef toBoolean(Parse *, const Token *, Value v);
static IrRef coercerval(Value v, Type t, Parse *, const Token *, bool allow_casts);
static Value immediateIntVal(Parse *, Type typ, u64 val);
static inline void removeEnumness(Parse *, Type *t);

// TODO May want signed values??
u64 evalPreprocExpression (Tokenization tokens, Arena *arena, Options *opt, const Token **pos) {
	Parse parse = {
		.arena = arena,
		.code_arena = arena,
		.build = {.block_arena = arena},

		.tokens = tokens,
		.pos = *pos,
		.opt = opt,
		.target = opt->target,
	};
	Value v = parseExpression(&parse);
	*pos = parse.pos;

	u64 val;
	if (!tryIntConstant(&parse, v, &val))
		parseerror(&parse, tokens.list.tokens, "preprocessor expression must be constant");
	return val;
}


void parseFunction (Parse *parse, Symbol *symbol) {
	FunctionType func_type = parse->current_func_type;
	u32 param_count = func_type.parameters.len;

	IrBuild *build = &parse->build;
	*build = (IrBuild) {
		.block_arena = parse->code_arena,
		.ir = {
			.params = ALLOCN(parse->code_arena, Parameter, param_count),
		},
	};
	build->ir.entry = startNewBlock(build, zstr("entry"));
	String name = symbol->name;
	bool is_main = eql("main", name);
	if (is_main) {
		if (func_type.rettype->kind != Kind_Basic || func_type.rettype->basic != Int_int)
		{
			parseerror(parse, parse->pos, "the return type of function main() must be %s", printTypeHighlighted(parse->arena, BASIC_INT));
		}
	}


	assert(parse->scope_depth == 0);
	Scope enclosing = pushScope(parse);
	setLoc(parse, parse->pos);

	for (u32 i = 0; i < param_count; i++) {
		Declaration param = parse->current_func_type.parameters.ptr[i];

		if (param.name == NULL) {
			requires(parse, "unnamed parameters", Features_C23);
			continue;
		}
		// TODO Omit declaration data if not generating debug info.
		IrRef slot = genStackAllocNamed(build, typeSize(param.type, &parse->target), param);

		build->ir.params.ptr[i].type = param.type;
		build->ir.params.ptr[i].size = typeSize(param.type, &parse->target);
		IrRef paramval = genParameter(build, i);
		genStore(build, slot, paramval, false);

		bool is_new;
		OrdinaryIdentifier *sym = genOrdSymbol(parse, param.name, &is_new);
		// TODO This is wrong, emit duplicate parameter error.
		assert(is_new);

		sym->kind = Sym_Value_Auto;
		sym->decl_location = sym->def_location = parse->pos;
		sym->value = (Value) {param.type, slot, Ref_LValue};
	}

	Symbol *func___sym = parse->tokens.special_identifiers[Special___func__];
	char *terminated = aalloc(parse->code_arena, name.len + 1);
	memcpy(terminated, name.ptr, name.len);
	terminated[name.len] = 0;
	PUSH(*parse->module, ((StaticValue) {
		.type = {
			.kind = Kind_Array,
			.qualifiers = Qualifier_Const,
			.array = { .inner = &const_chartype, .count = name.len + 1 },
		},
		.decl_location = parse->pos,
		.def_kind = Static_Variable,
		.def_state = Def_Defined,
		.value_data = {name.len+1, terminated},
		.name = func___sym->name,
		.parent_decl = parse->current_func_id,
	}));

	bool is_new;
	OrdinaryIdentifier *func_name_sym = genOrdSymbol(parse, func___sym, &is_new);
	assert(is_new);
	func_name_sym->kind = Sym_Value_Static;
	func_name_sym->decl_location = func_name_sym->def_location = parse->pos;
	func_name_sym->static_id = parse->module->len-1;


	parseCompound(parse);

	if (is_main) {
		genReturnVal(build, genImmediateInt(build, 0, typeSize(*func_type.rettype, &parse->target)));
	} else {
		genReturnVal(build, IDX_NONE);
	}

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

static Block *getLabeledBlock (Parse *parse, Symbol *sym) {
	if (sym->label.block == NULL) {
		sym->label.block = newBlock(&parse->build, sym->name);
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

	while ((t.kind == Tok_Identifier && parse->pos[1].kind == Tok_Colon)
		|| t.kind == Tok_Key_Case || t.kind == Tok_Key_Default)
	{
		Block *new_block;
		parse->pos++;
		if (t.kind == Tok_Key_Case) {
			if (!parse->switch_block)
				parseerror(parse, NULL, "case labels may only appear in a switch statement");
			const Token *label = parse->pos;
			Value v = parseExpression(parse);
			expect(parse, Tok_Colon);

			u64 val;
			if (!tryIntConstant(parse, v, &val))
				parseerror(parse, label, "case labels must be constant expressions");

			new_block = newBlock(build, zstr("case"));
			PUSH_A(parse->code_arena, parse->switch_block->exit.switch_.cases, ((SwitchCase) {val, new_block}));
		} else if (t.kind == Tok_Key_Default) {
			if (!parse->switch_block)
				parseerror(parse, NULL, "default labels may only appear in a switch statement");
			if (parse->switch_block->exit.switch_.default_case)
				parseerror(parse, NULL, "default label cannot appear multiple times within one switch statement");

			expect(parse, Tok_Colon);

			new_block = newBlock(build, zstr("default"));
			parse->switch_block->exit.switch_.default_case = new_block;
		} else {
			Symbol *sym = t.val.symbol;

			if (sym->label.block) {
				// TODO This does not catch a series two identical labels in a row.
				if (sym->label.block->exit.kind != Exit_None)
					redefinitionError(parse, parse->pos, sym->label.def_location, t.val.symbol);
			} else {
				sym->label.def_location = parse->pos;
				sym->label.block = newBlock(build, t.val.symbol->name);
				PUSH(parse->func_goto_labels, sym);
			}

			new_block = sym->label.block;

			parse->pos++;
		}
		genJump(build, new_block);
		labeled = true;
		t = *parse->pos;
	}

	const Token *primary = parse->pos;
	setLoc(parse, primary);

	switch (t.kind) {
	case Tok_OpenBrace:
		parseCompound(parse);
		break;
	case Tok_Key_Return: {
		parse->pos++;
		// TODO Check against function return type
		if (tryEat(parse, Tok_Semicolon)) {
			genReturnVal(build, IDX_NONE);
		} else {
			IrRef val = coerce(parseExpression(parse),
					*parse->current_func_type.rettype,
					parse, primary);
			expect(parse, Tok_Semicolon);

			genReturnVal(build, val);
		}

		// Unreferenced dummy block for further instructions, will be ignored
		startNewBlock(build, STRING_EMPTY);
	} break;
	case Tok_Key_Do: {
		parse->pos++;
		*had_non_declaration = true;

		Block *outer_head = parse->current_loop_head;
		Block *outer_exit = parse->current_loop_switch_exit;

		parse->current_loop_switch_exit = newBlock(build, zstr("do_while_join"));
		parse->current_loop_head = newBlock(&parse->build, zstr("do_while_body"));
		genJump(build, parse->current_loop_head);

		parseStatement(parse, had_non_declaration);
		expect(parse, Tok_Key_While);
		expect(parse, Tok_OpenParen);
		Value condition = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		expect(parse, Tok_Semicolon);

		genBranch(build, toBoolean(parse, primary, condition));
		build->insertion_block->exit.branch.on_true = parse->current_loop_head;
		build->insertion_block->exit.branch.on_false = parse->current_loop_switch_exit;


		startBlock(build, parse->current_loop_switch_exit);

		parse->current_loop_head = outer_head;
		parse->current_loop_switch_exit = outer_exit;
	} break;
	case Tok_Key_While: {
		parse->pos++;
		*had_non_declaration = true;

		Block *outer_head = parse->current_loop_head;
		Block *outer_exit = parse->current_loop_switch_exit;

		parse->current_loop_head = newBlock(build, zstr("while_head"));
		parse->current_loop_switch_exit = newBlock(&parse->build, zstr("while_join"));
		genJump(build, parse->current_loop_head);

		expect(parse, Tok_OpenParen);
		Value condition = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		Block *head_end = build->insertion_block;
		genBranch(build, toBoolean(parse, primary, condition));
		head_end->exit.branch.on_false = parse->current_loop_switch_exit;
		head_end->exit.branch.on_true = startNewBlock(build, zstr("while_body"));

		parseStatement(parse, had_non_declaration);
		genJump(build, parse->current_loop_head);

		startBlock(build, parse->current_loop_switch_exit);

		parse->current_loop_head = outer_head;
		parse->current_loop_switch_exit = outer_exit;
	} break;
	case Tok_Key_For: {
		*had_non_declaration = true;
		parse->pos++;
		Scope s = pushScope(parse);

		expect(parse, Tok_OpenParen);

		// TODO Limit this to declaration or expression (or only expression before C99).
		parseStatement(parse, had_non_declaration);

		Block *enclosing_head = parse->current_loop_head;
		Block *enclosing_exit = parse->current_loop_switch_exit;
		parse->current_loop_switch_exit = newBlock(build, zstr("for_join"));
		parse->current_loop_head = newBlock(build, zstr("for_head"));

		genJump(build, parse->current_loop_head);
		Block *to_tail = parse->current_loop_head;

		if (!tryEat(parse, Tok_Semicolon)) {
			IrRef cond = toBoolean(parse, primary, parseExpression(parse));
			expect(parse, Tok_Semicolon);

			genBranch(build, cond);
			build->insertion_block->exit.branch.on_false = parse->current_loop_switch_exit;
			build->insertion_block->exit.branch.on_true = newBlock(build, zstr("for_body"));
			startBlock(build, build->insertion_block->exit.branch.on_true);
		}

		if (!tryEat(parse, Tok_CloseParen)) {
			Block *current = build->insertion_block;
			to_tail = startNewBlock(build, zstr("for_tail"));
			discardValue(parse, primary, parseExpression(parse));
			expect(parse, Tok_CloseParen);

			// TODO This is just one example of how the basic block
			// construction interfaces are badly structured.
			// It takes a lot of code to achieve something simple, and
			// in the end, blocks are not even activated in the right
			// order, setting wrong first_instruction values. (Those are
			// currently fixed up by analysis passes, but I don't want
			// to do those for lightspeed codegen).
			genJump(build, parse->current_loop_head);
			build->insertion_block = current;
			parse->current_loop_head = to_tail;
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

		head->exit.branch.on_true = startNewBlock(build, zstr("if_true"));
		parseStatement(parse, had_non_declaration);
		Block *on_true = build->insertion_block;
		genJump(build, NULL);

		Block *join = newBlock(build, zstr("if_join"));

		if (parse->pos->kind == Tok_Key_Else) {
			parse->pos++;
			head->exit.branch.on_false = startNewBlock(build, zstr("if_else"));
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

		v = intPromote(v, parse, primary);
		genSwitch(build, v.inst);
		Block *outer_switch = parse->switch_block;
		Block *outer_exit = parse->current_loop_switch_exit;

		parse->switch_block = build->insertion_block;
		parse->current_loop_switch_exit = newBlock(build, zstr("switch_join"));

		startNewBlock(build, zstr("switch_ignored"));
		parseStatement(parse, had_non_declaration);
		genJump(build, parse->current_loop_switch_exit);

		if (parse->switch_block->exit.switch_.default_case == NULL)
			parse->switch_block->exit.switch_.default_case = parse->current_loop_switch_exit;

		parse->switch_block = outer_switch;
		parse->current_loop_switch_exit = outer_exit;
	} break;
	case Tok_Key_Break:
	case Tok_Key_Continue:
	case Tok_Key_Goto: {
		Block *dest;
		parse->pos++;

		switch (primary->kind) {
		case Tok_Key_Break:
			if (!parse->current_loop_switch_exit)
				parseerror(parse, primary, "break may only appear inside a loop or switch statement");
			dest = parse->current_loop_switch_exit;
			break;
		case Tok_Key_Continue:
			if (!parse->current_loop_head)
				parseerror(parse, primary, "continue may only appear inside a loop");
			dest = parse->current_loop_head;
			break;
		case Tok_Key_Goto:
			dest = getLabeledBlock(parse, expect(parse, Tok_Identifier).val.symbol);
			break;
		default: unreachable;
		}

		expect(parse, Tok_Semicolon);
		genJump(build, dest);

		// Unreferenced dummy block for further instructions, will be dropped.
		startNewBlock(build, zstr("dummy"));
	} break;
	case Tok_Semicolon:
		parse->pos++;
		break;
	default: {
		if (tryEatStaticAssert(parse))
			break;
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

		if (allowedNoDeclarator(parse, base_type))
			break;

		if (storage == Storage_Typedef) {
			parseTypedefDecls(parse, base_type);
			break;
		}


		do {
			const Token *decl_token = parse->pos;
			Declaration decl = parseDeclarator(parse, base_type, Decl_Named);

			const Token *definer = NULL;
			if (parse->pos->kind == Tok_Equal)
				definer = parse->pos++;
			OrdinaryIdentifier *ord = define(parse, decl, storage, primary, decl_token,
					definer, parse->current_func_id);

			if (ord->kind == Sym_Value_Auto) {
				ord->value = (Value) {decl.type, genStackAllocNamed(build, typeSize(decl.type, &parse->target), decl), Ref_LValue};
				if (definer)
					initializeAutoDefinition(parse, ord, decl.type, definer);
			} else {
				StaticValue *val = &parse->module->ptr[ord->static_id];
				if (!val->is_public) {
					if (definer) {
						initializeStaticDefinition(parse, ord, decl.type, definer);
					} else {
						val->type = decl.type;
						initializeStaticToZero(parse, val);
					}
				}
			}

			if (!definer && decl.type.kind == Kind_UnsizedArray)
				parseerror(parse, decl_token, "cannot infer size of array without initializer");
		} while (tryEat(parse, Tok_Comma));

		expect(parse, Tok_Semicolon);
	} break;
	}

	if (is_declaration && *had_non_declaration)
		requires(parse, "declarations after the begnning of the block", Features_C99);
	else
		*had_non_declaration = !is_declaration;
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
	Value v = parseExprConditional(parse);
	const Token *primary = parse->pos;

	if (!(primary->kind == Tok_Equal || primary->kind >= Tok_Assignment_Start))
		return v;

	needAssignableLval(parse, primary, v);

	parse->pos++;
	bool is_volatile = v.typ.qualifiers & Qualifier_Volatile;

	Value assigned = rvalue(parseExprAssignment(parse), parse);

	if (primary->kind != Tok_Equal) {
		Value loaded = {v.typ, genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target), is_volatile)};

		switch (primary->kind) {
		case Tok_PlusEqual:
			assigned = arithAdd(parse, primary, loaded, assigned);
			break;
		case Tok_MinusEqual:
			assigned = arithSub(parse, primary, loaded, assigned);
			break;
		case Tok_AsteriskEqual:
		case Tok_PercentEqual:
		case Tok_SlashEqual: {
			assigned = arithMultiplicativeOp(parse, primary, primary->kind - Tok_EQUALED, loaded, assigned);
		} break;
		case Tok_AmpersandEqual: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genAnd(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_PipeEqual: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genOr(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_HatEqual: {
			Type common = arithmeticConversions(parse, primary, &loaded, &assigned);
			assigned = (Value) {common, genXor(&parse->build, loaded.inst, assigned.inst)};
		} break;
		case Tok_DoubleLessEqual:
		case Tok_DoubleGreaterEqual: {
			loaded = intPromote(loaded, parse, primary);
			assigned = intPromote(assigned, parse, primary);

			if (primary->kind == (Tok_DoubleLessEqual))
				assigned = (Value) {loaded.typ, genShiftLeft(&parse->build, loaded.inst, assigned.inst)};
			else
				assigned = (Value) {loaded.typ, genShiftRight(&parse->build, loaded.inst, assigned.inst)};
		} break;
		default:
			parseerror(parse, primary, "TODO Compound assignments");
		}
	}
	assigned = (Value) {v.typ,
		coercerval(assigned, v.typ, parse, primary, false),
	};
	genStore(&parse->build, v.inst, assigned.inst, is_volatile);
	return assigned;
}

static Value parseExprConditional (Parse *parse) {
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
				startNewBlock(build, zstr("dummy"));
				parseExprConditional(parse);
				build->insertion_block = b;
			} else {
				Block *b = build->insertion_block;
				startNewBlock(build, zstr("dummy"));
				parseExprAssignment(parse);
				expect(parse, Tok_Colon);
				build->insertion_block = b;
				res = parseExprConditional(parse);
			}
			return res;
		}

		// This has to generate instructions out of block
		// order—because the two expressions need to be joined after
		// both types are known. Maybe the reordering pass really is
		// necessary.

		genBranch(build, ir);
		head->exit.branch.on_true = startNewBlock(build, STRING_EMPTY);
		Value lhs = rvalue(parseExprAssignment(parse), parse);
		Block *true_branch = build->insertion_block;

		const Token *colon = parse->pos;
		expect(parse, Tok_Colon);

		head->exit.branch.on_false = startNewBlock(build, STRING_EMPTY);
		Value rhs = rvalue(parseExprConditional(parse), parse);

		Type common;
		if (typeCompatible(lhs.typ, rhs.typ))
			common = lhs.typ;
		else if (comparablePointers(parse, lhs, rhs))
			// For function-null comparison
			common = lhs.typ.kind == Kind_Pointer ? lhs.typ : rhs.typ;
		else // This can only generate an extension instruction, which will be reordered into the right block.
			common = arithmeticConversions(parse, colon, &rhs, &lhs);

		IrRef src_r = genPhiOut(build, coercerval(rhs, common, parse, colon, true));
		Block *join = newBlock(&parse->build, STRING_EMPTY);
		genJump(build, join);

		build->insertion_block = true_branch;
		IrRef src_l = genPhiOut(build, coercerval(lhs, common, parse, colon, true));
		genJump(build, join);

		IrRef dest = genPhiIn(build, typeSize(common, &parse->target));
		setPhiOut(build, src_l, dest, IDX_NONE);
		setPhiOut(build, src_r, dest, IDX_NONE);
		cond = (Value) {common, dest};
	}
	return cond;
}


static Value parseExprOr (Parse *parse) {
	Value lhs = parseExprAnd(parse);
	const Token *primary = parse->pos;

	if (parse->pos->kind == Tok_DoublePipe)
		lhs = rvalue(lhs, parse);
	while (tryEat(parse, Tok_DoublePipe)) {
		const u16 int_size = parse->target.int_size;
		IrBuild *build = &parse->build;
		Block *head = build->insertion_block;


		Inst inst = build->ir.ptr[lhs.inst];
		// TODO Really need constant folding for branches to avoid this kind of stuff
		if (inst.kind == Ir_Constant || inst.kind == Ir_Reloc) {
			IrRef res;
			if (inst.kind == Ir_Reloc || inst.constant != 0) {
				Type t = parseValueType(parse, parseExprAnd);
				(void) t; // TODO Typecheck.
				res = genImmediateInt(build, 1, int_size);
			} else {
				res = toBoolean(parse, primary, parseExprAnd(parse));
			}
			lhs = (Value) {BASIC_INT, res};
		} else {
			IrRef constant = genPhiOut(build, genImmediateInt(build, 1, int_size));
			genBranch(build, toBoolean(parse, primary, lhs));
			head->exit.branch.on_false = startNewBlock(build, STRING_EMPTY);

			Value rhs = parseExprAnd(parse);

			IrRef rhs_val = genPhiOut(build, toBoolean(parse, primary, rhs));
			Block *join = newBlock(&parse->build, STRING_EMPTY);
			genJump(build, join);
			head->exit.branch.on_true = join;
			IrRef res = genPhiIn(build, int_size);

			setPhiOut(build, constant, res, IDX_NONE);
			setPhiOut(build, rhs_val, res, IDX_NONE);
			lhs = (Value) {BASIC_INT, res};
		}
	}
	return lhs;
}

// STYLE Copypasta, should probably be merged with parseExprOr.
static Value parseExprAnd (Parse *parse) {
	Value lhs = parseExprBitOr(parse);
	const Token *primary = parse->pos;
	if (parse->pos->kind == Tok_DoubleAmpersand)
		lhs = rvalue(lhs, parse);
	while (tryEat(parse, Tok_DoubleAmpersand)) {
		const u16 int_size = parse->target.int_size;
		IrBuild *build = &parse->build;
		Block *head = build->insertion_block;


		Inst inst = build->ir.ptr[lhs.inst];
		// TODO Really need constant folding for branches to avoid this kind of stuff
		if (inst.kind == Ir_Constant || inst.kind == Ir_Reloc) {
			IrRef res;
			if (inst.kind == Ir_Reloc || inst.constant != 0) {
				res = toBoolean(parse, primary, parseExprBitOr(parse));
			} else {
				Type t = parseValueType(parse, parseExprBitOr);
				(void) t; // TODO Typecheck.
				res = genImmediateInt(build, 0, int_size);
			}
			lhs = (Value) {BASIC_INT, res};
		} else {
			IrRef constant = genPhiOut(build, genImmediateInt(build, 0, int_size));
			genBranch(build, toBoolean(parse, primary, lhs));
			head->exit.branch.on_true = startNewBlock(build, STRING_EMPTY);

			Value rhs = parseExprBitOr(parse);

			IrRef rhs_val = genPhiOut(build, toBoolean(parse, primary, rhs));
			Block *join = newBlock(&parse->build, STRING_EMPTY);
			genJump(build, join);
			head->exit.branch.on_false = join;
			IrRef res = genPhiIn(build, int_size);

			setPhiOut(build, constant, IDX_NONE, res);
			setPhiOut(build, rhs_val, res, IDX_NONE);
			lhs = (Value) {BASIC_INT, res};
		}
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
	case Tok_EqualEqual:
	case Tok_BangEqual: {
		const Token *primary = parse->pos;
		parse->pos++;
		Value rhs = parseExprComparison(parse);

		if (lhs.inst == rhs.inst && lhs.category == rhs.category && parse->opt->warn_compare && lhs.typ.kind != Kind_Float)
			parsemsg(Log_Warn, parse, primary, "comparison to self is always %s",
					primary->kind == Tok_BangEqual ? "false" : "true");

		lhs = rvalue(lhs, parse);
		rhs = rvalue(rhs, parse);

		if (comparablePointers(parse, lhs, rhs)) {
			if (!typeCompatible(lhs.typ, rhs.typ)) {
				// Fixup size of null pointer constants.
				if (lhs.typ.kind != Kind_Pointer) {
					lhs.inst = genZeroExt(build, lhs.inst, parse->target.ptr_size);
				} else if (rhs.typ.kind != Kind_Pointer) {
					rhs.inst = genZeroExt(build, rhs.inst, parse->target.ptr_size);
				}
			}
		} else {
			arithmeticConversions(parse, primary, &lhs, &rhs);
		}

		IrRef eql = genEquals(build, lhs.inst, rhs.inst, parse->target.int_size, lhs.typ.kind == Kind_Float);
		if (primary->kind == (Tok_BangEqual))
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
	while (true) {
		switch (parse->pos->kind) {
		case Tok_Less:
		case Tok_LessEqual:
		case Tok_Greater:
		case Tok_GreaterEqual: {
			const Token *primary = parse->pos;
			parse->pos++;
			Value rhs = parseExprShift(parse);

			if (lhs.inst == rhs.inst && lhs.category == rhs.category && parse->opt->warn_compare)
				parsemsg(Log_Warn, parse, primary, "comparison to self is always %s",
						primary->kind == Tok_Less || primary->kind == Tok_Greater ? "false" : "true");
			lhs = rvalue(lhs, parse);
			rhs = rvalue(rhs, parse);

			Type common;
			if (lhs.typ.kind == Kind_Pointer && rhs.typ.kind == Kind_Pointer) {
				if (!typeCompatible(lhs.typ, rhs.typ)) {
					parseerror(parse, primary, "cannot compare pointers to incompatible types %s and %s",
						printTypeHighlighted(parse->arena, lhs.typ),
						printTypeHighlighted(parse->arena, rhs.typ));
				}
				lhs = rvalue(lhs, parse);
				rhs = rvalue(rhs, parse);
				common = lhs.typ;
			} else if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
				parseerror(parse, primary, "can not compare types %s and %s",
						printTypeHighlighted(parse->arena, lhs.typ), printTypeHighlighted(parse->arena, rhs.typ));
			} else {
				common = arithmeticConversions(parse, primary, &lhs, &rhs);
			}
			u16 size = parse->target.int_size;
			IrRef res;

			if (common.kind == Kind_Float) {
				switch (primary->kind) {
				case Tok_Less: res = genFLessThan(build, lhs.inst, rhs.inst, size); break;
				case Tok_LessEqual: res = genFLessThanOrEquals(build, lhs.inst, rhs.inst, size); break;
				case Tok_Greater: res = genFLessThan(build, rhs.inst, lhs.inst, size); break;
				case Tok_GreaterEqual: res = genFLessThanOrEquals(build, rhs.inst, lhs.inst, size); break;
				default: unreachable;
				}
			} else {
				Signedness sign = typeSign(common);
				switch (primary->kind) {
				case Tok_Less: res = genLessThan(build, lhs.inst, rhs.inst, size, sign); break;
				case Tok_LessEqual: res = genLessThanOrEquals(build, lhs.inst, rhs.inst, size, sign); break;
				case Tok_Greater: res = genLessThan(build, rhs.inst, lhs.inst, size, sign); break;
				case Tok_GreaterEqual: res = genLessThanOrEquals(build, rhs.inst, lhs.inst, size, sign); break;
				default: unreachable;
				}
			}

			lhs = (Value) { BASIC_INT, res };
		} break;
		default:
			return lhs;
		}
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

		if (primary->kind == Tok_Plus)
			lhs = arithAdd(parse, primary, lhs, rhs);
		else
			lhs = arithSub(parse, primary, lhs, rhs);
	}
}

static Value parseExprMultiplication (Parse *parse) {
	Value lhs = parseExprPrefix(parse);

	while (true) {
		const Token *primary = parse->pos;
		if (primary->kind != Tok_Asterisk && primary->kind != Tok_Slash && primary->kind != Tok_Percent)
			return lhs;
		parse->pos++;
		lhs = rvalue(lhs, parse);
		Value rhs = rvalue(parseExprPrefix(parse), parse);
		lhs = arithMultiplicativeOp(parse, primary, primary->kind, lhs, rhs);
	}
}

static Value parseExprPrefix (Parse *parse) {
	IrBuild *build = &parse->build;

	const Token *primary = parse->pos;
	parse->pos++;
	switch (primary->kind) {
	case Tok_DoublePlus:
	case Tok_DoubleMinus: {
		Value v = parseExprPrefix(parse);
		needAssignableLval(parse, primary, v);

		bool is_volatile = v.typ.qualifiers & Qualifier_Volatile;
		Value rval = (Value) {v.typ, genLoad(build, v.inst, typeSize(v.typ, &parse->target), is_volatile)};

		const i64 delta = primary->kind == Tok_DoublePlus ? 1 : -1;

		Value one = immediateIntVal(parse, BASIC_INT, delta);
		Value result = arithAdd(parse, primary, one, rval);

		genStore(build, v.inst, coercerval(result, v.typ, parse, primary, false), is_volatile);
		return result;
	}
	case Tok_Asterisk: {
		Value v = rvalue(parseExprPrefix(parse), parse);
		if (v.typ.kind == Kind_FunctionPtr) {
			v.typ.kind = Kind_Function;
			v.category = Ref_LValue;
			return v;
		}
		if (v.typ.kind != Kind_Pointer)
			parseerror(parse, primary, "cannot dereference value of type %s; expected a pointer type", printTypeHighlighted(parse->arena, v.typ));
		return dereference(parse, v);
	}
	case Tok_Bang: {
		Value v = rvalue(parseExprPrefix(parse), parse);
		IrRef zero = genImmediateInt(build, 0, typeSize(v.typ, &parse->target));
		return (Value) {BASIC_INT, genEquals(build, v.inst, zero, parse->target.typesizes[Int_int], v.typ.kind == Kind_Float)};
	}
	case Tok_Tilde: {
		Value v = intPromote(parseExprPrefix(parse), parse, primary);
		return (Value) {v.typ, genBitNot(build, v.inst)};
	}
	case Tok_Ampersand: {
		Value v = parseExprPrefix(parse);
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
			typ = parseValueType(parse, openparen ? parseExpression : parseExprPrefix);
		if (openparen)
			expect(parse, Tok_CloseParen);
		if (typ.kind == Kind_Function)
			parseerror(parse, primary, "the operand of a sizeof may not have a function type");

		Type res_type = parse->target.ptrdiff;
		res_type.basic |= Int_unsigned;
		return immediateIntVal(parse, res_type, typeSize(typ, &parse->target));
	}
	case Tok_Plus:
	case Tok_Minus: {
		Value val = parseExprPrefix(parse);
		if (val.typ.kind == Kind_Float) {
			val = rvalue(val, parse);
			if (primary->kind == Tok_Minus) {
				u32 zero = genImmediateInt(build, 0, typeSize(val.typ, &parse->target));
				val.inst = genFSub(build, zero, val.inst);
			}
		} else {
			val = intPromote(val, parse, primary);

			if (primary->kind == Tok_Minus) {
				u32 zero = genImmediateInt(build, 0, typeSize(val.typ, &parse->target));
				val.inst = genSub(build, zero, val.inst, typeSign(val.typ));
			}
		}
		return val;
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
					.address = stack,
				};
				parseInitializer(parse, dest, 0, cast_target);
				return (Value) {cast_target, stack, Ref_LValue};
			} else {
				// Cast operator
				Value v = rvalue(parseExprPrefix(parse), parse);
				return (Value) {cast_target, coercerval(v, cast_target, parse, primary, true)};
			}
		} else {
			parse->pos--;
			return parseExprPostfix(parse);
		}
	} break;
	default:
		parse->pos--;
		return parseExprPostfix(parse);
	}
}

static Value parseExprPostfix (Parse *parse) {
	IrBuild *build = &parse->build;
	Value v = parseExprBase(parse);

	while (true) {
		const Token *const primary = parse->pos;

		if (tryEat(parse, Tok_OpenParen)) {
			Value func = rvalue(v, parse);
			if (func.typ.kind != Kind_FunctionPtr)
				parseerror(parse, primary, "expected a function type, got an expression of type %s", printTypeHighlighted(parse->arena, func.typ));

			LIST(Argument) arguments = {0};
			DeclList params = func.typ.function.parameters;

			bool have_prototype = !func.typ.function.missing_prototype;
			bool is_vararg = func.typ.function.is_vararg;
			if (parse->pos->kind != Tok_CloseParen) {
				do {
					const Token *primary = parse->pos;
					Value val = parseExprAssignment(parse);
					if (have_prototype && !is_vararg && arguments.len == params.len)
						parseerror(parse, primary, "too many arguments to function call");

					if (have_prototype && arguments.len < params.len) {
						Type dest = params.ptr[arguments.len].type;
						val.inst = coerce(val, dest, parse, primary);
						val.typ = dest;
					} else {
						if (val.typ.kind == Kind_Basic) {
							val = intPromote(val, parse, primary);
						} else {
							val = rvalue(val, parse);
							if (val.typ.kind == Kind_Float && val.typ.real == Float_Single) {
								val.inst = genFCast(build, val.inst, floatSize(Float_Double));
								val.typ.real = Float_Double;
							}
						}
					}

					Argument arg = {.arg_inst = val.inst, .type = val.typ};
					PUSH_A(parse->code_arena, arguments, arg);
				} while (tryEat(parse, Tok_Comma));
			}

			expect(parse, Tok_CloseParen);

			if (have_prototype) {
				if (params.len && arguments.len < params.len)
					parseerror(parse, primary, "too few arguments to function call");
				else if (params.len && arguments.len > params.len && !is_vararg)
					parseerror(parse, primary, "too many arguments to function call");
			}

			ArgumentSpan args = {arguments.len, arguments.ptr};
			v = (Value) {
				*func.typ.function.rettype,
				genCall(build, func.inst, *func.typ.function.rettype, args, typeSize(*func.typ.function.rettype, &parse->target), is_vararg)
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
			// STYLE Copypasta from preincrement
			needAssignableLval(parse, primary, v);

			bool is_volatile = v.typ.qualifiers & Qualifier_Volatile;
			Value rval = (Value) {v.typ, genLoad(build, v.inst, typeSize(v.typ, &parse->target), is_volatile)};

			const i64 delta = primary->kind == Tok_DoublePlus ? 1 : -1;

			Value one = immediateIntVal(parse, BASIC_INT, delta);
			Value result = arithAdd(parse, primary, one, rval);

			genStore(build, v.inst, coercerval(result, v.typ, parse, primary, false), is_volatile);
			v = rval;
		} else if (parse->pos[0].kind == Tok_Dot || parse->pos[0].kind == Tok_Arrow) {
			bool arrow = parse->pos[0].kind == Tok_Arrow;
			parse->pos++;

			if (arrow) {
				if (v.typ.kind != Kind_Pointer)
					parseerror(parse, NULL, "the arrow %s->%s operator expects a pointer value. You may want to use a regular dot", BOLD, RESET);
				v = dereference(parse, v);
			}


			Symbol *member_name = expect(parse, Tok_Identifier).val.symbol;
			Type resolved = resolveType(v.typ);

			if (resolved.kind != Kind_Struct && resolved.kind != Kind_Union) {
				if (resolved.kind == Kind_Void && v.typ.kind != Kind_Void) {
					parseerror(parse, NULL, "cannot access a member on a value of incomplete type %s",
							printTypeHighlighted(parse->arena, v.typ));
				} else {
					Log l = Log_Err | (resolved.kind == Kind_Pointer ? Log_Fatal : 0);
					parsemsg(l, parse, NULL, "member access only works on %sstruct%ss and %sunion%ss, not on %s",
							BOLD, RESET, BOLD, RESET, printTypeHighlighted(parse->arena, resolved));
					parsemsg(Log_Info | Log_Fatal, parse, NULL, "use the %s->%s operator to access a member through a pointer", BOLD, RESET);
				}
			}

			u32 i = 0;
			for (; i < resolved.compound.members.len; i++) {
				if (resolved.compound.members.ptr[i].name == member_name)
					break;
			}
			if (i == resolved.compound.members.len)
				parseerror(parse, NULL, "type %s does not have a member named %.*s", printTypeHighlighted(parse->arena, v.typ), member_name->name.len, member_name->name.ptr);
			CompoundMember member = resolved.compound.members.ptr[i];

			v.typ = member.type;
			if (isByref(v)) {
				IrRef offset = genImmediateInt(build, member.offset, typeSize(parse->target.intptr, &parse->target));
				v.inst = genAdd(build, v.inst, offset, Unsigned);

				if (isAnyArray(member.type)) {
					v.typ = (Type) {Kind_Pointer, .pointer = resolved.compound.members.ptr[i].type.array.inner};
					v.category = Ref_RValue;
				}
			} else {
				assert(!isAnyArray(member.type) && "TODO Rvalue arrays");
				v.inst = genAccess(build, v.inst, member.offset, typeSize(v.typ, &parse->target));
				v.category = Ref_RValue;
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
	static Type void_type = {Kind_Void};
	IrBuild *build = &parse->build;
	Token t = *parse->pos;
	parse->pos++;

	switch (t.kind) {
	case Tok_OpenParen: {
		Value v = parseExpression(parse);
		expect(parse, Tok_CloseParen);
		return v;
	}
	case Tok_Char: {
		ConstInt i = charLiteralValue(&parse->tokens, parse->pos - 1);
		return immediateIntVal(parse, (Type) {Kind_Basic, .basic = i.type}, i.val);
	}
	case Tok_Integer: {
		ConstInt i = intLiteralValue(parse->pos - 1);
		return immediateIntVal(parse, (Type) {Kind_Basic, .basic = i.type}, i.val);
	}
	case Tok_IntegerReplaced:
		return immediateIntVal(parse, (Type) {Kind_Basic, .basic = t.literal_type}, t.val.integer);

	case Tok_Key_True:
	case Tok_Key_False:
		return immediateIntVal(parse, (Type) {Kind_Basic, .basic = Int_int}, t.kind == Tok_Key_True);
	case Tok_Key_Nullptr: // Not exactly correct—nullptr has its own type.
		return immediateIntVal(parse, (Type) {Kind_Pointer, .pointer = &void_type}, 0);
	case Tok_Real: {
		double val = floatLiteralValue(parse->pos - 1);
		Type typ = {Kind_Float, .real = t.literal_type};
		return (Value) { typ,  genImmediateReal(&parse->build, val, typeSize(typ, &parse->target)) };
	}

	case Tok_String: {
		parse->pos--;
		u32 id = parseStringLiteral(parse);
		Type strtype = parse->module->ptr[id].type;
		return (Value) { strtype, genGlobal(build, id), Ref_LValue };
	}
	case Tok_Key_Alignof: {
		expect(parse, Tok_OpenParen);
		Type t = parseTypeName(parse, NULL);
		expect(parse, Tok_CloseParen);
		return immediateIntVal(parse, BASIC_INT, typeAlignment(t, &parse->target));
	}
	case Tok_Intrinsic:
		return parseIntrinsic(parse, t.val.symbol->directive);
	case Tok_Identifier: {
		OrdinaryIdentifier *ident = t.val.symbol->ordinary;
		if (ident == NULL)
			parseerror(parse, NULL, "undefined identifier ‘%.*s’", STRING_PRINTAGE(t.val.symbol->name));
		ident->is_used = true;

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
		requires(parse, "generic selections", Features_C11);

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
					if (parse->pos->kind != Tok_CloseParen)
						expect(parse, Tok_Comma);
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

static String concatenatedStrings(Parse *parse) {
	const Token *scan = parse->pos;

	u32 len = 0;
	while (parse->pos->kind == Tok_String) {
		len += parse->pos->val.symbol->name.len;
		parse->pos++;
	}

	char *data = aalloc(parse->code_arena, len + 1);
	char *insert = data;
	while (scan->kind == Tok_String) {
		String str = scan->val.symbol->name;
		memcpy(insert, str.ptr, str.len);
		insert += str.len;
		scan++;
	}

	*insert = 0;
	return (String) {.ptr = data, .len = insert + 1 - data};
}

static u32 parseStringLiteral(Parse *parse) {
	const Token *begin = parse->pos;
	String data = concatenatedStrings(parse);
	Type strtype = {
		.kind = Kind_Array,
		.qualifiers = Qualifier_Const,
		.array = {.inner = &chartype, .count = data.len},
	};

	u32 id = parse->module->len;
	PUSH(*parse->module, ((StaticValue) {
		.type = strtype,
		.decl_location = begin,
		.is_used = true, // Conservative assumption until implementing an appropriate analysis pass.
		.def_kind = Static_Variable,
		.def_state = Def_Defined,
		.value_data = data,
	}));
	return id;
}

static Attributes parseAttributes (Parse *parse) {
	Attributes result = 0;
	while (tryEat(parse, Tok_Key_Attribute)) {
		requires(parse, "attributes", Features_GNU_Extensions);
		expect(parse, Tok_OpenParen);
		eatMatchingParens(parse);
	}
	while (parse->pos[0].kind == Tok_OpenBracket && parse->pos[1].kind == Tok_OpenBracket) {
		requires(parse, "attributes", Features_C23);
		parse->pos += 2;
		parseerror(parse, NULL, "TODO Implement attributes");
	}
	return result;
}


static void initializeAutoDefinition (Parse *parse, OrdinaryIdentifier *ord, Type type, const Token *init_token) {
	assert(ord->kind == Sym_Value_Auto);
	IrBuild *build = &parse->build;
	InitializationDest dest = { .address = ord->value.inst };

	if (type.kind == Kind_UnsizedArray) {
		// TODO Zero-initialize non-covered members.
		u32 *size_constant = &build->ir.ptr[dest.address].alloc.size;

		if (isStringType(type) && (parse->pos->kind == Tok_String || (parse->pos[0].kind == Tok_OpenBrace && parse->pos[1].kind == Tok_String))) {
			bool open = tryEat(parse, Tok_OpenBrace);
			// TODO Big constants
			u32 id = parseStringLiteral(parse);
			if (open) expect(parse, Tok_CloseBrace);
			StaticValue *val = &parse->module->ptr[id];

			u32 count = val->value_data.len;
			*size_constant = count;
			ord->value.typ.kind = Kind_Array;
			ord->value.typ.array.count = count;

			u32 loaded = genLoad(build, genGlobal(build, id), count, val->type.qualifiers & Qualifier_Volatile);
			genStore(build, dest.address, loaded, false);
			return;
		}

		type = *type.array.inner;
		expect(parse, Tok_OpenBrace);

		u32 member_size = typeSize(type, &parse->target);
		u32 count = 0;
		while (true) {
			if (tryEat(parse, Tok_CloseBrace))
				break;

			maybeBracedInitializer(parse, dest, count * member_size, type, no_value);
			count++;

			if (!tryEat(parse, Tok_Comma)) {
				expect(parse, Tok_CloseBrace);
				break;
			}
		}

		build->ir.ptr[dest.address].alloc.size = count * member_size;
		ord->value.typ.kind = Kind_Array;
		ord->value.typ.array.count = count;
		return;
	}

	if (type.kind == Kind_VLArray) {
		requires(parse, "initialization of variable-length arrays", Features_C23);

		expect(parse, Tok_OpenBrace);
		expect(parse, Tok_CloseBrace);

		parseerror(parse, NULL, "TODO Implement VLA initializers");
		return;
	}

	if (typeSize(type, &parse->target) == 0)
		parseerror(parse, init_token, "cannot initialize incomplete type %s", printTypeHighlighted(parse->arena, type));

	// PERFORMANCE This could generate significant load on the memory optimizer.
	genSetZero(build, dest.address, typeSize(type, &parse->target), false);
	parseInitializer(parse, dest, 0, type);
}


static void initializeStaticToZero (Parse *parse, StaticValue *val) {
	// TODO May not be an unknown-size array.
	u32 amount = typeSize(val->type, &parse->target);
	char *data = aalloc(parse->code_arena, amount);
	memset(data, 0, amount);
	val->value_data = (String) {amount, data};
	val->def_state = Def_Defined;
}

static void initializeStaticDefinition (Parse *parse, OrdinaryIdentifier *ord, Type type, const Token *init_token) {
	assert(ord->kind == Sym_Value_Static);
	StaticValue *static_val = &parse->module->ptr[ord->static_id];
	static_val->def_state = Def_Defined;
	RefsList refs = { 0 };
	InitializationDest dest = { .reloc_references = &refs };

	if (type.kind == Kind_UnsizedArray) {
		static_val->type.kind = Kind_Array;
		if (isStringType(type) && (parse->pos->kind == Tok_String || (parse->pos[0].kind == Tok_OpenBrace && parse->pos[1].kind == Tok_String))) {
			bool open = tryEat(parse, Tok_OpenBrace);
			String data = concatenatedStrings(parse);
			if (open) expect(parse, Tok_CloseBrace);

			static_val->type.array.count = data.len;
			static_val->value_data = (String) {data.len, data.ptr};
			return;
		}

		type = *type.array.inner;
		expect(parse, Tok_OpenBrace);

		u32 member_size = typeSize(type, &parse->target);
		u32 count = 0;
		u32 pos = 0;
		while (true) {
			if (tryEat(parse, Tok_CloseBrace))
				break;

			if (tryEat(parse, Tok_OpenBracket)) {
				// TODO REMOVE THIS HACK!
				Value v = parseExpression(parse);
				expect(parse, Tok_CloseBracket);
				expect(parse, Tok_Equal);
				u64 idx;
				if (!tryIntConstant(parse, v, &idx))
					parseerror(parse, NULL, "whatever");
				pos = idx;
			}

			if ((pos + 1) * member_size > dest.reloc_data.len) {
				MutableString new = ALLOCN(parse->code_arena, char,
						dest.reloc_data.len + (pos + 1) * member_size);
				if (dest.reloc_data.len)
					memcpy(new.ptr, dest.reloc_data.ptr, dest.reloc_data.len);
				memset(new.ptr + dest.reloc_data.len, 0, new.len - dest.reloc_data.len);
				dest.reloc_data = new;
			}

			maybeBracedInitializer(parse, dest, pos * member_size, type, no_value);

			pos++;
			if (pos > count) count = pos;

			if (!tryEat(parse, Tok_Comma)) {
				expect(parse, Tok_CloseBrace);
				break;
			}
		}

		static_val = &parse->module->ptr[ord->static_id];

		static_val->type.array.count = count;
		static_val->value_data = (String) {count * member_size, dest.reloc_data.ptr};
		static_val->value_references = (References) {refs.len, refs.ptr};
		return;
	}

	if (type.kind == Kind_VLArray) {
		unreachable;
		return;
	}

	if (typeSize(type, &parse->target) == 0)
		parseerror(parse, init_token, "cannot initialize incomplete type %s", printTypeHighlighted(parse->arena, type));

	dest.reloc_data = (MutableString) ALLOCN(parse->code_arena, char,
			typeSize(type, &parse->target));

	memset(dest.reloc_data.ptr, 0, typeSize(type, &parse->target));
	parseInitializer(parse, dest, 0, type);

	static_val = &parse->module->ptr[ord->static_id];
	static_val->value_data = (String) {dest.reloc_data.len, dest.reloc_data.ptr};
	static_val->value_references = (References) {refs.len, refs.ptr};
}


static Value parseIntrinsic (Parse *parse, Intrinsic i) {
	expect(parse, Tok_OpenParen);
	const Token *first = parse->pos;
	IrBuild *build = &parse->build;

	switch (i) {
	case Intrinsic_VaStart: {
		if (!parse->current_func_type.is_vararg)
			parseerror(parse, first, "va_start may only be called from a variadic function");
		DeclList params = parse->current_func_type.parameters;

		IrRef v = checkVaListValue(parse, parseExprAssignment(parse), first);

		if (!(parse->target.version & Features_C23) || parse->pos->kind == Tok_Comma) {
			expect(parse, Tok_Comma);
			const Token *param = parse->pos;
			Symbol *sym = expect(parse, Tok_Identifier).val.symbol;
			Symbol *rightmost = params.ptr[params.len-1].name;
			if (rightmost != sym) {
				parseerror(parse, param, "the second argument to va_start must be the rightmost named parameter (%.*s)",
						STRING_PRINTAGE(rightmost->name));
			}
		}
		expect(parse, Tok_CloseParen);
		genVaStart(build, v, IDX_NONE /*ignored*/);

		return (Value){ BASIC_VOID };
	}
	case Intrinsic_VaArg: {
		IrRef v = checkVaListValue(parse, parseExprAssignment(parse), first);
		expect(parse, Tok_Comma);
		Type t = parseTypeName(parse, NULL);
		expect(parse, Tok_CloseParen);

		return (Value){ t, genVaArg(build, v, typeSize(t, &parse->target), t) };
	}
	case Intrinsic_VaCopy: {
		IrRef dest = checkVaListValue(parse, parseExprAssignment(parse), first);
		expect(parse, Tok_Comma);
		IrRef src = checkVaListValue(parse, parseExprAssignment(parse), first);
		expect(parse, Tok_CloseParen);

		genStore(&parse->build, dest, genLoad(&parse->build, src, 24, false), false);
		return (Value){{Kind_Void}};
	}
	case Intrinsic_Expect: {
		Value a = parseExprAssignment(parse);
		expect(parse, Tok_Comma);
		parseExprAssignment(parse);
		expect(parse, Tok_CloseParen);
		return a;
	}
	case Intrinsic_Clz:
	case Intrinsic_Clzll:
	case Intrinsic_Ctz:
	case Intrinsic_Ctzll:
	case Intrinsic_VaEnd:
	case Intrinsic_Nanf:
	case Intrinsic_Inff:
	case Intrinsic_FrameAddress:
	case Intrinsic_Alloca:
	{
		eatMatchingParens(parse);
		return immediateIntVal(parse, BASIC_INT, 0);
	}
	default:
		unreachable;
	}
}

static IrRef checkVaListValue(Parse *parse, Value v, const Token *pos) {
	if (!isLvalue(v))
		parseerror(parse, pos, "va_arg expects an lvalue");
	if (v.typ.kind != Kind_Array || v.typ.pointer->kind != Kind_Basic)
		parseerror(parse, pos, "va_arg expects a va_list, not a %s", printTypeHighlighted(parse->arena, v.typ));
	return v.inst;
}


// Initializers: Another giant chunk of logic. STYLE Probably needs more comments.

#define MEMBERS_FULL UINT64_MAX
static u64 parseSubInitializer(Parse *parse, InitializationDest dest, u32 offset, Type type, Value current_value, bool designator_started, u64 member_idx);
static void initializeFromValue(Parse *parse, InitializationDest dest, u32 offset, Type dest_type, Value val);

static bool isStringType (Type typ) {
	if (!isAnyArray(typ)) return false;
	Type inner = *typ.array.inner;
	return inner.kind == Kind_Basic && inner.basic == Int_char;
}

static bool assignmentCompatible (Type src, Type dest) {
	if (src.kind == Kind_Array && dest.kind == Kind_Array) {
		return typeCompatible(*src.array.inner, *dest.array.inner);
	}
	return typeCompatible(src, dest);
}


// maybeBracedInitializer and parseSubInitializer may be called with a
// current_value from the initializer list (and its following comma)
// already parsed in; this is necessary because the recursion condition
// is not grammatical, but based on the type of the current element.

// STYLE This deep magic needs a more descriptive name.
static void maybeBracedInitializer (Parse *parse, InitializationDest dest, u32 offset, Type type, Value current_value) {
	bool is_scalar = !(type.kind == Kind_Struct || type.kind == Kind_Struct_Named
			|| type.kind == Kind_Union || type.kind == Kind_Union_Named
			|| isAnyArray(type));

	if (current_value.inst == IDX_NONE) {
		if (tryEat(parse, Tok_OpenBrace)) {
			u64 member_idx = 0;
			if (tryEat(parse, Tok_CloseBrace)) {
				requires(parse, "empty initializers", Features_C23);
				return;
			}

			if (is_scalar || (parse->pos->kind == Tok_String && isStringType(type))) {
				parsemsg(Log_Warn, parse, parse->pos-1, "unnecessary braces around scalar initializer");
				maybeBracedInitializer(parse, dest, offset, type, no_value);
				expect(parse, Tok_CloseBrace);
				return;
			}

			while (true) {
				const Token *primary = parse->pos;
				bool designated = parse->pos->kind == Tok_Dot || parse->pos->kind == Tok_OpenBracket;
				if (designated)
					requires(parse, "designated initializers", Features_C99);

				if (member_idx == MEMBERS_FULL && !designated)
					parseerror(parse, primary, "too much initialization");
				member_idx = parseSubInitializer(parse, dest, offset, type, no_value, designated, member_idx);
				if (tryEat(parse, Tok_Comma)) {
					if (tryEat(parse, Tok_CloseBrace))
						return;
				} else {
					expect(parse, Tok_CloseBrace);
					return;
				}
			}
			return;
		}
		current_value = parseExprAssignment(parse);
	}
	if (is_scalar || assignmentCompatible(current_value.typ, type)) {
		initializeFromValue(parse, dest, offset, type, current_value);
	} else {
		parseSubInitializer(parse, dest, offset, type, current_value, false, 0);
	}
}

static void parseInitializer (Parse *parse, InitializationDest dest, u32 offset, Type type) {
	// maybeBracedInitializer will descend if the initializing value is
	// not compatible to the desired type, but that only makes sense
	// with at least one enclosing set of braces.
	if (parse->pos->kind == Tok_OpenBrace) {
		maybeBracedInitializer(parse, dest, offset, type, no_value);
	} else {
		initializeFromValue(parse, dest, offset, type, parseExprAssignment(parse));
	}
}


u32 findDesignatedMemberIdx(Parse *parse, Type type);
u64 findDesignatedArrayIdx(Parse *parse, Type type);

static bool subInitializerEnded (Parse *parse) {
	// Yield to the enclosing braces' initialization for new designators
	// and close-braces.
	if (parse->pos[0].kind != Tok_Comma || parse->pos[1].kind == Tok_CloseBrace
		|| parse->pos[1].kind == Tok_Dot || parse->pos[1].kind == Tok_OpenBracket)
	{
		return true;
	}
	parse->pos++;
	return false;
}

// Takes enough elements from the initializer list to initialize the
// sub-type.
// Returns the next member index to be initialized.

// TODO Values must be constants when initializing static/_Thread_local values.
// TODO Clang and GCC allow unnamed members to be braced. By my reading,
// the standard does not actually permit that, but it still needs to be
// supported here.
static u64 parseSubInitializer (Parse *parse, InitializationDest dest, u32 offset, Type type, Value first_value, bool designator_started, u64 member_idx) {
	Type resolved = resolveType(type);
	bool is_array = resolved.kind == Kind_Array;

	if (designator_started) {
		if (tryEat(parse, Tok_Dot)) {
			member_idx = findDesignatedMemberIdx(parse, resolved);
		} else if (tryEat(parse, Tok_OpenBracket)) {
			member_idx = findDesignatedArrayIdx(parse, resolved);
		}
		if (parse->pos->kind == Tok_Dot || parse->pos->kind == Tok_OpenBracket) {
			if (is_array) {
				Type inner = *type.array.inner;
				u32 stride = typeSize(inner, &parse->target);
				parseSubInitializer(parse, dest, offset + member_idx * stride, inner, no_value, true, 0);
			} else {
				CompoundMember member = resolved.compound.members.ptr[member_idx];
				parseSubInitializer(parse, dest, offset + member.offset, member.type, no_value, true, 0);
			}
			member_idx++;
			if (member_idx >= type.array.count)
				return MEMBERS_FULL;
			if (subInitializerEnded(parse))
				return member_idx;
		} else {
			expect(parse, Tok_Equal);
		}
	}


	if (first_value.inst != IDX_NONE) {
		assert(member_idx == 0);
		if (is_array) {
			maybeBracedInitializer(parse, dest, offset, *type.array.inner, first_value);
		} else {
			CompoundMember member = resolved.compound.members.ptr[0];
			maybeBracedInitializer(parse, dest, offset + member.offset, member.type, first_value);
		}
		member_idx++;
		if (member_idx >= type.array.count)
			return MEMBERS_FULL;
		if (subInitializerEnded(parse))
			return member_idx;
	}


	if (is_array) {
		Type inner = *type.array.inner;
		u32 stride = typeSize(inner, &parse->target);
		while (true) {
			maybeBracedInitializer(parse, dest, offset + member_idx * stride, inner, no_value);
			member_idx++;
			if (member_idx >= type.array.count)
				return MEMBERS_FULL;
			if (subInitializerEnded(parse))
				return member_idx;
		}
	} else {
		Members members = resolved.compound.members;
		while (true) {
			CompoundMember m = members.ptr[member_idx];

			maybeBracedInitializer(parse, dest, offset + m.offset, m.type, no_value);
			member_idx++;
			if (member_idx >= members.len)
				return MEMBERS_FULL;
			if (subInitializerEnded(parse))
				return member_idx;
		}
	}
}

u32 findDesignatedMemberIdx (Parse *parse, Type type) {
	const Token *id = parse->pos;
	assert(id[-1].kind == Tok_Dot);
	if (type.kind != Kind_Struct && type.kind != Kind_Union) {
		parseerror(parse, id, "cannot use a member designator for initializing the type %s",
				printTypeHighlighted(parse->arena, type));
	}
	Members members = type.compound.members;
	Symbol *sym = expect(parse, Tok_Identifier).val.symbol;
	for (u32 idx = 0; idx < members.len; idx++) {
		if (members.ptr[idx].name == sym)
			return idx;
	}
	parseerror(parse, id, "%s does not have a member named %.*s",
			printTypeHighlighted(parse->arena, type), sym->name.len, sym->name.ptr);
}

u64 findDesignatedArrayIdx (Parse *parse, Type type) {
	const Token *id = parse->pos;
	assert(id[-1].kind == Tok_OpenBracket);
	if (!isAnyArray(type)) {
		parseerror(parse, id, "cannot use an array designator for initializing the type %s",
				printTypeHighlighted(parse->arena, type));
	}
	u64 pos;
	if (!tryIntConstant(parse, parseExpression(parse), &pos))
		parseerror(parse, id, "bracket designators must be constant integer expressions");
	expect(parse, Tok_CloseBracket);

	if (type.kind == Kind_Array && pos >= type.array.count)
		parseerror(parse, id, "index %lld is out of range of the array", (long long) pos);
	return pos;
}

static void initializeFromValue (Parse *parse, InitializationDest dest, u32 offset, Type type, Value val) {
	IrRef ref;
	IrBuild *build = &parse->build;
	if (val.typ.kind == Kind_Array && type.kind == Kind_Array) {
		// Generate a load that will be elided in the next step again.
		// Ewww.
		// TODO Warn when truncating.
		u32 src_size = typeSize(val.typ, &parse->target);
		u32 dest_size = typeSize(type, &parse->target);
		ref = genLoad(build, val.inst, src_size > dest_size ? dest_size : src_size, val.typ.qualifiers & Qualifier_Volatile);
	} else {
		ref = coerce(val, type, parse, NULL);
	}
	Inst inst = build->ir.ptr[ref];
	if (inst.kind != Ir_Constant && inst.kind != Ir_Reloc)
		requires(parse, "non-constant initializers", Features_C99);

	if (dest.reloc_data.ptr) {
		if (inst.kind == Ir_Constant) {
			memcpy(
				dest.reloc_data.ptr + offset,
				&inst.constant,
				inst.size);
		} else if (inst.kind == Ir_Reloc) {
			Reference ref = {offset, inst.reloc.id, inst.reloc.offset};
			PUSH(*dest.reloc_references, ref);
		} else if (inst.kind == Ir_Load && parse->build.ir.ptr[inst.mem.address].kind == Ir_Reloc) {
			Inst loadfrom = parse->build.ir.ptr[inst.mem.address];
			// Load value from a constant static.
			// Ugly hack, need to represent a loaded Reloc in the IR.
			StaticValue *val = &parse->module->ptr[loadfrom.reloc.id];
			if (val->def_kind != Static_Variable || !(val->type.qualifiers & Qualifier_Const))
				parseerror(parse, NULL, "TODO: ???");

			// TODO This does not yet copy references from the static value.
			memcpy(
				dest.reloc_data.ptr + offset,
				val->value_data.ptr + loadfrom.reloc.offset,
				inst.size
			);
		} else {
			parseerror(parse, NULL, "expected a static initializer (TODO print the non-static culprit)");
		}
	} else {
		IrRef offset_inst = genImmediateInt(build, offset, parse->target.ptr_size);
		IrRef dest_addr = genAdd(build, dest.address, offset_inst, Unsigned);
		genStore(build, dest_addr, ref, false);
	}
}








static Type parseValueType (Parse *parse, Value (*operator)(Parse *parse)) {
	Block *current = parse->build.insertion_block;
	// Emit the expression into a block which is then discarded. Pretty hacky.
	Block *dummy = startNewBlock(&parse->build, STRING_EMPTY);
	Type type = operator(parse).typ;
	discardBlock(dummy);
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
		// TODO I don't think default-int is allowed in all contexts;
		// make this more precise.
		if (parse->pos[0].kind == Tok_Identifier &&
			(parse->pos[1].kind == Tok_OpenParen || parse->pos[1].kind == Tok_CloseParen || parse->pos[1].kind == Tok_Semicolon
			|| parse->pos[1].kind == Tok_Comma || parse->pos[1].kind == Tok_Equal))
		{
			requires(parse, "expected a type name; default types of int", Features_DefaultInt);
			if (storage)
				*storage = Storage_Unspecified;
			return BASIC_INT;
		}
		parseerror(parse, parse->pos, "expected a type name");
	}
	return type;
}

static nodiscard Type parseStructUnionBody(Parse *parse, bool is_struct);

// declaration-specifiers if storage_dest is given,
// specifier-qualifier-list if storage_dest is NULL.
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
			base.kind = Kind_Float;
			base.real = Float_Double;
			bases++;
			break;
		case Tok_Key_Float:
			base.kind = Kind_Float;
			base.real = Float_Single;
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
			base.kind = Kind_Void;
			modifiable = false;
			bases++;
			break;
		case Tok_Intrinsic:
			if (parse->pos->val.symbol->directive == Intrinsic_VaList) {
				// TODO Better type checking.
				// On GCC x86-64, for example, it should work roughly as if there was defined:
				// typedef struct {
				// 	unsigned gp_offset;
				// 	unsigned fp_offset;
				// 	void *overflow_arg_area;
				// 	void *reg_save_area;
				// } __builtin_va_list[1];
				// ...but to enable stuff like compatibility warnings,
				// the information that this is a special type still
				// needs to be preserved—possibly via a qualifier or a
				// value property.
				base.kind = Kind_Array;
				base.array.inner = ALLOC(parse->code_arena, Type);
				*base.array.inner = (Type) { .kind = Kind_Basic, .basic = Int_long };
				base.array.count = 3;
				modifiable = false;
				bases++;
				break;
			} else {
				is_type_token = false;
				continue;
			}
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
				named = parse->pos->val.symbol;
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
						.shadowed = NULL,
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
							prev.compound.members.len == body_type.compound.members.len)
						{
							equal = true;
							for (u32 i = 0; i < prev.compound.members.len; i++) {
								if (!typeCompatible(prev.compound.members.ptr[i].type, body_type.compound.members.ptr[i].type)) {
									equal = false;
									break;
								}
							}
						}
						if (!equal)
							redefinitionError(parse, primary, existing->def_location, named);
					}
				}
				base.kind = is_struct ? Kind_Struct_Named : Kind_Union_Named;
				base.nametagged = named->nametagged;
			} else {
				base = body_type;
			}

			modifiable = false;
			parse->pos--;
		} break;
		case Tok_Key_Enum: {
			const Token *primary = parse->pos;
			const Tokenization *tok = &parse->tokens;
			Location loc = tok->list.positions[parse->pos - tok->list.tokens].source;
			SourceFile *source = tok->files.ptr[loc.file_id];
			parse->pos++;

			Attributes attr = parseAttributes(parse);
			(void) attr;

			base = BASIC_VOID;

			Symbol *named = NULL;
			NameTaggedType *existing = NULL;
			if (parse->pos->kind == Tok_Identifier) {
				named = parse->pos->val.symbol;
				parse->pos++;
				existing = named->nametagged;
			}

			BasicType underlying = parse->target.enum_int;
			if (tryEat(parse, Tok_Colon)) {
				requires(parse, "explicity enum-underlying types", Features_C23);
				const Token *type_token = parse->pos;
				Type under_type = parseTypeBase(parse, NULL);
				if (!isIntegerType(under_type))
					parseerror(parse, type_token, "the underlying type of an enumeration must be an integer type, %s is not permitted",
							printTypeHighlighted(parse->arena, under_type));
				underlying = under_type.basic;
			}

			bool can_declare = parse->pos->kind == Tok_Semicolon
					|| parse->pos->kind  == Tok_OpenBrace;

			if (tryEat(parse, Tok_OpenBrace)) {
				for (i32 value = 0;; value++) {
					const Token *primary = parse->pos;
					Symbol *name = expect(parse, Tok_Identifier).val.symbol;

					if (tryEat(parse, Tok_Equal)) {
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
						redefinitionError(parse, NULL, ident->def_location, name);

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
				base.unnamed_enum = (UnnamedEnum) {
					// TODO Actually record the members.
					.underlying = underlying,
					.line = loc.line,
					.column = loc.column,
					.source = source,
				};
			} else {
				if (!named)
					parseerror(parse, parse->pos-1, "name or %s required after %s", tokenName(Tok_OpenBrace), tokenName(Tok_Key_Enum));
				if (!existing)
					requires(parse, "enum pre-declarations", Features_GNU_Extensions);
			}

			if (named) {
				if (!existing ||
					(can_declare && existing->scope_depth < parse->scope_depth))
				{
					existing = ALLOC(parse->code_arena, NameTaggedType);
					*existing = (NameTaggedType) {
						.name = named->name,
						.shadowed = named->nametagged,
						.scope_depth = parse->scope_depth,
						.type = base,
					};
					named->nametagged = existing;
				} else {
					if (existing->type.kind == Kind_Void) {
						existing->type = base;
					} else if (base.kind != Kind_Void) {
						assert(existing->type.kind == Kind_Enum);
						bool equal = true;
						// TODO Check that the new definition matches the
						// previous one.
						if (!equal)
							redefinitionError(parse, primary, existing->def_location, named);
					}
				}
				base.kind = Kind_Enum_Named;
				base.nametagged = existing;
			}
			assert(base.kind != Kind_Void);

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
			Symbol *sym = parse->pos->val.symbol;

			// Hacky way to correctly accept re-typedefs.
			// TODO Find a correct parser.
			if (sym && sym->ordinary && sym->ordinary->kind == Sym_Typedef
				&& !(storage == Storage_Typedef &&
					(parse->pos[1].kind == Tok_Semicolon || parse->pos[1].kind == Tok_Comma)))
			{
				sym->ordinary->is_used = true;
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
		Attributes attr = parseAttributes(parse);
		(void) attr;
	}
	if (parse->pos == begin)
		return false;

	if (modifiable) {
		if (base.kind == Kind_Float) {
			assert(base.real == Float_Double);
			if (longness[0]) {
				if (longness[1])
					parseerror(parse, longness[1], "unknown type %slong long double%s", BOLD, RESET);
// 				parsemsg(Log_Warn, parse, longness[0], "%slong double%s not supported, defaulting to normal %sdouble%s", BOLD, RESET, BOLD, RESET);
// 				base.real = Float_LongDouble;
			}
			*type = base;
			return true;
		}
		assert(base.kind == Kind_Basic);
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
				// TODO Is this ever reachable?

				Version v = parse->target.version;
				if (v > Version_C89 && !(v & Features_GNU_Extensions))
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

static Type parseStructUnionBody(Parse *parse, bool is_struct) {
	LIST(CompoundMember) members = {0};

	const Tokenization *tok = &parse->tokens;
	Location loc = tok->list.positions[parse->pos - 2 - tok->list.tokens].source;
	SourceFile *source = tok->files.ptr[loc.file_id];

	u32 current_offset = 0;
	while (!tryEat(parse, Tok_CloseBrace)) {
		// May not be function type, struct or union ending
		// with an incomplete array, or incomplete type except
		// that the last item may be an incomplete array.
		// TODO Check this.
		const Token *begin = parse->pos;
		if (tryEatStaticAssert(parse))
			continue;

		u8 storage;
		Type base = parseTypeBase(parse, &storage);
		if (storage != Storage_Unspecified)
			parseerror(parse, begin, "members cannot specify storage");


		if (tryEat(parse, Tok_Semicolon)) {
			Type t = resolveType(base);
			if (t.kind != Kind_Struct && t.kind != Kind_Union)
				parseerror(parse, begin, "missing ???");
			requires(parse, "anonymous members", Features_C11);

			u32 offset = 0;
			if (is_struct)
				offset = addMemberOffset(&current_offset, t, &parse->target);
			for (u32 i = 0; i < t.compound.members.len; i++) {
				// TODO Check that the last member is not a flexible array.
				CompoundMember m = t.compound.members.ptr[i];
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
				if (basetype.basic != Int_bool && (basetype.basic & ~Int_unsigned) != Int_int) {
					requires(parse, "bit-fields of types other than int", Features_GNU_Extensions);
					removeEnumness(parse, &basetype);
					if (!isSignedOrUnsignedIntegerType(basetype))
						parseerror(parse, colon, "a bit-field must have integer type, %s does not work",
								printTypeHighlighted(parse->arena, basetype));
				}

				u64 bitsize;
				if (!tryIntConstant(parse, parseExpression(parse), &bitsize))
					parseerror(parse, colon, "the width of a bit field must be a constant integer expression");
				parsemsg(Log_Warn, parse, colon, "TODO bit-fields");
			}

			// TODO Support VLAs
			u32 member_offset = 0;
			if (is_struct)
				member_offset = addMemberOffset(&current_offset, decl.type, &parse->target);

			if (decl.type.kind == Kind_UnsizedArray) {
				if (!(parse->pos[0].kind == Tok_Semicolon && parse->pos[1].kind == Tok_CloseBrace)) {
					// TODO "such a structure (and any union containing,
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
		.kind = is_struct ? Kind_Struct : Kind_Union,
		.compound = {
			.members = { .ptr = members.ptr, .len = members.len },
			.line = loc.line,
			.column = loc.column,
			.source = source,
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
	case Kind_Enum_Named:
		return tryEat(parse, Tok_Semicolon);
	default:
		return false;
	}
}

static Declaration parseDeclarator (Parse *parse, Type base_type, Namedness named) {
	Declaration decl = {
		.type = base_type,
	};

	if (decl.type.kind == Kind_Function && tryEat(parse, Tok_Asterisk)) {
		decl.type.kind = Kind_FunctionPtr;
		parseAttributes(parse);
		decl.type.qualifiers = parseQualifiers(parse);
	}

	while (parse->pos->kind == Tok_Asterisk) {
		parse->pos++;
		Attributes attr = parseAttributes(parse);
		(void) attr;

		Type *ptr = ALLOC(parse->code_arena, Type);
		*ptr = decl.type;

		decl.type = (Type) {Kind_Pointer,
			.qualifiers = parseQualifiers(parse),
			.pointer = ptr,
		};
	}



	const Token *begin_inner = NULL;
	// For abstract declarators, empty parentheses in a type name are
	// interpreted as “function with no parameter specification”,
	// rather than redundant parentheses around the omitted identifier.
	if (parse->pos[0].kind == Tok_OpenParen && parse->pos[1].kind != Tok_CloseParen) {
		parse->pos++;
		// Mark the inner declarator expression; we'll return to it after parsing the outer stuff.
		begin_inner = parse->pos;
		eatMatchingParens(parse);
	} else {
		if (parse->pos->kind == Tok_Identifier && named != Decl_Abstract) {
			decl.name = parse->pos->val.symbol;
			parse->pos++;
		} else if (named == Decl_Named) {
			parseerror(parse, parse->pos, "expected an identifier");
		}
	}


	const char *func_in_arr = "an array cannot contain a function. You may want to store function pointers instead";
	const char *func_in_func = "a function cannot return a function. You may want to return a function pointer instead";
	const char *array_in_func = "a function cannot return an array. You may want to return a pointer to an array instead";
	if (tryEat(parse, Tok_OpenParen)) {
		if (decl.type.kind == Kind_Function) parseerror(parse, NULL, func_in_func);
		else if (decl.type.kind == Kind_Array) parseerror(parse, NULL, array_in_func);

		Type *inner = ALLOC(parse->code_arena, Type);
		*inner = decl.type;
		decl.type = (Type) {Kind_Function};
		FunctionType *func = &decl.type.function;
		func->rettype = inner;

		if (parse->pos->kind == Tok_CloseParen) {
			if (!(parse->target.version & Features_C23))
				func->missing_prototype = true;
		} else if (parse->pos[0].kind == Tok_Key_Void && parse->pos[1].kind == Tok_CloseParen) {
			parse->pos++;
		} else {
			do {
				if (tryEat(parse, Tok_TripleDot)) {
					func->is_vararg = true;
					break;
				}
				u8 storage;
				Type param_type = parseTypeBase(parse, &storage);
				Declaration param_decl = parseDeclarator(parse, param_type, Decl_PossiblyAbstract);
				if (isAnyArray(param_decl.type)) {
					param_decl.type.kind = Kind_Pointer;
					param_decl.type.pointer = param_decl.type.array.inner;
				} else if (param_decl.type.kind == Kind_Function) {
					param_decl.type.kind = Kind_FunctionPtr;
				}
				PUSH_A(parse->code_arena, func->parameters, param_decl);
			} while (tryEat(parse, Tok_Comma));
		}

		expect(parse, Tok_CloseParen);

		if (tryEat(parse, Tok_OpenParen)) parseerror(parse, NULL, func_in_func);
		else if (tryEat(parse, Tok_OpenBracket)) parseerror(parse, NULL, array_in_func);
	} else if (parse->pos->kind == Tok_OpenBracket) {
		// This will point at the outermost non-array type, where the next array layer is inserted.
		Type *pointed_type_level = &decl.type;

		if (pointed_type_level->kind == Kind_Function)
			parseerror(parse, NULL, func_in_arr);

		while (tryEat(parse, Tok_OpenBracket)) {
			Type *inner = ALLOC(parse->code_arena, Type);
			*inner = *pointed_type_level;
			*pointed_type_level = (Type) {Kind_Array,
				.qualifiers = parseQualifiers(parse),
				.array.inner = inner,
			};
			Type *array = pointed_type_level;
			pointed_type_level = inner;

			bool is_static = tryEat(parse, Tok_Key_Static);
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
				array->kind = Kind_UnsizedArray;
			} else if (parse->pos->kind == Tok_CloseBracket) {
				array->kind = Kind_UnsizedArray;
			} else {
				u64 count;
				Value count_val = parseExprAssignment(parse);
				if (tryIntConstant(parse, count_val, &count)) {
					if (count > 1000000)
						parseerror(parse, NULL, "%llu items is too big, for now", (unsigned long long) count);
					array->array.count = count;
				} else {
					requires(parse, "variable length arrays", Features_C99);
					array->kind = Kind_VLArray;
					array->array.count = count_val.inst;
					parseerror(parse, NULL, "TODO Support VLAs");
				}
			}

			expect(parse, Tok_CloseBracket);
		}

		if (tryEat(parse, Tok_OpenParen)) parseerror(parse, NULL, func_in_arr);
	}

	if (begin_inner) {
		const Token *end = parse->pos;
		parse->pos = begin_inner;
		decl = parseDeclarator(parse, decl.type, named);
		parse->pos = end;
	}

	return decl;
}


static StringList decl_scopes = {0};
static void emitDecl(FILE *dest, Arena *arena, SourceFile *source, Location loc, Declaration decl, DeclKind kind) {
	char *type_name = printType(arena, decl.type);
	char *kind_name = kind == DeclKind_Typedef ? "type" :
			kind == DeclKind_Declaration ? "decl" : "def";

	String name = decl.name->name;
	fprintf(dest, "%.*s%.*s:%lu:%lu:%s:", STRING_PRINTAGE(source->path), STRING_PRINTAGE(source->name),
			(unsigned long) loc.line, (unsigned long) loc.column, kind_name);
	for (u32 i = 0; i < decl_scopes.len; i++)
		fprintf(dest, "%.*s.", STRING_PRINTAGE(decl_scopes.ptr[i]));
	fprintf(dest, "%.*s:%s\n", STRING_PRINTAGE(name), type_name);
}

static inline void markDecl(const Parse *parse, const Token *tok, Declaration decl, DeclKind kind) {
	Options *opt = parse->opt;
	if (!opt->any_decl_emit) return;

	u32 idx = tok - parse->tokens.list.tokens;
	Location loc = parse->tokens.list.positions[idx].source;
	SourceFile *source = parse->tokens.files.ptr[loc.file_id];
	bool from_std = source->kind == Source_StandardHeader;

	if (opt->emit_decls && decl_scopes.len == 0 && !from_std)
		emitDecl(opt->emit_decls, parse->arena, source, loc, decl, kind);
	if (opt->emit_all_decls && !from_std)
		emitDecl(opt->emit_all_decls, parse->arena, source, loc, decl, kind);
	if (opt->emit_std_decls)
		emitDecl(opt->emit_std_decls, parse->arena, source, loc, decl, kind);
}

static inline void markDeclScopeBegin(String name) {
	PUSH(decl_scopes, name);
}
static inline void markDeclScopeEnd() {
	decl_scopes.len--;
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
				redefinitionError(parse, begin, ident->def_location, decl.name);
		} else {
			ident->kind = Sym_Typedef;
			ident->decl_location = ident->def_location = begin;
			ident->typedef_type = decl.type;
		}
		markDecl(parse, begin, decl, DeclKind_Typedef);
	} while (tryEat(parse, Tok_Comma));

	expect(parse, Tok_Semicolon);
}


// ===> Type operations and semantic analysis


/*

The semantics of declarations and linkages in C are the most
aesthetically offensive piece of logic I have ever read.
I'm no beginner at C, and yet it took me three damn tries to get this to
its current state. Hopefully it's finally approaching correctness.

*/



static void defGlobal (
	Parse *parse,
	Declaration decl,
	const Token *decl_token,
	bool tentative,
	bool is_public)
{
	if (decl.name->has_global) {
		StaticValue *existing_val = &parse->module->ptr[decl.name->global_val_id];
		Type existing_type = existing_val->type;

		if (!typeCompatible(decl.type, existing_type)) {
			parsemsg(Log_Err, parse, decl_token, "redeclaration of %.*s with incompatible type %s",
					STRING_PRINTAGE(decl.name->name), printTypeHighlighted(parse->arena, decl.type));
			parsemsg(Log_Info | Log_Fatal, parse, existing_val->decl_location, "previous declaration had type %s", printTypeHighlighted(parse->arena, existing_type));
		}
		if (tentative && existing_val->def_state == Def_Undefined)
			existing_val->def_state = Def_Tentative;
	} else {
		decl.name->has_global = true;
		decl.name->global_val_id = parse->module->len;
		PUSH(*parse->module, ((StaticValue) {
			.name = decl.name->name,
			.type = decl.type,
			.is_public = is_public,
			.decl_location = decl_token,
			.def_kind = decl.type.kind == Kind_Function ? Static_Function : Static_Variable,
			.def_state = tentative ? Def_Tentative : Def_Undefined,
			.parent_decl = IDX_NONE,
		}));
	}
}

typedef enum {
	Link_None,
	Link_External,
	Link_Internal,
} Linkage;

static Linkage linkage (Parse *parse, OrdinaryIdentifier *id) {
	if (id->kind != Sym_Value_Static)
		return Link_None;
	StaticValue *val = &parse->module->ptr[id->static_id];
	if (val->parent_decl != IDX_NONE)
		return Link_None;
	return val->is_public ? Link_External : Link_Internal;
}

static OrdinaryIdentifier *define (
	Parse *parse,
	Declaration decl,
	u8 storage,
	const Token *type_token,
	const Token *decl_token,
	const Token *defining_token,
	u32 parent_decl)
{
	bool file_scope = parent_decl == IDX_NONE;
	bool msvc = parse->target.version & Features_MSVC_Extensions;

	OrdinaryIdentifier *existing = decl.name->ordinary;
	u32 scope_depth = parse->scope_depth;
	assert(file_scope == (scope_depth == 0));
	Linkage existing_linkage;
	if (existing) existing_linkage = linkage(parse, existing);

	if (msvc && decl.type.kind == Kind_Function)
		scope_depth = 0;


	// If the declaration of an identifier for a function has no
	// storage-class specifier, its linkage is determined exactly as if
	// it were declared with the storage-class specifier extern.
	if (decl.type.kind == Kind_Function) {
		if (storage == Storage_Unspecified)
			storage = Storage_Extern;
		else if (!file_scope && storage != Storage_Extern)
			parseerror(parse, type_token, "a function at block scope may only be specified extern");
	} else {
		if (storage != Storage_Extern && typeSize(decl.type, &parse->target) == 0 && decl.type.kind != Kind_UnsizedArray) {
			parseerror(parse, decl_token, "cannot define a variable with incomplete type %s",
					printTypeHighlighted(parse->arena, decl.type));
		}
	}

	// TODO If linked with existing definition, modify the type to composite type.

	OrdinaryIdentifier *new = ALLOC(parse->arena, OrdinaryIdentifier);
	*new = (OrdinaryIdentifier) {
		.shadowed = existing,
		.scope_depth = scope_depth,
		.decl_location = decl_token,
		.def_location = defining_token,
		.kind = Sym_Value_Static,
	};


	switch (storage) {
	case Storage_Auto:
	case Storage_Register:
		assert(!file_scope);
		new->kind = Sym_Value_Auto;
		break;
	case Storage_Unspecified:
		if (file_scope)
			goto static_storage;
		new->kind = Sym_Value_Auto;
		break;
	case Storage_Static:
	case Storage_Constexpr:
	static_storage: {
		if (file_scope) {
			bool tentative = decl.type.kind != Kind_Function && !defining_token;
			defGlobal(parse, decl, decl_token, tentative, false);
			new->static_id = decl.name->global_val_id;
		} else {
			new->static_id = parse->module->len;
			PUSH(*parse->module, ((StaticValue) {
				.name = decl.name->name,
				.type = decl.type,
				.is_public = false,
				.decl_location = decl_token,
				.def_kind = decl.type.kind == Kind_Function ? Static_Function : Static_Variable,
				.def_state = Def_Undefined,
				.parent_decl = parent_decl,
			}));
		}
	} break;
	case Storage_Extern:
		if (defining_token && !file_scope)
			parseerror(parse, decl_token, "extern declaration may only be initialized at file scope");
		defGlobal(parse, decl, decl_token, false, true);
		new->static_id = decl.name->global_val_id;
		break;
	case Storage_Extern_Threadlocal:
	case Storage_Static_Threadlocal:
		parseerror(parse, NULL, "TODO Thread locals");
	default:
		unreachable;
	}


	// If an identifier has no linkage, there shall be no more than one
	// declaration of the identifier (in a declarator or type specifier)
	// with the same scope and in the same name space.
	if (existing && existing->scope_depth == scope_depth) {
		if (existing_linkage == Link_None || linkage(parse, new) == Link_None)
			redefinitionError(parse, decl_token, existing->decl_location, decl.name);
		assert(existing->kind == Sym_Value_Static);
		if (parse->module->ptr[existing->static_id].def_state == Def_Defined && defining_token)
			redefinitionError(parse, defining_token, existing->def_location, decl.name);
	}

	// This is not quite right: the first declaration of a static variable should arguably also be a definition.
	markDecl(parse, decl_token, decl, defining_token ? DeclKind_Definition : DeclKind_Declaration);

	decl.name->ordinary = new;
	PUSH(parse->current_scope, decl.name);

	return decl.name->ordinary;
}


static bool tryIntConstant (Parse *parse, Value v, u64 *result) {
	if (isByref(v)) return false;

	Inst inst = parse->build.ir.ptr[v.inst];
	if (inst.kind != Ir_Constant) return false;

	*result = inst.constant;
	return true;
}

static bool tryEatStaticAssert (Parse *parse) {
	const Token *keyword = parse->pos;
	if (!tryEat(parse, Tok_Key_StaticAssert))
		return false;
	expect(parse, Tok_OpenParen);
	Value condition = parseExprAssignment(parse);
	String message = {0};
	if (!(parse->target.version & Features_C23) || parse->pos->kind == Tok_Comma) {
		expect(parse, Tok_Comma);
		message = expect(parse, Tok_String).val.symbol->name;
	}
	expect(parse, Tok_CloseParen);
	expect(parse, Tok_Semicolon);

	u64 res;
	if (!tryIntConstant(parse, condition, &res))
		parseerror(parse, keyword, "static assert requires a constant expression");
	if (res == 0) {
		if (message.len)
			parseerror(parse, keyword, "static assertion failed: %.*s", STRING_PRINTAGE(message));
		else
			parseerror(parse, keyword, "static assertion failed");
	}

	return true;
}


static Value arithAdd (Parse *parse, const Token *primary, Value lhs, Value rhs) {
	assert(!isByref(lhs));
	assert(!isByref(rhs));
	if (lhs.typ.kind == Kind_Pointer || rhs.typ.kind == Kind_Pointer) {
		return pointerAdd(&parse->build, lhs, rhs, parse, primary);
	} else {
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
		IrRef inst;
		if (common.kind == Kind_Float) {
			inst = genFAdd(&parse->build, lhs.inst, rhs.inst);
		} else {
			inst = genAdd(&parse->build, lhs.inst, rhs.inst, typeSign(common));
		}
		return (Value) {common, inst};
	}
}

static Value arithSub (Parse *parse, const Token *primary, Value lhs, Value rhs) {
	assert(!isByref(lhs));
	assert(!isByref(rhs));
	IrBuild *build = &parse->build;
	if (lhs.typ.kind == Kind_Pointer) {
		u32 stride_const = typeSize(*lhs.typ.pointer, &parse->target);

		if (stride_const == 0) {
			parseerror(parse, primary, "cannot subtract from a pointer to incomplete type %s",
				printTypeHighlighted(parse->arena, *lhs.typ.pointer));
		}
		IrRef stride = genImmediateInt(build, stride_const, parse->target.ptr_size);

		if (rhs.typ.kind == Kind_Pointer) {
			if (!typeCompatible(*lhs.typ.pointer, *rhs.typ.pointer))
				parseerror(parse, primary, "cannot subtract pointers to different types %s and %s",
						printTypeHighlighted(parse->arena, *lhs.typ.pointer),
						printTypeHighlighted(parse->arena, *rhs.typ.pointer));
			IrRef diff = genSub(build, lhs.inst, rhs.inst, Unsigned);
			return (Value) {parse->target.ptrdiff, genDiv(build, diff, stride, Unsigned)};
		} else {
			IrRef idx = genMul(build, coercerval(rhs, parse->target.ptrdiff, parse, primary, false), stride, Signed);
			// TODO Handle overflow.
			return (Value) {lhs.typ, genSub(build, lhs.inst, idx, Unsigned)};
		}
	} else {
		if (rhs.typ.kind == Kind_Pointer)
			parseerror(parse, primary, "cannot subtract pointer from %s", printType(parse->arena, rhs.typ));
		Type common = arithmeticConversions(parse, primary, &lhs, &rhs);

		IrRef inst;
		if (common.kind == Kind_Float) {
			inst = genFSub(&parse->build, lhs.inst, rhs.inst);
		} else {
			inst = genSub(&parse->build, lhs.inst, rhs.inst, typeSign(common));
		}
		return (Value) {common, inst};
	}
}

static Value arithMultiplicativeOp (Parse *parse, const Token *primary, TokenKind op, Value lhs, Value rhs) {
	Type common = arithmeticConversions(parse, primary, &lhs, &rhs);
	IrRef inst;
	assert(op == Tok_Asterisk || op == Tok_Percent || op == Tok_Slash);
	if (common.kind == Kind_Float) {
		if (op == Tok_Asterisk)
			inst = genFMul(&parse->build, lhs.inst, rhs.inst);
		else if (op == Tok_Percent)
			inst = genFMod(&parse->build, lhs.inst, rhs.inst);
		else
			inst = genFDiv(&parse->build, lhs.inst, rhs.inst);
	} else {
		Signedness sign = typeSign(common);
		if (op == Tok_Asterisk)
			inst = genMul(&parse->build, lhs.inst, rhs.inst, sign);
		else if (op == Tok_Percent)
			inst = genMod(&parse->build, lhs.inst, rhs.inst, sign);
		else
			inst = genDiv(&parse->build, lhs.inst, rhs.inst, sign);
	}
	return (Value) {common, inst};
}


static Type arithmeticConversions (Parse *parse, const Token *primary, Value *a, Value *b) {
	if (a->typ.kind == Kind_Float) {
		if (b->typ.kind == Kind_Float) {
			if (b->typ.real > a->typ.real) {
				*a = (Value) {b->typ, .inst = genFCast(&parse->build, a->inst, floatSize(b->typ.real))};
				return b->typ;
			} else {
				*b = (Value) {a->typ, .inst = genFCast(&parse->build, b->inst, floatSize(a->typ.real))};
				return a->typ;
			}
		} else {
			needScalar(parse, primary, b->typ);
			removeEnumness(parse, &b->typ);
			*b = (Value) {a->typ, .inst = genIntToFloat(&parse->build, b->inst, floatSize(a->typ.real), typeSign(b->typ))};
			return a->typ;
		}
	}
	if (b->typ.kind == Kind_Float) {
		needScalar(parse, primary, a->typ);
			removeEnumness(parse, &a->typ);
		*a = (Value) {b->typ, .inst = genIntToFloat(&parse->build, a->inst, floatSize(b->typ.real), typeSign(a->typ))};
		return b->typ;
	}

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
						printTypeHighlighted(parse->arena, common));
				parsemsg(Log_Info | Log_Noexpand, parse, primary, "explicitly cast operands to an unsigned type to suppress this warning");
			}
		}
	}
	*a = (Value) {common, coercerval(*a, common, parse, primary, false)};

	return common;
}

static bool isZeroConstant (Parse *parse, Value v) {
	u64 val;
	return isIntegerType(v.typ) && tryIntConstant(parse, v, &val) && val == 0;
}

static Value pointerAdd (IrBuild *ir, Value lhs, Value rhs, Parse *op_parse, const Token *token) {
	assert(!isByref(lhs));
	assert(!isByref(rhs));
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
	u32 stride_const = typeSize(*ptr.typ.pointer, &op_parse->target);
	if (stride_const == 0) {
		parseerror(op_parse, token, "cannot add to a pointer to incomplete type %s",
			printTypeHighlighted(op_parse->arena, *ptr.typ.pointer));
	}

	IrRef stride = genImmediateInt(ir, stride_const, op_parse->target.ptr_size);
	IrRef diff = genMul(ir, stride, integer, Unsigned);
	return (Value) {ptr.typ, genAdd(ir, ptr.inst, diff, Unsigned)};
}

static bool comparablePointers (Parse *parse, Value lhs, Value rhs) {
	bool a_zero = isZeroConstant(parse, lhs);
	bool b_zero = isZeroConstant(parse, rhs);
	Type a = lhs.typ;
	Type b = rhs.typ;

	if (a_zero && b_zero)
		return lhs.typ.kind == Kind_Basic && rhs.typ.kind == Kind_Basic && lhs.typ.basic == rhs.typ.basic;

	if (!((a.kind == Kind_Pointer || a.kind == Kind_FunctionPtr || a_zero)
		&& (b.kind == Kind_Pointer || b.kind == Kind_FunctionPtr || b_zero)))
			return false;

	if (a.kind == Kind_Pointer) {
		if (a.pointer->kind == Kind_Void || b_zero)
			return true;
		a.pointer->qualifiers = 0;
	}
	if (b.kind == Kind_Pointer) {
		if (b.pointer->kind == Kind_Void || a_zero)
			return true;
		b.pointer->qualifiers = 0;
	}

	if (!typeCompatible(*a.pointer, *b.pointer))
		parseerror(parse, NULL, "incompatible pointer types");

	return true;
}

static Value dereference (Parse *parse, Value v) {
	assert(v.typ.kind == Kind_Pointer);

	if (isByref(v))
		v.inst = genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target), v.typ.qualifiers & Qualifier_Volatile);
	v.typ = *v.typ.pointer;
	v.category = Ref_LValue;
	return v;
}


static Value intPromote (Value val, Parse *p, const Token *primary) {
	val = rvalue(val, p);
	removeEnumness(p, &val.typ);

	if (!isIntegerType(val.typ)) {
		parseerror(p, primary, "requires an integer type, but %s was given",
				printTypeHighlighted(p->arena, val.typ));
	}

	int delta = rankDiff(val.typ.basic, Int_int);
	if (delta >= 0)
		return val;
	const Type unsignedint = {Kind_Basic, .basic = Int_int | Int_unsigned};

	if ((val.typ.basic & Int_unsigned) && (p->target.typesizes[val.typ.basic & ~Int_unsigned] == p->target.int_size))
		return (Value) {unsignedint, coerce(val, unsignedint, p, primary)};
	else
		return (Value) {BASIC_INT, coerce(val, BASIC_INT, p, primary)};
}

static void needAssignableLval (Parse *parse, const Token *token, Value v) {
	if (v.typ.kind == Kind_Function)
		parseerror(parse, token, "cannot modify a function");
	if (!isLvalue(v))
		parseerror(parse, token, "cannot modify an rvalue");
	if (isAnyArray(v.typ))
		parseerror(parse, token, "cannot modify an array. You may want to use memcpy() instead");
	if (v.typ.qualifiers & Qualifier_Const)
		parseerror(parse, token, "cannot modify a const-qualified value");
}

static void needScalar (Parse *p, const Token *token, Type t) {
	if (!isScalar(t))
		parseerror(p, token, "expected a scalar type, got %s", printTypeHighlighted(p->arena, t));
}

static void discardValue (Parse *parse, const Token *token, Value v) {
	(void) parse;
	(void) token;
	(void) v;
}

// Performs array to pointer, function to pointer and
// lvalue conversions as necessary. Those are strictly speaking separate
// concerns, but all of them need to be applied almost everywhere, with
// a couple of exceptions each that need to be treated specially anyway.
// This operation is idempotent, but I still want to avoid just
// sprinkling it everywhere.
Value rvalue (Value v, Parse *parse) {
	if (v.typ.kind == Kind_Function) {
		assert(isLvalue(v));
		v.typ.kind = Kind_FunctionPtr;
	} else if (isAnyArray(v.typ)) {
		assert(isByref(v));
		v.typ.kind = Kind_Pointer;
		v.typ.pointer = v.typ.array.inner;
	} else if (isByref(v)) {
		v.inst = genLoad(&parse->build, v.inst, typeSize(v.typ, &parse->target), v.typ.qualifiers & Qualifier_Volatile);
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
	v = rvalue(v, p);
	removeEnumness(p, &v.typ);

	if (v.typ.kind != Kind_Basic && v.typ.kind != Kind_Float && v.typ.kind != Kind_Pointer && v.typ.kind != Kind_FunctionPtr)
		parseerror(p, primary, "(TODO Explain this better) expected an expression of scalar type");

	u32 size = typeSize(v.typ, &p->target);
	IrRef zero = genImmediateInt(build, 0, size);

	return genNot(build, genEquals(build, rvalue(v, p).inst, zero, p->target.int_size, v.typ.kind == Kind_Float));
}

// Performs implicit conversions on rvalues.
static IrRef coercerval (Value v, Type t, Parse *p, const Token *primary, bool allow_casts) {
	assert(!isByref(v));
	IrBuild *build = &p->build;

	if (t.kind == Kind_Void)
		return IDX_NONE;
	removeEnumness(p, &v.typ);
	removeEnumness(p, &t);

	if (typeCompatible(t, v.typ))
		return v.inst;
	if (t.kind == Kind_Basic && t.basic == Int_bool)
		return toBoolean(p, primary, v);

	// Integer conversions
	if (t.kind == Kind_Basic) {
		u16 target = p->target.typesizes[t.basic & ~Int_unsigned];
		if (v.typ.kind == Kind_Basic) {
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
				if (v.typ.basic & Int_unsigned)
					return genZeroExt(build, v.inst, target);
				else
					return genSignExt(build, v.inst, target);
			}
		} else if (v.typ.kind == Kind_Float) {
			return genFloatToInt(build, v.inst, target, typeSign(t));
		}
	} else if (t.kind == Kind_Float) {
		u16 target = floatSize(t.real);
		if (v.typ.kind == Kind_Basic) {
			return genIntToFloat(build, v.inst, target, typeSign(v.typ));
		} else if (v.typ.kind == Kind_Float) {
			return genFCast(build, v.inst, target);
		}
	}


	bool value_is_pointer = v.typ.kind == Kind_Pointer || v.typ.kind == Kind_FunctionPtr;
	// NOTE This relies on pointers being the largest types.
	if (allow_casts && t.kind == Kind_Basic && value_is_pointer)
	{
		return genTrunc(build, v.inst, p->target.typesizes[t.basic & ~Int_unsigned]);
	}

	if ((t.kind == Kind_Pointer || t.kind == Kind_FunctionPtr)
		&& (allow_casts || isZeroConstant(p, v) || t.pointer->kind == Kind_Void
			|| (value_is_pointer && v.typ.pointer->kind == Kind_Void)))
	{
		if (v.typ.kind == Kind_Basic)
			return genZeroExt(build, v.inst, p->target.ptr_size);
		if (value_is_pointer)
			return v.inst;
	}

	parseerror(p, primary, "could not convert type %s to type %s",
		printTypeHighlighted(p->arena, v.typ), printTypeHighlighted(p->arena, t));
}

static Value immediateIntVal (Parse *p, Type typ, u64 val) {
	return (Value) { typ,
		genImmediateInt(&p->build, val, typeSize(typ, &p->target))
	};
}

static inline void removeEnumness (Parse *parse, Type *type) {
	if (type->kind == Kind_Enum_Named) {
		if (type->nametagged->type.kind == Kind_Void)
			parseerror(parse, parse->pos, "cannot use incomplete type %s",
					printTypeHighlighted(parse->arena, *type));
		*type = type->nametagged->type;
		assert(type->kind == Kind_Enum);
	}
	if (type->kind == Kind_Enum) {
		type->kind = Kind_Basic;
		type->basic = type->unnamed_enum.underlying;
	}
}

static void requires (Parse *parse, const char *desc, Features required) {
	if (!(parse->target.version & required)) {
		parseerror(parse, parse->pos, "%s are not supported under the current target (%s)",
				desc, versionName(parse->target.version));
	}
}
