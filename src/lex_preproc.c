#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "lex_preproc.h"
#include "arena.h"
#include "ansii.h"

bool isSpace(char);
bool isAlpha(char c);
bool isDigit(char c);
bool isAlnum(char c);

typedef enum {
	Space_None,
	Space_Regular,
	Space_Linebreak,
} SpaceClass;


typedef enum {
	If_Else,
	If_ElseIf,
	If_End,
} IfClass;

typedef struct {
	char *name;
	TokenKind key;
} Keyword;

// TODO Separate out keywords by version.
// TODO Sort and binary-search.
#define KEYWORDS Tok_Key_Last - Tok_Key_First + 1
Keyword names[] = {
	{"if", Tok_Key_If},
	{"else", Tok_Key_Else},
	{"goto", Tok_Key_Goto},
	{"while", Tok_Key_While},
	{"do", Tok_Key_Do},
	{"for", Tok_Key_For},
	{"return", Tok_Key_Return},
	{"break", Tok_Key_Break},
	{"default", Tok_Key_Default},
	{"sizeof", Tok_Key_Sizeof},
	{"_Alignof", Tok_Key_Alignof},
	{"_Alignas", Tok_Key_Alignas},
	{"typeof", Tok_Key_Typeof},
	{"typeof_unqual", Tok_Key_TypeofUnqual},
	{"_Generic", Tok_Key_Generic},

	{"struct", Tok_Key_Struct},
	{"enum", Tok_Key_Enum},
	{"union", Tok_Key_Union},
	{"_Bool", Tok_Key_Bool},
	{"char", Tok_Key_Char},
	{"int", Tok_Key_Int},
	{"void", Tok_Key_Void},
	{"float", Tok_Key_Float},
	{"double", Tok_Key_Double},

	{"long", Tok_Key_Long},
	{"short", Tok_Key_Short},
	{"signed", Tok_Key_Signed},
	{"unsigned", Tok_Key_Unsigned},
	{"const", Tok_Key_Const},
	{"volatile", Tok_Key_Volatile},
	{"restrict", Tok_Key_Restrict},
	{"_Atomic", Tok_Key_Atomic},
	{"typedef", Tok_Key_Typedef},
	{"auto", Tok_Key_Auto},
	{"register", Tok_Key_Register},
	{"static", Tok_Key_Static},
	{"extern", Tok_Key_Extern},
	{"_Thread_local", Tok_Key_Threadlocal},

	{"__builtin_va_list", Tok_Key_VaList},
	{"__restrict", Tok_Key_Restrict},
};

int keyword_cmp(const void *a, const void *b) {
	return strcmp(((Keyword*)a)->name, ((Keyword*)b)->name);
}

Token fromWord (String word) {
	for (int i = 0; i < KEYWORDS; ++i) {
		if (names[i].name && eql(names[i].name, word)) {
			Token ret = {0};
			ret.kind = names[i].key;
			return ret;
		}
	}
	return (Token) {Tok_Identifier, {.identifier = word}};
}


_Noreturn void lexerror (SourceFile source, const char *pos, const char *msg, ...) {
    printErr(source, pos - source.content.ptr);

    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, ".\n");

// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}

void lexwarning (SourceFile source, const char *pos, const char *msg, ...) {
    printWarn(source, pos - source.content.ptr);

    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, ".\n");
// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
}

String processStringLiteral(Arena *arena, String src);

// TODO Trigraphs (trivial) and digraphs (annoying)
Token getToken (Arena *str_arena, const char **p) {
	Token tok = {0};
	const char *pos = *p;

	switch (pos[0]) {
	case '\0': tok.kind = Tok_EOF; pos--; break;
	case '(': tok.kind = Tok_OpenParen; break;
	case ')': tok.kind = Tok_CloseParen; break;
	case '{': tok.kind = Tok_OpenBrace; break;
	case '}': tok.kind = Tok_CloseBrace; break;
	case '[': tok.kind = Tok_OpenBracket; break;
	case ']': tok.kind = Tok_CloseBracket; break;
	case ';': tok.kind = Tok_Semicolon; break;
	case ',': tok.kind = Tok_Comma; break;
	case ':': tok.kind = Tok_Colon; break;
	case '^': tok.kind = Tok_Hat; break;
	case '~': tok.kind = Tok_Tilde; break;
	case '.':
		if (pos[1] == '.' && pos[2] == '.') {
			pos += 2;
			tok.kind = Tok_TripleDot;
		} else {
			tok.kind = Tok_Dot;
		}
		break;
	case '<':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_LessEquals;
		} else if (pos[1] == '<') {
			pos++;
			tok.kind = Tok_DoubleLess;
		} else {
			tok.kind = Tok_Less;
		}
		break;
	case '>':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_GreaterEquals;
		} else if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_DoubleGreater;
		} else {
			tok.kind = Tok_Greater;
		}
		break;
	case '+':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_PlusEquals;
		} else if (pos[1] == '+') {
			pos++;
			tok.kind = Tok_DoublePlus;
		} else {
			tok.kind = Tok_Plus;
		}
		break;
	case '-':
		if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_Arrow;
		} else if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_MinusEquals;
		} else if (pos[1] == '-') {
			pos++;
			tok.kind = Tok_DoubleMinus;
		} else {
			tok.kind = Tok_Minus;
		}
		break;
	case '*':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_AsteriskEquals;
		} else {
			tok.kind = Tok_Asterisk;
		}
		break;
	case '/':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_SlashEquals;
		} else {
			tok.kind = Tok_Slash;
		}
		break;
	case '!':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_BangEquals;
		} else {
			tok.kind = Tok_Bang;
		}
		break;
	case '=':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_DoubleEquals;
		} else {
			tok.kind = Tok_Equals;
		}
		break;
	case '&':
		if (pos[1] == '&') {
			pos++;
			tok.kind = Tok_DoubleAmpersand;
		} else {
			tok.kind = Tok_Ampersand;
		}
		break;
	case '|':
	start_bar:
		if (pos[1] == '|' || (pos[1] == '?' && pos[2] == '?' && pos[3] == '!')) {
			pos++;
			tok.kind = Tok_DoublePipe;
		} else {
			tok.kind = Tok_Pipe;
		}
		break;
	case '#':
	start_hash:
		if (pos[1] == '#' || (pos[1] == '?' && pos[2] == '?' && pos[3] == '=')) {
			pos++;
			tok.kind = Tok_PreprocConcatenate;
		} else {
			pos++;
			while (pos[0] == ' ' || pos[0] == '\t') pos++;
			const char *start = pos;
			while (!isSpace(pos[0])) pos++;
			tok = (Token) { Tok_PreprocDirective, { .identifier = {pos - start, start} } };
			pos--;
		}
		break;
	case '?':
		if (pos[1] == '?') {
			// Trigraphs.
			// TODO Disable in C23.
			pos += 2;
			switch (*pos) {
			case '=': goto start_hash;
			case '!': goto start_bar;
			case '(':
				tok.kind = Tok_OpenBracket;
				break;
			case ')':
				tok.kind = Tok_CloseBracket;
				break;
			case '<':
				tok.kind = Tok_OpenBrace;
				break;
			case '>':
				tok.kind = Tok_CloseBrace;
				break;
			case '\'':
				tok.kind = Tok_Hat; // TODO goto hat_start;
				break;
			case '~':
				tok.kind = Tok_Tilde; // TODO goto tilde_start;
				break;
			default:
				pos -= 2;
				tok.kind = Tok_Question;
			}
		} else {
			tok.kind = Tok_Question;
		}
		break;
	case '\"': {
		pos++;
		const char *begin = pos;
		while (*pos && *pos != '\"') {
			if (*pos == '\\') pos++;
			pos++;
		}
		// TODO Error reporting
		String val = processStringLiteral(str_arena, (String) {pos - begin, begin});
		tok = (Token) { Tok_String, { .string = val } };
	} break;
	default:
		if (isAlpha(pos[0])) {
			const char *start = pos;
			while (isAlnum(*pos))
				pos++;

			tok = fromWord((String) {pos - start, start});
			pos--;
		} else if (isDigit(pos[0]) || pos[0] == '.') {
			const char *start = pos;

			while (isDigit(*pos))
				pos++;

			if (*pos == '.') {
				pos++;
				while (isDigit(*pos))
					pos++;
				if (*pos == 'e' || *pos == 'E')
					pos++;
				if (*pos == '-' || *pos == '+')
					pos++;
				while (isDigit(*pos))
					pos++;

				tok = (Token) {Tok_Real, {.real = strtod(start, NULL)}};
			} else {
				tok = (Token) {Tok_Integer, {.integer = strtoll(start, NULL, 10)}};
				if (pos[0] == 'l' || pos[1] == 'L') {
					if (pos[1] == 'l' || pos[1] == 'L') {
						pos += 2;
						tok.val.int_type = Int_longlong;
					} else {
						pos++;
						tok.val.int_type = Int_long;
					}
				} else {
					tok.val.int_type = Int_int;
				}
			}

			pos--;
		} else {
			// TODO...
		}
	}
	pos++;
	*p = pos;
	return tok;
}

typedef struct MacroToken {
	Token tok;
	u32 file_offset;
	u16 file_ref;
	// 0 if this token is not a parameter, 1+(index into parameters) if it is.
	u8 parameter;
	u8 preceded_by_space;
} MacroToken;

typedef struct {
	String name;
} Parameter;

typedef struct {
	String name;
	u16 source_ref;
	SPAN(MacroToken) tokens;
	SPAN(Parameter) parameters;
	bool is_function_like;
	bool is_vararg;
	bool being_replaced;
} Macro;

typedef struct Replacement {
	Macro *mac;
	u32 pos;
	// If mac is NULL, this is an expanded buffer to be read from.
	// Otherwise, this is an array of the function-like macro's arguments.
	// STYLE The files field of the Tokenization is not used here.
	Tokenization *toks;
} Replacement;

typedef LIST(Replacement) MacroStack;


typedef struct ExpansionParams {
	MacroStack *stack;
	Arena *strings_arena;
	const StringMap *macros;
	SourceFile source;
	u32 source_file_offset;
	const char **src;
} ExpansionParams;

typedef LIST(MacroToken) ExpansionBuffer;

static bool expandInto(ExpansionParams, Tokenization *dest, bool is_argument);
static Tokenization *takeArguments(ExpansionParams, Macro *);
static void ensureCapacity (Tokenization *t, u32 required);
static void appendOneToken(Tokenization *t, Token tok, TokenPosition pos);
static String restOfLine (SourceFile source, const char **pos);
static SpaceClass tryGobbleSpace(SourceFile source, const char **p);
static void skipToEndIf(SourceFile source, const char **p);
static IfClass skipToElseIfOrEnd(SourceFile source, const char **p);
static bool gobbleSpaceToNewline(const char **p);
static bool evalPreprocExpression(SourceFile source, StringMap defined, const char **p, ExpansionBuffer *buf);
static void predefineMacros(StringMap *macros, Target *target);
Tokenization lex (Arena *generated_strings, const char *filename, Paths paths, Target *target) {
	typedef struct {
		u16 file;
		const char *pos;
	} Inclusion;

	qsort(names, sizeof(names)/sizeof(names[0]), sizeof(names[0]), keyword_cmp);

	Arena macro_arena = create_arena(2048);
	Tokenization t = {0};

	SourceFile *initial_source = readAllAlloc(zString(""), zString(filename));
	if (initial_source == NULL) {
		fprintf(stderr, RED "error: " RESET "could not open file \"%s\"\n", filename);
		exit(1);
	}
	PUSH(t.files, initial_source);
	SourceFile source = *initial_source;

	StringMap sources = {0};
	StringMap macros = {0};
	StringMap macro_parameters = {0};
	LIST(Inclusion) includes_stack = {0};
	ExpansionBuffer prepreoc_evaluation_buf = {0};
	MacroStack macro_expansion_stack = {0};

	const char *pos = source.content.ptr;

	predefineMacros(&macros, target);
	ExpansionParams expansion = {
		.stack = &macro_expansion_stack,
		.strings_arena = generated_strings,
		.macros = &macros,
		.src = &pos,
	};

	while (*pos) {
		bool file_begin = pos == source.content.ptr;
		bool line_begin;
		Token tok;
		const char *begin;
		while (true) {
			line_begin = tryGobbleSpace(source, &pos) == Space_Linebreak || file_begin;
			begin = pos;
			tok = getToken(generated_strings, &pos);
			if (tok.kind != Tok_EOF || source.idx == 0)
				break;
			Inclusion inc = POP(includes_stack);
			pos = inc.pos;
			source = *t.files.ptr[inc.file];
		}

		u32 source_pos = begin - source.content.ptr;

		if (tok.kind == Tok_PreprocDirective) {
			if (!line_begin)
				lexerror(source, begin, "a preprocessor directive must be the first token of a line");
			String directive = tok.val.identifier;
			while (pos[0] == ' ' || pos[0] == '\t') pos++;

			if (eql("define", directive)) {
				const char *start = pos;
				if (!isAlpha(pos[0]))
					lexerror(source, pos, "expected a macro identifier starting with a letter or underscore");
				pos++;
				while (pos[0] != 0 && !isSpace(pos[0]) && pos[0] != '(') {
					if (!isAlnum(pos[0]))
						lexerror(source, pos, "macro identifier may only contain alphanumeric characters");
					pos++;
				}
				String name = {pos - start, start};
				void **entry = mapGetOrCreate(&macros, name);
// 				if (*entry != NULL)
// 					fprintf(stderr, "redefining %.*s (TODO Check that definitions are identical)\n", STRING_PRINTAGE(name));

				Macro *mac = ALLOC(&macro_arena, Macro);
				*entry = mac;
				*mac = (Macro) {name, source.idx};

				u32 parameters = 0;
				if (pos[0] == '(') {
					pos++;
					const char *p = pos;
					while (true) {
						gobbleSpaceToNewline(&p);
						if (*p == '\n' || *p == 0)
							lexerror(source, pos, "expected parameter before end of line");

						Token t = getToken(generated_strings, &p);
						if (t.kind != Tok_Identifier && t.kind != Tok_TripleDot)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
						parameters++;
						gobbleSpaceToNewline(&p);
						if (*p == '\n' || *p == 0)
							lexerror(source, pos, "expected parameter before end of line");

						t = getToken(generated_strings, &p);
						if (t.kind == Tok_CloseParen)
							break;
						else if (t.kind != Tok_Comma)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
					}
					mac->parameters.ptr = aalloc(&macro_arena, sizeof(Parameter) * parameters);
					mac->parameters.len = parameters;
					mac->is_function_like = true;

					for (u32 i = 0; i < parameters; i++) {
						gobbleSpaceToNewline(&pos);
						Token t = getToken(generated_strings, &pos);
						assert(t.kind == Tok_Identifier || t.kind == Tok_TripleDot);
						String name;
						if (t.kind == Tok_TripleDot) {
							if (i + 1 != parameters)
								lexerror(source, pos, "an ellipsis may only appear as the last parameter to a variadic macro");
							// TODO Version check for C99
							name = zString("__VA_ARGS__");
						} else {
							name = t.val.identifier;
						}

						mac->parameters.ptr[i].name = name;
						void **entry = mapGetOrCreate(&macro_parameters, name);
						if (*entry)
							lexerror(source, ((String *)*entry)->ptr, "macro parameters may not be duplicated");
						*entry = &mac->parameters.ptr[i];

						gobbleSpaceToNewline(&pos);
						t = getToken(generated_strings, &pos);
					}
				}

				LIST(MacroToken) macro_assembly = {0};
				while (true) {
					bool havespace = gobbleSpaceToNewline(&pos);
					if (*pos == '\n')
						break;

					u32 macro_pos = pos - source.content.ptr;
					MacroToken t = {
						.tok = getToken(generated_strings, &pos),
						.file_offset = macro_pos,
						.file_ref = source.idx,
						.preceded_by_space = havespace
					};
					if (t.tok.kind == Tok_EOF)
						lexerror(source, pos, "macro definition must end before end of source file");
					if (parameters && t.tok.kind == Tok_Identifier) {
						Parameter *param = mapGet(&macro_parameters, t.tok.val.identifier);
						if (param)
							t.parameter = param - mac->parameters.ptr + 1;
					}
					PUSH(macro_assembly, t);
				}
				mac->tokens.ptr = macro_assembly.ptr;
				mac->tokens.len = macro_assembly.len;

				if (parameters > 0)
					mapFree(&macro_parameters);

			} else if (eql("undef", directive)) {
				gobbleSpaceToNewline(&pos);
				if (*pos == '\n' || *pos == 0)
					lexerror(source, pos, "#undef expects one identifier");
				tok = getToken(generated_strings, &pos);

				gobbleSpaceToNewline(&pos);
				if (tok.kind != Tok_Identifier || *pos != '\n')
					lexerror(source, pos, "#undef expects one identifier");
				Macro *prev = mapRemove(&macros, tok.val.identifier);

				if (prev)
					free(prev->tokens.ptr);

			} else if (eql("include", directive)) {
				// TODO Preprocessor replacements on the arguments to #include (6.10.2.4)
				char delimiter;
				if (pos[0] == '\"')
					delimiter = '\"';
				else if (pos[0] == '<')
					delimiter = '>';
				else
					lexerror(source, pos, "#include expects <FILENAME> or \"FILENAME\"");

				pos++;
				const char *begin = pos;
				while (*pos != delimiter) {
					if (*pos == '\0') {
						lexerror(source, pos, "#include expects <FILENAME> or \"FILENAME\"");
					}
					pos++;
				}

				String includefilename = {pos - begin, begin};

				SourceFile *new_source = NULL;
				void **already_loaded = mapGetOrCreate(&sources, includefilename);
				if (*already_loaded) {
					new_source = *already_loaded;
				} else {
					if (delimiter == '\"') {
						for (u32 i = 0; i < paths.user_include_dirs.len && new_source == NULL; i++) {
							new_source = readAllAlloc(paths.user_include_dirs.ptr[i], includefilename);
						}
					}
					for (u32 i = 0; i < paths.sys_include_dirs.len && new_source == NULL; i++) {
						new_source = readAllAlloc(paths.sys_include_dirs.ptr[i], includefilename);
					}
					if (new_source == NULL)
						lexerror(source, begin, "could not open include file \"%.*s\"", STRING_PRINTAGE(includefilename));
					new_source->idx = t.files.len;
					PUSH(t.files, new_source);
					*already_loaded = new_source;
				}
				pos++;
				PUSH(includes_stack, ((Inclusion) {source.idx, pos}));

				source = *new_source;
				pos = source.content.ptr;

			} else if (eql("pragma", directive)) {
				// TODO: ???
				while (*pos && tryGobbleSpace(source, &pos) != Space_Linebreak) pos++;;

			} else if (eql("if", directive)) {
				while (!evalPreprocExpression(source, macros, &pos, &prepreoc_evaluation_buf) &&
					skipToElseIfOrEnd(source, &pos) == If_ElseIf);

			} else if (eql("ifdef", directive) || eql("ifndef", directive)) {
				if (tryGobbleSpace(source, &pos) == Space_Linebreak)
					lexerror(source, pos, "unknown preprocessor directive");
				tok = getToken(generated_strings, &pos);
				if (tok.kind != Tok_Identifier)
					lexerror(source, pos, "#ifdef must be followed by an identifier");


				bool got = mapGet(&macros, tok.val.identifier);
				bool required = directive.ptr[2] != 'n';
				if (got != required) {
					if (tryGobbleSpace(source, &pos) != Space_Linebreak)
						lexerror(source, pos, "#ifdef may not be followed by more than one identifier");

					while (skipToElseIfOrEnd(source, &pos) == If_ElseIf &&
						!evalPreprocExpression(source, macros, &pos, &prepreoc_evaluation_buf));

				}

			} else if (eql("else", directive) || eql("elseif", directive)) {
				// TODO Check correct nesting
				skipToEndIf(source, &pos);

			} else if (eql("endif", directive)) {
				// TODO Check correct nesting

			} else if (eql("error", directive)) {
				const char *start = pos;
				String str = restOfLine(source, &pos);
				lexerror(source, start, "%.*s", STRING_PRINTAGE(str));

			} else if (eql("warning", directive)) {
				const char *start = pos;
				String str = restOfLine(source, &pos);
				lexwarning(source, start, "%.*s", STRING_PRINTAGE(str));

			} else {
				lexerror(source, directive.ptr, "unknown preprocessor directive");
			}
			continue;
		}

		if (tok.kind == Tok_Identifier) {
			Macro *macro = mapGet(&macros, tok.val.identifier);

			// STYLE Copypasta from expandInto, because macro_file_ref
			// should not be set on unexpanded function-like macros.
			if (macro) {
				Tokenization *arguments = NULL;
				expansion.source = source;
				expansion.source_file_offset = source_pos;

				if (macro->is_function_like) {
					tryGobbleSpace(source, &pos);
					Token paren = getToken(generated_strings, &pos);

					if (paren.kind != Tok_OpenParen) {
						appendOneToken(&t, tok, (TokenPosition) {source_pos, source.idx});
						appendOneToken(&t, paren, (TokenPosition) {source_pos, source.idx});
						continue;
					}
					arguments = takeArguments(expansion, macro);
				}
				macro->being_replaced = true;
				PUSH(macro_expansion_stack, ((Replacement) {macro, .toks = arguments}));
				expandInto(expansion, &t, false);
				assert(macro_expansion_stack.len == 0);
				continue;
			}
		}
		appendOneToken(&t, tok, (TokenPosition) {source_pos, source.idx});

		if (tok.kind == Tok_EOF && source.idx == 0)
			break;
	}

	free(prepreoc_evaluation_buf.ptr);
	free(includes_stack.ptr);
	free(macro_expansion_stack.ptr);
	for (u32 i = 0; i < macros.capacity; i++) {
		if (macros.content[i] && ((Macro*)macros.content[i])->tokens.len)
			free(((Macro*)macros.content[i])->tokens.ptr);
	}
	free_arena(&macro_arena);
	mapFree(&macros);
	mapFree(&sources);
	return t;
}

static MacroToken takeToken(ExpansionParams ex, u32 *marker);
static void collapseMacroStack(MacroStack *stack, u32 *marker);

// If is_argument is specified, halt after a comma (returning true) or
// closing parenthesis (returning false)
static bool expandInto (const ExpansionParams ex, Tokenization *dest, bool is_argument) {
	u32 paren_depth = 0;

	// The standard says that arguments should be found before they are
	// expanded, but that would mean having to look at each argument
	// token twice. This function expands everything immediately, so it
	// needs to take care that the parens and commas that delimit an
	// argument do not come from an inner macro expansion.
	// The following variable denotes the stack level above the lowest
	// macro visited (or an argument Tokenization above that), above
	// which parens do not count towards paren_depth.
	u32 level_of_argument_source = ex.stack->len;

	collapseMacroStack(ex.stack, &level_of_argument_source);

	while (is_argument || ex.stack->len > 0) {
		MacroToken t = takeToken(ex, &level_of_argument_source);
		Macro *mac;

		if (t.tok.kind == Tok_Identifier
			&& (mac = mapGet(ex.macros, t.tok.val.identifier)) != NULL
			&& !mac->being_replaced)
		{
			Tokenization *arguments = NULL;
			if (mac->is_function_like) {
				MacroToken paren = takeToken(ex, &level_of_argument_source);
				if (paren.tok.kind != Tok_OpenParen) {
					appendOneToken(dest, t.tok, (TokenPosition) {
						.source_file_ref = ex.source.idx,
						.source_file_offset = ex.source_file_offset,
						.macro_file_ref = t.file_ref,
						.macro_file_offset = t.file_offset,
					});
					appendOneToken(dest, paren.tok, (TokenPosition) {
						.source_file_ref = ex.source.idx,
						.source_file_offset = ex.source_file_offset,
						.macro_file_ref = paren.file_ref,
						.macro_file_offset = paren.file_offset,
					});
					continue;
				}
				arguments = takeArguments(ex, mac);
			}
			mac->being_replaced = true;
			PUSH(*ex.stack, ((Replacement) {mac, .toks = arguments}));
			collapseMacroStack(ex.stack, &level_of_argument_source);
		} else {
			if (is_argument && level_of_argument_source == ex.stack->len) {
				switch (t.tok.kind) {
				case Tok_Comma:
					if (paren_depth == 0)
						return false;
					break;
				case Tok_OpenParen:
					paren_depth++;
					break;
				case Tok_CloseParen:
					if (paren_depth == 0)
						return true;
					paren_depth--;
					break;
				case Tok_EOF:
					lexerror(ex.source, ex.source.content.ptr + ex.source_file_offset,
							"missing closing parenthesis of macro invocation"); // TODO Better location
				default:
					break;
				}
			}
			appendOneToken(dest, t.tok, (TokenPosition) {
				.source_file_ref = ex.source.idx,
				.source_file_offset = ex.source_file_offset,
				.macro_file_ref = t.file_ref,
				.macro_file_offset = t.file_offset,
			});
		}
	}

	return false;
}

// Allocates the argument list.
// PERFOMANCE A single Tokenization for all pre-expansion should be
// enough if expanded-buffer Replacements store offset and length.
static Tokenization *takeArguments(const ExpansionParams ex, Macro *mac) {
	assert(mac->is_function_like);
	Tokenization *argument_bufs = calloc(mac->parameters.len, sizeof(Tokenization));
	u32 param_idx = 0;

	while (true) {
		bool arg_list_closed = expandInto(ex, &argument_bufs[param_idx], true);
		if (arg_list_closed) {
			u32 expected_count = mac->parameters.len;
			if (param_idx + 1 == expected_count ||
				(param_idx + 2 == expected_count && mac->is_vararg))
			{
				break;
			} else {
				lexerror(ex.source, ex.source.content.ptr + ex.source_file_offset,
						"too few arguments provided"); // TODO Better location
			}
		} else {
			param_idx++;
			if (param_idx == mac->parameters.len) {
				if (mac->is_vararg)
					param_idx--;
				else
					lexerror(ex.source, ex.source.content.ptr + ex.source_file_offset,
							"too many arguments provided"); // TODO Better location
			}
		}
	}
	return argument_bufs;
}

static MacroToken takeToken (const ExpansionParams ex, u32 *marker) {
	MacroStack *stack = ex.stack;
	while (stack->len) {
		Replacement *repl = &stack->ptr[stack->len - 1];
		Macro *macro = repl->mac;
		if (macro) {
			assert(repl->pos < macro->tokens.len);

			MacroToken t = repl->mac->tokens.ptr[repl->pos];
			repl->pos++;
			if (t.parameter) {
				if (t.tok.kind == Tok_PreprocDirective) {
					lexerror(ex.source, ex.source.content.ptr + ex.source_file_offset,
						"TODO Implement stringification");
				}

				Replacement arg = {
					.toks = &repl->toks[t.parameter-1],
				};
				if (*marker == stack->len)
					(*marker)++;
				PUSH(*stack, arg);
				collapseMacroStack(stack, marker);
			} else {
				collapseMacroStack(stack, marker);
				return t;
			}
		} else {
			assert(repl->pos < repl->toks->count);
			u32 pos = repl->pos++;
			collapseMacroStack(stack, marker);
			return (MacroToken) {
				.tok = repl->toks->tokens[pos],
				.file_ref = repl->toks->positions[pos].macro_file_ref,
				.file_offset = repl->toks->positions[pos].macro_file_offset,
			};
		}
	}

	bool have_space = tryGobbleSpace(ex.source, ex.src);
	u32 pos = *ex.src - ex.source.content.ptr;
	return (MacroToken) {
		.file_ref = ex.source.idx,
		.file_offset = pos,
		.preceded_by_space = have_space,
		.tok = getToken(ex.strings_arena, ex.src),
	};
}

// Pop completed replacements from the stack until hitting a token.
static void collapseMacroStack (MacroStack *stack, u32 *marker) {
	while (stack->len) {
		Replacement *repl = &stack->ptr[stack->len - 1];
		Macro *macro = repl->mac;
		if (macro) {
			if (repl->pos < macro->tokens.len)
				return;
			macro->being_replaced = false;
			if (macro->is_function_like) {
				for (u32 i = 0; i < macro->parameters.len; i++) {
					free(repl->toks[i].tokens);
					free(repl->toks[i].positions);
				}
				free(repl->toks);
			}
			if (*marker == stack->len)
				(*marker)--;
			stack->len--;
		} else {
			if (repl->pos < repl->toks->count)
				return;
			// PERFOMANCE It would be correct, but very confusing,
			// to drop this check on parameters.
			if (*marker == stack->len)
				(*marker)--;
			stack->len--;
		}
	}
}


static void ensureCapacity (Tokenization *t, u32 required) {
	if (t->count + required > t->capacity) {
		if (t->capacity == 0) {
			t->capacity = required;
			t->tokens = calloc(t->capacity, sizeof(t->tokens[0]));
			t->positions = calloc(t->capacity, sizeof(t->positions[0]));
		} else {
			t->capacity = t->capacity * 3 / 2 + required;
			t->tokens = realloc(t->tokens, sizeof(t->tokens[0]) * t->capacity);
			t->positions = realloc(t->positions, sizeof(t->positions[0]) * t->capacity);
		}
	}
}

static void appendOneToken (Tokenization *t, Token tok, TokenPosition pos) {
	ensureCapacity(t, 1);
	t->tokens[t->count] = tok;
	t->positions[t->count] = pos;
	t->count++;
}


static String restOfLine (SourceFile source, const char **pos) {
	tryGobbleSpace(source, pos);
	const char *start = *pos;
	while (**pos && **pos != '\n')
		(*pos)++;
	return (String) {*pos - start, start};
}

// PERFORMANCE This function may be critical for the parse.
static SpaceClass tryGobbleSpace (SourceFile source, const char **p) {
	const char *pos = *p;
	SpaceClass spacing = Space_Regular;
	while (true) {
		// TODO Do I really want to handle the ??/ trigraph everywhere
		// that newlines need to be skipped? Consider the performance
		// impact versus replacing all trigraphs on file input.
		if (pos[0] == '\\' && pos[1] == '\n') {
			pos += 2;
		} else if (pos[0] == '/' && pos[1] == '/') {
			// TODO Check for C99
			while (pos[0] && pos[0] != '\n') pos++;
		} else if (pos[0] == '/' && pos[1] == '*') {
			const char *begin = pos;
			while (pos[0] && !(pos[0] == '*' && pos[1] == '/')) pos++;
			if (!pos[0])
				lexerror(source, begin, "comment must end before end of file");
			pos += 2;
		} else {
			if (!isSpace(pos[0])) {
				if (pos == *p)
					return Space_None;
				*p = pos;
				return spacing;
			}
			if (pos[0] == '\n')
				spacing = Space_Linebreak;
			pos++;
		}
	}
}

// PERFORMANCE This function is potentially a very hot path.
static void skipToEndIf (SourceFile source, const char **p) {
	const char *pos = *p;

	bool linebegin = false;
	u32 nesting = 1;
	while (nesting > 0) {
		if (*pos == '\n') {
			linebegin = true;
		} else if (*pos == '#' && linebegin) {
			pos++;
			while (isSpace(*pos)) pos++;
			const char *begin = pos;
			while (*pos && !isSpace(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive)) {
				nesting++;
			} else if (eql("endif", directive)) {
				nesting--;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n')
				pos++;
			else
				linebegin = false;
		}
		pos++;
		if (*pos == 0)
			lexerror(source, *p, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
	}
	*p = pos;
}

static IfClass skipToElseIfOrEnd (SourceFile source, const char **p) {
	const char *pos = *p;

	bool linebegin = false;
	while (true) {
		if (*pos == '\n') {
			linebegin = true;
		} else if (*pos == '#' && linebegin) {
			pos++;
			while (isSpace(*pos)) pos++;
			const char *begin = pos;
			while (*pos && !isSpace(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive)) {
				skipToEndIf(source, &pos);
			} else if (eql("elseif", directive)) {
				*p = pos;
				return If_ElseIf;
			} else if (eql("else", directive)) {
				// TODO Check that the line ends after the else directive.
				*p = pos;
				return If_Else;
			} else if (eql("endif", directive)) {
				*p = pos;
				return If_End;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n')
				pos++;
			else
				linebegin = false;
		}
		pos++;
		if (*pos == 0)
			lexerror(source, *p, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
	}
}

static bool gobbleSpaceToNewline (const char **p) {
	bool got = false;
	const char *pos = *p;
	while (*pos && *pos != '\n') {
		if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n')
				pos++;
			else
				break;
		}
		got = true;
		pos++;
	}
	*p = pos;
	return got;
}


// === Parsing constant expressions for #if ===

typedef struct {
	SourceFile source;
	const MacroToken *tok;
} ConstParse;

static int parseOr(ConstParse *parse);
static int parseAnd(ConstParse *parse);
static int parseEq(ConstParse *parse);
static int parseCmp(ConstParse *parse);
static int parseUnop(ConstParse *parse);
static int parseCore(ConstParse *parse);

static int parseOr(ConstParse *parse) {
	int x = parseAnd(parse);
	while (parse->tok->tok.kind == Tok_DoublePipe) {
		parse->tok++;
		x = x || parseAnd(parse);
	}
	return x;
}

static int parseAnd(ConstParse *parse) {
	int x = parseEq(parse);
	while (parse->tok->tok.kind == Tok_DoublePipe) {
		parse->tok++;
		x = x || parseEq(parse);
	}
	return x;
}

static int parseEq(ConstParse *parse) {
	int x = parseCmp(parse);
	while (true) {
		if (parse->tok->tok.kind == Tok_DoubleEquals) {
			parse->tok++;
			x = x == parseCmp(parse);
		} else if (parse->tok->tok.kind == Tok_BangEquals) {
			parse->tok++;
			x = x != parseCmp(parse);
		} else {
			break;
		}
	}
	return x;
}
static int parseCmp(ConstParse *parse) {
	int x = parseUnop(parse);

	while (true) {
		if (parse->tok->tok.kind == Tok_Less) {
			parse->tok++;
			x = x < parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_LessEquals) {
			parse->tok++;
			x = x <= parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_Greater) {
			parse->tok++;
			x = x > parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_GreaterEquals) {
			parse->tok++;
			x = x >= parseUnop(parse);
		} else {
			break;
		}
	}
	return x;
}

static int parseUnop(ConstParse *parse) {
	switch (parse->tok->tok.kind) {
	case Tok_Minus:
		parse->tok++;
		return -parseUnop(parse);
	case Tok_Tilde:
		parse->tok++;
		return ~parseUnop(parse);
	case Tok_Bang:
		parse->tok++;
		return !parseUnop(parse);
	default:
		return parseCore(parse);
	}
}
static int parseCore(ConstParse *parse) {
	if (parse->tok->tok.kind == Tok_Integer) {
		int x = parse->tok->tok.val.integer;
		parse->tok++;
		return x;
	} else if (parse->tok->tok.kind == Tok_OpenParen) {
		parse->tok++;
		int x = parseOr(parse);
		if (parse->tok->tok.kind != Tok_CloseParen)
			lexerror(parse->source, parse->source.content.ptr + parse->tok->file_offset, "exepected closing parenthesis");
		parse->tok++;
		return x;
	} else {
		lexerror(parse->source, parse->source.content.ptr + parse->tok->file_offset, "exepected an expression");
	}
}

static bool evalPreprocExpression(SourceFile source, StringMap defined, const char **p, ExpansionBuffer *buf) {
	const char *pos = *p;
	Arena *const str_arena = NULL; // TODO
	while (true) {
		// TODO Perfom macro replacement
		gobbleSpaceToNewline(&pos);
		if (*pos == '\n' || *pos == 0)
			break;
		Token tok = getToken(str_arena, &pos);
		if (tok.kind == Tok_Identifier) {
			if (eql("defined", tok.val.identifier)) {
				tok = getToken(str_arena, &pos);
				bool parenthesized = tok.kind == Tok_OpenParen;
				if (parenthesized)
					tok = getToken(str_arena, &pos);
				if (tok.kind != Tok_Identifier)
					lexerror(source, pos, "the operator %sdefined%s expects an identifier as an argument", BOLD, RESET);

				bool found = mapGet(&defined, tok.val.identifier);
				tok = (Token) {Tok_Integer, {.integer = found, .int_type = Int_int}};

				if (parenthesized && getToken(str_arena, &pos).kind != Tok_CloseParen)
					lexerror(source, pos, "missing closing parenthesis");
			} else {
				Macro *m = mapGet(&defined, tok.val.identifier);
				if (m) {
					if (m->parameters.len > 0)
						lexerror(source, pos, "TODO Implement function-like macros");
					for (u32 i = 0; i < m->tokens.len; i++)
						PUSH(*buf, m->tokens.ptr[i]);
					continue;
				} else {
					tok = (Token) {Tok_Integer, {.integer = 0, .int_type = Int_int}};
				}
			}
		}
		PUSH(*buf, ((MacroToken) {tok, pos - *p}));
	}
	*p = pos;
	PUSH(*buf, ((MacroToken) {{Tok_EOF}, pos - *p}));

	ConstParse parse = {source, buf->ptr};
	int res = parseOr(&parse);
	buf->len = 0;
	return res;
}


// === Helpers ===


// TODO Come up with a better system for nice highlighting.
const char *token_names[Tok_EOF+1] = {
	[Tok_Identifier] = "identifier",
	[Tok_Integer] = "integer literal",
	[Tok_Real] = "floating point literal",
	[Tok_String] = "string literal",

	[Tok_OpenParen] = "(",
	[Tok_CloseParen] = ")",
	[Tok_OpenBrace] = "{",
	[Tok_CloseBrace] = "}",
	[Tok_OpenBracket] = "[",
	[Tok_CloseBracket] = "]",

	[Tok_Semicolon] = ";",
	[Tok_Comma] = ",",
	[Tok_Colon] = ":",
	[Tok_Dot] = ".",
	[Tok_TripleDot] = "...",

	[Tok_Bang] = "!",
	[Tok_BangEquals] = "!=",
	[Tok_Equals] = "=",
	[Tok_DoubleEquals] = "==",
	[Tok_Arrow] = "->",
	[Tok_Plus] = "+",
	[Tok_PlusEquals] = "+=",
	[Tok_Minus] = "-",
	[Tok_MinusEquals] = "-=",
	[Tok_Asterisk] = "*",
	[Tok_AsteriskEquals] = "*=",
	[Tok_Slash] = "/",
	[Tok_SlashEquals] = "/=",
	[Tok_Less] = "<",
	[Tok_LessEquals] = "<=",
	[Tok_DoubleLess] = "<<",
	[Tok_Greater] = ">",
	[Tok_GreaterEquals] = ">=",
	[Tok_DoubleGreater] = ">>",
	[Tok_Ampersand] = "&",
	[Tok_DoubleAmpersand] = "&&",
	[Tok_Pipe] = "|",
	[Tok_DoublePipe] = "||",
	[Tok_Hat] = "^",
	[Tok_Tilde] = "~",

	[Tok_EOF] = "end of file",
};

static char name[256] = {0};
static const char *const name_end = name + 256;

const char *tokenNameHighlighted (TokenKind kind) {
	switch (kind) {
	case Tok_Identifier: return "identifier";
	case Tok_Integer: return "integer literal";
	case Tok_Real: return "floating point literal";
	case Tok_String: return "string literal";
	default:;
		char *c = name;
		printto(&c, name_end, "%s%s%s", BOLD, tokenName(kind), RESET);
		return name;
	}
}

const char *tokenName (TokenKind kind) {
	if (token_names[kind]) {
		return token_names[kind];
	} else {
		if (kind >= Tok_Key_First && kind <= Tok_Key_Last) {
			const int count = sizeof(names) / sizeof(names[0]);
			for (int i = 0; i < count; i++) {
				if (names[i].key == kind)
					return names[i].name;
			}
		}
		return NULL;
	}
}

bool isSpace (char c) {
	switch (c) {
		case ' ':
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case '\t':
			return true;
		default:
			return false;
	}
}

bool isAlpha (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit (char c) {
	return c >= '0' && c <= '9';
}

bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}


static void predefineMacros (StringMap *macros, Target *target) {
	(void) macros;
	(void) target;
	// TODO __STDC_ANALYZABLE__, __GNU__ etc.
}

String processStringLiteral(Arena *arena, String src) {
	char *res = aalloc(arena, src.len + 1);
	u32 len = 0;
	for (u32 i = 0; i < src.len; i++) {
		res[len] = src.ptr[i];
		len++;
	}
	res[len] = 0;
	return (String) {len + 1, res};
}
