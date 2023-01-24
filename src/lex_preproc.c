#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "lex_preproc.h"
#include "arena.h"
#include "ansi.h"
#include "common.h"

/*

Combined lexer and preprocessor.

*/

#if !MAX_INCLUDES
#define MAX_INCLUDES 4096
#endif
#define ARRSIZE(x) sizeof(x)/sizeof(x[0])

static inline bool isSpace(char);
static inline bool isAlpha(char c);
static inline bool isHexDigit(char c);
static inline int hexToInt(char c);
static inline bool isDigit(char c);
static inline bool isAlnum(char c);


typedef struct {
	u32 hash;
	u32 idx;
} IndexEntry;

typedef struct {
	IndexEntry *entries;

	u32 capacity;
} SymbolMap;


// TODO Make it thread-local?
static SymbolMap map;

static Symbol *getSymbol(SymbolList *, String name);
static u32 getSymbolId(SymbolList *, String name);

typedef enum {
	Space_None,
	Space_Regular,
	Space_Linebreak,
} SpaceClass;


typedef enum {
	If_If,
	If_IfDef,
	If_IfnDef,
	If_Else,
	If_End,
} IfClass;

typedef struct {
	char *name;
	unsigned int key;
} Keyword;

Keyword standard_keywords[] = {
	{"if", Tok_Key_If},
	{"else", Tok_Key_Else},
	{"switch", Tok_Key_Switch},
	{"case", Tok_Key_Case},
	{"goto", Tok_Key_Goto},
	{"while", Tok_Key_While},
	{"do", Tok_Key_Do},
	{"for", Tok_Key_For},
	{"return", Tok_Key_Return},
	{"break", Tok_Key_Break},
	{"continue", Tok_Key_Continue},
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
	{"inline", Tok_Key_Inline},
	{"_Noreturn", Tok_Key_Noreturn},
	{"_Static_assert", Tok_Key_StaticAssert},


	{"__restrict", Tok_Key_Restrict},
	{"__inline", Tok_Key_Inline},
	{"__alignof__", Tok_Key_Alignof},

	{"__FILE__", Tok_Key_File},
	{"__LINE__", Tok_Key_Line},
};

Keyword special_identifiers[] = {
	{"__func__", Special___func__},
	{"memcpy", Special_memcpy},
	{"malloc", Special_malloc},
};

Keyword intrinsics[] = {
	{"__builtin_va_list", Intrinsic_VaList},
	{"__builtin_va_start", Intrinsic_VaStart},
	{"__builtin_va_end", Intrinsic_VaEnd},
	{"__builtin_va_arg", Intrinsic_VaArg},
	{"__builtin_va_copy", Intrinsic_VaCopy},

	{"__builtin_nanf", Intrinsic_Nanf},
	{"__builtin_inff", Intrinsic_Inff},

};

enum Directive {
	Directive_If = 1,
	Directive_Ifdef,
	Directive_Ifndef,
	Directive_Else,
	Directive_Elif,
	Directive_Elifdef,
	Directive_Elifndef,
	Directive_Endif,

	Directive_Error,
	Directive_Warn,

	Directive_Pragma,
	Directive_Define,
	Directive_Undef,
	Directive_Include,
};

Keyword preproc_directives[] = {
	{"if", Directive_If},
	{"ifdef", Directive_Ifdef},
	{"ifndef", Directive_Ifndef},
	{"else", Directive_Else},
	{"elif", Directive_Elif},
	{"elifdef", Directive_Elifdef},
	{"elifndef", Directive_Elifndef},
	{"endif", Directive_Endif},

	{"pragma", Directive_Pragma},
	{"error", Directive_Error},
	{"warning", Directive_Warn},
	{"define", Directive_Define},
	{"undef", Directive_Undef},
	{"include", Directive_Include},
};


char escape_codes[256] = {
	['0'] = 0,
	['\\'] = '\\',
	['\"'] = '\"',
	['r'] = '\r',
	['v'] = '\v',
	['t'] = '\t',
	['n'] = '\n',
};

char de_escape_codes[256] = {
	[0] = '0',
	['\\'] = '\\',
	['\"'] = '\"',
	['\r'] = 'r',
	['\v'] = 'v',
	['\t'] = 't',
	['\n'] = 'n',
};

static _Noreturn void lexerror (SourceFile source, Location loc, const char *msg, ...) {
    printErr(source, loc);

    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, ".\n");

	exit(1);
}

static void lexwarning (SourceFile source, Location loc, const char *msg, ...) {
    printWarn(source, loc);

    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, ".\n");
}

String processStringLiteral(Arena *arena, String src);

// TODO Trigraphs (trivial) and digraphs (annoying)
static Token getToken (Arena *str_arena, SourceFile source, Location *loc, SymbolList *syms, const char **p) {
	Token tok = {0};
	const char *pos = *p;

	switch (pos[0]) {
	case '\0':
		tok.kind = Tok_EOF;
		*p = pos;
		return tok;
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
		if (pos[1] == '<') {
			pos++;
			tok.kind = Tok_DoubleLess;
		} else {
			tok.kind = Tok_Less;
		}
		break;
	case '>':
		if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_DoubleGreater;
		} else {
			tok.kind = Tok_Greater;
		}
		break;
	case '+':
		if (pos[1] == '+') {
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
		} else if (pos[1] == '-') {
			pos++;
			tok.kind = Tok_DoubleMinus;
		} else {
			tok.kind = Tok_Minus;
		}
		break;
	case '*':
		tok.kind = Tok_Asterisk;
		break;
	case '/':
		tok.kind = Tok_Slash;
		break;
	case '!':
		tok.kind = Tok_Bang;
		break;
	case '%':
		tok.kind = Tok_Percent;
		// TODO Digraphs
		break;
	case '=':
		tok.kind = Tok_Equals;
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
			while (isAlnum(pos[0])) pos++;

			u32 id = getSymbolId(syms, (String) {pos - start, start});
			tok = (Token) {Tok_PreprocDirective, .val.symbol_idx = id};
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
		if (*pos == '\0')
			lexerror(source, *loc, "missing close paren");

		String val = processStringLiteral(str_arena, (String) {pos - begin, begin});
		tok = (Token) {Tok_String, .val.symbol_idx = getSymbolId(syms, val)};
	} break;
	case '\'': {
	char_literal:
		pos++;
		// TODO Get unicode vals.
		tok = (Token) {Tok_Integer, .literal_type = Int_int, .val.integer_s = pos[0]};
		if (pos[0] == '\\') {
			pos++;
			if (pos[0] == 'x') {
				assert(isHexDigit(pos[1]));
				assert(isHexDigit(pos[2]));
				tok.val.integer_s = hexToInt(pos[1])*16 + hexToInt(pos[2]);
				if (tok.val.integer_s >= 128)
					tok.val.integer_s -= 256;
				pos += 2;
			} else {
				tok.val.integer_s = escape_codes[(uchar)pos[0]];
			}
		}
		pos++;
		// TODO Error reporting
		if (pos[0] != '\'') unreachable;
	} break;
	default:
		if (isAlpha(pos[0])) {
			if (pos[0] == 'L' && pos[1] == '\'') {
				pos++;
				goto char_literal;
			}
			const char *start = pos;
			while (isAlnum(*pos))
				pos++;

			String word = {pos - start, start};
			tok = (Token) {Tok_Identifier, .val.symbol_idx = getSymbolId(syms, word)};

			pos--;
		} else if (isDigit(pos[0]) || pos[0] == '.') {
			// Could just use strtod/strtol for number parsing, but the
			// standard library functions have a pretty inefficient
			// interface and I will have to manually handle C23 stuff
			// like decimal-type suffixes and digit separators anyways.
			const char *start = pos;

			bool is_hex = false;
			if (pos[0] == '0' && pos[1] == 'x') {
				is_hex = true;
				pos += 2;
			}

			while (isDigit(*pos) || (is_hex && isHexDigit(*pos)))
				pos++;

			if (*pos == '.' || *pos == 'f' || *pos == 'F') {
				pos++;
				while (isDigit(*pos))
					pos++;
				if (*pos == 'e' || *pos == 'E')
					pos++;
				if ((*pos == '-' || *pos == '+') && isDigit(pos[1]))
					pos++;

				while (isDigit(*pos))
					pos++;
				tok = (Token) {
					Tok_Real,
					.literal_type = Float_Double,
					.val.real = strtod(start, NULL)
				};

				if (*pos == 'f' || *pos == 'F') {
					tok.literal_type = Float_Single,
					pos++;
				}
			} else {
				tok = (Token) {Tok_Integer,
					.literal_type = Int_int,
					.val.integer_s = strtoll(start, NULL, 0)
				};

				bool is_unsigned = pos[0] == 'u' || pos[0] == 'U';
				if (is_unsigned)
					pos++;
				if (pos[0] == 'l' || pos[0] == 'L') {
					pos++;
					tok.literal_type = Int_long;
					if (pos[0] == 'l' || pos[0] == 'L') {
						pos++;
						tok.literal_type = Int_longlong;
					}
				}
				if (pos[0] == 'u' || pos[0] == 'U') {
					is_unsigned = true;
					pos++;
				}
				// FIXME Use platform-correct integer sizes.
				if (is_unsigned) {
					if (tok.literal_type == Int_int && tok.val.integer_s > UINT32_MAX)
						tok.literal_type = Int_long;
					tok.literal_type |= Int_unsigned;
				} else {
					// ???
					if (tok.literal_type == Int_int && tok.val.integer_s > UINT32_MAX)
						tok.literal_type = Int_long;
				}
			}

			pos--;
		} else {
			// TODO...
		}
	}
	pos++;
	if (tok.kind >= Tok_Equalable_Start && pos[0] == '=') {
		pos++;
		tok.kind |= Tok_EQUALED;
	}
	loc->column += pos - *p;
	*p = pos;
	return tok;
}


typedef struct MacroToken {
	Token tok;
	Location loc;
	// 0 if this token is not a parameter, 1+(index into parameters) if it is.
	u16 parameter;
} MacroToken;

typedef struct {
	String name;
} Parameter;

typedef struct Macro {
	String name;
	SourceFile *source;

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
	TokenList *toks;
	Location loc;
	bool followed_by_concat;
} Replacement;

typedef LIST(Replacement) MacroStack;


typedef struct ExpansionParams {
	MacroStack *stack;
	FileList *files;

	Arena *strings_arena;
	Tokenization *tok;
	Options *opt;

	SourceFile source;
	Location expansion_start;
	Location *loc;
	const char **src;
} ExpansionParams;

typedef LIST(MacroToken) ExpansionBuffer;

static bool expandInto(ExpansionParams, TokenList *dest, bool is_argument);
static TokenList *takeArguments(ExpansionParams, Location, Macro *);
static void ensureCapacity (TokenList *t, u32 required);
static void appendOneToken(TokenList *t, Token tok, TokenLocation pos);
static String restOfLine (SourceFile source, Location *, const char **pos);
static SpaceClass tryGobbleSpace(SourceFile source, Location *, const char **p);
static void skipToValidBranchOrEnd(ExpansionParams, IfClass, u32 *depth, const char **p, TokenList *eval_buf);
static void skipToEndIf(SourceFile source, Location *, const char **p);
static IfClass skipToElseIfOrEnd(SourceFile source, Location *, const char **p);
static Token getTokenSpaced(Arena *, SourceFile, Location *, SymbolList *, const char **p);
static bool gobbleSpaceToNewline(const char **p, Location *loc);
static bool preprocExpression(ExpansionParams, TokenList *buf, Tokenization *main_tokenization);
static const char *defineMacro(Arena *arena, Arena *generated_strings, SymbolList *, String name, SourceFile *, Location *loc, const char *pos);
static void predefineMacros(Arena *arena, Arena *genrated_strings, SymbolList *, FileList *, StringList to_define, SourceKind);
static void resolveSymbolIndicesToPointers(TokenList t, Symbol *syms);


Tokenization lex (Arena *generated_strings, String filename, LexParams params) {
	typedef struct {
		Location loc;
		const char *pos;
	} Inclusion;

	Arena macro_arena = create_arena(256 * 1024L);
	Tokenization t = {0};

	StringMap sources = {0};
	LIST(Inclusion) includes_stack = {0};
	TokenList prepreoc_evaluation_buf = {0};
	MacroStack macro_expansion_stack = {0};
	u32 if_depth = 0;

	for (u32 i = 0; i < ARRSIZE(standard_keywords); i++)
		getSymbol(&t.symbols, zstr(standard_keywords[i].name))->keyword = standard_keywords[i].key;
	for (u32 i = 0; i < ARRSIZE(intrinsics); i++) {
		Symbol *s = getSymbol(&t.symbols, zstr(intrinsics[i].name));
		s->directive = intrinsics[i].key;
		s->keyword = Tok_Intrinsic;
	}
	u32 special_identifiers_idx[ARRSIZE(special_identifiers)];
	for (u32 i = 0; i < ARRSIZE(special_identifiers_idx); i++)
		special_identifiers_idx[i] = getSymbolId(&t.symbols, zstr(special_identifiers[i].name));

	for (u32 i = 0; i < ARRSIZE(preproc_directives); i++)
		getSymbol(&t.symbols, zstr(preproc_directives[i].name))->directive = preproc_directives[i].key;


	PUSH(t.files, NULL);
	SourceFile *initial_source = readAllAlloc(STRING_EMPTY, filename);
	if (initial_source == NULL)
		generalFatal("could not open file \"%.*s\"", STRING_PRINTAGE(filename));
	initial_source->idx = 1;

	PUSH(t.files, initial_source);
	const char *pos = initial_source->content.ptr;
	SourceFile source = *initial_source;
	Location loc = {source.idx, 1, 1};


	predefineMacros(&macro_arena, generated_strings, &t.symbols, &t.files,
			params.command_line_macros, Source_CommandLineMacro);
	predefineMacros(&macro_arena, generated_strings, &t.symbols, &t.files,
			params.system_macros, Source_SystemDefinedMacro);


	ExpansionParams expansion = {
		.stack = &macro_expansion_stack,
		.strings_arena = generated_strings,
		.files = &t.files,
		.tok = &t,
		.loc = &loc,
		.src = &pos,
		.opt = params.options,
	};
	bool first_line_in_file = true;
	while (true) {
		bool file_begin = pos == source.content.ptr;
		bool line_begin;
		Token tok;
		Location begin;

		while (true) {
			line_begin = tryGobbleSpace(source, &loc, &pos) == Space_Linebreak || file_begin;
			begin = loc;
			tok = getToken(generated_strings, source, &loc, &t.symbols, &pos);
			if (tok.kind != Tok_EOF || source.idx == initial_source->idx)
				break;
			Inclusion inc = POP(includes_stack);
			pos = inc.pos;
			source = *t.files.ptr[inc.loc.file_id];
			loc = inc.loc;
		}

		if (tok.kind == Tok_PreprocDirective) {
			if (!line_begin)
				lexerror(source, begin, "a preprocessor directive must be the first token of a line");
			gobbleSpaceToNewline(&pos, &loc);

			enum Directive dir = t.symbols.ptr[tok.val.symbol_idx].directive;
			switch (dir) {
			case Directive_Define: {
				const char *start = pos;
				if (!isAlpha(pos[0]))
					lexerror(source, begin, "expected a macro identifier starting with a letter or underscore");
				pos++;
				while (pos[0] != 0 && !isSpace(pos[0]) && pos[0] != '(') {
					if (!isAlnum(pos[0]))
						lexerror(source, begin, "macro identifier may only contain alphanumeric characters");
					pos++;
				}

				// TODO Disallow `defined`
				String name = {pos - start, start};
				loc.column += pos - start;
// 				if (*entry != NULL)
// 					fprintf(stderr, "redefining %.*s (TODO Check that definitions are identical)\n", STRING_PRINTAGE(name));

				pos = defineMacro(&macro_arena, generated_strings, &t.symbols, name, t.files.ptr[source.idx], &loc, pos);
			} break;
			case Directive_Undef: {
				tok = getTokenSpaced(generated_strings, source, &loc, &t.symbols, &pos);

				gobbleSpaceToNewline(&pos, &loc);
				if (tok.kind != Tok_Identifier || *pos != '\n')
					lexerror(source, begin, "#undef expects one identifier");
				Macro *prev = t.symbols.ptr[tok.val.symbol_idx].macro;
				t.symbols.ptr[tok.val.symbol_idx].macro = NULL;

				if (prev)
					free(prev->tokens.ptr);
			} break;
			case Directive_Include: {
				// TODO Preprocessor replacements on the arguments to #include (6.10.2.4)
				char delimiter;
				if (pos[0] == '\"')
					delimiter = '\"';
				else if (pos[0] == '<')
					delimiter = '>';
				else
					lexerror(source, begin, "#include expects <FILENAME> or \"FILENAME\"");

				pos++;
				const char *start = pos;
				while (*pos != delimiter) {
					if (*pos == '\0') {
						lexerror(source, begin, "#include expects <FILENAME> or \"FILENAME\"");
					}
					pos++;
				}
				if (pos == start)
					lexerror(source, begin, "#include expects <FILENAME> or \"FILENAME\"");

				String includefilename = {pos - start, start};

				SourceFile *new_source = NULL;
				void **already_loaded = mapGetOrCreate(&sources, includefilename);
				if (*already_loaded) {
					new_source = *already_loaded;
					new_source->included_count++;
				} else {
					if (delimiter == '\"') {
						for (u32 i = 0; i < params.user_include_dirs.len && new_source == NULL; i++) {
							new_source = readAllAlloc(params.user_include_dirs.ptr[i], includefilename);
						}
					}
					for (u32 i = 0; i < params.sys_include_dirs.len && new_source == NULL; i++) {
						new_source = readAllAlloc(params.sys_include_dirs.ptr[i], includefilename);
						if (new_source)
							new_source->kind = Source_StandardHeader;
					}
					if (new_source == NULL)
						lexerror(source, loc, "could not open include file \"%.*s\"", STRING_PRINTAGE(includefilename));
					new_source->idx = t.files.len;
					PUSH(t.files, new_source);
					*already_loaded = new_source;
				}
				pos++;
				if (includes_stack.len >= MAX_INCLUDES)
					lexerror(source, begin, "exceeded maximum #include depth of %d", MAX_INCLUDES);
				PUSH(includes_stack, ((Inclusion) {loc, pos}));

				source = *new_source;
				pos = source.content.ptr;
				loc = (Location) {source.idx, 1, 1};
				first_line_in_file = true;
				continue;
			}
			case Directive_Pragma: {
				String pragma = restOfLine(source, &loc, &pos);
// 				pos--;

				if (eql("once", pragma)) {
					if (!first_line_in_file)
						lexerror(source, begin, "#pragma once must appear at the beginning of the file");
					if (source.included_count) {
						Inclusion inc = POP(includes_stack);
						pos = inc.pos;
						source = *t.files.ptr[inc.loc.file_id];
						loc = inc.loc;
					}
				} else {
					lexwarning(source, begin, "ignoring unknown #pragma directive");
				}
			} break;
			case Directive_If: {
				expansion.source = source;
				expansion.expansion_start = begin;

				skipToValidBranchOrEnd(expansion, If_If, &if_depth, &pos, &prepreoc_evaluation_buf);
			} break;
			case Directive_Ifdef:
			case Directive_Ifndef: {
				expansion.source = source;
				expansion.expansion_start = begin;

				IfClass class = dir == Directive_Ifdef ? If_IfDef : If_IfnDef;
				skipToValidBranchOrEnd(expansion, class, &if_depth, &pos, &prepreoc_evaluation_buf);
			} break;
			case Directive_Else:
			case Directive_Elifdef:
			case Directive_Elifndef:
			case Directive_Elif: {
				if (if_depth == 0)
					lexerror(source, begin, "unmatched #else");
				if_depth--;
				skipToEndIf(source, &loc, &pos);
			} break;
			case Directive_Endif: {
				if (if_depth == 0)
					lexerror(source, begin, "unmatched #endif");
				if_depth--;
			} break;
			case Directive_Error: {
				Location start = loc;
				String str = restOfLine(source, &loc, &pos);
				lexerror(source, start, "%.*s", STRING_PRINTAGE(str));

			} break;
			case Directive_Warn: {
				Location start = loc;
				String str = restOfLine(source, &loc, &pos);
				lexwarning(source, start, "%.*s", STRING_PRINTAGE(str));

			} break;
			default:
				lexerror(source, begin, "unknown preprocessor directive");
			}
			first_line_in_file = false;
			continue;
		}
		first_line_in_file = false;

		// TODO Mark __FILE__, __LINE__ as macros
		if (tok.kind == Tok_Identifier) {
			Macro *macro = t.symbols.ptr[tok.val.symbol_idx].macro;

			// STYLE Copypasta from expandInto, because macro_file_ref
			// should not be set on unexpanded function-like macros.
			if (macro) {
				TokenList *arguments = NULL;
				expansion.source = source;
				expansion.expansion_start = begin;

				if (macro->is_function_like) {
					Token paren = getTokenSpaced(generated_strings, source, &loc, &t.symbols, &pos);

					if (paren.kind != Tok_OpenParen) {
						appendOneToken(&t.list, tok, (TokenLocation) {loc});
						appendOneToken(&t.list, paren, (TokenLocation) {loc});
						continue;
					}
					arguments = takeArguments(expansion, begin, macro); // Freed by expand_into.
				}
				macro->being_replaced = true;
				PUSH(macro_expansion_stack, ((Replacement) {
					macro,
					.loc = begin,
					.toks = arguments
				}));
				expandInto(expansion, &t.list, false);

				assert(macro_expansion_stack.len == 0);
				continue;
			}

			int keyword = t.symbols.ptr[tok.val.symbol_idx].keyword;
			if (keyword) {
				if (keyword == Tok_Key_File) {
					tok.kind = Tok_String;
					tok.val.symbol_idx = getSymbolId(&t.symbols, source.name);
				} else if (keyword == Tok_Key_Line) {
					tok.kind = Tok_Integer;
					tok.literal_type = Int_int;
					tok.val.integer_s = begin.line;
				} else {
					tok.kind = keyword;
				}
			}
		}
		appendOneToken(&t.list, tok, (TokenLocation) {begin});

		if (tok.kind == Tok_EOF) {
			assert(source.idx == initial_source->idx);
			break;
		}
	}

	resolveSymbolIndicesToPointers(t.list, t.symbols.ptr);

	for (u32 i = 0; i < ARRSIZE(t.special_identifiers); i++)
		t.special_identifiers[i] = &t.symbols.ptr[special_identifiers_idx[i]];

	free(prepreoc_evaluation_buf.tokens);
	free(prepreoc_evaluation_buf.positions);
	free(includes_stack.ptr);
	free(macro_expansion_stack.ptr);
	for (u32 i = 0; i < t.symbols.len; i++) {
		if (t.symbols.ptr[i].macro)
			free(t.symbols.ptr[i].macro->tokens.ptr);
	}
	free(map.entries);
	free_arena(&macro_arena, "macros");
	mapFree(&sources);
	return t;
}

static MacroToken takeToken(ExpansionParams ex, u32 *marker);
static void collapseMacroStack(MacroStack *stack, u32 *marker);

static void expansionError (const ExpansionParams ex, Location loc, const char *msg) {
	SourceFile **sources = ex.files->ptr;
	printErr(*sources[loc.file_id], loc);
	fprintf(stderr, "%s\n", msg);
	if (ex.stack->len) {
		i32 i = ex.stack->len - 1;
		Replacement repl = ex.stack->ptr[i];

		if (repl.mac) {
			Location loc = repl.mac->tokens.ptr[repl.pos-1].loc;
			printInfo(*sources[loc.file_id], loc);
			fprintf(stderr, "from this definition...\n");
		} else {
			Location loc = repl.toks->positions[repl.pos-1].source;
			printInfo(*sources[loc.file_id], loc);
			fprintf(stderr, "from this argument...\n");
		}


		if (ex.stack->ptr[i].mac == NULL) {
			i--;
		}

		for (; i >= 0; i--) {
			Replacement repl = ex.stack->ptr[i];
			Location loc = repl.loc;

			printInfo(*sources[loc.file_id], loc);
			fprintf(stderr, "in expansion of macro %.*s\n", STRING_PRINTAGE(repl.mac->name));
		}
	}
	exit(1);
}

// If is_argument is specified, halt after a comma (returning true) or
// closing parenthesis (returning false)
static bool expandInto (const ExpansionParams ex, TokenList *dest, bool is_argument) {
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

		if (t.tok.kind == Tok_Identifier) {
			Macro *mac;
			if ((mac = ex.tok->symbols.ptr[t.tok.val.symbol_idx].macro) != NULL
				&& !mac->being_replaced)
			{
				TokenList *arguments = NULL;
				if (mac->is_function_like) {
					collapseMacroStack(ex.stack, &level_of_argument_source);
					MacroToken paren = takeToken(ex, &level_of_argument_source);
					if (paren.tok.kind != Tok_OpenParen) {
						appendOneToken(dest, t.tok, (TokenLocation) {
							.source = t.loc, .macro = ex.expansion_start,
						});
						t = paren;
						goto no_macro;
					}
					arguments = takeArguments(ex, t.loc, mac);
				}
				mac->being_replaced = true;
				PUSH(*ex.stack, ((Replacement) {
					mac,
					.loc = t.loc,
					.toks = arguments,
				}));
				collapseMacroStack(ex.stack, &level_of_argument_source);
				continue;
			}

			int keyword = ex.tok->symbols.ptr[t.tok.val.symbol_idx].keyword;
			if (keyword)
				t.tok.kind = keyword;
		} else {
			no_macro:
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
					// TODO Use location of the beginning of the invocation.
					expansionError(ex, t.loc,
							"missing closing parenthesis of macro invocation");
				default:
					break;
				}
			}
		}

		collapseMacroStack(ex.stack, &level_of_argument_source);
		appendOneToken(dest, t.tok, (TokenLocation) {
			.source = t.loc,
			.macro = ex.expansion_start,
		});

	}

	return false;
}

// Allocates the argument list.
// PERFORMANCE A single Tokenization per argument list should be
// enough if Replacements store offset and length.
static TokenList *takeArguments (const ExpansionParams ex, Location invocation_loc, Macro *mac) {
	assert(mac->is_function_like);
	if (mac->parameters.len == 0) {
		u32 dummy_mark = 0;
		collapseMacroStack(ex.stack, &dummy_mark);
		MacroToken t = takeToken(ex, &dummy_mark);
		if (t.tok.kind != Tok_CloseParen) {
			expansionError(ex, invocation_loc,
					"too many arguments provided");
		}

		return NULL;
	}
	TokenList *argument_bufs = calloc(mac->parameters.len, sizeof(TokenList));
	u32 param_idx = 0;

	u32 expected_count = mac->parameters.len;
	while (true) {
		bool arg_list_closed = expandInto(ex, &argument_bufs[param_idx], true);
		if (arg_list_closed) {
			if (param_idx + 1 == expected_count || (mac->is_vararg && param_idx + 2 >= expected_count)) {
				break;
			} else {
				expansionError(ex, invocation_loc,
						"too few arguments provided"); // TODO Better location
			}
		} else {
			param_idx++;
			if (param_idx == expected_count) {
				if (mac->is_vararg) {
					param_idx--;

					// TODO Fill out preceded_by_space etc.
					appendOneToken(&argument_bufs[param_idx], (Token) {.kind = Tok_Comma, }, (TokenLocation) {0});
				} else {
					expansionError(ex, invocation_loc,
							"too many arguments provided"); // TODO Better location
				}
			}
		}
	}
	return argument_bufs;
}

// STYLE The following code has very dense logic and probably needs more
// comments.

static MacroToken toMacroToken(TokenList *tok, u32 pos);
static MacroToken concatenate(const ExpansionParams ex, Token first, u32 *marker);
static Token stringify(Arena *arena, SymbolList *symbols, TokenList t);

static MacroToken takeToken (const ExpansionParams ex, u32 *marker) {
	MacroStack *stack = ex.stack;
	while (stack->len) {
		Replacement *repl = &stack->ptr[stack->len - 1];
		Macro *macro = repl->mac;
		if (macro) {
			assert(repl->pos < macro->tokens.len);

			MacroToken t = macro->tokens.ptr[repl->pos];
			repl->pos++;

			// PERFORMANCE Remove this lookahead (mark it in defineMacro).
			bool followed_by_concat = repl->pos < macro->tokens.len &&
					macro->tokens.ptr[repl->pos].tok.kind == Tok_PreprocConcatenate;

			if (t.parameter) {
				if (t.tok.kind == Tok_PreprocDirective) {
					// TODO This should be an expansionError except when calling from preprocExpression.
					if (followed_by_concat)
						lexerror(ex.source, *ex.loc, "(TODO location, phrasing) stringified parameter can not followed by concatenation");
					TokenList arg = repl->toks[t.parameter-1];
					t.tok = stringify(ex.strings_arena, &ex.tok->symbols, arg);
					return t;
				}
				Replacement arg = {
					.toks = &repl->toks[t.parameter-1],
					.followed_by_concat = followed_by_concat,
				};
				if (arg.toks->count) {
					if (*marker == stack->len)
						(*marker)++;
					PUSH(*stack, arg);
				} else {
					if (followed_by_concat) {
						repl->pos++;
						t = macro->tokens.ptr[repl->pos];
						repl->pos++;
						if (!t.parameter || repl->toks[t.parameter-1].count)
							return t;
					}
					// Argument was empty, retry.
				}
			} else {
				if (followed_by_concat)
					return concatenate(ex, t.tok, marker);
				else {
					// PERFORMANCE Are multiple branches in the hot path really necessary?
					// STYLE Copypasta from the lex loop

					if (t.tok.kind == Tok_Identifier) {
						// TODO Isn't this redundant with the keywordification in takeArguments?
						int keyword = ex.tok->symbols.ptr[t.tok.val.symbol_idx].keyword;
						if (keyword) {
							if (keyword == Tok_Key_File) {
								t.tok.kind = Tok_String;
								t.tok.val.symbol_idx = getSymbolId(&ex.tok->symbols, ex.source.name);
							} else if (keyword == Tok_Key_Line) {
								t.tok.kind = Tok_Integer;
								t.tok.literal_type = Int_int;
								t.tok.val.integer_s = ex.expansion_start.line;
							} else {
								t.tok.kind = keyword;
							}
						}
					}
					return t;
				}
			}
		} else {
			// TODO Need to return not just a MacroToken, but a complete
			// TokenLocation consisting of the argument location and the
			// parameter location.
			MacroToken t = toMacroToken(repl->toks, repl->pos);
			repl->pos++;
			if (repl->pos < repl->toks->count)
				return t;

			if (repl->followed_by_concat) {
				stack->len--;
				return concatenate(ex, t.tok, marker);
			} else
				return t;
		}
		collapseMacroStack(stack, marker);
	}

	bool have_space = tryGobbleSpace(ex.source, ex.loc, ex.src);
	Location token_loc = *ex.loc;
	Token t = getToken(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, ex.src);
	t.preceded_by_space = have_space;
	return (MacroToken) {
		.tok = t,
		.loc = token_loc,
	};
}

void strPrintToken(char **dest, const char *end, Symbol *symbols, Token t);
u32 strPrintTokenLen(Token t, Symbol *symbols);

static MacroToken concatenate (const ExpansionParams ex, Token first, u32 *marker) {
	Replacement *repl = &ex.stack->ptr[ex.stack->len - 1];
	Macro *macro = repl->mac;
	MacroToken operator = macro->tokens.ptr[repl->pos];
	repl->pos++;
	MacroToken second = macro->tokens.ptr[repl->pos];
	repl->pos++;

	// TODO Allow a chain of concatenations.
	if (second.parameter) {
		Replacement arg = {
			.toks = &repl->toks[second.parameter-1],
			.pos = 1,
		};
		if (arg.toks->count == 0) {
			operator.tok = first;
			return operator;
		}

		// Concat to the first token from the argument,
		// push the rest of the argument onto the
		// replacement stack.
		second = toMacroToken(arg.toks, 0);

		if (arg.toks->count > 1) {
			if (*marker == ex.stack->len)
				(*marker)++;
			PUSH(*ex.stack, arg);
		}
	}
	collapseMacroStack(ex.stack, marker);

	u32 len = strPrintTokenLen(first, ex.tok->symbols.ptr) + strPrintTokenLen(second.tok, ex.tok->symbols.ptr) + 1;
	char *rep = aalloc(ex.strings_arena, len);
	char *insert = rep;
	strPrintToken(&insert, rep+len, ex.tok->symbols.ptr, first);
	strPrintToken(&insert, rep+len, ex.tok->symbols.ptr, second.tok);
	int *c = NULL;
	if (insert > rep + len)
		*c = 1;
	*insert = 0;

	const char *src = rep;
	operator.tok = getToken(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, &src);
	return operator;
}

static Token stringify (Arena *arena, SymbolList *symbols, TokenList arg) {
	u32 data_len = 0; // To account for the trailing \0 emitted by snprintf
	for (u32 i = 0; i < arg.count; i++)
		data_len += strPrintTokenLen(arg.tokens[i], symbols->ptr) + arg.tokens[i].preceded_by_space;

	char *start = aalloc(arena, data_len + 1);
	char *c = start;
	char *end = start + data_len + 1;
	for (u32 i = 0; i < arg.count; i++) {
		if (arg.tokens[i].preceded_by_space)
			printto(&c, end, " ");
		strPrintToken(&c, end, symbols->ptr, arg.tokens[i]);
	}

	u32 idx = getSymbolId(symbols, (String) {data_len, start});
	return (Token) {Tok_String, .val.symbol_idx = idx};
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
			if (*marker >= stack->len)
				(*marker)--;
			stack->len--;
		} else {
			if (repl->pos < repl->toks->count)
				return;

			if (*marker >= stack->len)
				(*marker)--;
			stack->len--;
		}
	}
}

static MacroToken toMacroToken (TokenList *tok, u32 pos) {
	return (MacroToken) {
		.tok = tok->tokens[pos],
		.loc = tok->positions[pos].macro,
	};
}


static void ensureCapacity (TokenList *t, u32 required) {
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

static void appendOneToken (TokenList *t, Token tok, TokenLocation pos) {
	ensureCapacity(t, 1);
	t->tokens[t->count] = tok;
	t->positions[t->count] = pos;
	t->count++;
}

static String restOfLine (SourceFile source, Location *loc, const char **pos) {
	if (tryGobbleSpace(source, loc, pos) == Space_Linebreak)
		lexerror(source, *loc, "expected a value befor the end of line");
	const char *start = *pos;
	while (**pos && **pos != '\n')
		(*pos)++;
	return (String) {*pos - start, start};
}

static inline void newLine(Location *loc) { loc->line++; loc->column = 1; }

// PERFORMANCE This function is potentially a very hot path. Optimize!
static SpaceClass tryGobbleSpace (SourceFile source, Location *loc, const char **p) {
	const char *pos = *p;
	SpaceClass spacing = Space_Regular;
	const char *line_begin = pos;
	while (*pos) {
		// TODO Do I really want to handle the ??/ trigraph everywhere
		// that newlines need to be skipped? Consider the performance
		// impact versus replacing all trigraphs on file input.
		if (pos[0] == '\\' && pos[1] == '\n') {
			pos += 2;
			newLine(loc);
			line_begin = pos;
		} else if (pos[0] == '/' && pos[1] == '/') {
			// TODO Check for C99
			while (pos[0] && pos[0] != '\n') pos++;
		} else if (pos[0] == '/' && pos[1] == '*') {
			Location begin = *loc;
			while (pos[0] && !(pos[0] == '*' && pos[1] == '/')) {
				if (*pos == '\n') {
					newLine(loc);
					line_begin = pos + 1;
				}
				pos++;
			}
			if (!pos[0])
				lexerror(source, begin, "comment must end before end of file");
			pos += 2;
		} else {
			if (!isSpace(pos[0]))
				break;
			if (pos[0] == '\n') {
				spacing = Space_Linebreak;
				newLine(loc);
				line_begin = pos + 1;
			}
			pos++;
		}
	}
	if (pos == *p)
		return Space_None;
	loc->column += pos - line_begin;
	*p = pos;
	return spacing;
}

// PERFORMANCE This function is potentially a very hot path. Optimize!
static void skipToEndIf (SourceFile source, Location *loc, const char **p) {
	while (skipToElseIfOrEnd(source, loc, p) != If_End)
		;
// 	const char *pos = *p;

// 	bool at_start_of_line = false;
// 	const char *line_begin = pos;
// 	u32 nesting = 1;
// 	while (nesting > 0) {
// 		if (*pos == '\n') {
// 			at_start_of_line = true;
// 			newLine(loc);
// 			line_begin = pos + 1;
// 		} else if (*pos == '#' && at_start_of_line) {
// 			pos++;
// 			while (isSpace(*pos)) {
// 				if (*pos == '\n') {
// 					newLine(loc);
// 					line_begin = pos + 1;
// 				}
// 				pos++;
// 			}
// 			const char *begin = pos;
// 			while (*pos && !isSpace(*pos)) pos++;
// 			String directive = {pos - begin, begin};
// 			if (eql("if", directive) || eql("ifdef", directive)) {
// 				nesting++;
// 			} else if (eql("endif", directive)) {
// 				nesting--;
// 			}
// 			pos--;
// 		} else if (!isSpace(*pos)) {
// 			if (pos[0] == '\\' && pos[1] == '\n')
// 				pos++; // TODO
// 			else
// 				at_start_of_line = false;
// 		}
// 		pos++;
// 		if (*pos == 0)
// 			lexerror(source, *p, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
// 	}
// 	*p = pos;
// 	loc->column += pos - line_begin;
}

static void skipToValidBranchOrEnd (
	ExpansionParams ex,
	IfClass class,
	u32 *depth,
	const char **p,
	TokenList *eval_buf)
{
	(*depth)++;
	while (true) {
		switch (class) {
		case If_If:
			if (preprocExpression(ex, eval_buf, ex.tok))
				return;
			break;
		case If_IfDef: {
			Token tok = getTokenSpaced(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, p);
			if (tok.kind != Tok_Identifier)
				lexerror(ex.source, *ex.loc, "#ifdef must be followed by an identifier");

			gobbleSpaceToNewline(ex.src, ex.loc);
			if (**ex.src != '\n')
				lexerror(ex.source, *ex.loc, "#ifdef may not be followed by more than one identifier");

			if (ex.tok->symbols.ptr[tok.val.symbol_idx].macro)
				return;
		} break;
		case If_IfnDef: {
			Token tok = getTokenSpaced(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, p);
			if (tok.kind != Tok_Identifier)
				lexerror(ex.source, *ex.loc, "#ifndef must be followed by an identifier");
			gobbleSpaceToNewline(ex.src, ex.loc);
			if (**ex.src != '\n')
				lexerror(ex.source, *ex.loc, "#ifndef may not be followed by more than one identifier");

			if (ex.tok->symbols.ptr[tok.val.symbol_idx].macro == NULL)
				return;
		} break;
		case If_Else:
			return;
		case If_End:
			(*depth)--;
			return;
		}
		class = skipToElseIfOrEnd(ex.source, ex.loc, p);
	}
}

static IfClass skipToElseIfOrEnd (SourceFile source, Location *loc, const char **p) {
	const char *pos = *p;

	bool at_start_of_line = false;
	const char *line_begin = pos;
	IfClass result;
	while (true) {
		if (*pos == '\n') {
			at_start_of_line = true;
			newLine(loc);
			line_begin = pos + 1;
		} else if (*pos == '#' && at_start_of_line) {
			pos++;
			while (isSpace(*pos) && *pos != '\n') pos++;

			const char *begin = pos;
			while (*pos && !isSpace(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive) || eql("ifndef", directive)) {
				while (*pos && pos[-1] != '\n') pos++;
				loc->line++;
				skipToEndIf(source, loc, &pos);
				line_begin = pos;
			} else if (eql("elif", directive)) {
				result = If_If;
				break;
			} else if (eql("elifdef", directive)) {
				result = If_IfDef;
				break;
			} else if (eql("elifndef", directive)) {
				result = If_IfnDef;
				break;
			} else if (eql("else", directive)) {
				// TODO Check that the line ends after the directive.
				result = If_Else;
				break;
			} else if (eql("endif", directive)) {
				// TODO Check that the line ends after the directive.
				result = If_End;
				break;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n') {
				pos++;
				newLine(loc);
				line_begin = pos + 1;
			} else
				at_start_of_line = false;
		}
		if (*pos)
			pos++;
		if (*pos == 0)
			lexerror(source, *loc, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
	}
	*p = pos;
	loc->column += pos - line_begin;
	return result;
}


static Token getTokenSpaced (Arena *str_arena, SourceFile source, Location *loc, SymbolList *symbols, const char **p) {
	gobbleSpaceToNewline(p, loc);
	if (**p == '\n' || **p == 0)
		lexerror(source, *loc, "expected a token before the end of the line");
	return getToken(str_arena, source, loc, symbols, p);
}

static bool gobbleSpaceToNewline (const char **p, Location *loc) {
	bool got = false;
	const char *pos = *p;
	const char *line_begin = pos;
	while (*pos && *pos != '\n') {
		if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n') {
				pos++;
				newLine(loc);
				line_begin = pos;
			} else
				break;
		}
		got = true;
		pos++;
	}
	loc->column += pos - line_begin;
	*p = pos;
	return got;
}


static bool preprocExpression (ExpansionParams params, TokenList *buf, Tokenization *main_tokenization) {
	const char *pos = *params.src;
	Arena *const str_arena = params.strings_arena;
	buf->count = 0;

	while (true) {
		gobbleSpaceToNewline(&pos, params.loc);
		if (*pos == '\n' || *pos == 0)
			break;
		Location begin = *params.loc;
		Token tok = getToken(str_arena, params.source, params.loc, &params.tok->symbols, &pos);
		if (tok.kind == Tok_Identifier) {
			Symbol *sym = &params.tok->symbols.ptr[tok.val.symbol_idx];
			if (eql("defined", sym->name)) {
				tok = getTokenSpaced(str_arena, params.source, params.loc, &params.tok->symbols, &pos);
				bool parenthesized = tok.kind == Tok_OpenParen;
				if (parenthesized)
					tok = getTokenSpaced(str_arena, params.source, params.loc, &params.tok->symbols, &pos);

				if (tok.kind != Tok_Identifier)
					lexerror(params.source, *params.loc, "the operator %sdefined%s expects an identifier as an argument", BOLD, RESET);

				bool found = params.tok->symbols.ptr[tok.val.symbol_idx].macro;
				tok = (Token) {Tok_Integer, .literal_type = Int_int, .val.integer_s = found};

				if (parenthesized) {
					gobbleSpaceToNewline(&pos, params.loc);
					if (*pos == '\n' || *pos == 0 || getToken(str_arena, params.source, params.loc, &params.tok->symbols, &pos).kind != Tok_CloseParen)
						lexerror(params.source, *params.loc, "missing closing parenthesis");
				}
			} else if (sym->macro) {
				if (sym->macro->parameters.len > 0)
					lexerror(params.source, *params.loc, "TODO Expand function-like macros in preprocessor constant expressions");
				PUSH(*params.stack, ((Replacement) {sym->macro}));
				expandInto(params, buf, false);
				continue;
			} else {
				tok.kind = Tok_Integer;
				tok.literal_type = Int_int;
				tok.val.integer_s = 0;
			}
		}
		appendOneToken(buf, tok, (TokenLocation) {begin});
	}


	*params.src = pos;

	appendOneToken(buf, (Token) {Tok_EOF}, (TokenLocation) {*params.loc});
	resolveSymbolIndicesToPointers(*buf, params.tok->symbols.ptr);
	Tokenization tok = *main_tokenization;
	tok.list = *buf;

	// TODO Is the strings_arena appropriate here?
	return evalPreprocExpression(tok, params.strings_arena, params.opt);
}


// === Helpers ===


// TODO Come up with a better system for nice highlighting.
const char *token_names[] = {
	[Tok_Identifier] = "identifier",
	[Tok_Integer] = "int-literal",
	[Tok_Real] = "floating-point-literal",
	[Tok_String] = "string-literal",
	[Tok_PreprocDirective] = "preprocessor-directive",

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

	[Tok_Arrow] = "->",
	[Tok_Question] = "?",
	[Tok_Bang] = "!",
	[Tok_Bang | Tok_EQUALED] = "!=",
	[Tok_Equals] = "=",
	[Tok_Equals | Tok_EQUALED] = "==",
	[Tok_Plus] = "+",
	[Tok_DoublePlus] = "++",
	[Tok_Plus | Tok_EQUALED] = "+=",
	[Tok_Minus] = "-",
	[Tok_DoubleMinus] = "--",
	[Tok_Minus | Tok_EQUALED] = "-=",
	[Tok_Asterisk] = "*",
	[Tok_Asterisk | Tok_EQUALED] = "*=",
	[Tok_Slash] = "/",
	[Tok_Slash | Tok_EQUALED] = "/=",
	[Tok_Percent] = "%",
	[Tok_Percent | Tok_EQUALED] = "%=",
	[Tok_Less] = "<",
	[Tok_Less | Tok_EQUALED] = "<=",
	[Tok_DoubleLess] = "<<",
	[Tok_DoubleLess | Tok_EQUALED] = "<<=",
	[Tok_Greater] = ">",
	[Tok_Greater | Tok_EQUALED] = ">=",
	[Tok_DoubleGreater] = ">>",
	[Tok_DoubleGreater | Tok_EQUALED] = ">>=",
	[Tok_Ampersand] = "&",
	[Tok_DoubleAmpersand] = "&&",
	[Tok_Ampersand | Tok_EQUALED] = "&=",
	[Tok_Pipe] = "|",
	[Tok_DoublePipe] = "||",
	[Tok_Pipe | Tok_EQUALED] = "|=",
	[Tok_Hat] = "^",
	[Tok_Hat | Tok_EQUALED] = "^=",
	[Tok_Tilde] = "~",
	[Tok_Tilde | Tok_EQUALED] = "~=",

	[Tok_EOF] = "end of file",
};

static char name[256] = {0};
static const char *const name_end = name + 256;

const char *tokenNameHighlighted (TokenKind kind) {
	switch (kind) {
	case Tok_Identifier: return "identifier";
	case Tok_Integer: return "integer-literal";
	case Tok_Real: return "floating-point-literal";
	case Tok_String: return "string-literal";
	case Tok_PreprocDirective: return "preprocessor-directive";
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
			for (u32 i = 0; i < ARRSIZE(standard_keywords); i++) {
				if (standard_keywords[i].key == kind)
					return standard_keywords[i].name;
			}
		}
		return NULL;
	}
}


u32 strPrintTokenLen (Token t, Symbol *syms) {
	switch (t.kind) {
	case Tok_Identifier: return syms[t.val.symbol_idx].name.len;
	case Tok_PreprocDirective: return syms[t.val.symbol_idx].name.len + 1;
	case Tok_String: return syms[t.val.symbol_idx].name.len * 2 + 2;
	case Tok_Integer: return 30;
	default:
		assert(tokenName(t.kind));
		return strlen(tokenName(t.kind));
	}
}

void strPrintToken (char **dest, const char *end, Symbol *symbols, Token t) {
	switch (t.kind) {
	case Tok_PreprocDirective:
		printto(dest, end, "#");
		FALLTHROUGH;
	case Tok_Identifier:
		printto(dest, end, "%.*s", STRING_PRINTAGE(symbols[t.val.symbol_idx].name));
		return;
	case Tok_String:
		printto(dest, end, "\"%.*s\"", STRING_PRINTAGE(symbols[t.val.symbol_idx].name));
		return;
	case Tok_Integer:
		printto(dest, end, "%lld", t.val.integer_s);
		return;
	default:
		printto(dest, end, "%s", tokenName(t.kind));
	}
}

static inline bool isSpace (char c) {
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

static inline bool isAlpha (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static inline bool isHexDigit (char c) {
	if (c >= '0' && c <= '9')
		return true;
	c |= 32;
	return c >= 'a' && c <= 'f';
}

static inline int hexToInt (char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	c |= 32;
	assert(c >= 'a' && c <= 'f');
	return c - 'a';
}

static inline bool isDigit (char c) {
	return c >= '0' && c <= '9';
}

static inline bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}


static const char *defineMacro (
	Arena *arena,
	Arena *generated_strings,
	SymbolList *symbols,
	String name,
	SourceFile *source,
	Location *loc,
	const char *pos)
{
	static StringMap parameters = {0};

	Macro *mac = ALLOC(arena, Macro);
	*mac = (Macro) {name, source};
	Symbol *sym = getSymbol(symbols, name);
	if (sym->macro)
		free(sym->macro->tokens.ptr);
	sym->macro = mac;

	u32 param_count = 0;
	if (pos[0] == '(') {
		pos++;
		loc->column++;
		const char *p = pos;
		Location dummy = *loc;
		Token t = getTokenSpaced(generated_strings, *source, &dummy, symbols, &p);
		if (t.kind == Tok_CloseParen) {
			pos = p;
		} else {
			while (true) {
				if (t.kind != Tok_Identifier && t.kind != Tok_TripleDot)
					lexerror(*source, dummy, "the parameters of function-like macros must be valid identifiers");
				param_count++;
				t = getTokenSpaced(generated_strings, *source, &dummy, symbols, &p);
				if (t.kind == Tok_CloseParen)
					break;
				else if (t.kind == Tok_EOF)
					lexerror(*source, dummy, "incomplete parameter list");
				else if (t.kind != Tok_Comma)
					lexerror(*source, dummy, "the parameters of function-like macros must be valid identifiers");
				t = getTokenSpaced(generated_strings, *source, &dummy, symbols, &p);
			}
		}
		mac->parameters.ptr = aalloc(arena, sizeof(Parameter) * param_count);
		mac->parameters.len = param_count;
		mac->is_function_like = true;

		for (u32 i = 0; i < param_count; i++) {
			// STYLE Copypasta
			gobbleSpaceToNewline(&pos, loc);
			if (*pos == '\n' || *pos == 0)
				lexerror(*source, *loc, "expected a token before the end of the line");
			Location begin = *loc;
			Token t = getToken(generated_strings, *source, loc, symbols, &pos);
			assert(t.kind == Tok_Identifier || t.kind == Tok_TripleDot);

			String name = symbols->ptr[t.val.symbol_idx].name;
			if (t.kind == Tok_TripleDot) {
				if (i + 1 != param_count)
					lexerror(*source, begin, "an ellipsis may only appear as the last parameter to a variadic macro");
				// TODO Version check for C99
				name = zstr("__VA_ARGS__");
				mac->is_vararg = true;
			}

			mac->parameters.ptr[i].name = name;
			void **entry = mapGetOrCreate(&parameters, name);
			if (*entry)
				lexerror(*source, begin, "macro parameters may not be duplicated");
			*entry = &mac->parameters.ptr[i];

			t = getTokenSpaced(generated_strings, *source, loc, symbols, &pos);
		}
	}

	LIST(MacroToken) macro_assembly = {0};
	while (true) {
		bool havespace = gobbleSpaceToNewline(&pos, loc);
		if (*pos == '\n')
			break;

		Location begin = *loc;
		MacroToken t = {
			.tok = getToken(generated_strings, *source, loc, symbols, &pos),
			.loc = begin,
		};
		t.tok.preceded_by_space = havespace;
		if (t.tok.kind == Tok_EOF)
			break;

		if (param_count && (t.tok.kind == Tok_Identifier || t.tok.kind == Tok_PreprocDirective)) {
			String param_name = symbols->ptr[t.tok.val.symbol_idx].name;
			Parameter *param = mapGet(&parameters, param_name);
			if (param)
				t.parameter = param - mac->parameters.ptr + 1;
		}
		PUSH(macro_assembly, t);
	}
	if (macro_assembly.len) {
		MacroToken first = macro_assembly.ptr[0];
		if (first.tok.kind == Tok_PreprocConcatenate)
			lexerror(*source, first.loc, "a %s##%s preprocessing token may not occur at the beginning of a macro definition");
		MacroToken last = macro_assembly.ptr[macro_assembly.len-1];
		if (last.tok.kind == Tok_PreprocConcatenate)
			lexerror(*source, last.loc, "a %s##%s preprocessing token may not occur at the end of a macro definition");
	}
	mac->tokens.ptr = macro_assembly.ptr;
	mac->tokens.len = macro_assembly.len;

	if (param_count > 0)
		mapFree(&parameters);
	return pos;
}

u32 version_vals[] = {
	[Version_C99] = 199901L,
	[Version_C17] = 201710L,
	[Version_C23] = 202301L,
	[Version_GNU] = 201710L,
	[Version_MSVC] = 201710L,
};

static void predefineMacros (
	Arena *arena,
	Arena *genrated_strings,
	SymbolList *symbols,
	FileList *files,
	StringList to_define,
	SourceKind kind)
{
#ifdef NDEBUG
	SourceFile *sources = calloc(to_define.len, sizeof(SourceFile));
#endif

	for (u32 i = 0; i < to_define.len; i++) {
#ifdef NDEBUG
		SourceFile *source = &sources[i];
#else
		SourceFile *source = calloc(sizeof(SourceFile), 1);
#endif
		source->kind = kind;
		source->idx = files->len;
		PUSH(*files, source);

		String def = to_define.ptr[i];

		u32 equals = 0;
		while (equals < def.len && def.ptr[equals] != '=') equals++;
		// TODO Without default "1", the name and content parsing would not need to be sparated.
		source->name = def;
		source->content = zstr("1");
		if (equals < def.len) {
			source->name.len = equals;
			equals++;
			source->content = (String) {def.len - equals, def.ptr + equals};
		}

		Location loc = {source->idx, 1, 1};
		defineMacro(arena, genrated_strings, symbols, source->name, source, &loc, source->content.ptr);
	}
}


String processStringLiteral(Arena *arena, String src) {
	char *res = aalloc(arena, src.len + 1);
	u32 len = 0;
	for (u32 i = 0; i < src.len; i++) {
		if (src.ptr[i] == '\\') {
			i++;
			if (src.ptr[i] == '\n') continue;
			res[len] = escape_codes[(uchar) src.ptr[i]];
		} else {
			res[len] = src.ptr[i];
		}
		len++;
	}
	return (String) {len, res};
}



static bool tokenHasSymbol (TokenKind k) {
	switch (k) {
	case Tok_Identifier:
	case Tok_String:
	case Tok_Intrinsic:
	case Tok_PreprocDirective:
		return true;
	default:
		return false;
	}
}

static void resolveSymbolIndicesToPointers (TokenList t, Symbol *syms) {
	// PERFORMANCE Should that happen here or in the parse?
	for (u32 i = 0; i < t.count; i++) {
		if (tokenHasSymbol(t.tokens[i].kind))
			t.tokens[i].val.symbol = &syms[t.tokens[i].val.symbol_idx];
	}
}




// Much copypasta from StringMap. Oh well.

#define MAX_LOAD_PERCENTAGE 70
static void insertSymbol(SymbolMap *, IndexEntry);
static void growSymbols(void);

static Symbol *getSymbol (SymbolList *list, String name) {
	u32 idx = getSymbolId(list, name);
	return &list->ptr[idx];
}

static u32 getSymbolId (SymbolList *list, String name) {
	u64 hash = strHash(name);

	if ((u32) hash == 0) hash = 1073741824; // 2^30, lel
	u32 searched = hash;
	u32 i = searched & (map.capacity - 1);

	IndexEntry ie = {0};
	while (i < map.capacity) {
		ie = map.entries[i];
		if (!ie.hash)
			break;
		if (ie.hash == searched && SPAN_EQL(list->ptr[ie.idx].name, name))
			break;
		i++;
	}

	if (i == map.capacity) {
		i = 0;
		while (true) {
			ie = map.entries[i];
			if (!ie.hash)
				break;
			if (ie.hash == searched && SPAN_EQL(list->ptr[ie.idx].name, name))
				break;
			i++;
		}
	}
	if (ie.hash) {
		return ie.idx;
	}


	IndexEntry new = {searched, list->len};
	PUSH(*list, ((Symbol) {name}));

	if (new.idx * 100 >= map.capacity * MAX_LOAD_PERCENTAGE) {
		growSymbols();
		insertSymbol(&map, new);
	} else {
		map.entries[i] = new;
	}
	return new.idx;
}

static void insertSymbol (SymbolMap *map, IndexEntry entry) {
	u32 i = entry.hash & (map->capacity - 1);

	while (i < map->capacity && map->entries[i].hash)
		i++;
	if (i == map->capacity) {
		i = 0;
		while (map->entries[i].hash)
			i++;
	}

	map->entries[i] = entry;
}

static void growSymbols (void) {
	u32 new_capacity = map.capacity == 0 ?
		8 :
		map.capacity * 2;

	SymbolMap new_map = (SymbolMap) {
		.entries = calloc(new_capacity, sizeof(IndexEntry)),
		.capacity = new_capacity,
	};

	for (u32 i = 0; i < map.capacity; i++) {
		if (!map.entries[i].hash)
			continue;

		insertSymbol(&new_map, map.entries[i]);
	}
	free(map.entries);

	map = new_map;
}

