#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "lex_preproc.h"
#include "arena.h"
#include "ansii.h"
#include "common.h"

/*

Combined lexer and preprocessor.

*/


bool isSpace(char);
bool isAlpha(char c);
bool isHexDigit(char c);
int hexToInt(char c);
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
Keyword names[] = {
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
	{"__inline", Tok_Key_Inline},
	{"_Noreturn", Tok_Key_Noreturn},

	{"__builtin_va_list", Tok_Key_VaList},
	{"__restrict", Tok_Key_Restrict},

	{"__FILE__", Tok_Key_File},
	{"__LINE__", Tok_Key_Line},
};
#define NAMES_COUNT (sizeof(names) / sizeof(names[0]))


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


int keyword_cmp(const void *a, const void *b) {
	return strcmp(((Keyword*)a)->name, ((Keyword*)b)->name);
}

Token fromWord (String word) {
	for (u32 i = 0; i < NAMES_COUNT; i++) {
		if (names[i].name && eql(names[i].name, word)) {
			Token ret = {0};
			ret.kind = names[i].key;
			return ret;
		}
	}
	assert(word.len);
	return (Token) {Tok_Identifier, .val.identifier = word};
}

_Noreturn void lexerrorOffset (SourceFile source, u32 offset, const char *msg, ...) {
    printErr(source, offset);

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
Token getToken (Arena *str_arena, SourceFile src, const char **p) {
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
			tok = (Token) {Tok_PreprocDirective, .val.identifier = {pos - start, start}};
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
		tok = (Token) {Tok_String, .val.string = val};
	} break;
	case '\'': {
	char_literal:
		pos++;
		// TODO Get unicode vals.
		tok = (Token) {Tok_Integer, .val.literal = {
			.integer = pos[0],
			.int_type = Int_int,
		}};
		if (pos[0] == '\\') {
			pos++;
			if (pos[0] == 'x') {
				assert(isHexDigit(pos[1]));
				assert(isHexDigit(pos[2]));
				tok.val.literal.integer = hexToInt(pos[1])*16 + hexToInt(pos[2]);
				if (tok.val.literal.integer >= 128)
					tok.val.literal.integer -= 256;
				pos += 2;
			} else {
				tok.val.literal.integer = escape_codes[(uchar)pos[0]];
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
			tok = fromWord((String) {pos - start, start});

			// TODO __FILE__ and __LINE__ should actually be handled by macro expansion.
			if (tok.kind == Tok_Key_File) {
				tok.kind = Tok_String;
				tok.val.string = src.name;
			} else if (tok.kind == Tok_Key_Line) {
				tok.kind = Tok_Integer;
				// TOOD Find line position. Re-searching for it every
				// time would be quadratic complexity. Bad, but maybe
				// does not occur very often...? Actually, assert-heavy
				// code probably does. Ugh, I'll have to count every
				// newline I skip... but only in the top-level lexer
				// loop, which is maybe not that bad.
				tok.val.literal.integer = 0;
				tok.val.literal.int_type = Int_int;
			}
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

				tok = (Token) {Tok_Real, .val.real = strtod(start, NULL)};
			} else {
				tok = (Token) {Tok_Integer, .val.literal.integer = strtoll(start, NULL, 0)};

				bool is_unsigned = pos[0] == 'u' || pos[0] == 'U';
				if (is_unsigned)
					pos++;
				BasicType literal_type = Int_int;
				if (pos[0] == 'l' || pos[0] == 'L') {
					pos++;
					literal_type = Int_long;
					if (pos[0] == 'l' || pos[0] == 'L') {
						pos++;
						literal_type = Int_longlong;
					}
				}
				if (is_unsigned)
					literal_type |= Int_unsigned;
				tok.val.literal.int_type = literal_type;
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
	*p = pos;
	return tok;
}


typedef struct MacroToken {
	Token tok;
	u32 file_offset;
	u16 file_ref;
	// 0 if this token is not a parameter, 1+(index into parameters) if it is.
	u8 parameter;
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
	bool followed_by_concat;
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
static Token getTokenSpaced(Arena *, SourceFile, const char **p);
static bool gobbleSpaceToNewline(const char **p);
static bool evalPreprocExpression(ExpansionParams, Tokenization *);
static const char *defineMacro(Arena *arena, Arena *generated_strings, StringMap *macros, String name, SourceFile, const char *pos);
static void predefineMacros(Arena *arena, Arena *genrated_strings, StringMap *macros, FileList *, StringList to_define, SourceKind);


Tokenization lex (Arena *generated_strings, String filename, Paths paths) {
	typedef struct {
		u16 file;
		const char *pos;
	} Inclusion;


	Arena macro_arena = create_arena(2048);
	Tokenization t = {0};

	StringMap sources = {0};
	StringMap macros = {0};
	LIST(Inclusion) includes_stack = {0};
	Tokenization prepreoc_evaluation_buf = {0};
	MacroStack macro_expansion_stack = {0};

	SourceFile *initial_source = readAllAlloc(STRING_EMPTY, filename);
	if (initial_source == NULL)
		generalFatal("could not open file \"%.*s\"\n", STRING_PRINTAGE(filename));

	PUSH(t.files, initial_source);
	SourceFile source = *initial_source;

	const char *pos = source.content.ptr;

	predefineMacros(&macro_arena, generated_strings, &macros, &t.files,
			paths.command_line_macros, Source_CommandLineMacro);
	predefineMacros(&macro_arena, generated_strings, &macros, &t.files,
			paths.system_macros, Source_SystemDefinedMacro);


	ExpansionParams expansion = {
		.stack = &macro_expansion_stack,
		.strings_arena = generated_strings,
		.macros = &macros,
		.src = &pos,
	};
	bool first_line_in_file = true;
	while (true) {
		bool file_begin = pos == source.content.ptr;
		bool line_begin;
		Token tok;
		const char *begin;
		while (true) {
			line_begin = tryGobbleSpace(source, &pos) == Space_Linebreak || file_begin;
			begin = pos;
			tok = getToken(generated_strings, source, &pos);
			if (tok.kind != Tok_EOF || source.idx == initial_source->idx)
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

				// TODO Disallow `defined`
				String name = {pos - start, start};
// 				if (*entry != NULL)
// 					fprintf(stderr, "redefining %.*s (TODO Check that definitions are identical)\n", STRING_PRINTAGE(name));

				pos = defineMacro(&macro_arena, generated_strings, &macros, name, source, pos);
			} else if (eql("undef", directive)) {
				tok = getTokenSpaced(generated_strings, source, &pos);

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
					new_source->included_count++;
				} else {
					if (delimiter == '\"') {
						for (u32 i = 0; i < paths.user_include_dirs.len && new_source == NULL; i++) {
							new_source = readAllAlloc(paths.user_include_dirs.ptr[i], includefilename);
						}
					}
					for (u32 i = 0; i < paths.sys_include_dirs.len && new_source == NULL; i++) {
						new_source = readAllAlloc(paths.sys_include_dirs.ptr[i], includefilename);
						if (new_source)
							new_source->kind = Source_StandardHeader;
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
				first_line_in_file = true;
				continue;

			} else if (eql("pragma", directive)) {
				if (tryGobbleSpace(source, &pos) == Space_Linebreak)
					lexerror(source, begin, "missing argument for #pragma directive");
				const char *start = pos;
				while (*pos && *pos != '\n') pos++;
				String pragma = {pos - start, start};
// 				pos--;

				if (eql("once", pragma)) {
					if (!first_line_in_file)
						lexerror(source, start, "#pragma once must appear at the beginning of the file");
					if (source.included_count) {
						Inclusion inc = POP(includes_stack);
						pos = inc.pos;
						source = *t.files.ptr[inc.file];
					}
				} else {
					lexwarning(source, start, "ignoring unknown #pragma directive");
				}
			} else if (eql("if", directive)) {
				expansion.source = source;
				expansion.source_file_offset = source_pos;
				while (!evalPreprocExpression(expansion, &prepreoc_evaluation_buf) &&
					skipToElseIfOrEnd(source, &pos) == If_ElseIf);

			} else if (eql("ifdef", directive) || eql("ifndef", directive)) {
				tok = getTokenSpaced(generated_strings, source, &pos);
				if (tok.kind != Tok_Identifier)
					lexerror(source, pos, "#ifdef must be followed by an identifier");


				bool got = mapGet(&macros, tok.val.identifier);
				bool required = directive.ptr[2] != 'n';
				if (got != required) {
					if (tryGobbleSpace(source, &pos) != Space_Linebreak)
						lexerror(source, pos, "#ifdef may not be followed by more than one identifier");

					expansion.source = source;
					expansion.source_file_offset = source_pos;
					while (skipToElseIfOrEnd(source, &pos) == If_ElseIf &&
						!evalPreprocExpression(expansion, &prepreoc_evaluation_buf));

				}

			} else if (eql("else", directive) || eql("elif", directive)) {
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
			first_line_in_file = false;
			continue;
		}
		first_line_in_file = false;

		if (tok.kind == Tok_Identifier) {
			Macro *macro = mapGet(&macros, tok.val.identifier);

			// STYLE Copypasta from expandInto, because macro_file_ref
			// should not be set on unexpanded function-like macros.
			if (macro) {
				Tokenization *arguments = NULL;
				expansion.source = source;
				expansion.source_file_offset = source_pos;

				if (macro->is_function_like) {
					Token paren = getTokenSpaced(generated_strings, source, &pos);

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

		if (tok.kind == Tok_EOF) {
			assert(source.idx == 0);
			break;
		}
	}

	free(prepreoc_evaluation_buf.tokens);
	free(prepreoc_evaluation_buf.positions);
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
				collapseMacroStack(ex.stack, &level_of_argument_source);
				MacroToken paren = takeToken(ex, &level_of_argument_source);
				if (paren.tok.kind != Tok_OpenParen) {
					appendOneToken(dest, t.tok, (TokenPosition) {
						.source_file_ref = t.file_ref,
						.source_file_offset = t.file_offset,
						.macro_file_ref = ex.source.idx,
						.macro_file_offset = ex.source_file_offset,
					});
					appendOneToken(dest, paren.tok, (TokenPosition) {
						.source_file_ref = paren.file_ref,
						.source_file_offset = paren.file_offset,
						.macro_file_ref = ex.source.idx,
						.macro_file_offset = ex.source_file_offset,
					});
					collapseMacroStack(ex.stack, &level_of_argument_source);
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
			collapseMacroStack(ex.stack, &level_of_argument_source);
			appendOneToken(dest, t.tok, (TokenPosition) {
				.source_file_ref = t.file_ref,
				.source_file_offset = t.file_offset,
				.macro_file_ref = ex.source.idx,
				.macro_file_offset = ex.source_file_offset,
			});
		}
	}

	return false;
}

// Allocates the argument list.
// PERFOMANCE A single Tokenization per argument list should be
// enough if Replacements store offset and length.
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

// STYLE The following code has very dense logic and probably needs more
// comments.

static MacroToken toMacroToken(Tokenization *tok, u32 pos);
static MacroToken concatenate(const ExpansionParams ex, Token first, u32 *marker);
static Token stringify(Arena *arena, Tokenization t);

static MacroToken takeToken (const ExpansionParams ex, u32 *marker) {
	MacroStack *stack = ex.stack;
	while (stack->len) {
		Replacement *repl = &stack->ptr[stack->len - 1];
		Macro *macro = repl->mac;
		if (macro) {
			assert(repl->pos < macro->tokens.len);

			MacroToken t = macro->tokens.ptr[repl->pos];
			repl->pos++;

			// PERFORMANCE Remove this lookahead (mark it in the #define parsing).
			bool followed_by_concat = repl->pos < macro->tokens.len &&
					macro->tokens.ptr[repl->pos].tok.kind == Tok_PreprocConcatenate;

			if (t.parameter) {
				if (t.tok.kind == Tok_PreprocDirective) {
					if (followed_by_concat)
						lexerror(ex.source, ex.source.content.ptr + ex.source_file_offset,
								"(TODO location, phrasing) stringified parameter can not followed by concatenation");
					Tokenization arg = repl->toks[t.parameter-1];
					t.tok = stringify(ex.strings_arena, arg);
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
				else
					return t;
			}
		} else {
			MacroToken t = toMacroToken(repl->toks, repl->pos);
			repl->pos++;
			if (repl->pos < repl->toks->count)
				return t;

			if (repl->followed_by_concat)
				return concatenate(ex, t.tok, marker);
			else
				return t;
		}
		collapseMacroStack(stack, marker);
	}

	bool have_space = tryGobbleSpace(ex.source, ex.src);
	u32 pos = *ex.src - ex.source.content.ptr;
	Token t = getToken(ex.strings_arena, ex.source, ex.src);
	t.preceded_by_space = have_space;
	return (MacroToken) {
		.tok = t,
		.file_ref = ex.source.idx,
		.file_offset = pos,
	};
}

void strPrintToken(char **dest, const char *end, Token t);
u32 strPrintTokenLen(Token t);

static MacroToken concatenate (const ExpansionParams ex, Token first, u32 *marker) {
	Replacement *repl = &ex.stack->ptr[ex.stack->len - 1];
	Macro *macro = repl->mac;
	MacroToken operator = macro->tokens.ptr[repl->pos];
	repl->pos++;
	MacroToken second = macro->tokens.ptr[repl->pos];
	repl->pos++;
	collapseMacroStack(ex.stack, marker);

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
	u32 len = strPrintTokenLen(first) + strPrintTokenLen(second.tok) + 1;
	char *rep = aalloc(ex.strings_arena, len);
	char *insert = rep;
	strPrintToken(&insert, rep+len, first);
	strPrintToken(&insert, rep+len, second.tok);
	int *c = NULL;
	if (insert > rep + len)
		*c = 1;
	*insert = 0;

	const char *src = rep;
	operator.tok = getToken(ex.strings_arena, ex.source, &src);
	return operator;
}

static Token stringify (Arena *arena, Tokenization arg) {
	u32 data_len = 0;
	for (u32 i = 0; i < arg.count; i++)
		data_len += strPrintTokenLen(arg.tokens[i]) + arg.tokens[i].preceded_by_space;

	char *start = aalloc(arena, data_len);
	char *c = start;
	char *end = c + data_len;
	for (u32 i = 0; i < arg.count; i++) {
		if (arg.tokens[i].preceded_by_space)
			printto(&c, end, " ");
		strPrintToken(&c, end, arg.tokens[i]);
	}

	return (Token) {Tok_String, .val.string = {data_len, start}};
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

			if (*marker == stack->len)
				(*marker)--;
			stack->len--;
		}
	}
}

static MacroToken toMacroToken (Tokenization *tok, u32 pos) {
	return (MacroToken) {
		.tok = tok->tokens[pos],
		.file_ref = tok->positions[pos].macro_file_ref,
		.file_offset = tok->positions[pos].macro_file_offset,
	};
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


// PERFORMANCE This function is potentially a very hot path. Optimize!
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

// PERFORMANCE This function is potentially a very hot path. Optimize!
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


static Token getTokenSpaced (Arena *str_arena, SourceFile source, const char **p) {
	gobbleSpaceToNewline(p);
	if (**p == '\n' || **p == 0)
		lexerror(source, *p, "expected a token before the end of the line");
	return getToken(str_arena, source, p);
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
	const Tokenization *tok;
	const Token *pos;
} ConstParse;

static int parseOr(ConstParse *parse);
static int parseAnd(ConstParse *parse);
static int parseEq(ConstParse *parse);
static int parseCmp(ConstParse *parse);
static int parseAdd(ConstParse *parse);
static int parseUnop(ConstParse *parse);
static int parseCore(ConstParse *parse);

static int parseOr(ConstParse *parse) {
	int x = parseAnd(parse);
	while (parse->pos->kind == Tok_DoublePipe) {
		parse->pos++;
		x = parseAnd(parse) || x;
	}
	return x;
}

static int parseAnd(ConstParse *parse) {
	int x = parseEq(parse);
	while (parse->pos->kind == Tok_DoubleAmpersand) {
		parse->pos++;
		x = parseEq(parse) && x;
	}
	return x;
}

static int parseEq(ConstParse *parse) {
	int x = parseCmp(parse);
	while (true) {
		if (parse->pos->kind == (Tok_Equals | Tok_EQUALED)) {
			parse->pos++;
			x = x == parseCmp(parse);
		} else if (parse->pos->kind == (Tok_Bang | Tok_EQUALED)) {
			parse->pos++;
			x = x != parseCmp(parse);
		} else {
			break;
		}
	}
	return x;
}

static int parseCmp(ConstParse *parse) {
	int x = parseAdd(parse);

	while (true) {
		if (parse->pos->kind == Tok_Less) {
			parse->pos++;
			x = x < parseAdd(parse);
		} else if (parse->pos->kind == (Tok_Less | Tok_EQUALED)) {
			parse->pos++;
			x = x <= parseAdd(parse);
		} else if (parse->pos->kind == Tok_Greater) {
			parse->pos++;
			x = x > parseAdd(parse);
		} else if (parse->pos->kind == (Tok_Greater | Tok_EQUALED)) {
			parse->pos++;
			x = x >= parseAdd(parse);
		} else {
			break;
		}
	}
	return x;
}

static int parseAdd(ConstParse *parse) {
	int x = parseUnop(parse);

	while (true) {
		if (parse->pos->kind == Tok_Plus) {
			parse->pos++;
			x += parseUnop(parse);
		} else if (parse->pos->kind == Tok_Minus) {
			parse->pos++;
			x -= parseUnop(parse);
		} else {
			break;
		}
	}
	return x;
}

static int parseUnop(ConstParse *parse) {
	switch (parse->pos->kind) {
	case Tok_Minus:
		parse->pos++;
		return -parseUnop(parse);
	case Tok_Tilde:
		parse->pos++;
		return ~parseUnop(parse);
	case Tok_Bang:
		parse->pos++;
		return !parseUnop(parse);
	default:
		return parseCore(parse);
	}
}

static int parseCore(ConstParse *parse) {
	if (parse->pos->kind == Tok_Integer) {
		int x = parse->pos->val.literal.integer;
		parse->pos++;
		return x;
	} else if (parse->pos->kind == Tok_OpenParen) {
		parse->pos++;
		int x = parseOr(parse);
		const char *pos = parse->source.content.ptr + parse->tok->positions[parse->pos - parse->tok->tokens].source_file_offset;
		if (parse->pos->kind != Tok_CloseParen)
			lexerror(parse->source, pos, "exepected closing parenthesis");
		parse->pos++;
		return x;
	} else if (parse->pos->kind == Tok_Identifier) {
		parse->pos++;
		return 0;
	} else {
		const char *pos = parse->source.content.ptr + parse->tok->positions[parse->pos - parse->tok->tokens].source_file_offset;
		lexerror(parse->source, pos, "expected an expression");
	}
}

static bool evalPreprocExpression(ExpansionParams params, Tokenization *buf) {
	const char *pos = *params.src;
	Arena *const str_arena = params.strings_arena;
	buf->count = 0;

	while (true) {
		gobbleSpaceToNewline(&pos);
		if (*pos == '\n' || *pos == 0)
			break;
		Token tok = getToken(str_arena, params.source, &pos);
		if (tok.kind == Tok_Identifier) {
			if (eql("defined", tok.val.identifier)) {
				tok = getTokenSpaced(str_arena, params.source, &pos);
				bool parenthesized = tok.kind == Tok_OpenParen;
				if (parenthesized)
					tok = getTokenSpaced(str_arena, params.source, &pos);

				if (tok.kind != Tok_Identifier)
					lexerror(params.source, pos, "the operator %sdefined%s expects an identifier as an argument", BOLD, RESET);

				bool found = mapGet(params.macros, tok.val.identifier);
				tok = (Token) {Tok_Integer, .val.literal = {.integer = found, .int_type = Int_int}};

				if (parenthesized) {
					gobbleSpaceToNewline(&pos);
					if (*pos == '\n' || *pos == 0 || getToken(str_arena, params.source, &pos).kind != Tok_CloseParen)
						lexerror(params.source, pos, "missing closing parenthesis");
				}
			} else {
				Macro *m = mapGet(params.macros, tok.val.identifier);
				if (m) {
					if (m->parameters.len > 0)
						lexerror(params.source, pos, "TODO Expand function-like macros in preprocessor constant expressions");
					PUSH(*params.stack, ((Replacement) {m}));
					expandInto(params, buf, false);
					continue;
				}
			}
		}
		appendOneToken(buf, tok, (TokenPosition) {pos - params.source.content.ptr, params.source.idx});
	}
	*params.src = pos;
	appendOneToken(buf, (Token) {Tok_EOF}, (TokenPosition) {pos - params.source.content.ptr, params.source.idx});

// 				tok = (Token) {Tok_Integer, {.integer = 0, .int_type = Int_int}};
	ConstParse parse = {params.source, buf, buf->tokens};
	int res = parseOr(&parse);
	return res;
}


// === Helpers ===


// TODO Come up with a better system for nice highlighting.
const char *token_names[Tok_EOF+1] = {
	[Tok_Identifier] = "identifier",
	[Tok_Integer] = "integer-literal",
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
			for (u32 i = 0; i < NAMES_COUNT; i++) {
				if (names[i].key == kind)
					return names[i].name;
			}
		}
		return NULL;
	}
}


u32 strPrintTokenLen (Token t) {
	switch (t.kind) {
	case Tok_Identifier: return t.val.identifier.len;
	case Tok_PreprocDirective: return t.val.identifier.len + 1;
	case Tok_String: return t.val.string.len * 2 + 2;
	case Tok_Integer: return 25;
	default:
		assert(tokenName(t.kind));
		return strlen(tokenName(t.kind));
	}
}

void strPrintToken (char **dest, const char *end, Token t) {
	switch (t.kind) {
	case Tok_PreprocDirective:
		printto(dest, end, "#");
		FALLTHROUGH;
	case Tok_Identifier:
		printto(dest, end, "%.*s", STRING_PRINTAGE(t.val.identifier));
		return;
	case Tok_String:
		printto(dest, end, "\"%.*s\"", STRING_PRINTAGE(t.val.string));
		return;
	case Tok_Integer:
		printto(dest, end, "%lld", t.val.literal.integer);
		return;
	default:
		printto(dest, end, "%s", tokenName(t.kind));
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

bool isHexDigit (char c) {
	if (c >= '0' && c <= '9')
		return true;
	c |= 32;
	return c >= 'a' && c <= 'f';
}

int hexToInt (char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	c |= 32;
	assert(c >= 'a' && c <= 'f');
	return c - 'a';
}

bool isDigit (char c) {
	return c >= '0' && c <= '9';
}

bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}


static const char *defineMacro (
	Arena *arena,
	Arena *generated_strings,
	StringMap *macros,
	String name,
	SourceFile source,
	const char *pos)
{
	static StringMap parameters = {0};

	void **entry = mapGetOrCreate(macros, name);
	Macro *mac = ALLOC(arena, Macro);
	*entry = mac;
	*mac = (Macro) {name, source.idx};


	u32 param_count = 0;
	if (pos[0] == '(') {
		pos++;
		const char *p = pos;
		while (true) {
			Token t = getTokenSpaced(generated_strings, source, &p);
			if (t.kind != Tok_Identifier && t.kind != Tok_TripleDot)
				lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
			param_count++;
			t = getTokenSpaced(generated_strings, source, &p);
			if (t.kind == Tok_CloseParen)
				break;
			else if (t.kind == Tok_EOF)
				lexerror(source, pos, "incomplete parameter list");
			else if (t.kind != Tok_Comma)
				lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
		}
		mac->parameters.ptr = aalloc(arena, sizeof(Parameter) * param_count);
		mac->parameters.len = param_count;
		mac->is_function_like = true;

		for (u32 i = 0; i < param_count; i++) {
			Token t = getTokenSpaced(generated_strings, source, &pos);
			assert(t.kind == Tok_Identifier || t.kind == Tok_TripleDot);

			String name = t.val.identifier;
			if (t.kind == Tok_TripleDot) {
				if (i + 1 != param_count)
					lexerror(source, pos, "an ellipsis may only appear as the last parameter to a variadic macro");
				// TODO Version check for C99
				name = zstr("__VA_ARGS__");
			}

			mac->parameters.ptr[i].name = name;
			void **entry = mapGetOrCreate(&parameters, name);
			if (*entry)
				lexerror(source, ((String *)*entry)->ptr, "macro parameters may not be duplicated");
			*entry = &mac->parameters.ptr[i];

			t = getTokenSpaced(generated_strings, source, &pos);
		}
	}

	LIST(MacroToken) macro_assembly = {0};
	while (true) {
		bool havespace = gobbleSpaceToNewline(&pos);
		if (*pos == '\n')
			break;

		u32 macro_pos = pos - source.content.ptr;
		MacroToken t = {
			.tok = getToken(generated_strings, source, &pos),
			.file_offset = macro_pos,
			.file_ref = source.idx,
		};
		t.tok.preceded_by_space = havespace;
		if (t.tok.kind == Tok_EOF)
			break;

		if (param_count && (t.tok.kind == Tok_Identifier || t.tok.kind == Tok_PreprocDirective)) {
			Parameter *param = mapGet(&parameters, t.tok.val.identifier);
			if (param)
				t.parameter = param - mac->parameters.ptr + 1;
		}
		PUSH(macro_assembly, t);
	}
	if (macro_assembly.len) {
		MacroToken first = macro_assembly.ptr[0];
		if (first.tok.kind == Tok_PreprocConcatenate)
			lexerrorOffset(source, first.file_offset, "a %s##%s preprocessing token may not occur at the beginning of a macro definition");
		MacroToken last = macro_assembly.ptr[macro_assembly.len-1];
		if (last.tok.kind == Tok_PreprocConcatenate)
			lexerrorOffset(source, last.file_offset, "a %s##%s preprocessing token may not occur at the end of a macro definition");
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
	StringMap *macros,
	FileList *files,
	StringList to_define,
	SourceKind kind)
{
#ifdef NDEBUG
	SourceFile *sources = calloc(sizeof(SourceFile), macros);
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

		defineMacro(arena, genrated_strings, macros, source->name, *source, source->content.ptr);
	}
}


String processStringLiteral(Arena *arena, String src) {
	char *res = aalloc(arena, src.len + 1);
	u32 len = 0;
	for (u32 i = 0; i < src.len; i++) {
		if (src.ptr[i] == '\\') {
			i++;
			res[len] = escape_codes[(uchar) src.ptr[i]];
		} else {
			res[len] = src.ptr[i];
		}
		len++;
	}
	return (String) {len, res};
}

