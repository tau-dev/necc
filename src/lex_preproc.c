#include "util.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#ifdef HAVE_POSIX
#include <stdlib.h>
#endif

#ifdef HAVE_WINDOWS
#include <stdlib.h>
#endif

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

typedef unsigned long long ullong;
typedef long long llong;


static inline bool isSpace(char);
static inline bool isAlpha(char c);
static inline bool isHexDigit(char c);
static inline bool isOctDigit(char c);
static inline int hexToInt(char c);
static inline bool isDigit(char c);
static inline bool isAlnum(char c);
static inline bool isLinebreak(const char *c);

static_assert(sizeof(Token) == 16, "sizeof(Token) == 16");

typedef struct {
	u32 hash;
	u32 idx;
} IndexEntry;

typedef struct {
	IndexEntry *entries;

	u32 capacity;
} SymbolMap;



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

typedef enum {
	Painted = 0x01,
	PrecededBySpace = 0x02,
} PreprocFlag;


typedef struct {
	char *name;
	unsigned int key;
} Keyword;



// This table also contains some keywords from C11. They were introduced
// with leading underscore, so should not collide, but it is a bit
// inconsistent.
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
	{"__attribute", Tok_Key_Attribute},
	{"__attribute__", Tok_Key_Attribute},

	{"__FILE__", Tok_Key_File},
	{"__LINE__", Tok_Key_Line},

	{"__const", Tok_Key_Const},
	{"__const__", Tok_Key_Const},
};

Keyword c99_keywords[] = {
	{"_Bool", Tok_Key_Bool},
	{"restrict", Tok_Key_Restrict},
};

Keyword c23_keywords[] = {
	{"alignof", Tok_Key_Alignof},
	{"alignas", Tok_Key_Alignas},
	{"bool", Tok_Key_Bool},
	{"constexpr", Tok_Key_Constexpr},
	{"false", Tok_Key_False},
	{"true", Tok_Key_True},
	{"nullptr", Tok_Key_Nullptr},
	{"static_assert", Tok_Key_StaticAssert},
	{"thread_local", Tok_Key_Threadlocal},
	{"_BitInt", Tok_Key_Bitint},
	{"_Decimal32", Tok_Key_Decimal32},
	{"_Decimal64", Tok_Key_Decimal64},
	{"_Decimal128", Tok_Key_Decimal128},
};

Keyword special_identifiers[] = {
	{"main", Special_main},
	{"__func__", Special_func},
	{"__VA_ARGS__", Special_VA_ARGS},
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

	{"__builtin_clz", Intrinsic_Clz},
	{"__builtin_clzl", Intrinsic_Clzll}, // lul
	{"__builtin_clzll", Intrinsic_Clzll},
	{"__builtin_ctz", Intrinsic_Ctz},
	{"__builtin_ctzl", Intrinsic_Ctzll},
	{"__builtin_ctzl", Intrinsic_Ctzll},
	{"__builtin_popcount", Intrinsic_Popcount},
	{"__builtin_popcountl", Intrinsic_Popcountll},
	{"__builtin_popcountll", Intrinsic_Popcountll},

	{"__builtin_expect", Intrinsic_Expect},
	{"__builtin_frame_address", Intrinsic_FrameAddress},
	{"__builtin_alloca", Intrinsic_Alloca},
	{"__builtin_offsetof", Intrinsic_Offsetof},
	{"__builtin_constant_p", Intrinsic_ConstantP},
};

#define MAX_TOKENS 4096
const char *token_names[MAX_TOKENS] = {
	[Tok_Invalid] = "invalid token",

	[Tok_Identifier] = "identifier",
	[Tok_String] = "string-literal",
	[Tok_Real] = "floating-point-literal",
	[Tok_Integer] = "int-literal",
	[Tok_Char] = "char-literal",

	[Tok_PreprocDirective] = "preprocessor-directive",
	[Tok_PreprocConcatenate] = "##",

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
	[Tok_Equal] = "=",
	[Tok_Equal | Tok_EQUALED] = "==",
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
	Directive_IncludeNext,
	Directive_Embed,

	Directive_Line,
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
	{"include_next", Directive_IncludeNext},
	{"embed", Directive_Embed},

	{"line", Directive_Line},
};


char escape_codes[256] = {
	['\\'] = '\\',
	['\"'] = '\"',
	['\''] = '\'',
	['\?'] = '?',
	['a'] = '\a',
	['b'] = '\b',
	['r'] = '\r',
	['v'] = '\v',
	['f'] = '\f',
	['t'] = '\t',
	['n'] = '\n',
};

char de_escape_codes[256] = {
	['\\'] = '\\',
	['\"'] = '\"',
	['\''] = '\'',
	['\?'] = '?',
	['\a'] = 'a',
	['\b'] = 'b',
	['\r'] = 'r',
	['\v'] = 'v',
	['\f'] = 'f',
	['\t'] = 't',
	['\n'] = 'n',
};



// TODO Make it thread-local?
static SymbolMap map;
static u32 special_identifiers_idx[ARRSIZE(special_identifiers)];

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

static u32 lexEscapeCode(SourceFile *source, Location loc, const char **p);
static String processStringLiteral(SourceFile *file, Location loc, Arena *arena,String src);
static SourceFile *tryOpen(StringMap *sources, FileList *files, String path, String name);

// TODO Trigraphs (trivial) and digraphs (annoying)
// PERFORMANCE The current SoureFile is passed by-value to most helper functions, because I assumed I want to access it quickly. Turns out a lot of instructions are spent on copying it around
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
		} else if (isDigit(pos[1])) {
			goto start_number;
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
		// TODO A minus sign should combine with a following number into a single token.
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
		tok.kind = Tok_Equal;
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
		tok.literal_type = Int_char;
	string:
		pos++;
		const char *begin = pos;
		while (*pos && *pos != '\"') {
			if (*pos == '\\') pos++;
			pos++;
		}
		if (*pos == '\0')
			lexerror(source, *loc, "missing closing quote");

		String val = processStringLiteral(&source, *loc, str_arena, (String) {pos - begin, begin});
		tok.kind = Tok_String;
		tok.val.symbol_idx = getSymbolId(syms, val);
	} break;
	case '\'':
	char_literal: {
		const char *begin = pos;
		pos++;
		if (pos[0] == '\'')
			lexerror(source, *loc, "character constant cannot be empty");

		// TODO Get unicode vals.
		if (pos[0] == '\\') {
			pos++;
			if (pos[0] == 'x')
				pos += 2;
		}
		pos++;
		if (pos[0] != '\'')
			lexerror(source, *loc, "missing closing single quote");

		tok = (Token) {Tok_Char,
			.literal_type = Int_int,
			.val.literal_src = begin,
			.literal_len = pos - begin + 1,
		};
	} break;

	// The lexer deviates from GCC's and Clang's behavior in that it
	// does not require white space between a constant and a following
	// identifier. I believe I am more correct here, in that the lexical
	// grammar does not forbid this construction, but since it is never
	// valid by the phrase grammar, this really does not matter much.
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	start_number: {
		// Could just use strtod/strtol for number parsing, but the
		// standard library functions have a pretty inefficient
		// interface and I will have to manually handle C23 stuff
		// like decimal-type suffixes and digit separators anyways.
		const char *start = pos;

		if (pos[0] == '0') {
			pos++;
			if (pos[0] == 'x') {
				pos++;
				while (isHexDigit(*pos)) pos++;
				if (*pos == '.' || *pos == 'p' || *pos == 'P') {
					if (*pos == '.') {
						pos++;
						while (isHexDigit(*pos))
							pos++;
					}
					if (*pos != 'p' && *pos != 'P')
						lexerror(source, *loc, "hexadecimal floating point constant must have a 'p' suffix");
					pos++;
					if ((*pos == '-' || *pos == '+') && isDigit(pos[1]))
						pos++;
					while (isDigit(*pos)) pos++;
					goto floating;
				}
				goto integer;
			} else if (pos[0] == 'b' || pos[0] == 'B') {
				pos++;
				while (*pos == '0' || *pos == '1' || *pos == '\'') pos++;
				goto integer;
			}
		}
		while (isDigit(*pos)) pos++;


		if (*pos == '.' || *pos == 'e' || *pos == 'E' || *pos == 'f' || *pos == 'F') {
			if (*pos == '.') {
				pos++;
				while (isDigit(*pos))
					pos++;
			}
			if (*pos == 'e' || *pos == 'E') {
				pos++;
				// FIXME: what should happen on missing exponent digits?
				if ((*pos == '-' || *pos == '+') && isDigit(pos[1]))
					pos++;
				while (isDigit(*pos)) pos++;
			}

			floating:

			if (*pos == 'f' || *pos == 'F') {
				tok.literal_type = Float_Single,
				pos++;
			} else if (*pos == 'l' || *pos == 'L') {
				tok.literal_type = Float_LongDouble,
				pos++;
			}

			tok = (Token) {Tok_Real,
				.literal_type = Float_Double,
				.val.literal_src = start,
			};
		} else {
			integer:;

			bool is_unsigned = pos[0] == 'u' || pos[0] == 'U';
			BasicType literal_type = Int_int;
			if (is_unsigned)
				pos++;
			if (pos[0] == 'l' || pos[0] == 'L') {
				pos++;
				literal_type = Int_long;
				if (pos[0] == 'l' || pos[0] == 'L') {
					pos++;
					literal_type = Int_longlong;
				}
			}
			if (pos[0] == 'u' || pos[0] == 'U') {
				is_unsigned = true;
				pos++;
			}
			if (is_unsigned)
				literal_type |= Int_unsigned;

			tok = (Token) {Tok_Integer,
				.literal_type = literal_type,
				.val.literal_src = start,
			};
		}
		u32 len = pos - start;
		if (len > UINT16_MAX)
			lexerror(source, *loc, "this number is too long; the maximum supported length is 65535 characters");
		tok.literal_len = (u16) len;

		pos--;
	} break;
	case 'L':
	case 'u':
	case 'U':
		if (pos[1] == '\'' || (pos[0] == 'u' && pos[1] == '8' && pos[2] == '\'')) {
			if (pos[1] == '8')
				pos++;
			pos++;
			goto char_literal;
		}
		if (pos[1] == '\"' || (pos[0] == 'u' && pos[1] == '8' && pos[2] == '\"')) {
			tok.literal_type = Int_short | Int_unsigned;
			if (pos[0] == 'u') {
				if (pos[1] == '8') {
					pos++;
					tok.literal_type = Int_suchar | Int_unsigned;
				}
			} else if (pos[0] == 'U') {
				tok.literal_type = Int_int | Int_unsigned;
			}
			pos++;
			goto string;
		}
		FALLTHROUGH;
	case '_':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z': {
		const char *start = pos;
		while (isAlnum(*pos))
			pos++;

		String word = {pos - start, start};
		tok = (Token) {Tok_Identifier, .val.symbol_idx = getSymbolId(syms, word)};

		pos--;
	} break;
	default:
		tok.kind = Tok_Invalid;
		tok.val.invalid_token = *pos;
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

String literalText (const Token t) {
	assert(t.kind == Tok_Integer || t.kind == Tok_Char || t.kind == Tok_Real);
	return (String) {
		.ptr = t.val.literal_src,
		.len = t.literal_len
	};
}

ConstInt intLiteralValue (const Token *t) {
	assert(t->kind == Tok_Integer);
	String text = literalText(*t);

	bool is_unsigned = t->literal_type & Int_unsigned;

	ConstInt res = {
		.type = t->literal_type & ~Int_unsigned,
	};
#ifdef NDEBUG
	res.val = strtoull(text.ptr, NULL, 0);
#else
	char *end;
	res.val = strtoull(text.ptr, &end, 0);

	while (*end == 'u' || *end == 'U' || *end == 'l' || *end == 'L')
		end++;
	const char *rest = text.ptr + text.len;
	assert (end == rest);
#endif

	// FIXME Use platform-correct integer sizes from options->target.
	if (res.type == Int_int && res.val > INT32_MAX) {
		if (res.val > UINT32_MAX)
			res.type = Int_long;
		else
			is_unsigned = true;
	}
	if (res.val > INT64_MAX) is_unsigned = true;

	if (is_unsigned)
		res.type |= Int_unsigned;
	return res;
}

ConstInt charLiteralValue (const Tokenization *tok, const Token *t) {
	assert(t->kind == Tok_Char);
	Location loc = tok->list.positions[t-tok->list.tokens].source;
	SourceFile *source = tok->files.ptr[loc.file_id];
	String text = literalText(*t);
	const char *src = text.ptr + 1;

	ConstInt res = {
		.type = t->literal_type & ~Int_unsigned,
		.val = *src,
	};
	if (res.val == '\\') {
		res.val = lexEscapeCode(source, loc, &src);
		src++;
		assert(*src == '\'');
		assert (src+1 == text.ptr + text.len);
	}
	return res;
}


double floatLiteralValue (const Token *t) {
	assert(t->kind == Tok_Real);
	String text = literalText(*t);

#ifdef NDEBUG
	return strtod(text.ptr, NULL);
#else
	char *end;
	double res = strtod(text.ptr, &end);
	if (*end == 'f' || *end == 'F' || *end == 'l' || *end == 'L')
		end++;
	assert(end == text.ptr + text.len);
	return res;
#endif
}


typedef struct MacroToken {
	Token tok;
	// Carries the original source of the token.
	Location loc;
	// 0 if this token is not a parameter, 1+(index into parameters) if it is.
	u16 parameter;

	bool followed_by_concat;
} MacroToken;


typedef struct Macro {
	String name;
	SourceFile *source;

	SPAN(MacroToken) tokens;
	u32 parameters_count;
	bool is_function_like;
	bool is_vararg;
	bool being_replaced;
} Macro;

typedef struct Replacement {
	Macro *mac;
	u32 pos;
	// If mac is NULL, this is an expanded buffer to be read from.
	// Otherwise, this is an array of the function-like macro's arguments.
	TokenList *toks;
	Location loc;
	bool followed_by_concat;
} Replacement;

typedef LIST(Replacement) MacroStack;

// PERFORMANCE Passed by value, same problem as with SourceFile.
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
static void ensureCapacity(TokenList *t, u32 required);
static void appendOneToken(TokenList *t, Token tok, TokenLocation pos);
inline static void resolveIdentToKeyword(Tokenization *t, Token *tok, Location loc, String name);
static String restOfLine(SourceFile source, Location *, const char **pos);
static SpaceClass tryGobbleSpace(SourceFile source, Location *, const char **p);
static bool followedByOpenParen(const char *p);
static void skipToValidBranchOrEnd(ExpansionParams, IfClass, u32 *depth, const char **p, TokenList *eval_buf);
static void skipToEndIf(SourceFile source, Location *, const char **p);
static IfClass skipToElseIfOrEnd(SourceFile source, Location *, const char **p);
static Token getTokenSpaced(Arena *, SourceFile, Location *, SymbolList *, const char **p);
static bool gobbleSpaceToNewline(const char **p, Location *loc);

static void preprocExpandLine(ExpansionParams params, TokenList *buf);
static String includeFilename(ExpansionParams params, TokenList *buf, bool *quoted);
static u64 preprocExpression(ExpansionParams params, TokenList *buf, const Token **end);
static const char *defineMacro(Arena *arena, Arena *generated_strings, SymbolList *, String name, SourceFile *, Location *loc, const char *pos, bool gnu);
static void predefineMacros(Arena *arena, Arena *genrated_strings, SymbolList *, FileList *, MacroDefiners to_define, SourceKind, bool gnu);
static void resolveSymbolIndicesToPointers(TokenList t, Symbol *syms);
static void markMacroDecl(FILE *dest, SourceFile source, Location, String macro_name);
static void loadKeywords(Tokenization *toks, Keyword *keys, u32 count);

static void loadKeywords (Tokenization *tok, Keyword *keys, u32 count) {
	for (u32 i = 0; i < count; i++) {
		Keyword key = keys[i];
		token_names[key.key] = key.name;
		getSymbol(&tok->symbols, zstr(key.name))->keyword = key.key;
	}
}

Tokenization lex (Arena *generated_strings, String input, LexParams params) {
	typedef struct {
		Location loc;
		const char *pos;
	} Inclusion;

	Arena macro_arena = create_arena(256 * 1024L);
	Tokenization t = {0};

	StringMap sources = {0};
	LIST(Inclusion) includes_stack = {0};
	TokenList preproc_evaluation_buf = {0};
	MacroStack macro_expansion_stack = {0};
	u32 if_depth = 0;

	loadKeywords(&t, standard_keywords, ARRSIZE(standard_keywords));
	if (params.options->target.version & Features_C99)
		loadKeywords(&t, c99_keywords, ARRSIZE(c99_keywords));
	if (params.options->target.version & Features_C23)
		loadKeywords(&t, c23_keywords, ARRSIZE(c23_keywords));
	bool gnu = params.options->target.version & Features_GNU_Extensions;

	for (u32 i = 0; i < ARRSIZE(intrinsics); i++) {
		Symbol *s = getSymbol(&t.symbols, zstr(intrinsics[i].name));
		s->directive = intrinsics[i].key;
		s->keyword = Tok_Intrinsic;
	}

	for (u32 i = 0; i < ARRSIZE(special_identifiers_idx); i++)
		special_identifiers_idx[i] = getSymbolId(&t.symbols, zstr(special_identifiers[i].name));

	for (u32 i = 0; i < ARRSIZE(preproc_directives); i++)
		getSymbol(&t.symbols, zstr(preproc_directives[i].name))->directive = preproc_directives[i].key;


	// Used so that an invalid file index can be discriminated.
	PUSH(t.files, NULL);


	size_t slash = input.len - 1;
	while (slash > 0 && !isDirSeparator(input.ptr[slash-1])) slash--;

	String input_directory = {slash, input.ptr};
	String filename = {input.len - slash, input.ptr + slash};

	SourceFile *initial_source = tryOpen(&sources, &t.files, input_directory, filename);
	if (initial_source == NULL)
		generalFatal("could not open file ‘%.*s’", STR_PRINTAGE(filename));

	const char *pos = initial_source->content.ptr;
	SourceFile source = *initial_source;
	Location loc = {source.idx, 1, 1};

	// Skip over shebang line
	if (pos[0] == '#' && pos[1] == '!') {
		while (pos && *pos != '\n') pos++;
	}

	predefineMacros(&macro_arena, generated_strings, &t.symbols, &t.files,
			params.command_line_macros, Source_CommandLineMacro, gnu);
	predefineMacros(&macro_arena, generated_strings, &t.symbols, &t.files,
			params.system_macros, Source_SystemDefinedMacro, gnu);


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

		// Get a new token. While hitting EOFs, pop from the include stack.
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

		// Handle preprocessor directives.
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

				// TODO Disallow the macro name `defined`
				String name = {pos - start, start};
				if (params.options->any_decl_emit) {
					if (params.options->emit_decls && source.kind != Source_StandardHeader)
						markMacroDecl(params.options->emit_decls, source, loc, name);
					if (params.options->emit_all_decls && source.kind != Source_StandardHeader)
						markMacroDecl(params.options->emit_all_decls, source, loc, name);
					if (params.options->emit_std_decls)
						markMacroDecl(params.options->emit_std_decls, source, loc, name);
				}

				loc.column += pos - start;
				pos = defineMacro(&macro_arena, generated_strings, &t.symbols, name, t.files.ptr[source.idx], &loc, pos, gnu);
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
			case Directive_IncludeNext:
			case Directive_Include: {
				i32 start_at_order = 0;
				if (dir == Directive_IncludeNext)
					start_at_order = source.next_include_index;

				expansion.source = source;
				expansion.expansion_start = begin;

				bool quoted;
				String includefilename = includeFilename(expansion, &preproc_evaluation_buf, &quoted);

				SourceFile *new_source = NULL;
				u32 include_order = start_at_order;
				if (quoted && dir != Directive_IncludeNext)
					new_source = tryOpen(&sources, &t.files, source.plain_path, includefilename);

				if (quoted && new_source == NULL) {
					for (u32 i = start_at_order; i < params.user_include_dirs.len; i++) {
						new_source = tryOpen(&sources, &t.files,  params.user_include_dirs.ptr[i], includefilename);
						include_order++;
						if (new_source) break;
					}
				}
				if (new_source == NULL) {
					start_at_order -= (i32) params.user_include_dirs.len;
					if (start_at_order < 0) start_at_order = 0;
					for (u32 i = start_at_order; i < params.sys_include_dirs.len; i++) {
						new_source = tryOpen(&sources, &t.files, params.sys_include_dirs.ptr[i], includefilename);
						include_order++;
						if (new_source) {
							new_source->kind = Source_StandardHeader;
							break;
						}
					}
				}
				if (new_source == NULL)
					lexerror(source, begin, "could not open include file ‘%.*s’", STR_PRINTAGE(includefilename));
				new_source->next_include_index = include_order;

				if (includes_stack.len >= MAX_INCLUDES)
					lexerror(source, begin, "exceeded maximum #include depth of %d", MAX_INCLUDES);
				PUSH(includes_stack, ((Inclusion) {loc, pos}));

				source = *new_source;
				pos = source.content.ptr;
				loc = (Location) {source.idx, 1, 1};
				first_line_in_file = true;
				continue;
			}
			case Directive_Embed: {
				lexerror(source, begin, "TODO #embed");
			} break;
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

				skipToValidBranchOrEnd(expansion, If_If, &if_depth, &pos, &preproc_evaluation_buf);
			} break;
			case Directive_Ifdef:
			case Directive_Ifndef: {
				expansion.source = source;
				expansion.expansion_start = begin;

				IfClass class = dir == Directive_Ifdef ? If_IfDef : If_IfnDef;
				skipToValidBranchOrEnd(expansion, class, &if_depth, &pos, &preproc_evaluation_buf);
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
				lexerror(source, start, "%.*s", STR_PRINTAGE(str));

			} break;
			case Directive_Warn: {
				Location start = loc;
				String str = restOfLine(source, &loc, &pos);
				lexwarning(source, start, "%.*s", STR_PRINTAGE(str));

			} break;
			case Directive_Line: {
				expansion.source = source;
				expansion.expansion_start = begin;
				// TODO May be followed by a file name.

				const Token *end;
				preprocExpandLine(expansion, &preproc_evaluation_buf);
				u64 lineno = preprocExpression(expansion, &preproc_evaluation_buf, &end);

				if (lineno > INT32_MAX)
					lexerror(source, begin, "line number out of range");
				lineno--;
				loc.line = lineno;
			} break;
			default:
				if (t.symbols.ptr[tok.val.symbol_idx].name.len != 0)
					lexerror(source, begin, "unknown preprocessor directive");
			}
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
					if (followedByOpenParen(pos)) {
						tryGobbleSpace(expansion.source, expansion.loc, expansion.src);
						assert(pos[0] == '(');
						pos++;
						expansion.loc->column++;
					} else {
						goto non_macro_ident;
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
			non_macro_ident:;
			resolveIdentToKeyword(&t, &tok, begin, source.plain_name);
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

	free(preproc_evaluation_buf.tokens);
	free(preproc_evaluation_buf.positions);
	free(includes_stack.ptr);
	free(macro_expansion_stack.ptr);
	foreach (i, t.symbols) {
		if (t.symbols.ptr[i].macro)
			free(t.symbols.ptr[i].macro->tokens.ptr);
	}

	free(map.entries);
	map = (SymbolMap) {0};

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
			fprintf(stderr, "in expansion of macro %.*s\n", STR_PRINTAGE(repl.mac->name));
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
	// needs to take care that the comma or paren that delimits an
	// argument does not come from an inner macro expansion
	// (collapseMacroStack sees to it that no parenthesis is stolen from
	// an enclosing call either (TODO)).
	// The following variable denotes the stack level above the lowest
	// macro visited (or an argument Tokenization above that), above
	// which parens do not count towards paren_depth.
	u32 level_of_argument_source = ex.stack->len;

	collapseMacroStack(ex.stack, &level_of_argument_source);

	while (is_argument || ex.stack->len > 0) {
		MacroToken t = takeToken(ex, &level_of_argument_source);

		if (t.tok.kind == Tok_Identifier) {
			retry:;
			Macro *mac;
			if ((mac = ex.tok->symbols.ptr[t.tok.val.symbol_idx].macro) != NULL
				&& !(t.tok.preproc_flags & Painted))
			{
				if (mac->being_replaced) {
					t.tok.preproc_flags |= Painted;
				} else {
					TokenList *arguments = NULL;
					if (mac->is_function_like) {
						collapseMacroStack(ex.stack, &level_of_argument_source);
						MacroToken paren = takeToken(ex, &level_of_argument_source);
						if (paren.tok.kind != Tok_OpenParen) {
							if (!is_argument) resolveIdentToKeyword(ex.tok, &t.tok, ex.expansion_start, ex.source.plain_name);

							appendOneToken(dest, t.tok, (TokenLocation) {
								.source = t.loc, .macro = ex.expansion_start,
							});
							t = paren;
							if (paren.tok.kind == Tok_Identifier)
								goto retry;
							else
								goto non_macro;
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
			}

		} else {
			non_macro:
			if (t.tok.kind == Tok_EOF) {
				if (is_argument) {
					// TODO Use location of the beginning of the invocation.
					expansionError(ex, ex.expansion_start,
							"missing closing parenthesis of macro invocation");
				} else {
					return false;
				}
			}
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
				default:
					break;
				}
			}
		}

		if (!is_argument && t.tok.kind == Tok_Identifier) resolveIdentToKeyword(ex.tok, &t.tok, ex.expansion_start, ex.source.plain_name);
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
	if (mac->parameters_count == 0) {
		u32 dummy_mark = 0;
		collapseMacroStack(ex.stack, &dummy_mark);
		MacroToken t = takeToken(ex, &dummy_mark);
		assert(t.tok.kind != Tok_EOF);
		if (t.tok.kind != Tok_CloseParen) {
			expansionError(ex, invocation_loc,
					"too many arguments provided");
		}

		return NULL;
	}
	TokenList *argument_bufs = calloc(mac->parameters_count, sizeof(TokenList));
	u32 param_idx = 0;

	u32 expected_count = mac->parameters_count;
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
static MacroToken concatenate(const ExpansionParams ex, MacroToken first, u32 *marker);
static Token stringify(Arena *arena, Tokenization *tok, TokenList t);

static MacroToken takeToken (const ExpansionParams ex, u32 *marker) {
	MacroStack *stack = ex.stack;
	while (stack->len) {
		Replacement *repl = &stack->ptr[stack->len - 1];
		Macro *macro = repl->mac;
		if (macro) {
			assert(repl->pos < macro->tokens.len);

			MacroToken t = macro->tokens.ptr[repl->pos];
			repl->pos++;

			if (t.parameter) {
				if (t.tok.kind == Tok_PreprocDirective) {
					TokenList arg = repl->toks[t.parameter-1];
					t.tok = stringify(ex.strings_arena, ex.tok, arg);
					if (t.followed_by_concat)
						return concatenate(ex, t, marker);
					return t;
				}
				Replacement arg = {
					.toks = &repl->toks[t.parameter-1],
					.followed_by_concat = t.followed_by_concat,
				};
				if (arg.toks->count) {
					if (*marker == stack->len)
						(*marker)++;
					PUSH(*stack, arg);
				}
			} else {
				if (t.followed_by_concat)
					return concatenate(ex, t, marker);
				else {
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
				return concatenate(ex, t, marker);
			} else
				return t;
		}
		collapseMacroStack(stack, marker);
	}

	bool have_space = tryGobbleSpace(ex.source, ex.loc, ex.src);
	Location token_loc = *ex.loc;
	Token t = getToken(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, ex.src);
	if (have_space) t.preproc_flags |= PrecededBySpace;

	return (MacroToken) {
		.tok = t,
		.loc = token_loc,
	};
}

void strPrintToken(char **dest, const char *end, const Symbol *symbols, Token t);
u32 strPrintTokenLen(Token t, Symbol *symbols);

static MacroToken concatenate (const ExpansionParams ex, MacroToken first, u32 *marker) {
	Replacement *repl = &ex.stack->ptr[ex.stack->len - 1];
	Macro *macro = repl->mac;

	MacroToken second = macro->tokens.ptr[repl->pos];
	repl->pos++;

	while (second.parameter) {
		Replacement arg = {
			.toks = &repl->toks[second.parameter-1],
			.pos = 1,
		};
		if (arg.toks->count == 0) {
			if (second.followed_by_concat) {
				second = macro->tokens.ptr[repl->pos];
				repl->pos++;
				continue;
			}
			return first;
		}
		bool param_followed_by_concat = second.followed_by_concat;

		// Take to the first token from the argument for concatenation,
		// push the rest of the argument onto the
		// replacement stack.
		second = toMacroToken(arg.toks, 0);

		if (arg.toks->count > 1) {
			if (*marker == ex.stack->len)
				(*marker)++;
			PUSH(*ex.stack, arg);
		} else {
			second.followed_by_concat = param_followed_by_concat;
		}
	}
	collapseMacroStack(ex.stack, marker);

	if (second.followed_by_concat)
		second = concatenate(ex, second, marker);


	Symbol *syms = ex.tok->symbols.ptr;
	u32 len = strPrintTokenLen(first.tok, syms) + strPrintTokenLen(second.tok, syms) + 1;
	char *rep = aalloc(ex.strings_arena, len);
	char *insert = rep;
	strPrintToken(&insert, rep+len, syms, first.tok);
	strPrintToken(&insert, rep+len, syms, second.tok);
	assert (insert < rep + len);
	*insert = 0;

	const char *src = rep;
	MacroToken t = {
		.tok = getToken(ex.strings_arena, ex.source, ex.loc, &ex.tok->symbols, &src),
		.loc = second.loc,
	};
	if (*src != 0) {
		fprintf(stderr, "{%s}\n", rep);
		expansionError(ex, t.loc,
				"concatenation did not produce a single token"); // TODO Better location
	}
	return t;
}

static Token stringify (Arena *arena, Tokenization *t, TokenList arg) {
	u32 data_len = 0;
	for (u32 i = 0; i < arg.count; i++) {
		data_len += strPrintTokenLen(arg.tokens[i], t->symbols.ptr);
		if (arg.tokens[i].preproc_flags & PrecededBySpace)
			data_len++;
	}

	char *start = aalloc(arena, data_len + 1);
	char *c = start;
	char *end = start + data_len + 1;
	Symbol *syms = t->symbols.ptr;
	for (u32 i = 0; i < arg.count; i++) {
		if (i > 0 && (arg.tokens[i].preproc_flags & PrecededBySpace))
			printto(&c, end, " ");

		strPrintToken(&c, end, syms, arg.tokens[i]);
	}

	u32 idx = getSymbolId(&t->symbols, (String) {c-start, start});
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
				for (u32 i = 0; i < macro->parameters_count; i++) {
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
		.loc = tok->positions[pos].source,
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

inline static void resolveIdentToKeyword (Tokenization *t, Token *tok, Location loc, String name) {
	int keyword = t->symbols.ptr[tok->val.symbol_idx].keyword;
	if (!keyword)
		return;

	if (keyword == Tok_Key_File) {
		tok->kind = Tok_String;
		tok->val.symbol_idx = getSymbolId(&t->symbols, name);
	} else if (keyword == Tok_Key_Line) {
		tok->kind = Tok_IntegerReplaced;
		tok->literal_type = Int_int;
		tok->val.integer = loc.line;
	} else {
		tok->kind = keyword;
	}
}

static String restOfLine (SourceFile source, Location *loc, const char **pos) {
	if (tryGobbleSpace(source, loc, pos) == Space_Linebreak)
		lexerror(source, *loc, "expected a value befor the end of line");
	const char *start = *pos;
	while (**pos && **pos != '\n')
		(*pos)++;
	size_t len = *pos - start;
	if ((*pos)[-1] == '\r') len--;
	return (String) {len, start};
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
		if (pos[0] == '\\' && isLinebreak(pos + 1)) {
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

static bool followedByOpenParen(const char *p) {
	while (*p) {
		if (p[0] == '\\' && isLinebreak(p + 1)) {
			p += 2;
		} else if (p[0] == '/' && p[1] == '/') {
			// TODO Check for C99
			while (*p && *p != '\n')
				p++;
		} else if (p[0] == '/' && p[1] == '*') {
			while (*p && !(p[0] == '*' && p[1] == '/'))
				p++;
			p += 2;
		} else {
			if (!isSpace(*p))
				return *p == '(';
			p++;
		}
	}
	return false;
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
			preprocExpandLine(ex, eval_buf);
			if (preprocExpression(ex, eval_buf, NULL))
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
			while (*pos && isAlnum(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive) || eql("ifndef", directive)) {
				while (*pos && *pos != '\n') pos++;
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
			} else {
				while (*pos && *pos != '\n') pos++;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && isLinebreak(pos + 1)) {
				pos += 2;
				if (*pos == '\n') pos++;
				newLine(loc);
				line_begin = pos;
			} else if (pos[0] == '/' && pos[1] == '*') {
				while (*pos && !(pos[0] == '*' && pos[1] == '/')) {
					if (*pos == '\n') {
						at_start_of_line = true;
						newLine(loc);
						line_begin = pos + 1;
					}
					pos++;
				}
				if (*pos) pos++;
			} else {
				at_start_of_line = false;
			}
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
	bool got_space = false;
	const char *pos = *p;
	const char *line_begin = pos;
	while (*pos && *pos != '\n') {
		if (!isSpace(*pos)) {
			if (pos[0] == '\\' && isLinebreak(pos + 1)) {
				pos += 2;
				if (*pos == '\n') pos++;
				newLine(loc);
				line_begin = pos;
				got_space = true;
				continue;
			} else if (pos[0] == '/' && pos[1] == '/') {
				while (*pos && *pos != '\n') pos++;
				break;
			} else if (pos[0] == '/' && pos[1] == '*') {
				while (*pos && !(pos[0] == '*' && pos[1] == '/')) {
					pos++;
					if (*pos == '\n') {
						pos++;
						newLine(loc);
						line_begin = pos;
					}
				}
				pos++;
			} else
				break;
		}
		got_space = true;
		pos++;
	}
	loc->column += pos - line_begin;
	*p = pos;
	return got_space;
}


static void preprocExpandLine (ExpansionParams params, TokenList *buf) {
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
			// 6.10.1-13
			// TODO Implement __has_include, __has_embed, __has_c_attribute.
			if (eql("defined", sym->name)) {
				tok = getTokenSpaced(str_arena, params.source, params.loc, &params.tok->symbols, &pos);
				bool parenthesized = tok.kind == Tok_OpenParen;
				if (parenthesized)
					tok = getTokenSpaced(str_arena, params.source, params.loc, &params.tok->symbols, &pos);

				if (tok.kind != Tok_Identifier)
					lexerror(params.source, *params.loc, "the operator %sdefined%s expects an identifier as an argument", BOLD, RESET);

				bool found = params.tok->symbols.ptr[tok.val.symbol_idx].macro;
				tok = (Token) {Tok_IntegerReplaced, .val.integer = found};

				if (parenthesized) {
					gobbleSpaceToNewline(&pos, params.loc);
					if (*pos == '\n' || *pos == 0 || getToken(str_arena, params.source, params.loc, &params.tok->symbols, &pos).kind != Tok_CloseParen)
						lexerror(params.source, *params.loc, "missing closing parenthesis");
				}
			} else if (sym->macro) {
				TokenList *arguments = NULL;
				ExpansionParams sub = params;
				sub.src = &pos;

				if (sym->macro->is_function_like) {
					gobbleSpaceToNewline(&pos, params.loc);
					if (pos[0] == '(') {
						pos++;
						params.loc->column++;
						arguments = takeArguments(sub, begin, sym->macro); // Freed by expand_into.
					} else {
						goto non_macro_ident;
					}
				}

				PUSH(*params.stack, ((Replacement) {
					params.tok->symbols.ptr[tok.val.symbol_idx].macro,
					.loc = begin,
					.toks = arguments,
				}));
				expandInto(sub, buf, false);
				continue;
			}

		}
		non_macro_ident:
		if (tok.kind == Tok_Identifier) resolveIdentToKeyword(params.tok, &tok, begin, params.source.plain_name);
		appendOneToken(buf, tok, (TokenLocation) {begin});
	}

	for (u32 i = 0; i < buf->count; i++) {
		Token *tok = &buf->tokens[i];
		if (tok->kind == Tok_Identifier || (tok->kind >= Tok_Key_First && tok->kind <= Tok_Key_Last)) {
			tok->kind = Tok_IntegerReplaced;
			tok->literal_type = Int_int;
			tok->val.integer = 0;
			if (tok->kind == Tok_Key_True)
				tok->val.integer = 1;
		}
	}
	*params.src = pos;
	appendOneToken(buf, (Token) {Tok_EOF}, (TokenLocation) {*params.loc});
}


static String includeFilename (ExpansionParams params, TokenList *buf, bool *quoted) {
	char delimiter;
	const char *pos = *params.src;

	SourceFile source = params.source;
	Location begin = params.expansion_start;

	if (pos[0] == '\"') {
		delimiter = '\"';
		*quoted = true;
	} else if (pos[0] == '<') {
		delimiter = '>';
		*quoted = false;
	} else {
		preprocExpandLine(params, buf);
		if (buf->count != 2 || buf->tokens[0].kind != Tok_String)
			lexerror(source, begin, "#include expects <FILENAME> or \"FILENAME\"");
		*quoted = true;
		return params.tok->symbols.ptr[buf->tokens[0].val.symbol_idx].name;
	}

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
	String res = {pos - start, start};
	pos++;
	params.loc->column += pos - start + 1;

	gobbleSpaceToNewline(&pos, params.loc);
	*params.src = pos;

	if (pos[0] && !isLinebreak(pos))
		lexerror(source, *params.loc, "extra text after file name");
	return res;
}

// If the end argument is NULL, expect the expression to cover the
// entire line; otherwise, write the end of the line to *end.
static u64 preprocExpression (ExpansionParams params, TokenList *buf, const Token **end) {
	Tokenization tok = *params.tok;
	tok.list = *buf;

	const Token *pos = tok.list.tokens;
	// TODO Is the strings_arena appropriate here?
	u64 res = evalPreprocExpression(tok, params.strings_arena, params.opt, &pos);
	if (end) {
		*end = pos;
	} else if (pos->kind != Tok_EOF) {
		u32 off = pos - tok.list.tokens;
		Location loc = buf->positions[off].source;
		lexerror(*params.tok->files.ptr[loc.file_id], loc, "extra tokens after expression");
	}
	return res;
}


// === Helpers ===

// TODO Come up with a better system for nice highlighting.


// FIXME This should be a static local variable, but that causes a
// linker error in self-hosted.
static char name[256] = { 0 };
const char *tokenNameHighlighted (TokenKind kind) {
	static const char *const name_end = name + 256;
	switch (kind) {
	case Tok_Identifier: return "identifier";
	case Tok_Real: return "floating-point-literal";
	case Tok_Integer: return "integer-literal";
	case Tok_Char: return "character-literal";
	case Tok_String: return "string-literal";
	case Tok_PreprocDirective: return "preprocessor-directive";
	default:;
		char *c = name;
		printto(&c, name_end, "%s%s%s", BOLD, tokenName(kind), RESET);
		return name;
	}
}

const char *tokenName (TokenKind kind) {
	const char *res = token_names[kind];
	assert(res);
	return res;
}

u32 strPrintTokenLen (Token t, Symbol *syms) {
	switch (t.kind) {
	case Tok_Intrinsic:
	case Tok_Identifier: return syms[t.val.symbol_idx].name.len;
	case Tok_PreprocDirective: return syms[t.val.symbol_idx].name.len + 1;
	case Tok_String: return syms[t.val.symbol_idx].name.len * 2 + 2;
	case Tok_Integer:
	case Tok_Real:
	case Tok_Char:
		return t.literal_len;
	case Tok_IntegerReplaced: return 30;
	default:
		return strlen(tokenName(t.kind));
	}
}

static void strPrintChar (char **dest, u32 c) {
	char de_escaped = de_escape_codes[c];
	// TODO Handle multibyte characters.
	if (de_escaped) {
		**dest = '\\';
		++*dest;
		**dest = de_escaped;
	} else {
		**dest = c;
	}
	++*dest;
}

void strPrintToken (char **dest, const char *end, const Symbol *symbols, Token t) {
	switch (t.kind) {
	case Tok_Invalid:
		**dest = t.val.invalid_token;
		++*dest;
		return;
	case Tok_PreprocDirective:
		**dest = '#';
		++*dest;
		FALLTHROUGH;
	case Tok_Intrinsic:
	case Tok_Identifier:
		printto(dest, end, "%.*s", STR_PRINTAGE(symbols[t.val.symbol_idx].name));
		return;
	case Tok_String: {
		**dest = '\"';
		++*dest;
		String data = symbols[t.val.symbol_idx].name;
		// TODO Correct this to printable characters only.
		foreach (i, data)
			strPrintChar(dest, (uchar) data.ptr[i]);
		**dest = '\"';
		++*dest;
	} return;
	case Tok_Real:
	case Tok_Integer:
	case Tok_Char: {
		printto(dest, end, "%.*s", STR_PRINTAGE(literalText(t)));
	} return;

	case Tok_IntegerReplaced:
		printto(dest, end, "%lld", (llong) t.val.integer);
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

static inline bool isOctDigit (char c) {
	return c >= '0' && c <= '7';
}

static inline int hexToInt (char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	c |= 32;
	assert(c >= 'a' && c <= 'f');
	return c - 'a' + 10;
}

static inline bool isDigit (char c) {
	return c >= '0' && c <= '9';
}

static inline bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}

static inline bool isLinebreak (const char *c) {
	return c[0] == '\n' || (c[0] == '\r' && c[1] == '\n');
}

static const char *defineMacro (
	Arena *arena,
	Arena *generated_strings,
	SymbolList *symbols,
	String name,
	SourceFile *source,
	Location *loc,
	const char *pos,
	bool gnu)
{
	Macro *mac = ALLOC(arena, Macro);
	*mac = (Macro) {name, source};
	Symbol *sym = getSymbol(symbols, name);
	if (sym->macro)
		free(sym->macro->tokens.ptr);
	sym->macro = mac;
	LIST(u32) params = {0};

	if (pos[0] == '(') {
		pos++;
		loc->column++;

		while (true) {
			gobbleSpaceToNewline(&pos, loc);
			Location l = *loc;
			if (*pos == '\n' || *pos == 0)
				lexerror(*source, l, "expected a token before the end of the line");
			Token t = getToken(generated_strings, *source, loc, symbols, &pos);

			if (t.kind != Tok_Identifier && t.kind != Tok_TripleDot) {
				if (t.kind == Tok_CloseParen && params.len == 0)
					break;
				lexerror(*source, *loc, "the parameters of function-like macros must be valid identifiers");
			}

			u32 sym = 0;
			if (t.kind == Tok_Identifier) {
				sym = t.val.symbol_idx;
			} else {
				mac->is_vararg = true;
				sym = special_identifiers_idx[Special_VA_ARGS];
			}

			Token next = getTokenSpaced(generated_strings, *source, loc, symbols, &pos);
			if (next.kind == Tok_TripleDot && gnu && !mac->is_vararg) {
				mac->is_vararg = true;
				next = getTokenSpaced(generated_strings, *source, loc, symbols, &pos);
			}

			PUSH(params, sym);
			if (symbols->ptr[sym].macro_param != 0)
				lexerror(*source, l, "macro parameters may not be duplicated");
			symbols->ptr[sym].macro_param = params.len;

			if (next.kind == Tok_CloseParen) {
				break;
			} else if (next.kind == Tok_Comma) {
				if (mac->is_vararg)
					lexerror(*source, *loc, "an ellipsis may only appear as the last parameter to a variadic macro");
			} else {
				lexerror(*source, *loc, "expected a comma or closing paren");
			}
		}

		mac->parameters_count = params.len;
		mac->is_function_like = true;
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
		if (t.tok.kind == Tok_PreprocConcatenate) {
			if (macro_assembly.len == 0)
				lexerror(*source, t.loc, "a ## preprocessing token may not occur at the beginning of a macro definition");
			if (!LAST(macro_assembly).followed_by_concat) {
				LAST(macro_assembly).followed_by_concat = true;
				continue;
			}
		}
		if (havespace)
			t.tok.preproc_flags |= PrecededBySpace;
		if (t.tok.kind == Tok_EOF)
			break;

		if (params.len && (t.tok.kind == Tok_Identifier || t.tok.kind == Tok_PreprocDirective)) {
			Symbol *sym = &symbols->ptr[t.tok.val.symbol_idx];
			if (sym->macro_param)
				t.parameter = sym->macro_param;
		}
		PUSH(macro_assembly, t);
	}

	if (macro_assembly.len && LAST(macro_assembly).followed_by_concat)
		lexerror(*source, *loc, "a ## preprocessing token may not occur at the end of a macro definition");

	mac->tokens.ptr = macro_assembly.ptr;
	mac->tokens.len = macro_assembly.len;

	foreach(i, params) symbols->ptr[params.ptr[i]].macro_param = 0;

	free(params.ptr);
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
	MacroDefiners to_define,
	SourceKind kind,
	bool gnu)
{
	foreach (i, to_define) {
		SourceFile *source = calloc(sizeof(SourceFile), 1);
		source->kind = kind;
		source->idx = files->len;
		PUSH(*files, source);

		MacroDefiner def = to_define.ptr[i];

		source->plain_name = zstr(def.name);
		source->content = zstr(def.content);

		Location loc = {source->idx, 1, 1};
		defineMacro(arena, genrated_strings, symbols, source->plain_name, source, &loc, source->content.ptr, gnu);
	}
}



static void markMacroDecl (FILE *dest, SourceFile source, Location loc, String macro_name) {
	String name = sourceName(&source);
	fprintf(dest, "%.*s:%lu:%lu:macro:%.*s:\n", STR_PRINTAGE(name),
			(unsigned long) loc.line, (unsigned long) loc.column, STR_PRINTAGE(macro_name));
}



static u32 lexEscapeCode (SourceFile *source, Location loc, const char **p) {
	const char *pos = *p;
	assert(pos[0] == '\\');
	pos++;
	u32 res;
	if (pos[0] == 'x') {
		pos++;
		if (!isHexDigit(pos[0]))
			lexerror(*source, loc, "invalid escape code");
		res = hexToInt(pos[0]);
		while (isHexDigit(pos[1])) {
			res = res * 16 + hexToInt(pos[1]);
			pos++;
		}
	} else if (isOctDigit(pos[0])) {
		res = pos[0] - '0';
		if (isOctDigit(pos[1])) {
			res = res * 8 + pos[1] - '0';
			pos++;
			if (isOctDigit(pos[1])) {
				res = res * 8 + pos[1] - '0';
				pos++;
			}
		}
	} else {
		res = escape_codes[(uchar)pos[0]];
		if (res == 0)
			lexerror(*source, loc, "invalid escape code");
	}
	*p = pos;
	return res;
}

static String processStringLiteral (SourceFile *file, Location loc, Arena *arena, String src) {
	char *res = aalloc(arena, src.len + 1);
	char *dest = res;

	const char *end = src.ptr + src.len;
	for (const char *p = src.ptr; p < end; p++) {
		if (*p == '\\') {
			*dest = lexEscapeCode(file, loc, &p);
		} else {
			*dest = *p;
		}
		dest++;
	}
	return (String) {dest - res, res};
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

static SourceFile *tryOpen (StringMap *sources, FileList *files, String dir, String name) {
	u32 full_len = dir.len + name.len;
	char *full_path = malloc(full_len + 1);
	memcpy(full_path, dir.ptr, dir.len);
	memcpy(full_path + dir.len, name.ptr, name.len);
	full_path[full_len] = 0;

#if HAVE_POSIX
	char *resolved = realpath(full_path, NULL);
	if (!resolved) {
		free(full_path);
		return NULL;
	}
#elif HAVE_WINDOWS
	char *resolved = _fullpath(NULL, full_path, _MAX_PATH);
	if (!resolved) {
		free(full_path);
		return NULL;
	}
#else
	char *resolved = mdupe(full_path, strlen(full_len) + 1)
#endif

	String resolved_str = zstr(resolved);
	void **entry = mapGetOrCreate(sources, resolved_str);
	SourceFile *file = *entry;
	if (file) {
		free(full_path);
		free(resolved);
		file->included_count++;
	} else {
		file = readAllAlloc(resolved_str);
		if (file) {
			*entry = file;

			u32 slash = full_len;
			while (slash > 0) {
				slash--;
				if (slash > 0 && isDirSeparator(full_path[slash-1])) break;
			}
			file->kind = Source_Regular;
			file->plain_name = (String) {full_len, full_path};
			file->plain_path = (String) {slash, full_path};
			file->idx = files->len;
			PUSH(*files, file);
		} else {
			free(full_path);
			free(resolved);
			mapRemove(sources, entry);
		}
	}

	return file;

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
	relAssert(name.ptr);
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

static Location sourceOf (TokenLocation loc) {
	if (loc.macro.file_id)
		return loc.macro;
	else
		return loc.source;
}

static void escapeTo(FILE *dest, u32 c) {
	char de_escaped = de_escape_codes[c];
	// TODO Handle multibyte characters.
	// PERFORMANCE Buffer this output.
	if (de_escaped) {
		fputc('\\', dest);
		fputc(de_escaped, dest);
	} else {
		fputc(c, dest);
	}
}

// STYLE PERFORMANCE This is mostly a duplicate of strPrintToken. The
// only significant difference is the form of symbol references;
// dispatching on that level seems very wasteful though. OTOH, this is
// probably not performance-sensitive.
void emitPreprocessed (Tokenization *tok, FILE *dest) {
	TokenList list = tok->list;
	Location prev = {0};

	for (u32 i = 0; i < list.count-1; i++) {
		Location loc = sourceOf(list.positions[i]);
		if (loc.file_id != prev.file_id) {
			SourceFile *newfile = tok->files.ptr[loc.file_id];
			String name = sourceName(newfile);
			fprintf(dest, "\n#line %llu \"%.*s\"\n", (ullong) loc.line, STR_PRINTAGE(name));
			prev = loc;
		} else if (loc.line != prev.line) {
			if (loc.line > prev.line && loc.line < prev.line + 6) { // Arbitrary.
				for (u32 i = prev.line; i < loc.line; i++)
					fprintf(dest, "\n");
			} else {
				fprintf(dest, "\n#line %llu\n", (ullong) loc.line);
			}
			prev = loc;
		} else {
			// TODO Try to hit the correct column.
			fprintf(dest, " ");
		}

		Token t = list.tokens[i];

		switch (t.kind) {
		case Tok_PreprocDirective:
			fprintf(dest, "#");
			FALLTHROUGH;
		case Tok_Intrinsic:
		case Tok_Identifier:
			fprintf(dest, "%.*s", STR_PRINTAGE(t.val.symbol->name));
			break;
		case Tok_String: {
			fputc('\"', dest);
			String str = t.val.symbol->name;
			foreach (i, str)
				escapeTo(dest, str.ptr[i]);
			fputc('\"', dest);
		} break;
		case Tok_Real:
		case Tok_Integer:
		case Tok_Char:
			fprintf(dest, "%.*s", STR_PRINTAGE(literalText(t)));
			break;
		case Tok_IntegerReplaced:
			fprintf(dest, "%llu", (ullong)t.val.integer);
			break;

		default:
			fprintf(dest, "%s", tokenName(t.kind));
			break;
		}
	}
	fprintf(dest, "\n");
}
