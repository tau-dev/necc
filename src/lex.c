#include <stdbool.h>
#include <ctype.h>
#include <stdio.h>

#include "lex.h"


typedef struct {
	int a;
} Macro;

struct Lexer {
	StringMap map;
};


#define KEYWORDS Tok_Key_Last - Tok_Key_First + 1
struct { char *name; TokenKind key; } names[KEYWORDS] = {
// 	{"or", Keyword_Or},
// 	{"and", Keyword_And},
	{"if", Tok_Key_If},
	{"else", Tok_Key_Else},
	{"goto", Tok_Key_Goto},
	{"while", Tok_Key_While},
	{"do", Tok_Key_Do},
	{"for", Tok_Key_For},
	{"return", Tok_Key_Return},
	{"#define", Tok_Key_Define},
	{"typedef", Tok_Key_Typedef},
};

Token fromWord (String word) {
	for (int i = 0; i < KEYWORDS; ++i) {
		if (eql(names[i].name, word)) {
			Token ret = {0};
			ret.kind = names[i].key;
			return ret;
		}
	}
	return (Token) {Tok_Identifier, {.identifier = word}};
}

Token peekToken (const char *c) {
	return getToken (&c);
}

Token getToken (const char **p) {
	Token tok = {0};
	const char *pos = *p;

	bool line_begin = 0;
	(void) line_begin;
	while (isspace(*pos)) {
		if (*pos == '\n')
			line_begin = true;
		pos++;
	}

	char c = *pos;
	switch (*pos) {
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
		} else {
			tok.kind = Tok_Plus;
		}
		break;
	case '-':
		if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_Arrow;
		} else if (pos[1] == '=') {
			tok.kind = Tok_MinusEquals;
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
		if (pos[1] == '|') {
			pos++;
			tok.kind = Tok_DoublePipe;
		} else {
			tok.kind = Tok_Pipe;
		}
		break;
	case '\0': tok.kind = Tok_EOF; pos--; break;
	default:
		if (isalpha(c)) {
			const char *start = pos;
			while (isalnum(*pos))
				pos++;

			tok = fromWord((String) {.len = pos - start, .ptr = start});

			pos--;
		} else if (isdigit(c)) {
			const char *start = pos;

			while (isdigit(*pos))
				pos++;

			if (*pos == '.') {
				pos++;
				while (isdigit(*pos))
					pos++;
				if (*pos == 'e' || *pos == 'E')
					pos++;
				if (*pos == '-' || *pos == '+')
					pos++;
				while (isdigit(*pos))
					pos++;

				tok = (Token) {Tok_Real, {.real = strtod(start, NULL)}};
			} else {
				tok = (Token) {Tok_Integer, {.integer = strtoll(start, NULL, 10)}};
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

Token_List lex(const char *code) {
	Token_List t = MAKE_LIST(Token);
	const char *pos = code;

	while (isspace(*pos))
		pos++;

	while (*pos) {
		Token tok = getToken(&pos);
		PUSH(t, tok);
	}

	return t;
}


const char *token_names[Tok_EOF+1] = {
	[Tok_Identifier] = "identifier",
	[Tok_Integer] = "integer literal",
	[Tok_Real] = "floating point literal",
	[Tok_String] = "string literal",

	[Tok_OpenParen] = "`(`",
	[Tok_CloseParen] = "`)`",
	[Tok_OpenBrace] = "`{`",
	[Tok_CloseBrace] = "`}`",
	[Tok_OpenBracket] = "`[`",
	[Tok_CloseBracket] = "`]`",

	[Tok_Semicolon] = "`;`",
	[Tok_Comma] = "`,`",
	[Tok_Colon] = "`:`",

	[Tok_Equals] = "`=`",
	[Tok_DoubleEquals] = "`==`",
	[Tok_Arrow] = "`->`",
	[Tok_Plus] = "`+`",
	[Tok_PlusEquals] = "`+=`",
	[Tok_Minus] = "`-`",
	[Tok_MinusEquals] = "`-=`",
	[Tok_Asterisk] = "`*`",
	[Tok_AsteriskEquals] = "`*=`",
	[Tok_Slash] = "`/`",
	[Tok_SlashEquals] = "`/=`",
	[Tok_Less] = "`<`",
	[Tok_DoubleLess] = "`<<`",
	[Tok_LessEquals] = "`<=`",
	[Tok_Greater] = "`>`",
	[Tok_DoubleGreater] = "`>>`",
	[Tok_GreaterEquals] = "`>=`",
	[Tok_Pipe] = "`|`",
	[Tok_DoublePipe] = "`||`",
	[Tok_Ampersand] = "`&`",
	[Tok_DoubleAmpersand] = "`&&`",
	[Tok_Hat] = "`^`",
	[Tok_Tilde] = "`~`",

	[Tok_Key_If] = "`if`",
	[Tok_Key_Else] = "`else`",
	[Tok_Key_Goto] = "`goto`",
	[Tok_Key_While] = "`while`",
	[Tok_Key_Do] = "`do`",
	[Tok_Key_For] = "`for`",
	[Tok_Key_Return] = "`return`",
	[Tok_Key_Define] = "`#define`",
	[Tok_Key_Typedef] = "`typedef`",

	[Tok_EOF] = "end of file",
};

static char name[1024] = {0};

const char *tokenName(TokenKind kind) {
	strcpy(name, "unidentified token, id (hacky, only the last emitted one is certain): ");
	char *end = name + strlen(name);

	if (kind >= 0 && kind <= Tok_EOF && token_names[kind])
		return token_names[kind];
	else {
		sprintf(end, "%d", kind);
		return name;
	}
}
