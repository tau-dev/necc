#include "util.h"


typedef enum {
	Tok_Identifier,
	Tok_Integer,
	Tok_Real,
	Tok_String,

	Tok_OpenParen,
	Tok_CloseParen,
	Tok_OpenBrace,
	Tok_CloseBrace,
	Tok_OpenBracket,
	Tok_CloseBracket,

	Tok_Semicolon,
	Tok_Comma,
	Tok_Colon,

	Tok_Equals,
	Tok_DoubleEquals,
	Tok_Arrow,
	Tok_Plus,
	Tok_PlusEquals,
	Tok_Minus,
	Tok_MinusEquals,
	Tok_Asterisk,
	Tok_AsteriskEquals,
	Tok_Slash,
	Tok_SlashEquals,
	Tok_Less,
	Tok_LessEquals,
	Tok_DoubleLess,
	Tok_Greater,
	Tok_GreaterEquals,
	Tok_DoubleGreater,
	Tok_Ampersand,
	Tok_DoubleAmpersand,
	Tok_Pipe,
	Tok_DoublePipe,
	Tok_Hat,
	Tok_Tilde,


	Tok_Key_If,
	Tok_Key_Else,
	Tok_Key_Goto,
	Tok_Key_While,
	Tok_Key_Do,
	Tok_Key_For,
	Tok_Key_Return,
	Tok_Key_Typedef,
	Tok_Key_First = Tok_Key_If,
	Tok_Key_Last = Tok_Key_Typedef,

	Tok_PreprocDirective,
	Tok_PreprocConcatenate,

	Tok_EOF,
} TokenKind;


const char *tokenName(TokenKind k);

typedef struct {
	TokenKind kind;

	union {
		String identifier;
// 		KeywordKind keyword;
		long long integer;
		double real;
		String string;
	} val;
} Token;

typedef struct Lexer Lexer;


typedef struct {
	u32 source_file_offset;
	u16 source_file_ref;
	u16 macro_file_ref;
	u32 macro_file_offset;
} TokenPosition;

// All members allocated with malloc
typedef struct {
	LIST(SourceFile) files;
	Token *tokens;
	TokenPosition *positions;
	u32 tokens_count;
} Tokenization;

typedef LIST(Token) Token_List;
Tokenization lex(const char *filename);

