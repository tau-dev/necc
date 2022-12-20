#pragma once
#include "util.h"
#include "types.h"


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
	Tok_Dot,
	Tok_TripleDot,

	Tok_Question,
	Tok_Arrow,
	Tok_DoubleAmpersand,
	Tok_DoublePipe,
	Tok_DoublePlus,
	Tok_DoubleMinus,

	Tok_Key_If,
	Tok_Key_Else,
	Tok_Key_Switch,
	Tok_Key_Case,
	Tok_Key_Goto,
	Tok_Key_While,
	Tok_Key_Do,
	Tok_Key_For,
	Tok_Key_Return,
	Tok_Key_Break,
	Tok_Key_Default,
	Tok_Key_Sizeof,
	Tok_Key_Alignof,
	Tok_Key_Alignas,
	Tok_Key_Typeof,
	Tok_Key_TypeofUnqual,
	Tok_Key_Generic,

	Tok_Key_Struct,
	Tok_Key_Enum,
	Tok_Key_Union,
	Tok_Key_Bool,
	Tok_Key_Char,
	Tok_Key_Int,
	Tok_Key_Void,
	Tok_Key_Float,
	Tok_Key_Double,

	Tok_Key_Long,
	Tok_Key_Short,
	Tok_Key_Signed,
	Tok_Key_Unsigned,
	Tok_Key_Const,
	Tok_Key_Volatile,
	Tok_Key_Restrict,
	Tok_Key_Atomic,

	Tok_Key_Typedef,
	Tok_Key_Auto,
	Tok_Key_Register,
	Tok_Key_Static,
	Tok_Key_Extern,
	Tok_Key_Threadlocal,
	Tok_Key_Inline,
	Tok_Key_Noreturn,

	Tok_Key_VaList,

	Tok_Key_File,
	Tok_Key_Line,

	Tok_Key_First = Tok_Key_If,
	Tok_Key_Last = Tok_Key_Line,

	Tok_PreprocDirective, // Directive or stringified argument.
	Tok_PreprocConcatenate,

	Tok_Equals,
	Tok_Less,
	Tok_Greater,
	Tok_Bang,

	Tok_Plus,
	Tok_Minus,
	Tok_Asterisk,
	Tok_Slash,
	Tok_Percent,
	Tok_DoubleLess,
	Tok_DoubleGreater,
	Tok_Ampersand,
	Tok_Pipe,
	Tok_Hat,
	Tok_Tilde,

	Tok_EQUALED = 0x80,

	Tok_EqualsEquals = Tok_Equals | Tok_EQUALED,
	Tok_LessEquals = Tok_Less | Tok_EQUALED,
	Tok_GreaterEquals = Tok_Greater | Tok_EQUALED,
	Tok_BangEquals = Tok_Bang | Tok_EQUALED,

	Tok_PlusEquals = Tok_Plus | Tok_EQUALED,
	Tok_MinusEquals = Tok_Minus | Tok_EQUALED,
	Tok_AsteriskEquals = Tok_Asterisk | Tok_EQUALED,
	Tok_SlashEquals = Tok_Slash | Tok_EQUALED,
	Tok_PercentEquals = Tok_Percent | Tok_EQUALED,
	Tok_DoubleLessEquals = Tok_DoubleLess | Tok_EQUALED,
	Tok_DoubleGreaterEquals = Tok_DoubleGreater | Tok_EQUALED,
	Tok_AmpersandEquals = Tok_Ampersand | Tok_EQUALED,
	Tok_PipeEquals = Tok_Pipe | Tok_EQUALED,
	Tok_HatEquals = Tok_Hat | Tok_EQUALED,
	Tok_TildeEquals = Tok_Tilde | Tok_EQUALED,

	Tok_EOF,

	Tok_Equalable_Start = Tok_Equals,
	Tok_Assignment_Start = Tok_PlusEquals,
} TokenKind;

const char *tokenName(TokenKind k);
// Dynamically allocates.
// [currently unused]
const char *tokenNameHighlighted(TokenKind k);

typedef struct {
	TokenKind kind;
	u8 preceded_by_space;

	union {
		struct {
			long long integer;
			BasicType int_type;
		} literal;
		String identifier;
		double real;
		String string;
	} val;
} Token;


typedef struct {
	u32 source_file_offset;
	u16 source_file_ref;
	u16 macro_file_ref;
	u32 macro_file_offset;
} TokenPosition;

// All members allocated with malloc
typedef struct {
	LIST(SourceFile*) files;

	Token *tokens;
	TokenPosition *positions;
	u32 count;
	u32 capacity;
} Tokenization;

typedef struct {
	SPAN(String) sys_include_dirs;
	SPAN(String) user_include_dirs;
} Paths;

typedef LIST(Token) Token_List;

Tokenization lex(Arena *generated_strings, String filename, Paths paths, Target *version);
