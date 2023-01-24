#pragma once
#include "util.h"
#include "types.h"
#include "common.h"
#include "common.h"


typedef enum {
	Tok_EOF,

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
	Tok_Key_Continue,
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
	Tok_Key_StaticAssert,

	Tok_Key_File,
	Tok_Key_Line,

	Tok_Key_First = Tok_Key_If,
	Tok_Key_Last = Tok_Key_Line,

	Tok_Intrinsic,

	Tok_Identifier,
	Tok_String,
	Tok_Real,
	Tok_Integer,

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


	Tok_Equalable_Start = Tok_Equals,
	Tok_Assignment_Start = Tok_PlusEquals,

	Tok_END = 0x100,
} TokenKind;

typedef enum {
	Special___func__,
	Special_memcpy,
	Special_malloc,

	Special_COUNT,
} SpecialIdentifier;

STATIC_ASSERT(Tok_EQUALED > Tok_Tilde && !(Tok_EQUALED & Tok_Tilde),
		"Tok_EQUALED must follow all other tokens");

const char *tokenName(TokenKind k);
// Dynamically allocates.
// [currently unused]
const char *tokenNameHighlighted(TokenKind k);


typedef struct Token {
	TokenKind kind;
	u8 preceded_by_space;
	u8 literal_type;

	union {
		i64 integer_s;
		u64 integer_u;
		double real;
		Symbol *symbol;
		u32 symbol_idx; // Only used by the lexer
	} val;
} Token;



typedef struct {
	Location source;
	// Macro this was expanded from, or zero.
	Location macro;
} TokenLocation;

typedef LIST(SourceFile*) FileList;


typedef struct {
	Token *tokens;
	TokenLocation *positions;
	u32 count;
	u32 capacity;
} TokenList;

// All members allocated with malloc
typedef struct Tokenization {
	FileList files;

	TokenList list;

	SymbolList symbols;
	Symbol *special_identifiers[Special_COUNT];
} Tokenization;

typedef struct {
	StringList sys_include_dirs;
	StringList user_include_dirs;

	StringList command_line_macros;
	StringList system_macros;

	Options *options;
} LexParams;

typedef LIST(Token) Token_List;

Tokenization lex(Arena *generated_strings, String filename, LexParams paths);
