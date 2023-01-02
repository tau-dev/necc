#pragma once

// Types that the lexer and the parser need to know.

typedef enum {
	I8 = 1,
	I16 = 2,
	I32 = 4,
	I64 = 8,
	I128 = 16,
} PrimitiveSize;

typedef struct Target {
	Type ptrdiff;
	Type intptr;
	BasicType enum_int;
	PrimitiveSize ptr_size;
	PrimitiveSize int_size;
	PrimitiveSize valist_size;
	PrimitiveSize typesizes[Int_unsigned];
	Version version;
} Target;


typedef struct {
	Target target;
	bool crash_on_error;
	bool gen_debug;

	bool warn_on_wrapping;
	bool warn_char_subscript;
	bool warn_compare;
	bool warn_unused;

	bool emitted_warnings;
	bool error_on_warnings;
} Options;



typedef struct OrdinaryIdentifier OrdinaryIdentifier;
typedef struct NameTaggedType NameTaggedType;
typedef struct Macro Macro;
typedef struct Block Block;
typedef struct Token Token;

typedef struct Symbol {
	String name;

	int keyword;
	int directive;
	Macro *macro;

	OrdinaryIdentifier *ordinary;
	NameTaggedType *nametagged;

	struct {
		Block *block;
		const Token *def_location;
	} label;
} Symbol;

typedef LIST(Symbol) SymbolList;


typedef enum {
	Intrinsic_VaList,
	Intrinsic_VaStart,
	Intrinsic_VaEnd,
	Intrinsic_VaArg,
	Intrinsic_VaCopy,

	Intrinsic_Nanf,
	Intrinsic_Inff,
} Intrinsic;
