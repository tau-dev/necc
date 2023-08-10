#pragma once

// Types that the lexer and the parser need to know.

typedef enum {
	I8 = 1,
	I16 = 2,
	I32 = 4,
	I64 = 8,
	I128 = 16,
} PrimitiveSize;


typedef enum {
// 	Features_C89 = 0x1,
	Features_C99 = 0x2,
	Features_C11 = 0x4,
	Features_C23 = 0x8,
	Features_GNU_Extensions = 0x10,
	Features_MSVC_Extensions = 0x20,

	Features_OldStyleDefinitions = 0x40,
	Features_DefaultInt = 0x80,
} Features;

typedef enum {
	Version_C89       = Features_OldStyleDefinitions | Features_DefaultInt,
	Version_C99       = Features_C99 | Features_OldStyleDefinitions,
	Version_C17       = Features_C99 | Features_C11 | Features_OldStyleDefinitions,
	Version_C23       = Features_C99 | Features_C11 | Features_C23,
	Version_GNU       = Version_C17 | Features_GNU_Extensions | Features_DefaultInt,
	Version_MSVC      = Version_C17 | Features_MSVC_Extensions,
	Version_Lax       = 0x7fffffff,
} Version;
const char *versionName(Version);


typedef struct Symbol Symbol;

typedef struct Target {
	Type ptrdiff;
	Type intptr;
	BasicType enum_int;
	PrimitiveSize ptr_size;
	PrimitiveSize int_size;
	PrimitiveSize valist_size;
	PrimitiveSize typesizes[Int_unsigned];
	Version version;
	bool link_std;
} Target;


typedef struct {
	Target target;
	bool crash_on_error;
	bool emit_debug;

	bool warn_on_wrapping;
	bool warn_char_subscript;
	bool warn_compare;
	bool warn_unused;

	bool emitted_warnings;
	bool error_on_warnings;

	bool any_decl_emit;
	FILE *emit_decls;
	FILE *emit_all_decls;
	FILE *emit_std_decls;
} Options;



typedef struct OrdinaryIdentifier OrdinaryIdentifier;
typedef struct NameTaggedType NameTaggedType;
typedef struct Macro Macro;
typedef struct Block Block;
typedef struct Token Token;

struct Symbol {
	String name;

	int keyword;
	int directive;
	Macro *macro;
	u32 macro_param;

	OrdinaryIdentifier *ordinary;
	NameTaggedType *nametagged;

	// The "external" (file scope) symbol this refers to.
	bool has_global;
	u32 global_val_id;

	struct {
		Block *block;
		const Token *def_location;
	} label;
};

typedef LIST(Symbol) SymbolList;


typedef enum {
	Intrinsic_VaList,
	Intrinsic_VaStart,
	Intrinsic_VaEnd,
	Intrinsic_VaArg,
	Intrinsic_VaCopy,

	Intrinsic_Nanf,
	Intrinsic_Inff,

	Intrinsic_Clz,
	Intrinsic_Clzll,
	Intrinsic_Ctz,
	Intrinsic_Ctzll,
	Intrinsic_Popcount,
	Intrinsic_Popcountll,

	Intrinsic_Expect,
	Intrinsic_FrameAddress,
	Intrinsic_Alloca,
	Intrinsic_Offsetof,
	Intrinsic_ConstantP,
} Intrinsic;


typedef struct {
	// attribute declaration before case or default label
	int Attribute_Fallthrough: 1;
	// declaration, member, enumerator
	int Attribute_MaybeUnused: 1;
	// declaration, member, enumerator, label
	int Attribute_Deprecated: 1;
	// function
	int Attribute_Noreturn: 1;
	// function, definition of a struct/union/enum
	int Attribute_Nodiscard: 1;
	// function declarator, type specifier that has a function type
	int Attribute_Unsequenced: 1;
	int Attribute_Reproducible: 1;

	// function
	int Attribute_Constructor: 1;
	int Attribute_Destructor: 1;
	// external declaration
	int Attribute_Weak: 1;

	// struct member
	int Attribute_Packed: 1;
	// variable, struct member
	u32 Attribute_Aligned;
	// declaration
	Type mode;

	// variable, type, function
	enum {
		Visibility_default,
		Visibility_hidden,
		Visibility_internal,
		Visibility_protected,
	} Attribute_Visibility;


	// declaration
	Symbol *Attribute_Alias;
	// automatic variable
	Symbol *Attribute_Cleanup;
} Attributes;

typedef struct Tokenization Tokenization;
u64 evalPreprocExpression(Tokenization tokens, Arena *arena, Options *opt, const Token **pos);


typedef struct {
	BasicType type;
	u64 val; // Bits aboove the represented value are 0.
} ConstInt;

// Start from *pos and store the next token after the expression back to
// *pos.
ConstInt evalIntegerConstantExpression(Tokenization *tokens, const char *cause, const Options *opt, const Token **pos);
