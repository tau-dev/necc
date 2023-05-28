#pragma once

// #include "ast.h"
#include "arena.h"
#include "ir.h"
#include "lex_preproc.h"
#include "common.h"


typedef enum {
	Ref_RValue,
	Ref_LValue_Register,
	Ref_LValue,
} Category;


// PERFORMANCE Reduce size
typedef struct {
	Type typ;

	IrRef inst;
	// Category. inst holds the direct value only for
	// Reference_RValue, pointer to the actual value otherwise.
	u8 category;
	u8 no_discard;
} Value;


typedef enum {
	Sym_Typedef,
	Sym_EnumConstant,
	// Allocated on the stack
	Sym_Value_Auto,
	Sym_Value_Static,
} IdentKind;


// An identifier designating a function or object or a typedef, as
// opppsed to a label or a struct/union/enum tag.
typedef struct OrdinaryIdentifier OrdinaryIdentifier;

struct OrdinaryIdentifier {
	IdentKind kind;
	OrdinaryIdentifier *shadowed;

	u32 scope_depth;
	const Token *decl_location;
	const Token *def_location;

	Type typedef_type;

	u32 static_id;

	Value value;

	i32 enum_constant;
	bool is_used;
};

typedef struct NameTaggedType NameTaggedType;

struct NameTaggedType {
	String name;
	NameTaggedType *shadowed;
	const Token *def_location;

	u32 scope_depth;

	Type type;
};


typedef struct {
	u32 splice_pos;

	u32 source_id;
	i64 offset;
} Reference;

typedef SPAN(Reference) References;


typedef struct {
	enum {
		Static_Variable,
		Static_Function,
	} def_kind;
	enum {
		Def_Undefined,
		Def_Tentative,
		Def_Defined,
	} def_state;
	const Token *decl_location;
	// External linkage.
	bool is_public;
	// Whether the value is referred to by any expression other than
	// a non-VLA sizeof or an _Alignof.
	bool is_used;

	Type type;

	String name;
	u32 parent_decl; // IDX_NONE if absent

	String value_data;
	References value_references;

	IrList function_ir;
} StaticValue;

typedef LIST(StaticValue) Module;


// STYLE TODO With IR sizing, lvalues are now the same as byref. Merge these two.
static inline bool isByref(Value val) { return val.category; }
static inline bool isLvalue(Value val) { return val.category != Ref_RValue; }

void parse(Arena *arena, Tokenization tokens, Options *opt, Module *module);


