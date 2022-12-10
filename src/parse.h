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


// PERFOMANCE Reduce size
typedef struct {
	Type typ;

	IrRef inst;
	// ReferenceClass. ir holds the direct value only for
	// Reference_RValue, pointer to the actual value otherwise.
	u8 category;
} Value;

typedef enum {
	Sym_Typedef,
	Sym_EnumConstant,
	// Allocated on the stack
	Sym_Value_Auto,
	// Statically allocated
	Sym_Value_Static,
} SymbolKind;

typedef struct OrdinaryIdentifier OrdinaryIdentifier;

struct OrdinaryIdentifier {
	SymbolKind kind;
	OrdinaryIdentifier *shadowed;
	u32 scope_depth;
	const Token *decl_location;
	const Token *def_location;

	Type typedef_type;

	u32 static_id;

	Value value;

	i32 enum_constant;
};

typedef struct NameTaggedType NameTaggedType;

struct NameTaggedType {
	String name;
	NameTaggedType *shadowed;
	const Token *def_location;

	u32 scope_depth;

	Type type;
};

typedef struct Symbol Symbol;
typedef struct Symbol {
	String name;

	OrdinaryIdentifier *ordinary;
	NameTaggedType *nametagged;

	struct {
		Block *block;
		const Token *def_location;
	} label;
} Symbol;

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

	String value_data;
	References value_references;

	IrList function_ir;
	Block *function_entry;
} StaticValue;

typedef LIST(StaticValue) Module;


static inline bool isByref(Value val) { return val.category; }
static inline bool isLvalue(Value val) { return val.category != Ref_RValue; }

void parse(Arena *arena, Tokenization tokens, Options opt, Module *module);


