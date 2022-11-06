#pragma once

// #include "ast.h"
#include "arena.h"
#include "ir.h"
#include "lex.h"


typedef enum {
	Ref_RValue,
	Ref_RValue_Struct, // TODO Support this
	Ref_LValue_Register,
	Ref_LValue,
} ReferenceClass;

// PERFOMANCE Reduce size
typedef struct {
	Type typ;

	union {
		struct {
			IrRef ir;
			// ReferenceClass. ir holds the direct value only for
			// Reference_RValue, pointer to the actual value otherwise.
			u8 byref;
		};
		// STYLE Type of the function is overdetermined!
		Function *function;
	};
} Value;

typedef enum {
	Sym_Type,
	Sym_Value,
} SymbolKind;

typedef struct Symbol Symbol;
typedef struct Symbol {
	String name;
	Symbol *shadowed;
	SymbolKind kind;
	u8 storage;

	union {
		Value value;
		Type type;
	};
} Symbol;

extern StringMap symbols;
void parse(Arena *arena, Tokenization tokens, Target target);



