#pragma once

// #include "ast.h"
#include "arena.h"
#include "ir.h"
#include "lex.h"


typedef enum {
	Sym_Type,
	Sym_Value,
} SymbolKind;

typedef struct {
	Type typ;
	union {
		struct {
			IrRef ir;
			// If true, ir is a pointer to the value. All lvalues are byref, as well as rvalues of structure or union type.
			bool byref;
		};
		// FIXME Type of the function is overdetermined!
		Function *function;
	};
} Value;

typedef struct Symbol Symbol;

typedef struct Symbol {
	String name;
	Symbol *shadowed;
	SymbolKind kind;

	union {
		Value value;
		Type type;
	};
} Symbol;

extern StringMap symbols;
void parse(Arena *arena, Tokenization tokens);



