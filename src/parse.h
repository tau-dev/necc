#pragma once

// #include "ast.h"
#include "arena.h"
#include "ir.h"


typedef enum {
	Sym_Type,
	Sym_Value,
} SymbolKind;

typedef struct {
	Type typ;
	union {
		struct {
			IrRef ir;
			// If true, ir is a pointer to the value.
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
void parseFile(Arena *arena, const char *code);


char *printDeclaration(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
void printTypeBase(Type t, char **insert, char *end);
void printTypeHead(Type t, char **insert, char *end);
void printTypeTail(Type t, char **insert, char *end);


