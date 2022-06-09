#pragma once

#include "main.h"
#include "util.h"


struct Type;

typedef struct {
	char *name;
	struct Type *type;
} Variable;

typedef struct {
	SPAN(Variable) items;
} Record;

typedef enum {
	BaseType_U8,
	BaseType_U16,
	BaseType_U32,
	BaseType_I8,
	BaseType_I16,
	BaseType_I32,
	BaseType_Ptr,
} BaseType;


typedef enum {
	Type_Record,
	Type_Named,
	Type_Basic,
} TypeKind;

typedef struct Type {
	TypeKind kind;
	union {
		Record record;
		char *named;
		BaseType basic;
	} val;
} Type;
//=================



typedef enum {
	Statement_Call,
} StatementKind;

typedef struct {
	StatementKind kind;

	union {
		int dummy;
	} val;
} Statement;

typedef struct {
	SPAN(Statement) statements;
} Block;
//=================



typedef struct {
	char *name;
	Type *type;
} Argument;

typedef struct {
	char *name; // in global hash table
	SPAN(Argument) args;
	Block *body;
} FunctionDef;

typedef enum {
	Definition_FunctionDef,
} DefinitionKind;

typedef struct {
	DefinitionKind kind;
	union {
		FunctionDef function;
	} val;
} Definition;
//=================



typedef struct {
	char *name;
	// SPAN(Definition*) definitions;
	struct { size_t len; Definition* *ptr; } definitions;
} Namespace;
