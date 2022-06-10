#pragma once

#include <stddef.h>
#include <stdbool.h>

#include "main.h"
#include "util.h"
#include "arena.h"

#define IR_REF_NONE ((u32) -1)

typedef struct Block Block;
typedef struct Inst Inst;
typedef struct Function Function;

typedef u32 IrRef;
typedef SPAN(IrRef) ValuesSpan;
typedef LIST(IrRef) IrRefList;
typedef LIST(Inst) IrList;


typedef struct {
	IrRef function_ptr;
	ValuesSpan parameters;
} Call;



typedef enum {
	Ir_Function,
	Ir_Constant,
	Ir_Call,
	Ir_Phi,
	Ir_Parameter,
	Ir_StackAlloc,
	Ir_Load,
	Ir_Store,
	Ir_Add,
	Ir_Sub,
	Ir_Mul,
	Ir_Div,
	Ir_BitAnd,
	Ir_BitOr,
	Ir_BitNot,
	Ir_BitXor,
	Ir_LessThan,
	Ir_LessThanOrEquals,
} InstKind;

// typedef SPAN(PhiNode) PhiNodes;

typedef struct Inst {
	InstKind kind;

	union {
		u64 constant;
		Call call;
		LIST(IrRef) phi;
		Function *funcref;
		IrRef unop;
		struct {
			IrRef lhs;
			IrRef rhs;
		} bin;
	};
} Inst;

typedef struct {
	IrRef condition;
	Block *on_true;
	Block *on_false;
} Branch;

typedef enum {
	Exit_None, // Used for blocks that are currently in construction or that have already been visited.
	Exit_Branch,
	Exit_Unconditional,
	Exit_Return,
} ExitKind;

typedef struct {
	ExitKind kind;

	union {
		Branch branch;
		Block *unconditional;
		IrRef ret;
	};
} Exit;


typedef struct Block {
	String label; // null-terminated
	IrRef first_inst;
	IrRef last_inst; // Used for unoptimized output, irrelevant when scheduler applies
	IrRefList mem_instructions; // Loads and stores
	IrRefList side_effecting_instructions; // Calls and stores
	Exit exit;
	bool visited;
} Block;


typedef struct IrBuild {
	u32 block_count;
	IrList ir;
	Block *insertion_block;
} IrBuild;
//=================

// TODO The IR really is untyped, this belongs somewhere else.

typedef struct Type Type;
typedef struct Declaration Declaration;

typedef LIST(Declaration) DeclList;

typedef enum {
	Basic_int,
	Basic_char,
} BasicType;

typedef enum {
	Kind_Void,
	Kind_Basic,
	Kind_Struct,
	Kind_Function, // Only used for function declarations.
	Kind_FunctionPtr,
	Kind_Pointer,
	Kind_Enum,
	Kind_Array,
} TypeKind;

enum {
	Storage_Auto,
	Storage_Static,
	Storage_Extern,
	Storage_Typedef,
};
enum {
	Qualifier_Const = 1,
	Qualifier_Volatile = 2,
	Qualifier_Restrict = 4,
	Qualifier_Atomic = 8,
};

typedef struct {
	Type *rettype;
	DeclList parameters;
} FunctionType;

typedef struct {
	Type *inner;
	u32 count;
} ArrayType;


typedef struct Type {
	u8 kind;
	u8 storage;
	u8 qualifiers;

	union {
		FunctionType function;
		Type *pointer;
		BasicType basic;
		ArrayType array;
		// TODO Struct etc...
	};
} Type;

struct Function {
	FunctionType type;
	String name;
	Block *entry;
	IrList ir;
	StringMap labels; // Maps labels to Blocks
};

typedef struct Declaration {
	String name;
	Type type;
} Declaration;

bool typeEqual(Type a, Type b);
bool fnTypeEqual(FunctionType a, FunctionType b);
u32 typeSize(Type t);
char *printDeclaration(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
void printTypeBase(Type t, char **insert, char *end);
void printTypeHead(Type t, char **insert, char *end);
void printTypeTail(Type t, char **insert, char *end);

#define BASIC_INT ((Type) {Kind_Basic, .basic = Basic_int})
