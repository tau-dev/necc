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

	Ir_Truncate,
	Ir_SignExtend,
	Ir_IntToFloat,
	Ir_FloatToInt,

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

typedef enum {
	I8,
	I16,
	I32,
	I64,
	I128,
} Size;

// typedef SPAN(PhiNode) PhiNodes;

typedef struct Inst {
	u8 kind;
	u8 size;

	union {
		u64 constant;
		Call call;
		LIST(IrRef) phi;
		Function *funcref;
		IrRef unop;
		struct {
			IrRef lhs;
			IrRef rhs;
		} binop;
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

// TODO The IR is untyped, this really belongs somewhere else.

typedef struct Type Type;
typedef struct Declaration Declaration;

typedef LIST(Declaration) DeclList;

typedef enum {
	Int_bool,
	Int_char,
	Int_suchar, // signed or unsigned (plain char is neither!)
	Int_short,
	Int_int,
	Int_long,
	Int_longlong,
	Int_unsigned = 8,
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
	String name;
	FunctionType type;
	Block *entry;
	IrList ir;
	StringMap labels; // Maps labels to Blocks
};

typedef struct Declaration {
	String name;
	Type type;
} Declaration;

typedef struct {
	Type ptrdiff;
	Type intptr;
	int typesizes[Int_unsigned];
} Target;

bool typeEqual(Type a, Type b);
bool fnTypeEqual(FunctionType a, FunctionType b);
u32 typeSize(Type t, const Target *target);
char *printDeclaration(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
void printTypeBase(Type t, char **insert, char *end);
void printTypeHead(Type t, char **insert, char *end);
void printTypeTail(Type t, char **insert, char *end);

#define BASIC_VOID ((Type) {Kind_Void})
#define BASIC_INT ((Type) {Kind_Basic, .basic = Int_int})
#define INT_SIZE I32

