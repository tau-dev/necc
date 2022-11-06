#pragma once

#include <stddef.h>
#include <stdbool.h>

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
	Ir_ZeroExtend,
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
	Ir_Equals,
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

// TODO Compress the heck out of this data.
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
		struct {
			IrRef source;
			IrRef dest;
			IrRef len;
		} copy;
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
	IrRefList mem_instructions; // Loads, stores, and copys
	IrRefList side_effecting_instructions; // Calls, stores and copys
	Exit exit;
	bool visited;
} Block;


typedef struct IrBuild {
	u32 block_count;
	IrList ir;
	Block *insertion_block;
} IrBuild;

