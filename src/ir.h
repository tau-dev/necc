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
typedef LIST(Block*) Blocks;


typedef struct {
	IrRef function_ptr;
	ValuesSpan parameters;
} Call;


typedef enum {
	Ir_Reloc,
	Ir_Constant,
	Ir_Call,
	Ir_Parameter,
	Ir_PhiOut, // Not referred to
	Ir_PhiIn,
	Ir_StackAlloc,
	Ir_StackDealloc, // Not referred to
	Ir_Copy,
	Ir_Load,
	Ir_Store, // Not referred to

	Ir_Access,
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
	I8 = 1,
	I16 = 2,
	I32 = 4,
	I64 = 8,
	I128 = 16,
} PrimitiveSize;

// typedef SPAN(PhiNode) PhiNodes;

// TODO Compress the heck out of this data.
typedef struct Inst {
	u8 kind;
	u16 size;

	union {
		u64 constant;
		Call call;

		IrRef unop;
		struct {
			u32 id;
			i64 offset;
		} reloc;
		struct {
			IrRef lhs;
			IrRef rhs;
		} binop;
		struct {
			IrRef size;
			u32 known_offset; // STYLE Without this, instructions could be immutable
		} alloc;
		// Apparently this kind of representation is called "parameterized basic
		// blocks". It does not matter very much.
		struct {
			IrRef source;
			IrRef on_true;
			IrRef on_false; // Unused for jumps.
		} phi_out;
	};
} Inst;

#define BINOP_CASES \
	case Ir_Add: \
	case Ir_Sub: \
	case Ir_Mul: \
	case Ir_Div: \
	case Ir_BitOr: \
	case Ir_BitXor: \
	case Ir_BitAnd: \
	case Ir_Equals: \
	case Ir_LessThan: \
	case Ir_LessThanOrEquals: \
	case Ir_Access: \
	case Ir_Store

#define UNOP_CASES \
	case Ir_Load: \
	case Ir_BitNot: \
	case Ir_Truncate: \
	case Ir_SignExtend: \
	case Ir_ZeroExtend


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
	IrRefList ordered_instructions; // Calls, loads, stores, phi_outs
	Exit exit;

	u32 id;
	u32 visited;
	Block *dominator;
	Blocks incoming;
} Block;


typedef struct IrBuild {
	IrList ir;
	Block *entry;

	u32 block_count;
	Block *insertion_block;
} IrBuild;

