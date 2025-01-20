#pragma once

#include <stddef.h>
#include <stdbool.h>

#include "util.h"
#include "arena.h"
#include "types.h"

#define AUX_DATA(type, ir, idx) (*(type*) ((ir).aux_data.ptr + (idx)))

typedef struct Block Block;
typedef struct Inst Inst;
typedef struct Function Function;

typedef u32 IrRef;
typedef LIST(IrRef) IrRefList;
typedef LIST(Block*) Blocks;

typedef struct {
	IrRef arg_inst;
	Type type;
} Argument;
typedef SPAN(Argument) ArgumentSpan;
typedef struct {
	u16 size;
	Type type;
} Parameter;
typedef SPAN(Parameter) ParameterSpan;

typedef struct {
	u32 len;
	u32 capacity;
	Inst *ptr;
	// PERFORMANCE That's a lot of data. Only storing offsets into
	// Tokenization's positions may benefit the copying of this data in
	// decimateIr and the skimming for positions changes in the debug
	// info generator.
	Location *locations;

	LIST(char) aux_data;
	LIST(Parameter) params;
	Block *entry;
} IrList;


typedef struct {
	// PERFORMANCE The arguments could probably be stored as a flexible
	// array member.
	ArgumentSpan arguments;
	Type rettype;
} Call;


typedef enum {
	Ir_Reloc,
	Ir_Constant,
	Ir_Call,
	Ir_Parameter,
	Ir_PhiOut, // Not referred to
	Ir_PhiIn,
	Ir_StackAllocFixed,
	Ir_StackAllocVLA,
	Ir_StackDeallocVLA, // Not referred to
	Ir_Copy,
	Ir_Load,
	Ir_Store, // Not referred to

	Ir_Access,
	Ir_Truncate,
	Ir_SignExtend,
	Ir_ZeroExtend,
	Ir_UIntToFloat,
	Ir_SIntToFloat,
	Ir_FloatToSInt,
	Ir_FloatToUInt,

	Ir_Add,
	Ir_Sub,
	Ir_Mul,
	Ir_Div,
	Ir_SDiv,
	Ir_Mod,
	Ir_SMod,

	Ir_FAdd,
	Ir_FSub,
	Ir_FMul,
	Ir_FDiv,
	Ir_FMod,
	Ir_FCast,

	Ir_BitAnd,
	Ir_BitOr,
	Ir_BitNot,
	Ir_BitXor,
	Ir_LessThan,
	Ir_SLessThan,
	Ir_FLessThan,
	Ir_LessThanOrEquals,
	Ir_SLessThanOrEquals,
	Ir_FLessThanOrEquals,
	Ir_Equals,
	Ir_FEquals,
	Ir_ShiftLeft,
	Ir_ShiftRight,

	Ir_VaStart,
	Ir_VaArg,
} InstKind;


enum InstProperties {
	Prop_Mem_Volatile = 1,

	Prop_Arith_NoSignedOverflow = 1,
	Prop_Arith_WouldOverflow = 2,

	Prop_Call_Vararg = 1,
};

typedef struct Type Type;

// typedef SPAN(PhiNode) PhiNodes;

typedef struct {
	u32 id;
	i64 offset;
} Relocation;

// TODO Compress the heck out of this data.
typedef struct Inst {
	u8 kind;
	u8 properties;
	u16 size;

	union {
		u64 constant;
		IrRef unop;

		struct {
			IrRef function_ptr;
			IrRef ordered_after;
			u32 data;
		} call;
		Relocation reloc;
		struct {
			IrRef lhs;
			IrRef rhs;
		} binop;
		struct {
			IrRef val;
			u32 offset;
		} unop_const;
		struct {
			u32 size; // Constant size for Ir_StackAllocFixed, IrRef for Ir_StackAllocVLA
			// May be IDX_NONE or point to Declaration data.
			u32 decl_data;
		} alloc;
		struct {
			IrRef address;
			IrRef source; // Only used for stores
			// Defines an ordering of memory accesses. This is not
			// actually useful for stores, which are put into
			// ordered_instructions. I cannot tell what to do about that
			// now; I need a list of side-effecting instructions that
			// need to happen, without constraining their ordering.
			//
			// The program source defines a total ordering on
			// operations; we have to work to relax that order. Loads
			// that are unreferenced can just be dropped, but it takes
			// analyses to find stores and calls may be elided.
			IrRef ordered_after;
		} mem;
		// Apparently this kind of representation is called "parameterized basic
		// blocks". It does not matter very much.
		struct {
			IrRef source;
			// Does not yet handle switch expressions.
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
	case Ir_SDiv: \
	case Ir_Mod: \
	case Ir_SMod: \
	case Ir_FAdd: \
	case Ir_FSub: \
	case Ir_FMul: \
	case Ir_FDiv: \
	case Ir_FMod: \
	case Ir_BitOr: \
	case Ir_BitXor: \
	case Ir_BitAnd: \
	case Ir_Equals: \
	case Ir_FEquals: \
	case Ir_LessThan: \
	case Ir_SLessThan: \
	case Ir_FLessThan: \
	case Ir_LessThanOrEquals: \
	case Ir_SLessThanOrEquals: \
	case Ir_FLessThanOrEquals: \
	case Ir_ShiftLeft: \
	case Ir_ShiftRight


#define UNOP_CASES \
	case Ir_BitNot: \
	case Ir_Truncate: \
	case Ir_SignExtend: \
	case Ir_ZeroExtend: \
	case Ir_FCast: \
	case Ir_FloatToSInt: \
	case Ir_FloatToUInt: \
	case Ir_SIntToFloat: \
	case Ir_UIntToFloat: \
	case Ir_StackAllocVLA: \
	case Ir_VaArg

#define UNOP_CONST_CASES \
	case Ir_Access: \
	case Ir_VaStart

#define ZEROOP_CASES \
	case Ir_Reloc: \
	case Ir_Constant: \
	case Ir_Parameter: \
	case Ir_StackAllocFixed: \
	case Ir_PhiIn

typedef struct {
	IrRef condition;
	Block *on_true;
	Block *on_false;
} Branch;

typedef struct {
	u64 value;
	Block *dest;
} SwitchCase;

typedef LIST(SwitchCase) Cases;

typedef struct {
	IrRef value;
	Block *default_case;
	Cases cases;
} Switch;

typedef enum {
	Exit_None, // Used for blocks that are currently in construction or that have already been visited.
	Exit_Branch,
	Exit_Unconditional,
	Exit_Switch,
	Exit_Return,
} ExitKind;

typedef struct {
	ExitKind kind;

	union {
		Branch branch;
		Switch switch_;
		Block *unconditional;
		IrRef ret;
	};

	Location loc;
} Exit;


typedef struct Block {
	String label; // null-terminated
	IrRef first_inst;
	IrRef inst_end; // Used for unoptimized output, irrelevant when scheduler applies
	IrRefList mem_instructions; // Loads and stores
	IrRefList ordered_instructions; // Calls, stores, phi_outs
	IrRef last_store_op; // Used for construction of meemory instructions.
	Exit exit;

	u32 id;
	u32 visited;
	Block *dominator;
	Blocks incoming;
	u16 loop_depth;
} Block;


typedef struct IrBuild {
	IrList ir;
	Location loc;
	Arena *block_arena;

	u32 block_count;
	Block *insertion_block;
} IrBuild;

