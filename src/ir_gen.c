#include <math.h>
#include "ir_gen.h"
#include "checked_arith.h"
#include "emit.h"

typedef unsigned long ulong;
typedef unsigned int uint;

/*

Generates IR while doing a first constant-folding pass, and prints it in a nicely formatted form.
Arithmetic simplifications which require e.g. use-counts happen in the analysis pass arithSimplify.

*/

// TODO Get this down to 16.
static_assert(sizeof(Inst) == 24, "sizeof(Inst) == 24");
static inline u64 truncate(u64 constant, u16 size);

#define HAS_EXITED(ir) ((ir)->insertion_block->exit.kind != Exit_None)
// TODO Store pointer size in IrBuild
#define PTR_SIZE I64


static IrRef append (IrBuild *build, Inst inst) {
	IrList *list = &build->ir;

	if (list->len >= list->capacity) {
		list->capacity = list->len * 3 / 2 + 4;
		list->ptr = realloc(list->ptr, sizeof(*list->ptr) * list->capacity);
		list->locations = realloc(list->locations, sizeof(*list->locations) * list->capacity);
		if (list->ptr == NULL)
			generalFatal("out of memory on list growth.");
	}

	// This is probably not strict-aliasing-correct, but whatevs.
	list->ptr[list->len] = inst;
	list->locations[list->len] = build->loc;

	return list->len++;
}


static u32 pushAuxData(IrList *ir, const void *data, u32 len) {
	// TODO Alignment, blah, blah.
	len = (len + 7) / 8 * 8;

	if (ir->aux_data.len + len >= ir->aux_data.capacity) {
		ir->aux_data.capacity = (ir->aux_data.len + len) * 3 / 2 + 4;
		ir->aux_data.ptr = realloc(ir->aux_data.ptr, sizeof(*ir->aux_data.ptr) * ir->aux_data.capacity);
		if (ir->aux_data.ptr == NULL) {
			puts("ERROR: Out of memory on list growth.");
			exit(EXIT_FAILURE);
		}
	}
	memcpy(ir->aux_data.ptr + ir->aux_data.len, data, len);
	u32 res = ir->aux_data.len;
	ir->aux_data.len += len;
	return res;
}


static IrRef getPrevMemOp (IrBuild *build) {
	IrRefList mems = build->insertion_block->mem_instructions;
	if (mems.len == 0)
		return IDX_NONE;
	return mems.ptr[mems.len-1];
}

IrRef genParameter (IrBuild *build, u32 param_id) {
	return append(build, (Inst) {Ir_Parameter, .size = build->ir.params.ptr[param_id].size, .unop = param_id});
}

IrRef genStackAllocFixed (IrBuild *build, u32 size) {
	return append(build, (Inst) {Ir_StackAllocFixed, .size = PTR_SIZE, .alloc = {size, IDX_NONE}});
}

IrRef genStackAllocNamed(IrBuild *build, IrRef size, Declaration decl) {
	u32 data = pushAuxData(&build->ir, &decl, sizeof decl);
	return append(build, (Inst) {Ir_StackAllocFixed, .size = PTR_SIZE, .alloc = {size, data}});
}

IrRef genStackAllocVla (IrBuild *build, IrRef size) {
	Inst *inst = build->ir.ptr;
	if (inst[size].kind == Ir_Constant)
		return genStackAllocFixed(build, inst[size].constant);
	return append(build, (Inst) {Ir_StackAllocVLA, .size = PTR_SIZE, .alloc = {size, IDX_NONE}});
}

IrRef genStackAllocVlaNamed (IrBuild *build, IrRef size, Declaration decl) {
	Inst *inst = build->ir.ptr;
	if (inst[size].kind == Ir_Constant)
		return genStackAllocNamed(build, inst[size].constant, decl);
	u32 data = pushAuxData(&build->ir, &decl, sizeof decl);
	return append(build, (Inst) {Ir_StackAllocVLA, .size = PTR_SIZE, .alloc = {size, data}});
}

IrRef genStackDealloc (IrBuild *build, IrRef allocation) {
	IrRef inst = append(build, (Inst) {
		Ir_StackDeallocVLA,
		.mem = { .address = allocation, .ordered_after = build->insertion_block->last_store_op }
	});
	build->insertion_block->last_store_op = inst;

	PUSH_A(build->block_arena, build->insertion_block->mem_instructions, inst);
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, inst);
	return inst;
}

void genReturnVal (IrBuild *build, IrRef val) {
	build->insertion_block->inst_end = build->ir.len;
	build->insertion_block->exit = (Exit) {
		Exit_Return,
		.ret = val,
		.loc = build->loc,
	};
}

void genBranch (IrBuild *build, IrRef condition) {
// 	build->insertion_block->last_inst = build->ir.len;
	build->insertion_block->exit = (Exit) {
		Exit_Branch,
		.branch = {condition},
		.loc = build->loc,
	};
}

void genJump (IrBuild *build, Block *dest) {
// 	build->insertion_block->last_inst = build->ir.len;
	build->insertion_block->exit = (Exit) {
		Exit_Unconditional,
		.unconditional = dest,
		.loc = build->loc,
	};
	if (dest)
		startBlock(build, dest);
}


void genSwitch (IrBuild *build, IrRef val) {
// 	build->insertion_block->last_inst = build->ir.len;
	build->insertion_block->exit = (Exit) {
		Exit_Switch,
		.switch_.value = val,
		.loc = build->loc,
	};
}

Block *newBlock (IrBuild *build, String label) {
	Block *new_block = ALLOC(build->block_arena, Block);
	*new_block = (Block) {
		.label = label,
		.id = build->block_count++,
		.last_store_op = IDX_NONE,
	};
	return new_block;
}

void startBlock (IrBuild *build, Block *block) {
	block->first_inst = build->ir.len;
	build->insertion_block = block;
}

Block *startNewBlock (IrBuild *build, String label) {
	Block *blk = newBlock(build, label);
	startBlock(build, blk);
	return blk;
}

void discardBlock(Block *blk) {
	(void) blk;
}


static inline double doubleFromConst(u64 i, u16 size) {
	switch (size) {
	case 4: {
		float flt;
		memcpy(&flt, &i, 4);
		return flt;
	}
	case 8: {
		double flt;
		memcpy(&flt, &i, 8);
		return flt;
	}
	default: unreachable;
	}
}

static inline u64 constFromFloat(float f) {
	u64 i = 0;
	memcpy(&i, &f, 4);
	return i;
}
static inline u64 constFromDouble(double f) {
	u64 i = 0;
	memcpy(&i, &f, 8);
	return i;
}

static IrRef genBinOp (IrBuild *build, InstKind op, u8 properties, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	u16 size = inst[a].size;

	assert(size == inst[b].size || op == Ir_ShiftLeft || op == Ir_ShiftRight);
	Inst i = {
		.kind = op,
		.properties = properties,
		.size = size,
		.binop = {a, b}
	};

	bool commutative = op == Ir_Add || op == Ir_Mul || op == Ir_BitAnd || op == Ir_BitOr || op == Ir_BitXor || op == Ir_Equals;
	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		u64 lhs = inst[a].constant;
		u64 rhs = inst[b].constant;

		bool no_overflow = properties & Prop_Arith_NoSignedOverflow;

		switch (op) {
		case Ir_Add:
			if (no_overflow && addSignedOverflow(toSigned(lhs), toSigned(rhs), i.size)) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = truncate(lhs + rhs, size);
			break;
		case Ir_FAdd:
			if (size == 4)
				i.constant = constFromFloat(doubleFromConst(lhs, 4) + doubleFromConst(rhs, 4));
			else if (size == 8)
				i.constant = constFromDouble(doubleFromConst(lhs, 8) + doubleFromConst(rhs, 8));
			else
				unreachable;
			break;
		case Ir_Sub:
			if (no_overflow && subSignedOverflow(toSigned(lhs), toSigned(rhs), i.size)) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = lhs - rhs;
			break;
		case Ir_FSub:
			if (size == 4)
				i.constant = constFromFloat(doubleFromConst(lhs, 4) - doubleFromConst(rhs, 4));
			else if (size == 8)
				i.constant = constFromDouble(doubleFromConst(lhs, 8) - doubleFromConst(rhs, 8));
			else
				unreachable;
			break;
		case Ir_Mul:
			if (no_overflow && mulSignedOverflow(toSigned(lhs), toSigned(rhs), i.size)) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = lhs * rhs;
			break;
		case Ir_FMul:
			if (size == 4)
				i.constant = constFromFloat(doubleFromConst(lhs, 4) * doubleFromConst(rhs, 4));
			else if (size == 8)
				i.constant = constFromDouble(doubleFromConst(lhs, 8) * doubleFromConst(rhs, 8));
			else
				unreachable;
			break;
		case Ir_Div:
			if (rhs == 0) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = lhs / rhs;
			break;
		case Ir_SDiv:
			if (rhs == 0 || (toSigned(lhs) == INT64_MIN && toSigned(rhs) == -1)) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = toSigned(lhs) / toSigned(rhs);
			break;
		case Ir_FDiv:
			if (size == 4)
				i.constant = constFromFloat(doubleFromConst(lhs, 4) / doubleFromConst(rhs, 4));
			else if (size == 8)
				i.constant = constFromDouble(doubleFromConst(lhs, 8) / doubleFromConst(rhs, 8));
			else
				unreachable;
			break;
		case Ir_Mod:
		case Ir_SMod:
			if (rhs == 0) {
				i.properties |= Prop_Arith_WouldOverflow;
				goto no_fold;
			}
			i.constant = lhs % rhs;
			break;
		case Ir_FMod:
			if (size == 4)
				i.constant = constFromFloat(fmod(doubleFromConst(lhs, 4), doubleFromConst(rhs, 4)));
			else if (size == 8)
				i.constant = constFromDouble(fmod(doubleFromConst(lhs, 8), doubleFromConst(rhs, 8)));
			else
				unreachable;
			break;
		case Ir_BitAnd:
			i.constant = lhs & rhs; break;
		case Ir_BitOr:
			i.constant = lhs | rhs; break;
		case Ir_BitXor:
			i.constant = lhs ^ rhs; break;
		case Ir_Equals:
			i.constant = lhs == rhs; break;
		case Ir_LessThan:
			i.constant = lhs < rhs; break;
		case Ir_LessThanOrEquals:
			i.constant = lhs <= rhs; break;
		case Ir_ShiftLeft:
			// TODO Check shift exponent range
			i.constant = lhs << rhs; break;
		case Ir_ShiftRight:
			// TODO Check shift exponent range
			i.constant = lhs >> rhs; break;
		default:
			unreachable;
		}
		i.kind = Ir_Constant;
		return append(build, i);
	} else {
		if (inst[a].kind == Ir_Constant) {
			if (commutative) {
				i.binop.lhs = b;
				i.binop.rhs = a;
			} else {
				u64 constant = inst[a].constant;

				switch (op) {
				case Ir_Div:
				case Ir_SDiv:
				case Ir_Mul:
				case Ir_ShiftRight:
				case Ir_ShiftLeft:
					if (constant == 0)
						return a;
					break;
				default: break;
				}
			}
		}
		if (inst[i.binop.rhs].kind == Ir_Constant) {
			u64 constant = inst[i.binop.rhs].constant;
			double fconstant = doubleFromConst(constant, size);

			switch (op) {
			case Ir_FAdd:
			case Ir_FSub:
				if (fconstant == 0)
					return i.binop.lhs;
				break;
			case Ir_Add:
			case Ir_Sub:
				if (constant == 0)
					return i.binop.lhs;
				break;
			case Ir_FMul:
				if (fconstant == 0)
					return i.binop.rhs;
				if (fconstant == 1)
					return i.binop.lhs;
				break;
			case Ir_Mul:
				if (constant == 0)
					return i.binop.rhs;
				if (constant == 1)
					return i.binop.lhs;
				break;
			case Ir_Div:
			case Ir_SDiv:
				if (constant == 0)
					break;
				if (constant == 1)
					return i.binop.lhs;
				break;
			case Ir_Mod:
				// TODO mod 1
				break;
			case Ir_ShiftLeft:
				if (constant == 0)
					return i.binop.lhs;
				break;
			case Ir_ShiftRight:
				if (constant == 0)
					return i.binop.lhs;
				break;
			default: break;
			}
		}
	}

	if ((op == Ir_Add || op == Ir_Sub) &&
		inst[i.binop.lhs].kind == Ir_Reloc &&
		inst[i.binop.rhs].kind == Ir_Constant)
	{
		i.kind = Ir_Reloc;
		i64 delta = op == Ir_Add ? (i64)inst[i.binop.rhs].constant : -(i64)inst[i.binop.rhs].constant;
		u32 prev = i.binop.lhs;
		i.reloc.id = inst[prev].reloc.id;
		i.reloc.offset = inst[prev].reloc.offset + delta;
	}

	no_fold:
	return append(build, i);
}

IrRef genAdd (IrBuild *build, IrRef a, IrRef b, Signedness sign) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_Add, sign ? 0 : Prop_Arith_NoSignedOverflow, a, b);
}
IrRef genFAdd (IrBuild *build, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_FAdd, 0, a, b);
}

IrRef genSub (IrBuild *build, IrRef a, IrRef b, Signedness sign) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_Sub, sign ? Prop_Arith_NoSignedOverflow : 0, a, b);
}
IrRef genFSub (IrBuild *build, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_FSub, 0, a, b);
}


IrRef genMul (IrBuild *build, IrRef a, IrRef b, Signedness sign) {
	return genBinOp(build, Ir_Mul, sign ? Prop_Arith_NoSignedOverflow : 0, a, b);
}
IrRef genFMul (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_FMul, 0, a, b);
}

IrRef genDiv (IrBuild *build, IrRef a, IrRef b, Signedness sign) {
	return genBinOp(build, sign ? Ir_SDiv : Ir_Div, sign ? Prop_Arith_NoSignedOverflow : 0, a, b);
}
IrRef genFDiv (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_FDiv, 0, a, b);
}

IrRef genMod (IrBuild *build, IrRef a, IrRef b, Signedness sign) {
	return genBinOp(build, sign ? Ir_SMod : Ir_Mod, sign ? Prop_Arith_NoSignedOverflow : 0, a, b);
}
IrRef genFMod (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_FMod, 0, a, b);
}

IrRef genOr (IrBuild *build, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_BitOr, 0, a, b);
}

IrRef genXor (IrBuild *build, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_BitXor, 0, a, b);
}

IrRef genAnd (IrBuild *build, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	return genBinOp(build, Ir_BitAnd, 0, a, b);
}

IrRef genShiftLeft (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_ShiftLeft, 0, a, b);
}

IrRef genShiftRight (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_ShiftRight, 0, a, b);
}


IrRef genBitNot (IrBuild *build, IrRef a) {
	Inst *inst = build->ir.ptr;
	Inst i = {Ir_BitNot, .size = inst[a].size, .unop = a};
	if (inst[a].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = ~inst[a].constant & bitsBelow(i.size);
	} else if (inst[a].kind == Ir_BitNot) {
		return inst[a].unop;
	}
	return append(build, i);
}

IrRef genNot (IrBuild *build, IrRef a) {
	Inst *inst = build->ir.ptr;
	Inst i = {Ir_Equals, .size = inst[a].size};

	if (inst[a].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = !inst[a].constant;
	} else if (inst[a].kind == Ir_Reloc) {
		i.kind = Ir_Constant;
		i.constant = 0;
	} else {
		i.binop.lhs = a;
		i.binop.rhs = genImmediateInt(build, 0, inst[a].size);
	}

	return append(build, i);
}

IrRef genEquals (IrBuild *build, IrRef a, IrRef b, u16 size, bool is_float) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {is_float ? Ir_FEquals : Ir_Equals, .size = size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant) {
		if (inst[b].kind == Ir_Constant) {
			i.kind = Ir_Constant;
			if (is_float)
				i.constant = doubleFromConst(inst[a].constant, inst[a].size) == doubleFromConst(inst[b].constant, inst[b].size);
			else
				i.constant = inst[a].constant == inst[b].constant;
		} else {
			i.binop.lhs = b;
			i.binop.rhs = a;
		}
	}

	if (inst[a].kind == Ir_Reloc && !is_float) {
		if (inst[b].kind == Ir_Reloc) {
			i.kind = Ir_Constant;
			i.constant = inst[a].reloc.id == inst[b].reloc.id
					&& inst[a].reloc.offset == inst[b].reloc.offset;
		} else if (inst[b].kind == Ir_Constant && inst[b].constant == 0) {
			// Relocs can never be zero.
			i.kind = Ir_Constant;
			i.constant = 0;
		}
	}

	return append(build, i);
}


IrRef genLessThan (IrBuild *build, IrRef a, IrRef b, u16 size, Signedness sign) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {sign ? Ir_SLessThan : Ir_LessThan, .size = size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		if (sign)
			i.constant = toSigned(inst[a].constant) < toSigned(inst[b].constant);
		else
			i.constant = inst[a].constant < inst[b].constant;
	}

	return append(build, i);
}
IrRef genFLessThan (IrBuild *build, IrRef a, IrRef b, u16 size) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {Ir_FLessThan, .size = size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = doubleFromConst(inst[a].constant, inst[a].size) < doubleFromConst(inst[b].constant, inst[b].size);
	}

	return append(build, i);
}

IrRef genLessThanOrEquals (IrBuild *build, IrRef a, IrRef b, u16 size, Signedness sign) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {sign ? Ir_SLessThanOrEquals : Ir_LessThanOrEquals, .size = size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		if (sign)
			i.constant = toSigned(inst[a].constant) <= toSigned(inst[b].constant);
		else
			i.constant = inst[a].constant <= inst[b].constant;
	}

	return append(build, i);
}

IrRef genFLessThanOrEquals (IrBuild *build, IrRef a, IrRef b, u16 size) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {Ir_FLessThanOrEquals, .size = size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = doubleFromConst(inst[a].constant, inst[a].size) <= doubleFromConst(inst[b].constant, inst[b].size);
	}

	return append(build, i);
}

IrRef genImmediateInt (IrBuild *build, long long i, u16 size) {
	assert(size <= 8);
	return append(build, (Inst) {Ir_Constant, .size = size, .constant = i});
}

IrRef genImmediateReal (IrBuild *build, double r, u16 size) {
	union {
		double r;
		u64 i;
	} pun = {.r = r};
	Inst inst = {Ir_Constant, .size = size, .constant = pun.i};
	return append(build, inst);
}

static inline u64 truncate (u64 constant, u16 size) {
	if (size == 8) return constant;
	return constant & (((u64) 1 << size*8) - 1);
}
static inline i64 signExtend (u64 constant, u16 src, u16 target) {
	u64 fully_extended = constant | bitsAbove(src) * sign(constant, src);
	// Wrapping unsigned->signed casts are strictly speaking illegal.
	// They happen everywhere in the constant folding though and my
	// compilers don't seem to mess with them, so I'll worry about that
	// later.
	return truncate(fully_extended, target);
}

IrRef genTrunc (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size >= target);
	if (inst[source].size == target)
		return source;
	if (inst[source].kind == Ir_Constant)
		return genImmediateInt(build, truncate(inst[source].constant, target), target);

	return append(build, (Inst) {Ir_Truncate, .size = target, .unop = source});
}

IrRef genSignExt (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	u16 src_size = inst[source].size;
	assert(src_size <= target);
	if (src_size == target)
		return source;
	if (inst[source].kind == Ir_Constant) {
		u64 c = inst[source].constant;
		return genImmediateInt(build, signExtend(c, src_size, target), target);
	}


	return append(build, (Inst) {Ir_SignExtend, .size = target, .unop = source});
}

IrRef genZeroExt (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size <= target);
	if (inst[source].size == target)
		return source;
	if (inst[source].kind == Ir_Constant)
		return genImmediateInt(build, inst[source].constant, target);

	return append(build, (Inst) {Ir_ZeroExtend, .size = target, .unop = source});
}

IrRef genFCast (IrBuild *build, IrRef source, u16 target) {
	Inst source_inst = build->ir.ptr[source];
	Inst i = {
		.kind = Ir_FCast,
		.size = target,
		.unop = source
	};
	if (source_inst.size == target)
		return source;
	if (source_inst.kind == Ir_Constant) {
		i.kind = Ir_Constant;
		if (target == 4 && source_inst.size == 8)
			i.constant = constFromFloat(doubleFromConst(source_inst.constant, 8));
		else if (target == 8 && source_inst.size == 4)
			i.constant = constFromDouble(doubleFromConst(source_inst.constant, 4));
		else
			unreachable;
	}

	return append(build, i);
}

IrRef genIntToFloat (IrBuild *build, IrRef source, u16 target, Signedness sign) {
	Inst source_inst = build->ir.ptr[source];
	Inst i = {
		.kind = sign ? Ir_SIntToFloat : Ir_UIntToFloat,
		.size = target,
		.unop = source,
	};
	if (source_inst.kind == Ir_Constant) {
		i.kind = Ir_Constant;
		if (sign == Unsigned) {
			if (target == 4)
				i.constant = constFromFloat((float) source_inst.constant);
			else if (target == 8)
				i.constant = constFromDouble((double) source_inst.constant);
			else
				unreachable;
		} else {
			i64 s = signExtend(source_inst.constant, source_inst.size, I64);
			if (target == 4)
				i.constant = constFromFloat((float) s);
			else if (target == 8)
				i.constant = constFromDouble((double) s);
			else
				unreachable;
		}
	}
	return append(build, i);
}

IrRef genFloatToInt (IrBuild *build, IrRef source, u16 target, Signedness sign) {
	Inst source_inst = build->ir.ptr[source];
	Inst i = {
		.kind = sign ? Ir_FloatToSInt : Ir_FloatToUInt,
		.size = target,
		.unop = source,
	};

	if (source_inst.kind == Ir_Constant) {
		double flt = doubleFromConst(source_inst.constant, source_inst.size);

		if (sign) {
			if (flt >= INT64_MIN && flt <= INT64_MAX) {
				i.kind = Ir_Constant;
				i.constant = truncate((i64) flt, target);
			} else {
				i.properties |= Prop_Arith_WouldOverflow;
			}
		} else {
			if (flt >= 0 && flt <= UINT64_MAX) {
				i.kind = Ir_Constant;
				i.constant = truncate((u64) flt, target);
			} else {
				i.properties |= Prop_Arith_WouldOverflow;
			}
		}
	}

	return append(build, i);
}

IrRef genCall (IrBuild *build, IrRef func, Type rettype, ArgumentSpan args, u16 size, bool is_vararg) {
	Call call = {args, rettype};
	u32 data = pushAuxData(&build->ir, &call, sizeof call);

	IrRef inst = append(build, (Inst) {
		Ir_Call,
		.properties = is_vararg ? Prop_Call_Vararg : 0,
		.size = size,
		.call = {func, getPrevMemOp(build), data}
	});
	build->insertion_block->last_store_op = inst;

	PUSH_A(build->block_arena, build->insertion_block->mem_instructions, inst);
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, inst);
	return inst;
}

IrRef genGlobal (IrBuild *build, u32 id) {
	return append(build, (Inst) {Ir_Reloc, .size = PTR_SIZE, .binop = {id}});
}

IrRef genLoad (IrBuild *build, IrRef ref, u16 size, bool is_volatile) {
	IrRef load = append(build, (Inst) {
		Ir_Load,
		.properties = is_volatile ? Prop_Mem_Volatile : 0,
		.size = size,
		.mem = { .address = ref, .ordered_after = build->insertion_block->last_store_op }
	});
	PUSH_A(build->block_arena, build->insertion_block->mem_instructions, load);
	return load;
}

IrRef genStore (IrBuild *build, IrRef dest, IrRef value, bool is_volatile) {
	Inst val = build->ir.ptr[value];
	// TODO Assert dest->size == target pointer size
	IrRef store = append(build, (Inst) {
		Ir_Store,
		.properties = is_volatile ? Prop_Mem_Volatile : 0,
		.size = val.size,
		.mem = { .address = dest, .source = value, .ordered_after = getPrevMemOp(build) }
	});
	build->insertion_block->last_store_op = store;

	PUSH_A(build->block_arena, build->insertion_block->mem_instructions, store);
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, store);
	return store;
}

IrRef genAccess (IrBuild *build, IrRef value, IrRef offset, IrRef size) {
	return append(build, (Inst) {Ir_Access, .size = size, .unop_const = {value, offset}});
}

IrRef genPhiIn (IrBuild *build, u16 size) {
	return append(build, (Inst) {Ir_PhiIn, .size = size});
}


IrRef genPhiOut (IrBuild *build, IrRef source) {
	u32 inst = append(build, (Inst) {Ir_PhiOut, .size = build->ir.ptr[source].size, .phi_out = {source, IDX_NONE, IDX_NONE}});
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, inst);
	return inst;
}

void setPhiOut (IrBuild *build, IrRef phi, IrRef dest_true, IrRef dest_false) {
	Inst *inst = &build->ir.ptr[phi];
	assert(inst->kind == Ir_PhiOut);
	inst->phi_out.on_true = dest_true;
	inst->phi_out.on_false = dest_false;
}

IrRef genVaArg (IrBuild *build, IrRef va_list_addr, u16 size, Type type) {
	u32 data = pushAuxData(&build->ir, &type, sizeof type);
	IrRef store = append(build, (Inst) {Ir_VaArg, .size = size, .unop_const = {va_list_addr, data}});

	// TODO This should be a mem instruction too.
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, store);
	return store;
}

IrRef genVaStart (IrBuild *build, IrRef va_list_addr, IrRef param) {
	IrRef store = append(build, (Inst) {Ir_VaStart, .size = 0, .unop_const = {va_list_addr, param}});

	// TODO This should be a mem instruction too.
	PUSH_A(build->block_arena, build->insertion_block->ordered_instructions, store);
	return store;
}


void replaceWithCopy (IrList ir, IrRef original, IrRef replacement, IrRef ordered_after) {
	ir.ptr[original].kind = Ir_Copy;
	ir.ptr[original].binop.lhs = replacement;
	ir.ptr[original].binop.rhs = ordered_after;
}

void replaceWithNop (IrList ir, IrRef original) {
	replaceWithCopy(ir, original, IDX_NONE, ir.ptr[original].mem.ordered_after);
}

void genSetZero(IrBuild *build, IrRef address, u32 size, bool is_volatile) {
	u32 zero8 = genImmediateInt(build, 0, I64);
	u32 pos = 0;
	while (pos + 8 <= size) {
		u32 offset = genImmediateInt(build, pos, I64);
		u32 dest = genAdd(build, address, offset, Unsigned);
		genStore(build, dest, zero8, is_volatile);
		pos += 8;
	}
	if (size - pos >= 4) {
		u32 zero_rest = genImmediateInt(build, 0, 4);
		u32 offset = genImmediateInt(build, pos, I64);
		u32 dest = genAdd(build, address, offset, Unsigned);
		genStore(build, dest, zero_rest, is_volatile);
		pos += 4;
	}
	if (size - pos >= 2) {
		u32 zero_rest = genImmediateInt(build, 0, 2);
		u32 offset = genImmediateInt(build, pos, I64);
		u32 dest = genAdd(build, address, offset, Unsigned);
		genStore(build, dest, zero_rest, is_volatile);
		pos += 2;
	}

	if (size - pos >= 1) {
		assert(size - pos == 1);
		u32 zero_rest = genImmediateInt(build, 0, 1);
		u32 offset = genImmediateInt(build, pos, I64);
		u32 dest = genAdd(build, address, offset, Unsigned);
		genStore(build, dest, zero_rest, is_volatile);
	}
}


void discardIrBuilder (IrBuild *builder) {
	// TODO Free loose blocks
	free(builder->ir.ptr);
}






void emitIr (EmitParams params) {
	FILE *dest = params.out;
	Module module = params.module;

	for (u32 i = 0; i < module.len; i++) {
		StaticValue val = module.ptr[i];
		if (val.is_public)
			fprintf(dest, "public ");

		if (val.def_state != Def_Defined) {
			fprintf(dest, "extern %.*s\n", STRING_PRINTAGE(val.name));
		} else if (val.def_kind == Static_Function) {
			fprintf(dest, "%s:\n", printDeclarator(params.arena, val.type, val.name));
			printBlock(dest, val.function_ir.entry, val.function_ir);
		} else {
			if (val.type.qualifiers & Static_Variable)
				fprintf(dest, "variable ");
			else
				fprintf(dest, "constant ");
			String name = val.name;
			if (name.len == 0)
				name = zstr("[anon]");
			fprintf(dest, "%d (%.*s):\n", (int) i, STRING_PRINTAGE(name));

			bool is_string = true;
			String data = val.value_data;
			for (u32 i = 0; i < data.len - 1; i++) {
				if (data.ptr[i] < 32 || (uchar) data.ptr[i] >= 128)
					is_string = false;
			}
			is_string = is_string && data.ptr[data.len - 1] == 0;
			if (is_string) {
				fprintf(dest, "\"%.*s\"\n", STRING_PRINTAGE(data));
			} else {
				for (u32 i = 0; i < data.len; i++) {
					fprintf(dest, "%02hhx ", data.ptr[i]);
				}
				fprintf(dest, "\n");
			}
		}
	}
}


static void printOrder (FILE *out, IrRef r) {
	if (r != IDX_NONE)
		fprintf(out, " after %lu", (ulong) r);
	fprintf(out, "\n");
}
void printBlock (FILE *dest, Block *blk, IrList ir) {
	if (blk->visited == 31)
		return;
	blk->visited = 31;

	fprintf(dest, " %.*s%lu: { ", STRING_PRINTAGE(blk->label), (ulong) blk->id);
	if (blk->mem_instructions.len) {
		fprintf(dest, "mem: ");
		for (u32 i = 0; i < blk->mem_instructions.len; i++)
			fprintf(dest, "%lu, ", (ulong) blk->mem_instructions.ptr[i]);
	}
	if (blk->ordered_instructions.len) {
		fprintf(dest, "ordered: ");
		for (u32 i = 0; i < blk->ordered_instructions.len; i++)
			fprintf(dest, "%lu, ", (ulong) blk->ordered_instructions.ptr[i]);
	}
	fprintf(dest, "}\n");

	for (size_t i = blk->first_inst; i < blk->inst_end; i++) {
		Inst inst = ir.ptr[i];
		fprintf(dest, " %3lu /%lu = ", (ulong) i, (ulong) inst.size);

		switch ((InstKind) inst.kind) {
		case Ir_Reloc:
			fprintf(dest, "global %d %+lld\n", inst.reloc.id, (long long)inst.reloc.offset);
			continue;
		case Ir_Constant:
			fprintf(dest, "const 0x%lx (%lu)\n", (ulong) inst.constant, (ulong) inst.constant);
			continue;
		case Ir_Call: {
			fprintf(dest, "call %lu (", (ulong) inst.call.function_ptr);

			Call call = AUX_DATA(Call, ir, inst.call.data);
			for (u32 i = 0; i < call.arguments.len; i++) {
				fprintf(dest, "%lu", (ulong) call.arguments.ptr[i].arg_inst);
				if (i + 1 < call.arguments.len)
					fprintf(dest, ", ");
			}
			fprintf(dest, ")");
			printOrder(dest, inst.call.ordered_after);
		} continue;
		case Ir_PhiOut:
			fprintf(dest, "out-phi %lu ? ", (ulong) inst.phi_out.source);
			if (inst.phi_out.on_true != IDX_NONE)
				fprintf(dest, " true->%lu", (ulong) inst.phi_out.on_true);
			if (inst.phi_out.on_false != IDX_NONE)
				fprintf(dest, " false->%lu", (ulong) inst.phi_out.on_false);
			fprintf(dest, "\n");
			continue;
		case Ir_PhiIn:
			fprintf(dest, "in-phi\n");
			continue;
		case Ir_Parameter:
			fprintf(dest, "param %lu\n", (ulong) inst.size);
			continue;
		case Ir_StackAllocFixed:
			fprintf(dest, "stack %lu\n", (ulong) inst.alloc.size);
			continue;
		case Ir_StackAllocVLA:
			fprintf(dest, "stack_vla %lu\n", (ulong) inst.alloc.size);
			continue;
		case Ir_Copy:
			fprintf(dest, "copy %lu\n", (ulong) inst.unop);
			continue;
		case Ir_StackDeallocVLA:
			fprintf(dest, "discard %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Load:
			fprintf(dest, "load%s %lu", inst.properties & Prop_Mem_Volatile ? " volatile" : "",
					(ulong) inst.mem.address);
			printOrder(dest, inst.mem.ordered_after);
			continue;
		case Ir_Store:
			fprintf(dest, "store%s [%lu] %lu", inst.properties & Prop_Mem_Volatile ? " volatile" : "",
					(ulong) inst.mem.address, (ulong) inst.mem.source);
			printOrder(dest, inst.mem.ordered_after);
			continue;
		case Ir_BitNot:
			fprintf(dest, "not %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Truncate:
			fprintf(dest, "trunc i%lu, %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_SignExtend:
			fprintf(dest, "signex i%lu, %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_ZeroExtend:
			fprintf(dest, "zeroex i%lu, %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_FCast:
			fprintf(dest, "fcast f%lu, %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			break;
		case Ir_Access:
			fprintf(dest, "access /%lu, %lu @ %lu\n", (ulong) inst.size, (ulong) inst.unop_const.val, (ulong) inst.unop_const.offset);
			continue;
		case Ir_SIntToFloat:
			fprintf(dest, "s%lu->float %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_UIntToFloat:
			fprintf(dest, "u%lu->float %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_FloatToSInt:
			fprintf(dest, "float->s%lu %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_FloatToUInt:
			fprintf(dest, "float->u%lu %lu\n", (ulong) inst.size * 8, (ulong) inst.unop);
			continue;
		case Ir_VaArg:
			fprintf(dest, "va_arg /%lu, %lu\n", (ulong) inst.size, (ulong) inst.unop);
			continue;
		case Ir_VaStart:
			fprintf(dest, "va_start %lu, param %lu\n", (ulong) inst.unop_const.val, (ulong) inst.unop_const.offset);
			continue;
		case Ir_Add: fprintf(dest, "add"); break;
		case Ir_FAdd: fprintf(dest, "fadd"); break;
		case Ir_Sub: fprintf(dest, "sub"); break;
		case Ir_FSub: fprintf(dest, "fsub"); break;
		case Ir_Mul: fprintf(dest, "mul"); break;
		case Ir_FMul: fprintf(dest, "fmul"); break;
		case Ir_Div: fprintf(dest, "div"); break;
		case Ir_SDiv: fprintf(dest, "sdiv"); break;
		case Ir_FDiv: fprintf(dest, "fdiv"); break;
		case Ir_Mod: fprintf(dest, "mod"); break;
		case Ir_SMod: fprintf(dest, "smod"); break;
		case Ir_FMod: fprintf(dest, "fmod"); break;
		case Ir_BitAnd: fprintf(dest, "and"); break;
		case Ir_BitOr: fprintf(dest, "or"); break;
		case Ir_BitXor: fprintf(dest, "xor"); break;
		case Ir_LessThan: fprintf(dest, "cmpu<"); break;
		case Ir_SLessThan: fprintf(dest, "cmps<"); break;
		case Ir_FLessThan: fprintf(dest, "cmpf<"); break;
		case Ir_LessThanOrEquals: fprintf(dest, "cmpu<="); break;
		case Ir_SLessThanOrEquals: fprintf(dest, "cmps<="); break;
		case Ir_FLessThanOrEquals: fprintf(dest, "cmpf<="); break;
		case Ir_Equals: fprintf(dest, "cmp=="); break;
		case Ir_FEquals: fprintf(dest, "cmpf=="); break;
		case Ir_ShiftLeft: fprintf(dest, "shift<<"); break;
		case Ir_ShiftRight: fprintf(dest, "shift>>"); break;
// 		default: unreachable;
		}
		fprintf(dest, " %lu %lu\n", (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
	}

	Exit exit = blk->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		fprintf(dest, "       jmp %.*s%lu\n", STRING_PRINTAGE(exit.unconditional->label), (ulong) exit.unconditional->id);
		printBlock(dest, exit.unconditional, ir);
		break;
	case Exit_Branch:
		fprintf(dest, "       branch %lu ? %.*s%lu : %.*s%lu\n",
			(ulong) exit.branch.condition,
			STRING_PRINTAGE(exit.branch.on_true->label), (ulong) exit.branch.on_true->id,
			STRING_PRINTAGE(exit.branch.on_false->label), (ulong) exit.branch.on_false->id);
		printBlock(dest, exit.branch.on_true, ir);
		printBlock(dest, exit.branch.on_false, ir);
		break;
	case Exit_Return:
		if (exit.ret == IDX_NONE)
			fprintf(dest, "       ret\n");
		else
			fprintf(dest, "       ret %lu\n", (ulong) exit.ret);
		break;
	case Exit_Switch: {
		fprintf(dest, "       switch %lu: ", (ulong) exit.switch_.value);
		Cases cases = exit.switch_.cases;
		for (u32 i = 0; i < cases.len; i++) {
			Block *target = cases.ptr[i].dest;
			fprintf(dest, "%llu => %.*s%lu, ", (unsigned long long) cases.ptr[i].value,
					STRING_PRINTAGE(target->label), (ulong) target->id);
		}
		Block *def = exit.switch_.default_case;
		fprintf(dest, "default => %.*s%lu\n", STRING_PRINTAGE(def->label), (ulong) def->id);
		for (u32 i = 0; i < cases.len; i++) {
			printBlock(dest, cases.ptr[i].dest, ir);
		}
		printBlock(dest, exit.switch_.default_case, ir);
	} break;
	default: unreachable;
	}
}

