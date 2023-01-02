#include "analysis.h"
#include "ir_gen.h"


/*

Routines for analyzing and transforming the IR (transformations actually
should be moved into a separate file at some point).

*/


#define RECURSE_NEXT_BLOCKS(visitor, blk, ...)  \
	switch (blk->exit.kind) { \
	case Exit_Unconditional: \
		visitor(blk->exit.unconditional, __VA_ARGS__); \
		break; \
	case Exit_Branch: \
		visitor(blk->exit.branch.on_false, __VA_ARGS__); \
		visitor(blk->exit.branch.on_true, __VA_ARGS__); \
		break; \
	case Exit_Return: break; \
	case Exit_None: unreachable; \
	}



static IrRef copyUsed (IrList source, IrRef i, IrList *dest, IrRef *relocs) {
	if (relocs[i] != IR_REF_NONE) return relocs[i];

	Inst inst = source.ptr[i];
	switch ((InstKind) inst.kind) {
	BINOP_CASES:
		copyUsed(source, inst.binop.lhs, dest, relocs);
		copyUsed(source, inst.binop.rhs, dest, relocs);
		break;
	UNOP_CASES:
		copyUsed(source, inst.unop, dest, relocs);
		break;
	UNOP_CONST_CASES:
		copyUsed(source, inst.unop_const.val, dest, relocs);
		break;
	case Ir_Store:
	case Ir_StoreVolatile:
		if (inst.mem.ordered_after != IR_REF_NONE)
			copyUsed(source, inst.mem.ordered_after, dest, relocs);
		copyUsed(source, inst.mem.address, dest, relocs);
		copyUsed(source, inst.mem.source, dest, relocs);
		break;
	case Ir_Load:
	case Ir_LoadVolatile:
		if (inst.mem.ordered_after != IR_REF_NONE)
			copyUsed(source, inst.mem.ordered_after, dest, relocs);
		copyUsed(source, inst.mem.address, dest, relocs);
		break;
	case Ir_StackAlloc:
		copyUsed(source, inst.alloc.size, dest, relocs);
		break;
	case Ir_PhiOut:
		copyUsed(source, inst.phi_out.source, dest, relocs);
		break;
	case Ir_Call:
		if (inst.call.ordered_after != IR_REF_NONE)
			copyUsed(source, inst.call.ordered_after, dest, relocs);
		copyUsed(source, inst.call.function_ptr, dest, relocs);
		ValuesSpan params = inst.call.parameters;
		for (u32 p = 0; p < params.len; p++)
			copyUsed(source, params.ptr[p], dest, relocs);
		break;
	ZEROOP_CASES:
		break;
	}

	IrRef new = dest->len;
	PUSH(*dest, inst);
	relocs[i] = new;
	return new;
}


static void applyRelocs (IrList ir, IrRef i, IrRef *relocs) {
	Inst *inst = &ir.ptr[i];
	switch ((InstKind) inst->kind) {
	BINOP_CASES:
		inst->binop.lhs = relocs[inst->binop.lhs];
		if (inst->binop.rhs != IR_REF_NONE)
			inst->binop.rhs = relocs[inst->binop.rhs];
		break;
	UNOP_CASES:
		inst->unop = relocs[inst->unop];
		break;
	UNOP_CONST_CASES:
		inst->unop_const.val = relocs[inst->unop_const.val];
		break;
	case Ir_Store:
	case Ir_StoreVolatile:
		inst->mem.source = relocs[inst->mem.source];
		FALLTHROUGH;
	case Ir_Load:
	case Ir_LoadVolatile:
		inst->mem.address = relocs[inst->mem.address];
		if (inst->mem.ordered_after != IR_REF_NONE)
			inst->mem.ordered_after = relocs[inst->mem.ordered_after];
		break;
	case Ir_StackAlloc:
		inst->alloc.size = relocs[inst->alloc.size];
		break;
	case Ir_PhiOut:
		inst->phi_out.source = relocs[inst->phi_out.source];
		if (inst->phi_out.on_true != IR_REF_NONE)
			inst->phi_out.on_true = relocs[inst->phi_out.on_true];
		if (inst->phi_out.on_false != IR_REF_NONE)
			inst->phi_out.on_false = relocs[inst->phi_out.on_false];
		break;
	case Ir_Call:
		if (inst->call.ordered_after != IR_REF_NONE)
			inst->call.ordered_after = relocs[inst->call.ordered_after];
		inst->call.function_ptr = relocs[inst->call.function_ptr];
		ValuesSpan params = inst->call.parameters;
		for (u32 p = 0; p < params.len; p++)
			params.ptr[p] = relocs[params.ptr[p]];
		break;
	ZEROOP_CASES:
		break;
	}
}

void decimateIr (IrList *ir, Blocks blocks) {
	IrRef *relocs = malloc(ir->len * sizeof(IrRef));
	IrList out = {.params = ir->params, .entry = ir->entry};

	for (u32 i = 0; i < ir->len; i++) relocs[i] = IR_REF_NONE;

	for (u32 i = 0; i < blocks.len; i++) {
		Block *blk = blocks.ptr[i];
		IrRefList mems = blk->mem_instructions;
		IrRefList insts = blk->ordered_instructions;

		blk->first_inst = out.len;
		for (u32 j = 0; j < insts.len; j++)
			insts.ptr[j] = copyUsed(*ir, insts.ptr[j], &out, relocs);

		u32 dest_idx = 0;
		for (u32 j = 0; j < mems.len; j++) {
			assert(mems.ptr[j] != IR_REF_NONE);

			if (relocs[mems.ptr[j]] != IR_REF_NONE) {
				mems.ptr[dest_idx] = relocs[mems.ptr[j]];
				dest_idx++;
			}
		}
		blk->mem_instructions.len = dest_idx;

		if (blk->exit.kind == Exit_Branch)
			blk->exit.branch.condition = copyUsed(*ir, blk->exit.branch.condition, &out, relocs);
		if (blk->exit.kind == Exit_Return && blk->exit.ret != IR_REF_NONE)
			blk->exit.ret = copyUsed(*ir, blk->exit.ret, &out, relocs);

		blk->inst_end = out.len;
	}

	for (u32 i = 0; i < out.len; i++) applyRelocs(out, i, relocs);

	free(relocs);
	free(ir->ptr);
	*ir = out;
}

void calcLifetimes (IrList ir, ValuesSpan lastuses) {
	assert(ir.len == lastuses.len);
	IrRef *uses = lastuses.ptr;
	memset(uses, 0, sizeof(IrRef) * lastuses.len);
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		BINOP_CASES:
			uses[inst.binop.lhs] = i;
			if (inst.binop.rhs != IR_REF_NONE)
				uses[inst.binop.rhs] = i;
			break;
		UNOP_CASES:
			uses[inst.unop] = i;
			break;
		UNOP_CONST_CASES:
			uses[inst.unop_const.val] = i;
			break;
		case Ir_Store:
		case Ir_StoreVolatile:
			uses[inst.mem.source] = i;
			FALLTHROUGH;
		case Ir_Load:
		case Ir_LoadVolatile:
			uses[inst.mem.address] = i;
			if (inst.mem.ordered_after != IR_REF_NONE)
				uses[inst.mem.ordered_after] = i;
			break;
		case Ir_StackAlloc:
			uses[inst.alloc.size] = i;
			break;
		case Ir_PhiOut:
			uses[inst.phi_out.source] = i;
			break;
		case Ir_Call:
			uses[inst.call.function_ptr] = i;
			ValuesSpan params = inst.call.parameters;
			for (u32 p = 0; p < params.len; p++) {
				uses[params.ptr[p]] = i;
			}
			break;
		ZEROOP_CASES:
			break;
		}
	}
}


// TODO Schedule ordered by id.
static void scheduleBlockStraight (Arena *a, Block *blk, Block *prev, Blocks *dest, u32 id) {
	if (prev)
		PUSH_A(a, blk->incoming, prev);
	if (blk->visited == id) return;

	blk->visited = id;
	PUSH (*dest, blk);

	switch (blk->exit.kind) {
	case Exit_Unconditional:
		scheduleBlockStraight(a, blk->exit.unconditional, blk, dest, id);
		break;
	case Exit_Branch:
// 		if (blk->exit.branch.on_false->id < blk->exit.branch.on_true)
		scheduleBlockStraight(a, blk->exit.branch.on_false, blk, dest, id);
		scheduleBlockStraight(a, blk->exit.branch.on_true, blk, dest, id);
		break;
	case Exit_Switch: {
		Switch s = blk->exit.switch_;
		for (u32 i = 0; i < s.cases.len; i++)
			scheduleBlockStraight(a, s.cases.ptr[i].dest, blk, dest, id);
		scheduleBlockStraight(a, s.default_case, blk, dest, id);
	} break;
	case Exit_Return: break;
	case Exit_None: unreachable;
	}

// 	RECURSE_NEXT_BLOCKS(scheduleBlockStraight, blk, blk, dest, id);
}


void scheduleBlocksStraight (Arena *a, Block *blk, Blocks *dest) {
	static u32 invocation = 1;
	scheduleBlockStraight(a, blk, NULL, dest, invocation);
	invocation++;
}


void calcUsage (IrList ir, u16 *usage) {
	memset(usage, 0, sizeof(usage[0]) * ir.len);

	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch ((InstKind) inst.kind) {
		BINOP_CASES:
			usage[inst.binop.lhs]++;
			if (inst.binop.rhs != IR_REF_NONE)
				usage[inst.binop.rhs]++;
			break;
		UNOP_CASES:
			usage[inst.unop]++;
			break;
		UNOP_CONST_CASES:
			usage[inst.unop_const.val]++;
			break;
		case Ir_Store:
		case Ir_StoreVolatile:
			usage[inst.mem.source]++;
			FALLTHROUGH;
		case Ir_Load:
		case Ir_LoadVolatile:
			usage[inst.mem.address]++;
			if (inst.mem.ordered_after != IR_REF_NONE)
				usage[inst.mem.ordered_after]++;
			break;
		case Ir_StackAlloc:
			usage[inst.alloc.size]++;
			break;
		case Ir_PhiOut:
			usage[inst.phi_out.source]++;
			break;
		case Ir_Call:
			if (inst.call.ordered_after != IR_REF_NONE)
				usage[inst.call.ordered_after]++;
			usage[inst.call.function_ptr]++;
			ValuesSpan params = inst.call.parameters;
			for (u32 p = 0; p < params.len; p++)
				usage[params.ptr[p]]++;
			break;
		ZEROOP_CASES:
			break;
		}
	}
}


void arithSimplify (IrList ir, u16 *uses) {
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		case Ir_Equals: {
			Inst lhs = ir.ptr[inst.binop.lhs];
			Inst rhs = ir.ptr[inst.binop.rhs];
			// Handle logical not.
			if (rhs.kind == Ir_Constant && rhs.constant == 0 && uses[inst.binop.lhs] == 1) {
				switch (lhs.kind) {
				case Ir_LessThan:
					ir.ptr[i].kind = Ir_LessThanOrEquals;
					ir.ptr[i].binop.lhs = lhs.binop.rhs;
					ir.ptr[i].binop.rhs = lhs.binop.lhs;
					break;
				case Ir_LessThanOrEquals:
					ir.ptr[i].kind = Ir_LessThanOrEquals;
					ir.ptr[i].binop.lhs = lhs.binop.rhs;
					ir.ptr[i].binop.rhs = lhs.binop.lhs;
					break;
				case Ir_Equals:
					if (uses[i] == 0) {
						// Used for a branch: the positive result is not
						// restricted to the value 1, so a double
						// negation can be elided.
						rhs = ir.ptr[lhs.binop.rhs];
						if (rhs.kind == Ir_Constant && rhs.constant == 0)
							replaceWithCopy(ir, i, lhs.binop.lhs, IR_REF_NONE);
					}
					break;
				default: break;
				}
			}
		} break;
		default: break;
		}
	}
}


static void decopy (IrList ir, IrRef *ref) {
	while (ir.ptr[*ref].kind == Ir_Copy) {
		assert(ir.ptr[*ref].binop.lhs != *ref);
		*ref = ir.ptr[*ref].binop.lhs;
	}
}

static void decopyOrder (IrList ir, IrRef *ref) {
	while (*ref != IR_REF_NONE && ir.ptr[*ref].kind == Ir_Copy) {
		assert(ir.ptr[*ref].binop.rhs != *ref);
		*ref = ir.ptr[*ref].binop.rhs;
	}
}


static bool isMemInstruction(InstKind inst) {
	switch (inst) {
	case Ir_Load:
	case Ir_LoadVolatile:
	case Ir_Store:
	case Ir_StoreVolatile:
	case Ir_Call:
		return true;
	default:
		return false;
	}
}

static bool isOrderedInstruction(InstKind inst) {
	switch (inst) {
	case Ir_Store:
	case Ir_StoreVolatile:
	case Ir_Call:
	case Ir_PhiOut:
	case Ir_VaArg:
		return true;
	default:
		return false;
	}
}


// Remove memory instructions and side-effecting instructions from their
// lists if they were replaced by copies.
static void cleanUpCopyReplacements(IrList ir, Block *blk) {
	IrRefList mem = blk->mem_instructions;
	IrRefList se = blk->ordered_instructions;

	u32 dest_idx = 0;
	for (u32 k = 0; k < mem.len; k++) {
		if (isMemInstruction(ir.ptr[mem.ptr[k]].kind)) {
			mem.ptr[dest_idx] = mem.ptr[k];
			dest_idx++;
		}
	}
	blk->mem_instructions.len = dest_idx;
	dest_idx = 0;
	for (u32 k = 0; k < se.len; k++) {
		if (isOrderedInstruction(ir.ptr[se.ptr[k]].kind)) {
			se.ptr[dest_idx] = se.ptr[k];
			dest_idx++;
		}
	}
	blk->ordered_instructions.len = dest_idx;
}

void resolveCopies (IrList ir, Blocks blocks) {
	for (u32 i = 0; i < ir.len; i++) {
		Inst *inst = &ir.ptr[i];
		switch (inst->kind) {
		BINOP_CASES:
			decopy(ir, &inst->binop.lhs);
			if (inst->binop.rhs != IR_REF_NONE)
				decopy(ir, &inst->binop.rhs);
			break;
		UNOP_CASES:
			decopy(ir, &inst->unop);
			break;
		UNOP_CONST_CASES:
			decopy(ir, &inst->unop_const.val);
			break;
		case Ir_Store:
		case Ir_StoreVolatile:
			decopy(ir, &inst->mem.source);
			FALLTHROUGH;
		case Ir_Load:
		case Ir_LoadVolatile:
			decopy(ir, &inst->mem.address);
			decopyOrder(ir, &inst->mem.ordered_after);
			break;
		case Ir_StackAlloc:
			decopy(ir, &inst->alloc.size);
			break;
		case Ir_PhiOut:
			decopy(ir, &inst->phi_out.source);
			break;
		case Ir_Call:
			decopyOrder(ir, &inst->call.ordered_after);
			decopy(ir, &inst->call.function_ptr);
			ValuesSpan params = inst->call.parameters;
			for (u32 p = 0; p < params.len; p++)
				decopy(ir, &params.ptr[p]);
			break;
		ZEROOP_CASES:
			break;
		}
	}

	for (u32 i = 0; i < blocks.len; i++) {
		Block *blk = blocks.ptr[i];
		if (blk->exit.kind == Exit_Return && blk->exit.ret != IR_REF_NONE)
			decopy(ir, &blk->exit.ret);
		else if (blk->exit.kind == Exit_Branch)
			decopy(ir, &blk->exit.branch.condition);
		else if (blk->exit.kind == Exit_Switch)
			decopy(ir, &blk->exit.switch_.value);
	}
// 	for (u32 i = 0; i < blocks.len; i++)
// 		cleanUpCopyReplacements(ir, blocks.ptr[i]);
}

typedef u32 PropId;



void storeLoadPropagate (IrList ir, Blocks blocks) {
	Inst *insts = ir.ptr;

	for (u32 b = 0; b < blocks.len; b++) {
		IrRefList mem = blocks.ptr[b]->mem_instructions;

		for (u32 i = 0; i < mem.len; i++) {
			IrRef ref = mem.ptr[i];
			Inst inst = insts[ref];

			if (inst.kind == Ir_Load && inst.mem.ordered_after != IR_REF_NONE) {
				Inst store = insts[inst.mem.ordered_after];
				if (store.kind == Ir_Store && store.mem.address == inst.mem.address)
					replaceWithCopy(ir, ref, store.mem.source, store.mem.ordered_after);
			}
		}
		cleanUpCopyReplacements(ir, blocks.ptr[b]);
	}
}

typedef enum {
	Alias_Never,
	Alias_Maybe,
	Alias_Always,
} Alias;

Alias canAlias(IrList ir, Inst inst, IrRef address1) {
	switch (inst.kind) {
	case Ir_Call:
		return Alias_Maybe;
	case Ir_Load:
	case Ir_LoadVolatile:
	case Ir_Store:
	case Ir_StoreVolatile: {
		IrRef address2 = inst.mem.address;
		if (address1 == address2)
			return Alias_Always;
		Inst address_inst1 = ir.ptr[address1];
		Inst address_inst2 = ir.ptr[address2];
		if (address_inst1.kind == Ir_StackAlloc) {
			if (address_inst2.kind == Ir_StackAlloc)
				return Alias_Never;
			Inst tmp = address_inst2;
			address_inst2 = address_inst1;
			address_inst1 = tmp;
		}
		if (address_inst2.kind == Ir_StackAlloc && (address_inst1.kind == Ir_Reloc || address_inst1.kind == Ir_Parameter))
			return Alias_Never;
		if (address_inst1.kind == Ir_Reloc && address_inst2.kind == Ir_Reloc) {
			if (address_inst1.reloc.id != address_inst2.reloc.id)
				return Alias_Never;
			if (address_inst1.reloc.offset == address_inst2.reloc.offset)
				return Alias_Always;
			// TODO Calculate overlap
		}

		// TODO A StackAlloc can also never alias a parameter or a global
		return Alias_Maybe;
	}
	case Ir_Copy:
		return Alias_Never;
	default:
		unreachable;
	}
}

void innerBlockPropagate (IrList ir, Blocks blocks) {
	Inst *insts = ir.ptr;

	for (u32 b = 0; b < blocks.len; b++) {
		IrRefList mem = blocks.ptr[b]->mem_instructions;

		for (u32 i = 0; i < mem.len; i++) {
			IrRef ref = mem.ptr[i];
			Inst inst = insts[ref];

			if (inst.kind == Ir_Load || inst.kind == Ir_Store) {
				while (inst.mem.ordered_after != IR_REF_NONE) {
					Inst prev = insts[inst.mem.ordered_after];
					Alias a = canAlias(ir, prev, inst.mem.address);
					if (a == Alias_Never) {
						inst.mem.ordered_after = prev.mem.ordered_after;
					} else if (a == Alias_Always) {
						if (prev.kind == Ir_Store || prev.kind == Ir_StoreVolatile) {
							if (inst.kind == Ir_Load)
								replaceWithCopy(ir, ref, prev.mem.source, prev.mem.ordered_after);
							else {
								// TODO
							}
							break;
						} else {
							assert(prev.kind == Ir_Load || prev.kind == Ir_LoadVolatile);
							if (inst.kind == Ir_Load)
								replaceWithCopy(ir, ref, inst.mem.ordered_after, prev.mem.ordered_after);
							break;
						}
					} else {
						break;
					}
				}
			}
		}
		cleanUpCopyReplacements(ir, blocks.ptr[b]);
	}
}
