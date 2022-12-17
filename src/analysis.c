#include "analysis.h"
#include "ir_gen.h"


/*

Routines for analyzing and transforming the IR (the latter part should
be moved into a separate file at some point).

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
	switch (inst.kind) {
	BINOP_CASES:
		copyUsed(source, inst.binop.lhs, dest, relocs);
		copyUsed(source, inst.binop.rhs, dest, relocs);
		break;
	UNOP_CASES:
		copyUsed(source, inst.unop, dest, relocs);
		break;
	case Ir_Store:
		if (inst.mem.ordered_after != IR_REF_NONE)
			copyUsed(source, inst.mem.ordered_after, dest, relocs);
		copyUsed(source, inst.mem.address, dest, relocs);
		copyUsed(source, inst.mem.source, dest, relocs);
		break;
	case Ir_Load:
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
	default: break;
	}

	IrRef new = dest->len;
	PUSH(*dest, inst);
	relocs[i] = new;
	return new;
}


static void applyRelocs (IrList ir, IrRef i, IrRef *relocs) {
	Inst *inst = &ir.ptr[i];
	switch (inst->kind) {
	BINOP_CASES:
		inst->binop.lhs = relocs[inst->binop.lhs];
		inst->binop.rhs = relocs[inst->binop.rhs];
		break;
	UNOP_CASES:
		inst->unop = relocs[inst->unop];
		break;
	case Ir_Store:
		inst->mem.source = relocs[inst->mem.source];
		FALLTHROUGH;
	case Ir_Load:
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
	default: break;
	}
}

void decimateIr (IrList *ir, Blocks blocks) {
	IrRef *relocs = malloc(ir->len * sizeof(IrRef));
	IrList out = {0};

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

		blk->last_inst = out.len - 1;
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
			uses[inst.binop.rhs] = i;
			break;
		UNOP_CASES:
			uses[inst.unop] = i;
			break;
		case Ir_Store:
			uses[inst.mem.source] = i;
			FALLTHROUGH;
		case Ir_Load:
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
		default: break;
		}
	}
}


static void scheduleBlockStraight (Block *blk, Block *prev, Blocks *dest, u32 id) {
	if (prev)
		PUSH(blk->incoming, prev);
	if (blk->visited == id) return;

	blk->visited = id;
	PUSH (*dest, blk);

	RECURSE_NEXT_BLOCKS(scheduleBlockStraight, blk, blk, dest, id);
}


void scheduleBlocksStraight (Block *blk, Blocks *dest) {
	static u32 invocation = 1;
	scheduleBlockStraight(blk, NULL, dest, invocation);
	invocation++;
}


void calcUsage (IrList ir, u16 *usage) {
	memset(usage, 0, sizeof(usage[0]) * ir.len);

	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		BINOP_CASES:
			usage[inst.binop.lhs]++;
			usage[inst.binop.rhs]++;
			break;
		UNOP_CASES:
			usage[inst.unop]++;
			break;
		case Ir_Store:
			usage[inst.mem.source]++;
			FALLTHROUGH;
		case Ir_Load:
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
		default: break;
		}
	}
}


static void decopy (IrList ir, IrRef *ref) {
	while (ir.ptr[*ref].kind == Ir_Copy)
		*ref = ir.ptr[*ref].binop.lhs;
}

static void decopyOrder (IrList ir, IrRef *ref) {
	while (*ref != IR_REF_NONE && ir.ptr[*ref].kind == Ir_Copy)
		*ref = ir.ptr[*ref].binop.rhs;
}


// Remove memory instructions and side-effecting instructions from their
// lists if they were replaced by copies.
static void cleanUpCopyReplacements(IrList ir, Block *blk) {
	IrRefList mem = blk->mem_instructions;
	IrRefList se = blk->ordered_instructions;

	u32 dest_idx = 0;
	for (u32 k = 0; k < mem.len; k++) {
		if (ir.ptr[mem.ptr[k]].kind != Ir_Copy) {
			mem.ptr[dest_idx] = mem.ptr[k];
			dest_idx++;
		}
	}
	blk->mem_instructions.len = dest_idx;
	dest_idx = 0;
	for (u32 k = 0; k < se.len; k++) {
		if (ir.ptr[se.ptr[k]].kind != Ir_Copy) {
			se.ptr[dest_idx] = se.ptr[k];
			dest_idx++;
		} else {
			unreachable; // We are not currently replacing stores or calls.
		}
	}
	blk->ordered_instructions.len = dest_idx;
}

void resolveCopies (IrList ir) {
	for (u32 i = 0; i < ir.len; i++) {
		Inst *inst = &ir.ptr[i];
		switch (inst->kind) {
		BINOP_CASES:
			decopy(ir, &inst->binop.lhs);
			decopy(ir, &inst->binop.rhs);
			break;
		UNOP_CASES:
			decopy(ir, &inst->unop);
			break;
		case Ir_Store:
			decopy(ir, &inst->mem.source);
			FALLTHROUGH;
		case Ir_Load:
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
		default: break;
		}
	}

// 	for (u32 i = 0; i < blocks.len; i++)
// 		cleanUpCopyReplacements(ir, blocks.ptr[i]);
}

typedef u32 PropId;


bool mayAlias(IrList ir, IrRef possible, IrRef target) {
	(void) ir;
	(void) possible;
	(void) target;
	return true;
}

bool doesAlias(IrList ir, IrRef possible, IrRef target) {
	(void) ir;
	(void) possible;
	(void) target;
	return false;
}

void innerBlockPropagate (IrList ir, Blocks blocks) {
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
