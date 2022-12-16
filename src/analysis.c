#include "analysis.h"
#include "ir_gen.h"

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
	relocs[i] = 0;

	Inst inst = source.ptr[i];
	switch (inst.kind) {
	BINOP_CASES:
		copyUsed(source, inst.binop.lhs, dest, relocs);
		copyUsed(source, inst.binop.rhs, dest, relocs);
		break;
	UNOP_CASES:
		copyUsed(source, inst.unop, dest, relocs);
		break;
	case Ir_StackAlloc:
		copyUsed(source, inst.alloc.size, dest, relocs);
		break;
	case Ir_PhiOut:
		copyUsed(source, inst.phi_out.source, dest, relocs);
		break;
	case Ir_Call:
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

		for (u32 j = 0; j < mems.len; j++)
			mems.ptr[j] = relocs[mems.ptr[j]];

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


void scheduleBlocksStraight (Block *blk, Blocks *dest, u32 id) {
	if (blk->visited == id) return;
	blk->visited = id;
	PUSH (*dest, blk);

	RECURSE_NEXT_BLOCKS(scheduleBlocksStraight, blk, dest, id);
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
		case Ir_StackAlloc:
			usage[inst.alloc.size]++;
			break;
		case Ir_PhiOut:
			usage[inst.phi_out.source]++;
			break;
		case Ir_Call:
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
		*ref = ir.ptr[*ref].unop;
}



void resolveCopies (IrList ir, Blocks blocks) {
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		BINOP_CASES:
			decopy(ir, &ir.ptr[i].binop.lhs);
			decopy(ir, &ir.ptr[i].binop.rhs);
			break;
		UNOP_CASES:
			decopy(ir, &ir.ptr[i].unop);
			break;
		case Ir_StackAlloc:
			decopy(ir, &ir.ptr[i].alloc.size);
			break;
		case Ir_PhiOut:
			decopy(ir, &ir.ptr[i].phi_out.source);
			break;
		case Ir_Call:
			decopy(ir, &ir.ptr[i].call.function_ptr);
			ValuesSpan params = inst.call.parameters;
			for (u32 p = 0; p < params.len; p++)
				decopy(ir, &params.ptr[p]);
			break;
		default: break;
		}
	}

	for (u32 i = 0; i < blocks.len; i++) {
		IrRefList mem = blocks.ptr[i]->mem_instructions;
		IrRefList se = blocks.ptr[i]->ordered_instructions;
		for (u32 k = 0; k < mem.len; k++)
			decopy(ir, &mem.ptr[k]);
		for (u32 k = 0; k < se.len; k++)
			decopy(ir, &se.ptr[k]);
	}
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
	PropId *generation = calloc(ir.len, sizeof(PropId));

	PropId current = 1;
	for (u32 b = 0; b < blocks.len; b++) {
		IrRefList mem = blocks.ptr[b]->mem_instructions;

		u32 last_store = 0;
		u32 last_stored = 0;
		for (u32 i = 0; i < mem.len; i++) {
			IrRef ref = mem.ptr[i];
			Inst inst = insts[ref];
			switch (inst.kind) {
			case Ir_Store:
				last_store = inst.binop.lhs;
				last_stored = inst.binop.rhs;
				break;
			case Ir_Call:
				last_store = IR_REF_NONE;
				break;
			case Ir_Load:
				if (inst.unop == last_store)
					replaceWithCopy(ir, ref, last_stored);
				break;
			default: unreachable;
			}
		}
		current++;
	}


	free(generation);
}
