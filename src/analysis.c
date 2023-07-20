#include "analysis.h"
#include "ir_gen.h"


/*

Routines for analyzing and transforming the IR (transformations actually
should be moved into a separate file at some point).

*/

#define ON_OPERANDS_AND_ORDER(ir, inst, operand_var, operation, ordering_operation)	\
	do {	\
		IrRef *operand_var;	\
		switch ((InstKind) (inst).kind) {	\
		case Ir_Copy:	\
			if ((inst).binop.lhs != IDX_NONE) /*Copying IDX_NONE means deletion.*/ {	\
				operand_var = &(inst).binop.lhs; operation;	\
			} \
			operand_var = &(inst).binop.rhs; ordering_operation;	\
			break;	\
		BINOP_CASES:	\
			operand_var = &(inst).binop.lhs; operation;	\
			operand_var = &(inst).binop.rhs; operation;	\
			break;	\
		UNOP_CASES:	\
			operand_var = &(inst).unop; operation;	\
			break;	\
		UNOP_CONST_CASES:	\
			operand_var = &(inst).unop_const.val; operation;	\
			break;	\
		case Ir_Store:	\
			operand_var = &(inst).mem.source; operation;	\
			FALLTHROUGH;	\
		case Ir_Load:	\
			operand_var = &(inst).mem.address; operation;	\
			operand_var = &(inst).mem.ordered_after; ordering_operation;	\
			break;	\
		case Ir_StackDeallocVLA:	\
			operand_var = &(inst).mem.address; operation;	\
			operand_var = &(inst).mem.ordered_after; ordering_operation;	\
			break;	\
		case Ir_PhiOut:	\
			operand_var = &(inst).phi_out.source; operation;	\
			break;	\
		case Ir_Call:	\
			operand_var = &(inst).call.ordered_after; ordering_operation;	\
			operand_var = &(inst).call.function_ptr; operation;	\
	\
			Call EXPANSION_call = AUX_DATA(Call, (ir), (inst).call.data);	\
			for (u32 p = 0; p < EXPANSION_call.arguments.len; p++) {	\
				operand_var = &EXPANSION_call.arguments.ptr[p].arg_inst; operation;	\
			}	\
			break;	\
		ZEROOP_CASES:	\
			break;	\
		}	\
	} while (0)


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



static IrRef copyUsed (const IrList *source, IrRef i, IrList *dest, IrRef *relocs) {
	if (relocs[i] != IDX_NONE) return relocs[i];

	Inst inst = source->ptr[i];

	// This needs absence of cycles.
	ON_OPERANDS_AND_ORDER(*source, inst, op, copyUsed(source, *op, dest, relocs),
		if (*op != IDX_NONE) copyUsed(source, *op, dest, relocs));

	IrRef new = dest->len++;
	dest->ptr[new] = inst;
	dest->locations[new] = source->locations[i];
	relocs[i] = new;

	return new;
}


static void applyRelocs (IrList ir, IrRef i, IrRef *relocs) {
	Inst *inst = &ir.ptr[i];
	if (inst->kind == Ir_PhiOut) {
		if (inst->phi_out.on_true != IDX_NONE)
			inst->phi_out.on_true = relocs[inst->phi_out.on_true];
		if (inst->phi_out.on_false != IDX_NONE)
			inst->phi_out.on_false = relocs[inst->phi_out.on_false];
	}

	ON_OPERANDS_AND_ORDER(ir, *inst, op, *op = relocs[*op],
		if (*op != IDX_NONE) *op = relocs[*op]);
}

void decimateIr (IrList *ir, Blocks blocks) {
	IrRef *relocs = malloc(ir->len * sizeof(IrRef));
	IrList out = {
		.ptr = calloc(ir->len, sizeof(*ir->ptr)),
		.locations = calloc(ir->len, sizeof(*ir->locations)),
		.capacity = ir->len,

		.aux_data = ir->aux_data,
		.params = ir->params,
		.entry = ir->entry
	};

	for (u32 i = 0; i < ir->len; i++) relocs[i] = IDX_NONE;

	for (u32 i = 0; i < blocks.len; i++) {
		Block *blk = blocks.ptr[i];
		IrRefList mems = blk->mem_instructions;
		IrRefList insts = blk->ordered_instructions;

		blk->first_inst = out.len;
		for (u32 j = 0; j < insts.len; j++)
			insts.ptr[j] = copyUsed(ir, insts.ptr[j], &out, relocs);

		u32 dest_idx = 0;
		for (u32 j = 0; j < mems.len; j++) {
			assert(mems.ptr[j] != IDX_NONE);

			if (relocs[mems.ptr[j]] != IDX_NONE) {
				mems.ptr[dest_idx] = relocs[mems.ptr[j]];
				dest_idx++;
			}
		}
		blk->mem_instructions.len = dest_idx;

		switch (blk->exit.kind) {
		case Exit_Branch:
			blk->exit.branch.condition = copyUsed(ir, blk->exit.branch.condition, &out, relocs);
			break;
		case Exit_Return:
			if (blk->exit.ret != IDX_NONE)
				blk->exit.ret = copyUsed(ir, blk->exit.ret, &out, relocs);
			break;
		case Exit_Switch:
			blk->exit.switch_.value = copyUsed(ir, blk->exit.switch_.value, &out, relocs);
			break;
		case Exit_Unconditional: break;
		case Exit_None:
			unreachable;
		}
		blk->inst_end = out.len;
	}

	for (u32 i = 0; i < out.len; i++) applyRelocs(out, i, relocs);

	free(relocs);
	free(ir->ptr);
	free(ir->locations);
	*ir = out;
}

void calcLifetimes (IrList ir, IrRefList lastuses) {
	assert(ir.len == lastuses.len);
	IrRef *uses = lastuses.ptr;
	memset(uses, 0, sizeof(IrRef) * lastuses.len);

	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		ON_OPERANDS_AND_ORDER(ir, inst, op, uses[*op] = i, (void) 0);
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
		ON_OPERANDS_AND_ORDER(ir, inst, op, usage[*op]++, (void) 0);
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
							replaceWithCopy(ir, i, lhs.binop.lhs, IDX_NONE);
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
	while (*ref != IDX_NONE && ir.ptr[*ref].kind == Ir_Copy) {
		assert(ir.ptr[*ref].binop.rhs != *ref);
		*ref = ir.ptr[*ref].binop.rhs;
	}
}


static bool isMemInstruction(InstKind inst) {
	switch (inst) {
	case Ir_Load:
	case Ir_Store:
	case Ir_Call:
	case Ir_VaArg:
		return true;
	default:
		return false;
	}
}

static bool isOrderedInstruction(InstKind inst) {
	switch (inst) {
	case Ir_Store:
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
		ON_OPERANDS_AND_ORDER(ir, ir.ptr[i], op,
			decopy(ir, op), decopyOrder(ir, op));
	}

	for (u32 i = 0; i < blocks.len; i++) {
		Block *blk = blocks.ptr[i];
		if (blk->exit.kind == Exit_Return && blk->exit.ret != IDX_NONE)
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

			if (inst.kind == Ir_Load && inst.mem.ordered_after != IDX_NONE) {
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
	Alias_AlwaysPartial,
	Alias_AlwaysExact,
} Alias;

Alias canAlias(IrList ir, Inst inst, IrRef address1, u16 size1) {
	switch (inst.kind) {
	case Ir_Call:
		return Alias_Maybe;
	case Ir_Load:
	case Ir_Store: {
		IrRef address2 = inst.mem.address;
		u32 size2 = inst.size;
		if (address1 == address2) {
			return size1 == size2 ? Alias_AlwaysExact : Alias_AlwaysPartial;
		}
		Inst address_inst1 = ir.ptr[address1];
		Inst address_inst2 = ir.ptr[address2];

		// TODO Ir_StackAllocVLA cannot alias either
		if (address_inst1.kind == Ir_StackAllocFixed) {
			if (address_inst2.kind == Ir_StackAllocFixed)
				return Alias_Never;
			Inst tmp = address_inst2;
			address_inst2 = address_inst1;
			address_inst1 = tmp;
		}
		if (address_inst2.kind == Ir_StackAllocFixed && (address_inst1.kind == Ir_Reloc || address_inst1.kind == Ir_Parameter))
			return Alias_Never;
		if (address_inst1.kind == Ir_Reloc && address_inst2.kind == Ir_Reloc) {
			if (address_inst1.reloc.id != address_inst2.reloc.id)
				return Alias_Never;
			u32 offset1 = address_inst1.reloc.offset;
			u32 offset2 = address_inst2.reloc.offset;
			if (offset1 == offset2 && size1 == size2)
				return Alias_AlwaysExact;
			if (offset1 < offset2 + size2 && offset1 + size1 > offset2)
				return Alias_AlwaysPartial;
			else
				return Alias_Never;
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

static inline bool instVolatile (Inst i) { return i.properties & Prop_Mem_Volatile; }

void innerBlockPropagate (IrList ir, Blocks blocks) {
	Inst *insts = ir.ptr;

	for (u32 b = 0; b < blocks.len; b++) {
		IrRefList mem = blocks.ptr[b]->mem_instructions;

		for (u32 i = 0; i < mem.len; i++) {
			IrRef ref = mem.ptr[i];
			Inst inst = insts[ref];

			if ((inst.kind == Ir_Load || inst.kind == Ir_Store) && !instVolatile(inst)) {
				while (inst.mem.ordered_after != IDX_NONE) {
					IrRef prev_ref = inst.mem.ordered_after;
					Inst prev = insts[prev_ref];

					Alias a = canAlias(ir, prev, inst.mem.address, inst.size);
					if (a == Alias_Never) {
						inst.mem.ordered_after = prev.mem.ordered_after;
					} else if (a == Alias_AlwaysExact) {
						if (prev.kind == Ir_Store) {
							if (inst.kind == Ir_Load) {
								// Replace the load with a copy of the stored data
								replaceWithCopy(ir, ref, prev.mem.source, prev.mem.ordered_after);
								break;
							} else {
								if (instVolatile(prev))
									break;
								replaceWithNop(ir, prev_ref);
								inst.mem.ordered_after = prev.mem.ordered_after;
							}
						} else {
							assert(prev.kind == Ir_Load);
							if (inst.kind == Ir_Load)
								replaceWithCopy(ir, ref, inst.mem.ordered_after, prev.mem.ordered_after);
							break;
						}
					} else {
						// Loads should always be able to move past each
						// other. A store ordered after one of the loads
						// would get misplaced by that though.
						// Ergo: ordered_after is a bad system.
						break;
					}
				}
			}
		}
		cleanUpCopyReplacements(ir, blocks.ptr[b]);
	}
}
