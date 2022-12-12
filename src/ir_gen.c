#include "ir_gen.h"

/*

Generates IR, while doing a first constant-folding pass, and prints it in a nicely formatted form.
Some arithmetic simplifications require e.g. use-counts, and need to be
performed in a separate step.

*/

#define HAS_EXITED(ir) ((ir)->insertion_block->exit.kind != Exit_None)
// TODO Store pointer size in IrBuild
#define PTR_SIZE I64


IrRef append (IrBuild *build, Inst inst) {
	PUSH (build->ir, inst);
	return build->ir.len - 1;
}

IrRef genParameter (IrBuild *build, u16 size) {
	return append(build, (Inst) {Ir_Parameter, size});
}

IrRef genStackAllocFixed (IrBuild *build, u32 size) {
	IrRef r = genImmediateInt(build, size, I32);
	return genStackAlloc(build, r);
}
IrRef genStackAlloc (IrBuild *build, IrRef size) {
	return append(build, (Inst) {Ir_StackAlloc, PTR_SIZE, {.alloc = {size}}});
}

void genReturnVal (IrBuild *build, IrRef val) {
	build->insertion_block->last_inst = build->ir.len - 1;
	build->insertion_block->exit = (Exit) {Exit_Return, .ret = val};
}

void genBranch (IrBuild *build, IrRef condition) {
	build->insertion_block->last_inst = build->ir.len - 1;
	build->insertion_block->exit = (Exit) {
		Exit_Branch,
		.branch = {condition}
	};
}

void genJump (IrBuild *build, Block *dest) {
	build->insertion_block->last_inst = build->ir.len - 1;
	build->insertion_block->exit = (Exit) {
		Exit_Unconditional,
		.unconditional = dest
	};
	if (dest)
		startBlock(build, dest);
}

Block *newBlock (Arena *arena, String label) {
	Block *new_block = ALLOC(arena, Block);
	*new_block = (Block) { .label = label };
	return new_block;
}

void startBlock (IrBuild *build, Block *block) {
	block->first_inst = build->ir.len;
	build->insertion_block = block;
}

Block *startNewBlock (IrBuild *build, Arena *arena, String label) {
	Block *blk = newBlock(arena, label);
	startBlock(build, blk);
	return blk;
}


static IrRef genBinOp (IrBuild *build, InstKind op, IrRef a, IrRef b) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {op, inst[a].size, .binop = {a, b}};

	bool commutative = op == Ir_Add || op == Ir_Mul || op == Ir_BitAnd || op == Ir_BitOr || op == Ir_BitXor;
	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		switch (op) {
		case Ir_Add:
			i.constant = inst[a].constant + inst[b].constant; break;
		case Ir_Sub:
			i.constant = inst[a].constant - inst[b].constant; break;
		case Ir_Mul:
			i.constant = inst[a].constant * inst[b].constant; break;
		case Ir_Div:
			i.constant = inst[a].constant / inst[b].constant; break;
		case Ir_BitAnd:
			i.constant = inst[a].constant & inst[b].constant; break;
		case Ir_BitOr:
			i.constant = inst[a].constant | inst[b].constant; break;
		case Ir_BitXor:
			i.constant = inst[a].constant ^ inst[b].constant; break;
		case Ir_LessThan:
			i.constant = inst[a].constant < inst[b].constant; break;
		case Ir_LessThanOrEquals:
			i.constant = inst[a].constant <= inst[b].constant; break;
		default:
			unreachable;
		}
	} else if (commutative && inst[a].kind == Ir_Constant) {
		i.binop.lhs = b;
		i.binop.rhs = a;
	}
	if ((op == Ir_Add || op == Ir_Sub) &&
		inst[i.binop.lhs].kind == Ir_Reloc &&
		inst[i.binop.rhs].kind == Ir_Constant)
	{
		i.kind = Ir_Reloc;
		i64 delta = op == Ir_Add ? (i64)inst[i.binop.rhs].constant : -(i64)inst[i.binop.rhs].constant;
		i.reloc.id = inst[i.binop.lhs].reloc.id;
		i.reloc.offset = inst[i.binop.lhs].reloc.offset + delta;
	}
	return append(build, i);
}

IrRef genAdd (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_Add, a, b);
}

IrRef genSub (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_Sub, a, b);
}


IrRef genMul (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_Mul, a, b);
}

IrRef genDiv (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_Div, a, b);
}

IrRef genOr (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_BitOr, a, b);
}

IrRef genXor (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_BitXor, a, b);
}

IrRef genAnd (IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_BitAnd, a, b);
}


IrRef genBitNot (IrBuild *build, IrRef a) {
	Inst *inst = build->ir.ptr;
	Inst i = {Ir_BitNot, inst[a].size, .unop = a};
	if (inst[a].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = ~inst[a].constant;
	}
	return append(build, i);
}

IrRef genNot(IrBuild *build, IrRef a) {
	Inst *inst = build->ir.ptr;
	Inst i = {Ir_Equals, inst[a].size};

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

IrRef genEquals(IrBuild *build, IrRef a, IrRef b, u16 size) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {Ir_Equals, size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant) {
		if (inst[b].kind == Ir_Constant) {
			i.kind = Ir_Constant;
			i.constant = inst[a].constant == inst[b].constant;
		} else {
			i.binop.lhs = b;
			i.binop.rhs = a;
		}
	}

	if (inst[a].kind == Ir_Reloc) {
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

IrRef genLessThan(IrBuild *build, IrRef a, IrRef b, u16 size) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {Ir_LessThan, size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant < inst[b].constant;
	}

	return append(build, i);
}

IrRef genLessThanOrEquals(IrBuild *build, IrRef a, IrRef b, u16 size) {
	Inst *inst = build->ir.ptr;
	assert(inst[a].size == inst[b].size);
	Inst i = {Ir_LessThanOrEquals, size, .binop = {a, b}};

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant <= inst[b].constant;
	}

	return append(build, i);
}

IrRef genImmediateInt (IrBuild *build, long long i, u16 size) {
	return append(build, (Inst) {Ir_Constant, size, {.constant = i}});
}

IrRef genImmediateReal (IrBuild *build, double r) {
	(void) r;
	(void) build;
	return 0; // TODO
}

IrRef genTrunc (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size > target);
	if (inst[source].kind == Ir_Constant)
		return genImmediateInt(build, inst[source].constant & (((u64) 1 << target*8) - 1), target);

	return append(build, (Inst) {Ir_Truncate, target, {source}});
}

IrRef genSignExt (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size < target);
	if (inst[source].kind == Ir_Constant) {
		// TODO Actually sign extend the constant
		return genImmediateInt(build, inst[source].constant, target);
	}


	return append(build, (Inst) {Ir_SignExtend, target, {source}});
}

IrRef genZeroExt (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size < target);
	if (inst[source].kind == Ir_Constant)
		return genImmediateInt(build, inst[source].constant, target);

	return append(build, (Inst) {Ir_ZeroExtend, target, {source}});
}

IrRef genCall (IrBuild *build, IrRef func, ValuesSpan args, u16 size) {
	IrRef call = append(build, (Inst) {Ir_Call, size, .call = {func, args}});
	PUSH (build->insertion_block->side_effecting_instructions, call);
	return call;
}

IrRef genGlobal (IrBuild *build, u32 id) {
	return append(build, (Inst) {Ir_Reloc, PTR_SIZE, .binop = {id}});
}

IrRef genLoad (IrBuild *build, IrRef ref, u16 size) {
	IrRef load = append(build, (Inst) {Ir_Load, size, .unop = ref});
	PUSH (build->insertion_block->mem_instructions, load);
	return load;
}

IrRef genStore (IrBuild *build, IrRef dest, IrRef value) {
	Inst val = build->ir.ptr[value];
	// TODO Assert dest->size == target pointer size
	IrRef store = append(build, (Inst) {Ir_Store, val.size, .binop = {dest, value}});
	PUSH (build->insertion_block->mem_instructions, store);
	PUSH (build->insertion_block->side_effecting_instructions, store);
	return store;
}

IrRef genPhi (IrBuild *build, Arena *arena, IrRef *insts, u16 size) {
	IrRef *r = insts;
	while (*r != IR_REF_NONE) r++;
	u32 count = r - insts;
	Inst inst = {Ir_Phi, size, .phi = ALLOCN(arena, IrRef, count)};
	memcpy(inst.phi.ptr, insts, count * sizeof(insts[0]));
	for (u32 i = 0; i < count; i++)
		assert(build->ir.ptr[insts[i]].size == size);


	return append(build, inst);
}

void discardIrBuilder(IrBuild *builder) {
	// TODO Free loose blocks
	free(builder->ir.ptr);
}


typedef unsigned long ulong;
typedef unsigned int uint;
void printBlock (FILE *dest, Block *blk, IrList ir) {
	if (blk->visited)
		return;
	blk->visited = true;

	fprintf(dest, " %.*s%p:\n", STRING_PRINTAGE(blk->label), (void *) blk);

	for (size_t i = blk->first_inst; i <= blk->last_inst; i++) {
		fprintf(dest, " %3lu = ", (ulong) i);
		Inst inst = ir.ptr[i];

		switch ((InstKind) inst.kind) {
		case Ir_Reloc:
			fprintf(dest, "global %d %+lld\n", inst.reloc.id, (long long)inst.reloc.offset);
			continue;
		case Ir_Constant:
			fprintf(dest, "const 0x%lx (%lu)\n", (ulong) inst.constant, (ulong) inst.constant);
			continue;
		case Ir_Call: {
			fprintf(dest, "call %lu (", (ulong) inst.call.function_ptr);
			ValuesSpan params = inst.call.parameters;
			for (u32 i = 0; i < params.len; i++) {
				fprintf(dest, "%lu", (ulong) params.ptr[i]);
				if (i + 1 < params.len)
					fprintf(dest, ", ");
			}
			fprintf(dest, ")\n");
		} continue;
		case Ir_Phi:
			fprintf(dest, "phi ");
			for (u32 i = 0; i < inst.phi.len; i++) {
				fprintf(dest, "%lu", (ulong) inst.phi.ptr[i]); break;
				if (i + 1 < inst.phi.len)
					fprintf(dest, ", ");
			}
			fprintf(dest, "\n");
			continue;
		case Ir_Parameter:
			fprintf(dest, "param %lu\n", (ulong) inst.size);
			continue;
		case Ir_StackAlloc:
			fprintf(dest, "stack %lu\n", (ulong) inst.alloc.size);
			continue;
		case Ir_StackDealloc:
			fprintf(dest, "discard %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Load:
			fprintf(dest, "load i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_Store:
			fprintf(dest, "store [%lu] %lu\n", (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
			continue;
		case Ir_BitNot:
			fprintf(dest, "not %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Truncate:
			fprintf(dest, "trunc i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_SignExtend:
			fprintf(dest, "signex i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_ZeroExtend:
			fprintf(dest, "zeroex i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_Access:
			fprintf(dest, "access i%ud, %lu @ %lu\n", (uint) inst.size, (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
			continue;
		case Ir_IntToFloat:
			fprintf(dest, "int->float %lu\n", (ulong) inst.unop);
			continue;
		case Ir_FloatToInt:
			fprintf(dest, "float->int %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Add: fprintf(dest, "add"); break;
		case Ir_Sub: fprintf(dest, "sub"); break;
		case Ir_Mul: fprintf(dest, "mul"); break;
		case Ir_Div: fprintf(dest, "div"); break;
		case Ir_BitAnd: fprintf(dest, "and"); break;
		case Ir_BitOr: fprintf(dest, "or"); break;
		case Ir_BitXor: fprintf(dest, "xor"); break;
		case Ir_LessThan: fprintf(dest, "cmp<"); break;
		case Ir_LessThanOrEquals: fprintf(dest, "cmp<="); break;
		case Ir_Equals: fprintf(dest, "cmp=="); break;
		}
		fprintf(dest, " %lu %lu\n", (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
	}

	Exit exit = blk->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		fprintf(dest, "       jmp %.*s\n", STRING_PRINTAGE(exit.unconditional->label));
		printBlock(dest, exit.unconditional, ir);
		break;
	case Exit_Branch:
		fprintf(dest, "       branch %lu ? %.*s%p : %.*s%p\n",
			(ulong) exit.branch.condition,
			STRING_PRINTAGE(exit.branch.on_true->label), (void *) exit.branch.on_true,
			STRING_PRINTAGE(exit.branch.on_false->label), (void *) exit.branch.on_false);
		printBlock(dest, exit.branch.on_true, ir);
		printBlock(dest, exit.branch.on_false, ir);
		break;
	case Exit_Return:
		if (exit.ret == IR_REF_NONE)
			fprintf(dest, "       ret\n");
		else
			fprintf(dest, "       ret %lu\n", (ulong) exit.ret);
		break;
	default: {}
	}
}

