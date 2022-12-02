#include "ir_gen.h"

// Some constant folding is performed here.
// Some arithmetic simplifications require e.g. use-counts, and need to be performed in a separate step.


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
	// TODO constant propagation

	return append(build, (Inst) {Ir_SignExtend, target, {source}});
}

IrRef genZeroExt (IrBuild *build, IrRef source, u16 target) {
	Inst *inst = build->ir.ptr;
	assert(inst[source].size < target);
	if (inst[source].kind == Ir_Constant)
		return genImmediateInt(build, inst[source].constant, target);

	return append(build, (Inst) {Ir_ZeroExtend, target, {source}});
}

IrRef genCall (IrBuild *build, IrRef func, ValuesSpan args) {
	IrRef call = append(build, (Inst) {Ir_Call, .call = {func, args}});
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

void discardIrBuilder(IrBuild *builder) {
	// TODO Free loose blocks
	free(builder->ir.ptr);
}


typedef unsigned long ulong;
typedef unsigned int uint;
void printBlock (Block *blk, IrList ir) {
	if (blk->visited)
		return;
	blk->visited = true;

	printf(" ");
	printString(blk->label);
	printf(":\n");
	for (size_t i = blk->first_inst; i <= blk->last_inst; i++) {
		printf(" %3lu = ", (ulong) i);
		Inst inst = ir.ptr[i];

		switch ((InstKind) inst.kind) {
		case Ir_Reloc:
			printf("global %d %+lld\n", inst.reloc.id, (long long)inst.reloc.offset);
			continue;
		case Ir_Constant:
			printf("const 0x%lx (%lu)\n", (ulong) inst.constant, (ulong) inst.constant);
			continue;
		case Ir_Call: {
			printf("call %lu (", (ulong) inst.call.function_ptr);
			ValuesSpan params = inst.call.parameters;
			for (u32 i = 0; i < params.len; i++) {
				printf("%lu", (ulong) params.ptr[i]);
				if (i + 1 < params.len)
					printf(", ");
			}
			printf(")\n");
		} continue;
		case Ir_Phi:
			printf("phi ");
			for (u32 i = 0; i < inst.phi.len; i++) {
				printf("%lu", (ulong) inst.phi.ptr[i]); break;
				if (i + 1 < inst.phi.len)
					printf(", ");
			}
			continue;
		case Ir_Parameter:
			printf("param %lu", (ulong) inst.size);
			continue;
		case Ir_StackAlloc:
			printf("stack %lu", (ulong) inst.alloc.size);
			continue;
		case Ir_StackDealloc:
			printf("discard %lu", (ulong) inst.unop);
			continue;
		case Ir_Load:
			printf("load i%ud, %lu", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_Store:
			printf("store [%lu] %lu", (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
			continue;
		case Ir_BitNot:
			printf("not %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Truncate:
			printf("trunc i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_SignExtend:
			printf("signex i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_ZeroExtend:
			printf("zeroex i%ud, %lu\n", (uint) inst.size, (ulong) inst.unop);
			continue;
		case Ir_Access:
			printf("access i%ud, %lu @ %lu\n", (uint) inst.size, (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
			continue;
		case Ir_IntToFloat:
			printf("int->float %lu\n", (ulong) inst.unop);
			continue;
		case Ir_FloatToInt:
			printf("float->int %lu\n", (ulong) inst.unop);
			continue;
		case Ir_Add: printf("add"); break;
		case Ir_Sub: printf("sub"); break;
		case Ir_Mul: printf("mul"); break;
		case Ir_Div: printf("div"); break;
		case Ir_BitAnd: printf("and"); break;
		case Ir_BitOr: printf("or"); break;
		case Ir_BitXor: printf("xor"); break;
		case Ir_LessThan: printf("cmp<"); break;
		case Ir_LessThanOrEquals: printf("cmp<="); break;
		case Ir_Equals: printf("cmp=="); break;
		}
		printf("%lu %lu\n", (ulong) inst.binop.lhs, (ulong) inst.binop.rhs);
	}

	Exit exit = blk->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		printf("       jmp ");
		printString(exit.unconditional->label);
		printf("\n");
		printBlock(exit.unconditional, ir);
		break;
	case Exit_Branch:
		printf("       branch %lu ? ", (ulong) exit.branch.condition);
		printString(exit.branch.on_true->label);
		printf(" : ");
		printString(exit.branch.on_false->label);
		printf("\n");
		printBlock(exit.branch.on_true, ir);
		printBlock(exit.branch.on_false, ir);
		break;
	case Exit_Return:
		if (exit.ret == IR_REF_NONE)
			printf("       ret\n");
		else
			printf("       ret %lu\n", (ulong) exit.ret);
		break;
	default: {}
	}
}

