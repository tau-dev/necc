#include "ir_gen.h"
#define unreachable assert(0)
#define HAS_EXITED(ir) ((ir)->insertion_block->exit.kind != Exit_None)

IrRef append (IrBuild *build, Inst inst) {
	PUSH (build->ir, inst);
	return build->ir.len - 1;
}

IrRef genParameter (IrBuild *build, Size size) {
	return append(build, (Inst) {Ir_Parameter, .unop = size});
}

IrRef genStackAlloc (IrBuild *build, Size size) {
	return append(build, (Inst) {Ir_StackAlloc, .unop = size});
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
}


Block *genNewBlock (Arena *arena, IrBuild *build) {
	SPAN(char) name = ALLOCN(arena, char, 24);
	snprintf(name.ptr, name.len, "__%d", build->block_count);
	build->block_count++;
	return genNewBlockLabeled(arena, build, (String) {strlen(name.ptr), name.ptr});
}

Block *genNewBlockLabeled (Arena *arena, IrBuild *build, String label) {
	Block *new_block = ALLOC(arena, Block);
	*new_block = (Block) {
		.label = label,
		.first_inst = build->ir.len
	};
	build->insertion_block = new_block;
	return new_block;
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
		IrRef tmp = i.binop.lhs;
		i.binop.lhs = i.binop.rhs;
		i.binop.rhs = tmp;
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

IrRef genLessThan(IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_LessThan, a, b);
}

IrRef genLessThanOrEquals(IrBuild *build, IrRef a, IrRef b) {
	return genBinOp(build, Ir_LessThanOrEquals, a, b);
}

IrRef genImmediateInt (IrBuild *build, long long i, Size size) {
	return append(build, (Inst) {Ir_Constant, size, {i}});
}

IrRef genImmediateReal (IrBuild *build, double r) {
	(void) r;
	(void) build;
	return 0; // TODO
}


IrRef genCall (IrBuild *build, IrRef func, ValuesSpan args) {
	IrRef call = append(build, (Inst) {Ir_Call, .call = {func, args}});
	PUSH (build->insertion_block->side_effecting_instructions, call);
	return call;
}

IrRef genFunctionRef (IrBuild *build, Function *func) {
	return append(build, (Inst) {Ir_Function, .funcref = func});
}

IrRef genLoad (IrBuild *build, IrRef ref, Size size) {
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

static void printBlock (Block *blk, IrList ir) {
	if (blk->visited)
		return;
	blk->visited = true;

	printf(" ");
	printString(blk->label);
	printf(":\n");
	for (size_t i = blk->first_inst; i <= blk->last_inst; i++) {
		printf(" %3lu = ", (unsigned long) i);
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		case Ir_Function:
			printf("func ");
			printString(inst.funcref->name);
			break;
		case Ir_Constant:
			printf("const 0x%lx (%lu)", (unsigned long) inst.constant, (unsigned long) inst.constant);
			break;
		case Ir_Call: {
			printf("call %lu (", (unsigned long) inst.call.function_ptr);
			ValuesSpan params = inst.call.parameters;
			for (u32 i = 0; i < params.len; i++) {
				printf("%lu", (unsigned long) params.ptr[i]);
				if (i + 1 < params.len)
					printf(", ");
			}
			printf(")");
		} break;
		case Ir_Phi:
			printf("phi ");
			for (u32 i = 0; i < inst.phi.len; i++) {
				printf("%lu", (unsigned long) inst.phi.ptr[i]); break;
				if (i + 1 < inst.phi.len)
					printf(", ");
			}
			break;
		case Ir_Parameter:
			printf("param %lu", (unsigned long) inst.unop);
			break;
		case Ir_StackAlloc:
			printf("stack %lu", (unsigned long) inst.unop);
			break;
		case Ir_Load:
			printf("load %lu", (unsigned long) inst.unop);
			break;
		case Ir_Store:
			printf("store [%lu] %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_Add:
			printf("add %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_Sub:
			printf("sub %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_Mul:
			printf("mul %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_Div:
			printf("div %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_BitAnd:
			printf("and %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_BitOr:
			printf("or %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_BitNot:
			printf("not %lu", (unsigned long) inst.unop);
			break;
		case Ir_BitXor:
			printf("xor %lu %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_LessThan:
			printf("cmp %lu < %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		case Ir_LessThanOrEquals:
			printf("cmp %lu <= %lu", (unsigned long) inst.binop.lhs, (unsigned long) inst.binop.rhs);
			break;
		}
		printf("\n");
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
		printf("       branch %lu ? ", (unsigned long) exit.branch.condition);
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
			printf("       ret %lu\n", (unsigned long) exit.ret);
		break;
	default: {}
	}
}

void printIr (Function *func) {
	printBlock(func->entry, func->ir);
}

