#include "ir_gen.h"
#define HAS_EXITED(ir) ((ir)->insertion_block->exit.kind != Exit_None)

IrRef append (IrBuild *ir, Inst inst) {
	PUSH (ir->ir, inst);
	return ir->ir.len - 1;
}

IrRef genParameter (IrBuild *ir, Type t) {
	return append(ir, (Inst) {Ir_Parameter, .unop = typeSize(t)});
}

IrRef genStackAlloc (IrBuild *ir, Type t) {
	return append(ir, (Inst) {Ir_StackAlloc, .unop = typeSize(t)});
}

void genReturnVal (IrBuild *ir, IrRef val) {
	ir->insertion_block->last_inst = ir->ir.len - 1;
	ir->insertion_block->exit = (Exit) {Exit_Return, .ret = val};
}

void genBranch (IrBuild *ir, IrRef condition) {
	ir->insertion_block->last_inst = ir->ir.len - 1;
	ir->insertion_block->exit = (Exit) {
		Exit_Branch,
		.branch = {condition}
	};
}

void genJump (IrBuild *ir, Block *dest) {
	ir->insertion_block->last_inst = ir->ir.len - 1;
	ir->insertion_block->exit = (Exit) {
		Exit_Unconditional,
		.unconditional = dest
	};
}


Block *genNewBlock (Arena *arena, IrBuild *ir) {
	SPAN(char) name = ALLOCN(arena, char, 24);
	snprintf(name.ptr, name.len, "[block %d]", ir->block_count);
	ir->block_count++;
	return genNewBlockLabeled(arena, ir, (String) {strlen(name.ptr), name.ptr});
}

Block *genNewBlockLabeled (Arena *arena, IrBuild *ir, String label) {
	Block *new_block = ALLOC(arena, Block);
	*new_block = (Block) {
		.label = label,
		.first_inst = ir->ir.len
	};
	ir->insertion_block = new_block;
	return new_block;
}



IrRef genAdd (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_Add, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant + inst[b].constant;
	}
	return append(ir, i);
}

IrRef genSub (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_Sub, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant - inst[b].constant;
	}
	return append(ir, i);
}


IrRef genMul (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_Mul, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant * inst[b].constant;
	}
	return append(ir, i);
}

IrRef genDiv (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_Div, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
// 		if (inst[b].constant == 0)
// 			comperror();
		i.constant = inst[a].constant / inst[b].constant;
	}
	return append(ir, i);
}

IrRef genOr (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_BitOr, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
// 		if (inst[b].constant == 0)
// 			comperror();
		i.constant = inst[a].constant | inst[b].constant;
	}
	return append(ir, i);
}

IrRef genXor (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_BitXor, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
// 		if (inst[b].constant == 0)
// 			comperror();
		i.constant = inst[a].constant ^ inst[b].constant;
	}
	return append(ir, i);
}

IrRef genAnd (IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_BitAnd, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
// 		if (inst[b].constant == 0)
// 			comperror();
		i.constant = inst[a].constant & inst[b].constant;
	}
	return append(ir, i);
}

IrRef genLessThan(IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_LessThan, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant < inst[b].constant;
	}
	return append(ir, i);
}

IrRef genLessThanOrEquals(IrBuild *ir, IrRef a, IrRef b) {
	Inst i = {Ir_LessThanOrEquals, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant <= inst[b].constant;
	}
	return append(ir, i);
}


IrRef genImmediateInt (IrBuild *ir, long long i) {
	return append(ir, (Inst) {Ir_Constant, {i}});
}

IrRef genImmediateReal (IrBuild *ir, double r) {
	(void) r;
	(void) ir;
	return 0; // TODO
}


IrRef genCall (IrBuild *ir, IrRef func, ValuesSpan args) {
	IrRef call = append(ir, (Inst) {Ir_Call, .call = {func, args}});
	PUSH (ir->insertion_block->side_effecting_instructions, call);
	return call;
}

IrRef genFunctionRef (IrBuild *ir, Function *func) {
	return append(ir, (Inst) {Ir_Function, .funcref = func});
}

IrRef genLoad (IrBuild *ir, IrRef ref) {
	IrRef load = append(ir, (Inst) {Ir_Load, .unop = ref});
	PUSH (ir->insertion_block->mem_instructions, load);
	return load;
}

IrRef genStore (IrBuild *ir, IrRef dest, IrRef value) {
	IrRef store = append(ir, (Inst) {Ir_Store, .bin = {dest, value}});
	PUSH (ir->insertion_block->mem_instructions, store);
	PUSH (ir->insertion_block->side_effecting_instructions, store);
	return store;
}

void printString(String s) {
	fwrite(s.ptr, 1, s.len, stdout);
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
			fwrite(inst.funcref->name.ptr, 1, inst.funcref->name.len, stdout);
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
			printf("alloc %lu", (unsigned long) inst.unop);
			break;
		case Ir_Load:
			printf("load %lu", (unsigned long) inst.unop);
			break;
		case Ir_Store:
			printf("store %lu <- %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_Add:
			printf("add %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_Sub:
			printf("sub %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_Mul:
			printf("mul %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_Div:
			printf("div %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_BitAnd:
			printf("and %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_BitOr:
			printf("or %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_BitNot:
			printf("not %lu", (unsigned long) inst.unop);
			break;
		case Ir_BitXor:
			printf("xor %lu %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_LessThan:
			printf("cmp %lu < %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
			break;
		case Ir_LessThanOrEquals:
			printf("cmp %lu <= %lu", (unsigned long) inst.bin.lhs, (unsigned long) inst.bin.rhs);
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

