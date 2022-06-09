#include "ir_gen.h"
#define HAS_EXITED(ir) ((ir)->insertion_block->exit.kind != Exit_None)

IrRef append (IrBuild *ir, Inst inst) {
	PUSH (ir->ir, inst);
	return ir->ir.len - 1;
}

IrRef genParameter (IrBuild *ir, Type t) {
	(void) t;
	return append(ir, (Inst) {Ir_Parameter});
}

void genReturnVal (IrBuild *ir, IrRef val) {
	ir->insertion_block->exit.kind = Exit_Return;
	ir->insertion_block->exit.ret = val;
}


IrRef genAdd (IrBuild *ir, IrRef a, IrRef b) {
	if (HAS_EXITED(ir))
		return IR_REF_NONE;
	Inst i = {Ir_Add, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant + inst[b].constant;
	}
	return append(ir, i);
}

IrRef genSub (IrBuild *ir, IrRef a, IrRef b) {
	if (HAS_EXITED(ir))
		return IR_REF_NONE;
	Inst i = {Ir_Sub, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant - inst[b].constant;
	}
	return append(ir, i);
}


IrRef genMul (IrBuild *ir, IrRef a, IrRef b) {
	if (HAS_EXITED(ir))
		return IR_REF_NONE;
	Inst i = {Ir_Mul, .bin = {a, b}};
	Inst *inst = ir->ir.ptr;

	if (inst[a].kind == Ir_Constant && inst[b].kind == Ir_Constant) {
		i.kind = Ir_Constant;
		i.constant = inst[a].constant * inst[b].constant;
	}
	return append(ir, i);
}

IrRef genDiv (IrBuild *ir, IrRef a, IrRef b) {
	if (HAS_EXITED(ir))
		return IR_REF_NONE;
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

IrRef genImmediateInt (IrBuild *ir, long long i) {
	return append(ir, (Inst) {Ir_Constant, {i}});
}

IrRef genImmediateReal (IrBuild *ir, double r) {
	(void) r;
	(void) ir;
	return 0; // TODO
}


IrRef genCall (IrBuild *ir, IrRef func, ValuesSpan args) {
	if (HAS_EXITED(ir))
		return IR_REF_NONE;
	return append(ir, (Inst) {Ir_Call, .call = {func, args}});
}

IrRef genFunctionRef (IrBuild *ir, Function *func) {
	return append(ir, (Inst) {Ir_Function, .funcref = func});
}

IrRef genLoad (IrBuild *ir, IrRef ref) {
	return append(ir, (Inst) {Ir_Load, .unop = ref});
}


static u32 getBlockId (Block *blk, u32 *blockid) {
	if (blk->exit.kind != Exit_None) {
		blk->id = *blockid;
		(*blockid)++;
	}
	return blk->id;
}

static void printBlock (Block *blk, IrList ir, u32 *blockid) {
	size_t inst_end;
	Exit exit = blk->exit;
	switch (exit.kind) {
	case Exit_Return:
		inst_end = exit.ret + 1;
		break;
	case Exit_Unconditional:
		inst_end = exit.unconditional->first_inst;
		break;
	case Exit_Branch:
		inst_end = exit.branch.test + 1;
		break;
	default:
		return;
	}
	printf(" b%lu:\n", (unsigned long) blk->id);

	for (size_t i = blk->first_inst; i < inst_end; i++) {
		printf(" %3lu = ", (unsigned long) i);
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		case Ir_Function:
			printf("function ");
			fwrite(inst.funcref->name.ptr, 1, inst.funcref->name.len, stdout);
			break;
		case Ir_Constant:
			printf("const 0x%lx (%lu)", (unsigned long) inst.constant, (unsigned long) inst.constant);
			break;
		case Ir_Call: {
			printf("call %lu(", (unsigned long) inst.call.function_ptr);
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
			printf("param");
			break;
		case Ir_Load:
			printf("load %lu", (unsigned long) inst.unop);
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
		}
		printf("\n");
	}
	ExitKind tmp = blk->exit.kind;
	blk->exit.kind = Exit_None;
	switch (exit.kind) {
	case Exit_Unconditional:
		printf("       jmp b%lu\n", (unsigned long) getBlockId(exit.unconditional, blockid));
		printBlock(exit.unconditional, ir, blockid);
		break;
	case Exit_Branch:

		printf("       branch %lu ? b%lu : b%lu\n", (unsigned long) exit.branch.test,
			(unsigned long) getBlockId(exit.branch.on_true, blockid), (unsigned long) getBlockId(exit.branch.on_false, blockid));
		printBlock(exit.branch.on_true, ir, blockid);
		printBlock(exit.branch.on_false, ir, blockid);
		break;
	case Exit_Return:
		printf("       ret %lu\n", (unsigned long) exit.ret);
		break;
	default: {}
	}
	blk->exit.kind = tmp;
}

void printIr (Function *func) {
	u32 blockid = 0;
	printBlock(func->entry, func->ir, &blockid);
}

