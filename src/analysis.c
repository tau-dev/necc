#include "analysis.h"
void calcLifetimes(IrList ir, ValuesSpan lastuses) {
	assert(ir.len == lastuses.len);
	IrRef *uses = lastuses.ptr;
	memset(uses, 0, sizeof(IrRef) * lastuses.len);
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		case Ir_Add:
		case Ir_Sub:
		case Ir_Mul:
		case Ir_Div:
		case Ir_BitOr:
		case Ir_BitXor:
		case Ir_BitAnd:
		case Ir_LessThan:
		case Ir_LessThanOrEquals:
		case Ir_Store:
			uses[inst.binop.lhs] = i;
			uses[inst.binop.rhs] = i;
			break;
		case Ir_Load:
		case Ir_StackAlloc:
		case Ir_BitNot:
			uses[inst.unop] = i;
			break;
		case Ir_Phi:
			for (u32 p = 0; p < inst.phi.len; p++) {
				uses[inst.phi.ptr[p]] = i;
			}
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
