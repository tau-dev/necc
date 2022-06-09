#include "ir.h"
#include "parse.h"

IrRef genParameter(IrBuild *ir, Type t);
void genReturnVal(IrBuild *ir, IrRef val);
// IrRef genReturn(IrBuild *ir);
IrRef genAdd(IrBuild *ir, IrRef a, IrRef b);
IrRef genSub(IrBuild *ir, IrRef a, IrRef b);
IrRef genMul(IrBuild *ir, IrRef a, IrRef b);
IrRef genDiv(IrBuild *ir, IrRef a, IrRef b);
IrRef genImmediateInt(IrBuild *ir, long long i);
IrRef genImmediateReal(IrBuild *ir, double r);
IrRef genCall(IrBuild *ir, IrRef func, ValuesSpan args);
IrRef genFunctionRef(IrBuild *ir, Function *func);
IrRef genLoad(IrBuild *ir, IrRef ref);

void printIr(Function *func);

