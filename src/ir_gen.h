#include "ir.h"
#include "parse.h"

IrRef genParameter(IrBuild *ir, Type t);
IrRef genStackAlloc(IrBuild *ir, Type t);
// IrRef genReturn(IrBuild *ir);
IrRef genAdd(IrBuild *ir, IrRef a, IrRef b);
IrRef genSub(IrBuild *ir, IrRef a, IrRef b);
IrRef genMul(IrBuild *ir, IrRef a, IrRef b);
IrRef genDiv(IrBuild *ir, IrRef a, IrRef b);
IrRef genOr(IrBuild *ir, IrRef a, IrRef b);
IrRef genXor(IrBuild *ir, IrRef a, IrRef b);
IrRef genAnd(IrBuild *ir, IrRef a, IrRef b);
IrRef genLessThan(IrBuild *ir, IrRef a, IrRef b);
IrRef genLessThanOrEquals(IrBuild *ir, IrRef a, IrRef b);
IrRef genImmediateInt(IrBuild *ir, long long i);
IrRef genImmediateReal(IrBuild *ir, double r);
IrRef genCall(IrBuild *ir, IrRef func, ValuesSpan args);
IrRef genFunctionRef(IrBuild *ir, Function *func);
IrRef genLoad(IrBuild *ir, IrRef ref);
IrRef genStore(IrBuild *ir, IrRef dest, IrRef value);

void genReturnVal(IrBuild *ir, IrRef val);
void genBranch(IrBuild *ir, IrRef condition);
void genJump(IrBuild *ir, Block *blk);
Block *genNewBlockLabeled(Arena *arena, IrBuild *ir, String label);
Block *genNewBlock(Arena *arena, IrBuild *ir);

void printIr(Function *func);

