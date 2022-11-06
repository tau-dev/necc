#include "ir.h"
#include "parse.h"

IrRef genParameter(IrBuild *ir, Size size);
IrRef genStackAlloc(IrBuild *ir, IrRef size);
IrRef genStackAllocFixed(IrBuild *ir, Size size);
// IrRef genReturn(IrBuild *ir);
IrRef genAdd(IrBuild *ir, IrRef a, IrRef b);
IrRef genSub(IrBuild *ir, IrRef a, IrRef b);
IrRef genMul(IrBuild *ir, IrRef a, IrRef b);
IrRef genDiv(IrBuild *ir, IrRef a, IrRef b);
IrRef genOr(IrBuild *ir, IrRef a, IrRef b);
IrRef genXor(IrBuild *ir, IrRef a, IrRef b);
IrRef genAnd(IrBuild *ir, IrRef a, IrRef b);
IrRef genBinNot(IrBuild *ir, IrRef a);
IrRef genNot(IrBuild *ir, IrRef a);
IrRef genEquals(IrBuild *ir, IrRef a, IrRef b, Size size);
IrRef genLessThan(IrBuild *ir, IrRef a, IrRef b, Size size);
IrRef genLessThanOrEquals(IrBuild *ir, IrRef a, IrRef b, Size size);
IrRef genImmediateInt(IrBuild *ir, long long i, Size size);
IrRef genImmediateReal(IrBuild *ir, double r);
IrRef genTrunc(IrBuild *ir, IrRef source, Size target);
IrRef genSignExt(IrBuild *ir, IrRef source, Size target);
IrRef genZeroExt(IrBuild *ir, IrRef source, Size target);
IrRef genCall(IrBuild *ir, IrRef func, ValuesSpan args);
IrRef genFunctionRef(IrBuild *ir, Function *func);
IrRef genLoad(IrBuild *ir, IrRef ref, u32 size);
IrRef genStore(IrBuild *ir, IrRef dest, IrRef value);

void genReturnVal(IrBuild *ir, IrRef val);
void genBranch(IrBuild *ir, IrRef condition);
void genJump(IrBuild *ir, Block *blk);
Block *genNewBlockLabeled(Arena *arena, IrBuild *ir, String label);
Block *genNewBlock(Arena *arena, IrBuild *ir);

void printIr(Function *func);

