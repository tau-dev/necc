#pragma once

#include "ir.h"
#include "parse.h"

IrRef genParameter(IrBuild *, u16 size);
IrRef genStackAlloc(IrBuild *, IrRef size);
IrRef genStackAllocFixed(IrBuild *, u32 size);
// IrRef genReturn(IrBuild *);
IrRef genAdd(IrBuild *, IrRef, IrRef);
IrRef genSub(IrBuild *, IrRef, IrRef);
IrRef genMul(IrBuild *, IrRef, IrRef);
IrRef genDiv(IrBuild *, IrRef, IrRef);
IrRef genOr(IrBuild *, IrRef, IrRef);
IrRef genXor(IrBuild *, IrRef, IrRef);
IrRef genAnd(IrBuild *, IrRef, IrRef);
IrRef genBinNot(IrBuild *, IrRef);
IrRef genNot(IrBuild *, IrRef);
IrRef genEquals(IrBuild *, IrRef, IrRef, u16 size);
IrRef genLessThan(IrBuild *, IrRef, IrRef, u16 size);
IrRef genLessThanOrEquals(IrBuild *, IrRef, IrRef, u16 size);
IrRef genImmediateInt(IrBuild *, long long i, u16 size);
IrRef genImmediateReal(IrBuild *, double r);
IrRef genTrunc(IrBuild *, IrRef source, u16 target);
IrRef genSignExt(IrBuild *, IrRef source, u16 target);
IrRef genZeroExt(IrBuild *, IrRef source, u16 target);
IrRef genCall(IrBuild *, IrRef func, ValuesSpan args, u16 size);
IrRef genGlobal(IrBuild *, u32 id);
IrRef genLoad(IrBuild *, IrRef ref, u16 size);
IrRef genStore(IrBuild *, IrRef dest, IrRef value);
IrRef genPhiIn(IrBuild *build, u16 size);
IrRef genPhiOut(IrBuild *build, IrRef source);
void setPhiOut(IrBuild *build, IrRef phi, IrRef dest_true, IrRef dest_false);
void genReturnVal(IrBuild *, IrRef val);
void genBranch(IrBuild *, IrRef condition);
void genJump(IrBuild *, Block *blk);
Block *newBlock(Arena *arena, String label);
void startBlock(IrBuild *, Block *blk);
void discardBlock(Block *blk);
void discardIrBuilder(IrBuild *);
Block *startNewBlock(IrBuild *build, Arena *arena, String label);

void printBlock(FILE *dest, Block *entry, IrList ir);

