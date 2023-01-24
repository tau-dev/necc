#pragma once

#include "ir.h"
#include "parse.h"

IrRef genParameter(IrBuild *, u32 param_id);
IrRef genStackAllocVLA(IrBuild *, IrRef size);
IrRef genStackAllocNamed(IrBuild *, IrRef size, Declaration);
IrRef genStackAllocFixed(IrBuild *, u32 size);
// IrRef genReturn(IrBuild *);
IrRef genAdd(IrBuild *, IrRef, IrRef);
IrRef genAddSigned(IrBuild *, IrRef, IrRef, bool *overflow);
IrRef genFAdd(IrBuild *, IrRef, IrRef);
IrRef genSub(IrBuild *, IrRef, IrRef);
IrRef genSubSigned(IrBuild *, IrRef, IrRef, bool *overflow);
IrRef genFSub(IrBuild *, IrRef, IrRef);
IrRef genMulUnsigned(IrBuild *, IrRef, IrRef);
IrRef genMulSigned(IrBuild *, IrRef, IrRef, bool *overflow);
IrRef genFMul(IrBuild *, IrRef, IrRef);
IrRef genDivUnsigned(IrBuild *, IrRef, IrRef, bool *overflow_or_div0);
IrRef genDivSigned(IrBuild *, IrRef, IrRef, bool *overflow_or_div0);
IrRef genFDiv(IrBuild *, IrRef, IrRef);
IrRef genModUnsigned(IrBuild *, IrRef, IrRef, bool *overflow_or_div0);
IrRef genModSigned(IrBuild *, IrRef, IrRef, bool *overflow_or_div0);
IrRef genFMod(IrBuild *, IrRef, IrRef);
IrRef genOr(IrBuild *, IrRef, IrRef);
IrRef genXor(IrBuild *, IrRef, IrRef);
IrRef genAnd(IrBuild *, IrRef, IrRef);
IrRef genBitNot(IrBuild *, IrRef);
IrRef genNot(IrBuild *, IrRef);
IrRef genLessThan(IrBuild *, IrRef, IrRef, u16 size, bool is_unsigned);
IrRef genLessThanOrEquals(IrBuild *, IrRef, IrRef, u16 size, bool is_unsigned);
IrRef genEquals(IrBuild *, IrRef, IrRef, u16 size);
IrRef genShiftLeft(IrBuild *, IrRef, IrRef);
IrRef genShiftRight(IrBuild *, IrRef, IrRef);
IrRef genImmediateInt(IrBuild *, long long i, u16 size);
IrRef genImmediateReal(IrBuild *, double r, u16 size);
IrRef genTrunc(IrBuild *, IrRef source, u16 target);
IrRef genSignExt(IrBuild *, IrRef source, u16 target);
IrRef genZeroExt(IrBuild *, IrRef source, u16 target);
IrRef genFCast(IrBuild *, IrRef source, u16 target);
IrRef genFloatToInt(IrBuild *, IrRef source, u16 target, bool is_unsigned);
IrRef genIntToFloat(IrBuild *, IrRef source, u16 target, bool is_unsigned);
IrRef genCall(IrBuild *, IrRef func, ValuesSpan args, u16 size, bool is_vararg);
IrRef genGlobal(IrBuild *, u32 id);
IrRef genLoad(IrBuild *, IrRef ref, u16 size, bool is_volatile);
IrRef genStore(IrBuild *, IrRef dest, IrRef value, bool is_volatile);
IrRef genAccess(IrBuild *, IrRef value, IrRef offset, IrRef size);
IrRef genPhiIn(IrBuild *build, u16 size);
IrRef genPhiOut(IrBuild *build, IrRef source);
void setPhiOut(IrBuild *build, IrRef phi, IrRef dest_true, IrRef dest_false);

IrRef genVaStart(IrBuild *build, IrRef va_list_addr, IrRef param);
IrRef genVaArg(IrBuild *build, IrRef va_list_addr, IrRef size);

void replaceWithCopy(IrList ir, IrRef original, IrRef replacement, IrRef ordered_after);
void genSetZero(IrBuild *, IrRef address, u32 size, bool is_volatile);

void genReturnVal(IrBuild *, IrRef val);
void genBranch(IrBuild *, IrRef condition);
void genJump(IrBuild *, Block *blk);
void genSwitch(IrBuild *, IrRef val);
Block *newBlock(IrBuild *build, String label);
void startBlock(IrBuild *, Block *blk);
Block *startNewBlock(IrBuild *build, String label);

void discardBlock(Block *blk);
void discardIrBuilder(IrBuild *);

void printBlock(FILE *dest, Block *entry, IrList ir);

