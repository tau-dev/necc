#include "ir.h"

// Reduce the ir to only those instructions used by the blocks, in
// def-before-use order.
void decimateIr(IrList *ir, Blocks blocks);

// Find the last usage of all instructions.
void calcLifetimes(IrList ir, IrRefList lastuses);

// Calculate the usage count of all instructions into uses.
void calcUsage(IrList ir, u16 *uses);

// Make all references to copy instructions refer to the original.
void resolveCopies(IrList ir, Blocks blocks);

// Linearize the graph of blocks reachable from blk into dest.
void scheduleBlocksStraight(Arena *blocks_arena, Block *blk, Blocks *dest);

// Perform arithmetic simplifications.
void arithSimplify(IrList ir, u16 *uses);

// Elide store-copy sequences.
void storeLoadPropagate(IrList ir, Blocks blocks);

// Elide stores and copies within a block
void innerBlockPropagate(IrList ir, Blocks blocks);
// Perform more involved memory propagation; should do mostly complete SSAification of variables.
// TODO
