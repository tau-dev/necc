#include "ir.h"

// Reduce the ir to only those instructions used by the blocks, in
// def-before-use order.
void decimateIr(IrList *ir, Blocks blocks);
// Find the last usage of all instructions.
void calcLifetimes(IrList ir, ValuesSpan lastuses);
// Calculate the usage cout of all instructions.
void calcUsage(IrList ir, u16 *uses);
// Make all references to copy instructions refer to the original.
void resolveCopies(IrList ir);
// Linearize the graph of blocks reachable from blk into dest.
void scheduleBlocksStraight(Block *blk, Blocks *dest);
// Elide store-copy sequences.
void innerBlockPropagate(IrList ir, Blocks blocks);
