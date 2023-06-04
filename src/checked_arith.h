/*

Overflow checks for operations on constants.

A constant is always represented as u64 or i64. Signed constants that
represent smaller types than I64 should be fully sign-extended.

*/

bool addUnsignedOverflow(u64 a, u64 b, PrimitiveSize size);
bool addSignedOverflow(i64 a, i64 b, PrimitiveSize size);
// subUnsignedOverflow is trivial
bool subSignedOverflow(i64 a, i64 b, PrimitiveSize size);
bool mulUnsignedOverflow(u64 a, u64 b, PrimitiveSize size);
bool mulSignedOverflow(i64 a, i64 b, PrimitiveSize size);


static inline bool sign(u64 x, PrimitiveSize size) {
	assert(size);
	assert(size <= I64);
	return (x & ((u64) 1 << (size * 8 - 1))) != 0;
}

static inline u64 bitsAbove(u16 size) {
	return size >= 8 ? 0 : ~(((u64) 1 << (size*8)) - 1);
}
