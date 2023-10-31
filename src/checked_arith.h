/*

Overflow checks for operations on constants.

A constant is always represented as u64 or i64. Signed constants that
represent smaller types than I64 should be fully sign-extended.

*/


// Unsigned->signed conversion: unlike signed->unsigned casts that are
// wrapping, unsigned->signed casts out of range would be undefined
// behavior.
// Correction: in C17, at least, the result is actually an
// implementation-defined value or a signal. Hm.
static inline i64 toSigned(u64 i) {
	union {
		u64 u;
		i64 s;
	} pun;
	pun.u = i;
	return pun.s;
}


bool addUnsignedOverflow(u64 a, u64 b, PrimitiveSize size);
bool addSignedOverflow(i64 a, i64 b, PrimitiveSize size);
// subUnsignedOverflow is trivial
bool subSignedOverflow(i64 a, i64 b, PrimitiveSize size);
bool mulUnsignedOverflow(u64 a, u64 b, PrimitiveSize size);
bool mulSignedOverflow(i64 a, i64 b, PrimitiveSize size);


static inline u64 signBit (PrimitiveSize size) {
	return ((u64) 1 << (size * 8 - 1));
}

static inline bool sign (u64 x, PrimitiveSize size) {
	assert(size);
	assert(size <= I64);
	return (x & signBit(size)) != 0;
}

static inline u64 bitsBelow (PrimitiveSize size) {
	return size >= 8 ? ~(u64) 0 : ((u64) 1 << (size*8)) - 1;
}

static inline u64 bitsAbove (PrimitiveSize size) {
	return size >= 8 ? 0 : ~(((u64) 1 << (size*8)) - 1);
}
