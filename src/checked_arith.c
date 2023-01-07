#include "types.h"
#include "common.h"
#include "util.h"

static inline bool sign(u64 x, PrimitiveSize size) {
	return (x & ((u64) 1 << (size * 8 - 1))) != 0;
}

bool addUnsignedOverflow(u64 a, u64 b, PrimitiveSize size) {
	assert(size <= I64);
	if (size == I64)
		return a + b < a;
	else
		return a + b >= (u64) 1 << (size + 8);
}

bool addSignedOverflow(i64 a, i64 b, PrimitiveSize size) {
	assert(size <= I64);
	if (sign(a, size) != sign(b, size)) return false;

	return sign(a, size) != sign((u64) a + (u64) b, size);
}

bool subSignedOverflow(i64 a, i64 b, PrimitiveSize size) {
	assert(size <= I64);

	u64 sign_bits = size >= 8 ? 0 : ~(((u64) 1 << size*8) - 1);
	// Unused bits should have the correct sign.
	assert(((u64) a & sign_bits) == sign_bits * sign(a, size));
	assert(((u64) b & sign_bits) == sign_bits * sign(b, size));

	if (sign(a, size) == sign(b, size)) return false;

	return sign(a, size) != sign((u64) a - (u64) b, size);
}

bool mulUnsignedOverflow(u64 a, u64 b, PrimitiveSize size) {
	assert(size <= I64);
	if (a <= 1 || b <= 1) return false;

	if (size > I32)
		unreachable;
	else {
		u64 res = a * b;
		return res >= (u64) 1 << size * 8;
	}
}

bool mulSignedOverflow(i64 a, i64 b, PrimitiveSize size) {
	assert(size <= I64);
	if (a == 0 || a == 1 || b == 0 || b == 1) return false;

	if (size > I32)
		return false; // FIXME lul

// 	if (a == -1) return b == -((u32) 1 << );
	i64 res = a * b;
	return res >= (i64) 1 << size*8 || res < -((i64) 1 << size*8);
}
