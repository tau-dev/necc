#include "types.h"
#include "common.h"
#include "util.h"
#include "checked_arith.h"

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

	u64 sign_bits = bitsAbove(size);
	a |= sign_bits * sign(a, size);
	b |= sign_bits * sign(b, size);

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

	u64 sign_bits = bitsAbove(size);
	a |= sign_bits * sign(a, size);
	b |= sign_bits * sign(b, size);

	if (size > I32)
		return true; // FIXME lul

	i64 res = a * b;
	return res >= (i64) 1 << size*8 || res < -((i64) 1 << size*8);
}

