//!necc-dbg -run @
#include <stdio.h>

struct c {
	int bar;
	long long baz;
	int foo;
};

int main() {
	if (__builtin_constant_p(printf("woo!")))
		return 1;

	return __builtin_constant_p(3 + 4) * __builtin_offsetof(struct c, foo);
}
