//!necc-dbg -run @
#include <stdio.h>

int count = 0;

int foo(int a, int b) {
	if (a < 2)
		return b;
	count++;
	int uneven = a & 1;
	int *c = &uneven;
	if (*c)
		return foo(a * 3 + 1, b + 1);
	else
		return foo(a / 2, b + 1);

}

int fib(int x) {
	if (x < 2)
		return 1;
	return fib(x-1) + fib(x-2);
}

int main() {
	printf("%d\n", fib(20));
	int w = foo(17, 0);
	printf("%d %d\n", w, count);
	return 0;
}
