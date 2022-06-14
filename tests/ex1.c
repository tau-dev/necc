
int foo(int a, int b) {
	if (a < 2)
		return 1;
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
