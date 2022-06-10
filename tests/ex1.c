
int foo(int a) {
	int uneven = a & 1;
	int *c = &uneven;
	if (a < 2)
		return 1;
	goto mylabel;
mylabel:
	if (*c)
		return foo(a * 3 + 1);
	else
		return foo(a / 2);

}
