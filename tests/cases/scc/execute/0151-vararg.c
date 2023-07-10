//!necc-dbg @ -run
int printf(const char *__restrict, ...);

struct foo {
	int i, j, k;
	char *p;
	float v;
};

int
f1(struct foo f, struct foo *p, int n, ...)
{
	if (f.i != p->i)
		return 0;
	return p->j + n;
}

int
main(void)
{
	struct foo f;

	f.i = f.j = 1;
	printf("%d, %d\n", f1(f, &f, 2), f1(f, &f, 2, 1, f, &f));

	return 0;
}
