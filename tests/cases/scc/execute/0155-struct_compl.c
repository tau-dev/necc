//!necc-dbg @ -run
struct X x;
int foo();

int main()
{
	struct X x;
	return &x == 0;
}

struct X {int v;};

int foo()
{
	x.v = 0;
	return x.v;
}
