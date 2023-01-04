//!necc-dbg @ -run
struct S {int a; int b;};
struct S s = { .b = 2, .a = 1};

//!necc-dbg @ -run
int
main()
{
	if(s.a != 1)
		return 1;
	if(s.b != 2)
		return 2;
	return 0;
}
