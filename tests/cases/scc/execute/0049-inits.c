//!necc-dbg @ -run
struct S {int a; int b;};
struct Z {int a; struct S b; int c;};
struct Z s = { .c = 2, .b.a = 5, .b.b = 6, .a = 1};

int printf( const char *format, ... );

int
main()
{
	printf("%d\n", s.a);
	printf("%d\n", s.c);
	printf("%d\n", s.b.a);
	printf("%d\n", s.b.b);
	return 0;
}
