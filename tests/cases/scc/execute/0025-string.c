//!necc-dbg @ -run
int strlen(char *);

int
main()
{
	char *p;

	p = "hello";
	return strlen(p) - 5;
}
