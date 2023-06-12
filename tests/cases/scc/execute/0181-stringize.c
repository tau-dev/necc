//!necc-dbg @ -run
#define XSTR(x) #x
#define STR(x)  XSTR(x)
#define S       y = "str"
int printf(const char *__restrict, ...);

int
main()
{
	char *s = "y = \"str\"", *t = STR(S);
	printf("'%s'\n", t);
	printf("'%s'\n", s);

	for (; *s && *t; ++s, ++t) {
		if (*s != *t)
			return 1;
	}

	return 0;
}
