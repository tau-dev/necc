//!necc-dbg @ -run
#include "00-test.h"

#define M1(x,y) "This is a string $ or # or ## " #x y
#define STR "This is a string $ or # or ## and it is ok!\n"

int
main(void)
{
    char *s, *p, *t = M1(and, " it is ok!\n");
    p = t;
	for (s = STR; *s && *s == *p; ++s)
		++p;

    printf("%s", t);
        return *p;
}
