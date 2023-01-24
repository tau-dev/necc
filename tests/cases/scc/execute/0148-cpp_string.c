//!necc-dbg @ -run
#include "00-test.h"

#define x(y) #y


int
main(void)
{
	char *p;
	p = x(hello)  " is better than bye";
	printf("%s\n", p);

	return (*p == 'h') ? 0 : 1;
}
