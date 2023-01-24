//!necc-dbg @ -run
#include "00-test.h"
#define M(x) x
#define A(a,b) a(b)

int
main(void)
{
	char *a = A(M, "hi");
	printf("%s\n", a);

	return (a[1] == 'i') ? 0 : 1;
}
