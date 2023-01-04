//!necc-dbg @ -run
#include <limits.h>

static long long var = LLONG_MAX + 1;

int
main()
{
	return var;
}
