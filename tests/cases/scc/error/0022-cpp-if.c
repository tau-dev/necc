//!necc-dbg @ -run
#include <assert.h>

#if 3 != (1,2,3)
   #error 3 != (1,2,3)
#endif

static_assert(3 != (1,2,3));

int
main()
{
	return 0;
}
