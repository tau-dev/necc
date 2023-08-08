//!necc-dbg @ -run
#include <stdarg.h>

int
fun(int first, ...)
{
	int c;

	va_list va;

	va_start(va, first);
	c = va_arg(va, double);
	va_end(va);
	if (first)
		fun(0, 33.0);

	return c;
}

int
main()
{
	fun(1, 33.0, 1, 1, 1, 1, 1, 1, 1);
	return fun(1, 33.0, 1, 1, 1, 1, 1, 1);
}
