//!necc-dbg @ -run
#if 0
int x = 2;
#elif 0
int x = 1;
#elif 1
int x = 0;
#endif

int
main()
{
	return x;
}
