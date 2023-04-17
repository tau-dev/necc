//!necc-dbg @ -run
#undef  line
#define line 1000

#line line
#if 1000 != __LINE__
	#error "  # line line" not working as expected
#endif

int
main()
{
	return 0;
}
