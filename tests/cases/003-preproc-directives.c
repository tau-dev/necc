//!necc-dbg @ -run

#define A
#define B

#ifdef A
#ifdef B
#endif
#ifdef C
#ifdef D
#endif

#ifdef E
#endif
#endif
#endif

int main() {
	return x;
}
