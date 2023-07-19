//!necc-dbg @ -run

void a(void) {}
void b(void) {a();}

typedef unsigned char u8;
typedef struct {
	// If 0, this is passed in memory.
	u8 count;
	// Number of eightbytes that are passed in the xmm registers; the remaining count-sse_count are integers.
	u8 sse_count;
	u8 registers[8];
} ParameterClass;


ParameterClass foo(int w) {}
int c(int w) {return w + 1;}
int d(int w) {return w + c(w);}



int more(int a, int b, int c, int d, int e, int f, int g) {
	return a + b + c + d + e + f + g;
}

main() {
	b();
	return d(4) + more(1, 2, 3, 4, 5, 6, 7);
}
