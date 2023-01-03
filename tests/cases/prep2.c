//!necc-dbg -run @
#define X(a,b) int
#define A 1,2
#define B(a) X(a)

int main(int argc, char **args) {
	B(A) c = 0;
	return c;
}
