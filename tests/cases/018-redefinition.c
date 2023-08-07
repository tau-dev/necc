//!necc-dbg -ir @
struct hit { int foo; };

typedef struct hit hit;

int main() {
	struct hit hit;
	hit.foo = 32;
}
