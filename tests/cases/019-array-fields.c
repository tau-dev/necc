//!necc-dbg -ir @

typedef struct {
	int foo;
} Bar;
struct dohaddr {
  int type;
  Bar v4[4]; /* network byte order */
};

int main() {
	struct dohaddr *a;
	(&a->v4)[0][1].foo = a->v4->foo;
}

