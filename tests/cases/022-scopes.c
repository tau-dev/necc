//!necc-dbg -ir @

// Based on "A Simple, Possibly Correct LR Parser for C11" by Jacques-Henri Jourdan and François Pottier.
typedef int T, U;

int x;
void f(void) {
	if (sizeof(enum {T}))
		x = sizeof(enum {U}) + T;
	else {
		U u = (int) T;
	}
	switch (sizeof(enum {U})) x = U;
	T t; U u;
}
