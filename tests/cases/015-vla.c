//!necc-dbg @ -ir -run

#include <string.h>

int main() {
	int v = 2000;
	{
		int c[v];
		memset(c, 42, v * sizeof(c[0]));
		v = c[200];
	}

	return v;
}
