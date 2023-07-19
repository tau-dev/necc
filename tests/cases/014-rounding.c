//!necc-dbg @ -run
#include <stdio.h>

int main() {
	printf("%d %d %d %d %d %d\n", (int) -1.1, (int) -1.0, (int) -0.9, (int) -0.5, (int) -0.1, (int) 0);
	printf("%d %d %d %d %d %d\n", (int) 1.1, (int) 1.0, (int) 0.9, (int) 0.5, (int) 0.1, (int) 0);
}
