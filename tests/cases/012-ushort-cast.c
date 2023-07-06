//!necc-dbg @ -run
int printf(const char *__restrict, ...);

int main () {
	printf("%d\n", ((unsigned int)0) > -9);
}

