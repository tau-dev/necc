//!necc-dbg @ -run

enum { WOO = 3 };
int printf(const char *format, ...);

int main() {
	printf("%d\n", WOO);
	{
		enum { WOO = 4 };
		printf("%d\n", WOO);
	}
	printf("%d\n", WOO);
	return 0;
}
