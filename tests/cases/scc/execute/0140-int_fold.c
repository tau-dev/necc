//!necc-dbg @ -ir -O
int
main(void)
{
	int i;

	i = 1 + 2;
	i = 2 - 1;
	i = 3 * 6;
	i = 10 / 5;
	i = 10 % 5;
	i = i % 0;
	i = i % 0;
	i = 1 << 3;
	i = 8 >> 2;
	i = 12 & 4;
	i = 8 | 4;
	i = 12 ^ 4;
	i = -(3);
	i = ~12;
	i = 1 < 3;
	i = 2 > 3;
	i = 2 >= 3;
	i = 2 <= 3;
	i = 1 == 0;

	return 0;
}
