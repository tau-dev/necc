//!necc-dbg @ -run
char s0[] = "foo";
char s1[7] = "bar";
char s2[3] = "baz";
char s3[] = {"bam"};
char *p = "foo";

int
cmp(char *s1, char *s2)
{
	while (*s1 && *s1++ == *s2++)
		;
	return *s1;
}

int
main()
{
	if (sizeof(s0) != 4) return 1;
	if (cmp(s0, "foo"))  return 2;
	if (cmp(s1, "bar"))  return 3;
	if (s2[0] != 'b')    return 4;
	if (s2[1] != 'a')    return 5;
	if (sizeof(s3) != 4) return 6;
	if (cmp(s3, "bam"))  return 7;
	return 0;
}
