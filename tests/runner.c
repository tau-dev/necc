#include "util.c"

typedef enum {
	Pass,
	Skip,
	Fail,
	Create,
} Result;

Result performTest(const char *name);
Result testCompare(const char *name, const char *reference);

#if HAVE_POSIX
int posixVisit(
	const char *fpath,
	const struct stat *sb,
	int typeflag,
	struct FTW *ftwbuf)
{
	(void) sb;
	(void) ftwbuf;
// 	printf("->%s\n", fpath);
	if (typeflag == FTW_F && endsWith(fpath, ".c")) {
// 		printf("%s\n", fpath);
		performTest(fpath);
	}
	return 0;
}
#endif


int fails = 0;
int skips = 0;
int created = 0;
int passes = 0;
bool quiet = false;

const char *plural(int x) { return x == 1 ? "" : "s"; }

char output_path[L_tmpnam];
int main(int argc, char **argv) {
	(void) argc;
	tmpnam(output_path);

	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-q")) {
			quiet = true;
		} else {
			fprintf(stderr, "ignoring unknown argument %s", argv[i]);
		}
	}

// 	char *real = realpath(argv[0], NULL);
	char *base = dupe(dirname(argv[0]));
	chdir(dirname(base));
	nftw("./tests/cases", posixVisit, 64, 0);



	if (skips)
		fprintf(stderr, "%d file%s skipped.\n", skips, plural(skips));
	if (created)
		fprintf(stderr, "%d test%s created.\n", created, plural(created));
	if (fails) {
		fprintf(stderr, "%d test%s failed.\n", fails, plural(fails));
		return 1;
	} else {
		if (passes)
			fprintf(stderr, "All %d test%s passed.\n", passes, plural(passes));
		return 0;
	}
}

Result performTest(const char *name) {
	const char *reference_path = concat(name, ".res");

	FILE *out = stderr;
	if (!quiet)
		fprintf(out, "%s: ", name);

	Result res = testCompare(name, reference_path);

	switch (res) {
	case Fail:
		if (quiet)
			fprintf(out, "%s: ", name);
		fprintf(out, "FAILED.\n	Compare %s to %s\n", reference_path, output_path);
		tmpnam(output_path);
		fails++;
		break;
	case Create:
		if (quiet)
			fprintf(out, "%s: ", name);
		fprintf(out, "missing reference output, created %s.res.\n", name);
		created++;
		break;
	case Skip:
		if (!quiet)
			fprintf(out, "%s", isatty(1) ? "\033[1G\033[0K" : "skipping.\n");
		skips++;
		break;
	case Pass:
		if (!quiet)
			fprintf(out, "%s", isatty(1) ? "\033[1G\033[0K" : "passed.\n");
		passes++;
		break;
	}
// 	fflush(out);
	return res;
}

#define BUF 4096
Result testCompare(const char *name, const char *reference_path) {
	Result res = Pass;
	FILE *f = fopen(name, "r");

	char line[BUF] = {0};
	size_t len = readAll(f, line, BUF);
	if (!startsWith(line, "//!"))
		return Skip;

	for (size_t i = 0; i < len; i++) {
		if (line[i] == '\n') {
			line[i] = 0;
			break;
		}
	}
	fclose(f);


	// Some ad-hockery to put the file name into the command.
	char *self_pos = strchr(line, '@');
	if (self_pos) {
		*self_pos = 0;
		self_pos++;
	}
	char cmd_buf[BUF] = {0};
	char *cmd = cmd_buf;
	append(&cmd, "./bin/");
	append(&cmd, line+3);
	if (self_pos) {
		append(&cmd, name);
		append(&cmd, self_pos);
	}
	append(&cmd, " 2>&1 ; echo \"exit code $?\"");
	cmd = cmd_buf;

	char buf1[BUF] = {0};
	char buf2[BUF] = {0};

	FILE *reference = fopen(reference_path, "r");
	FILE *output = popen(cmd, "r");

	if (!reference) {
		res = Create;
		reference = fopen(reference_path, "w");
		size_t len;
		while ((len = readAll(output, buf1, BUF)))
			writeAll(reference, buf1, len);
	} else {
		FILE *output_dest = fopen(output_path, "w");

		size_t len1, len2;
		while ((len1 = readAll(output, buf1, BUF))
			&& (len2 = readAll(reference, buf2, len1)))
		{
			writeAll(output_dest, buf1, len1);
			if (len1 != len2)
				break;
			if (memcmp(buf1, buf2, len1))
				return Fail;
		}

		if (len1 > len2)
			return Fail;
		if (readAll(reference, buf2, BUF))
			return Fail;

		fclose(output_dest);
	}
	fclose(reference);
	pclose(output);

	return res;
}

