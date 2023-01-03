#if defined(__unix__)
#define HAVE_POSIX 1
#define _XOPEN_SOURCE 500
#include <unistd.h>
#include <ftw.h>
#include <libgen.h>
#elif defined(_WIN32)
#error "TODO Tests on Windows."
#else
#error "The test runner must be run on a POSIX or Windows system."
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

const char *concat(const char *a, const char *b) {
	int la = strlen(a);
	int lb = strlen(b);
	char *x = malloc(la+lb+1);
	memcpy(x, a, la);
	memcpy(x+la, b, lb);
	x[la+lb] = 0;
	return x;
}


bool startsWith(const char *str, const char *end) {
	return !memcmp(str, end, strlen(end));
}
bool endsWith(const char *str, const char *end) {
	int la = strlen(str);
	int lb = strlen(end);
	if (la < lb) return false;

	return !memcmp(str+la-lb, end, lb);
}


void writeAll(FILE *file, const char *data, size_t count) {
	size_t total = 0, got = 0;
	assert(file);
	assert(count);
	assert(data);
	while ((got = fwrite(data+total, 1, count-total, file)))
		total += got;
	if (total != count || ferror(file)) {
		perror("fwrite() failed");
		exit(1);
	}
}

size_t readAll(FILE *file, char *data, size_t count) {
	assert(file);
	assert(count);
	assert(data);
	size_t total = 0, got = 0;
	while ((got = fread(data+total, 1, count-total, file)))
		total += got;

	if (ferror(file)) {
		perror("fread() failed");
		exit(1);
	}
	return total;
}

void append(char **dest, const char *str) {
	int len = strlen(str);
	memcpy(*dest, str, len);
	*dest += len;
}

char *dupe(const char *c) {
	int len = strlen(c);
	char *x = malloc(len+1);
	memcpy(x, c, len);
	x[len] = 0;
	return x;
}
