#include <unistd.h>

static void setupTerminal (void) {
	have_terminal = isatty(2);
}

static void setupStdIncludes (StringList *str) {
	PUSH(*dirs, zstr(GLIBC_DIR "/include/"));
	PUSH(*dirs, zstr("/usr/include/"));
	PUSH(*dirs, zstr(GLIBC_DIR "/include-fixed/"));
}
