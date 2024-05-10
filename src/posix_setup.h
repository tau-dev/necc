#include <unistd.h>

static void setupTerminal (void) {
	have_ansi_terminal = isatty(2);
}

static void setupStdIncludes (StringList *dirs) {
	PUSH(*dirs, zstr(GLIBC_DIR "/include/"));
	PUSH(*dirs, zstr("/usr/include/"));
	PUSH(*dirs, zstr(GLIBC_DIR "/include-fixed/"));
}
