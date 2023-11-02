#include <windows.h>


static char *include_envvar;

static bool trySetupStdIncludes (StringList *includes) {
	size_t var_len = 0;
	getenv_s(&var_len, NULL, 0, "INCLUDE");

	if (var_len == 0)
		return false;

	include_envvar = malloc(var_len+1);
	if (getenv_s(&var_len, include_envvar, var_len+1, "INCLUDE") != 0)
		return false;

	char *incl_path = include_envvar;
	while (incl_path < include_envvar + var_len) {
		char *sep = incl_path;
		while (*sep != ';' && *sep != '\0') {
			// if (*sep == '\\') *sep = '/';
			sep++;
		}
		// Include paths are expected to end in a slash to allow straightforward concatenation.
		*sep = '/';
		sep++;

		String path = { sep - incl_path, incl_path };
		// fprintf(stderr, "<%.*s>\n", STR_PRINTAGE(path));
		PUSH(*includes, path);

		incl_path = sep;
	}

	return true;
}


static void setupStdIncludes (StringList *includes) {
	if (!trySetupStdIncludes(includes)) {
		printInfo((SourceFile) {0}, (Location) {0});
		fprintf(stderr, "%%INCLUDE%% is not set. You may want to launch NECC in the Developer Command Prompt.\n");
		free(include_envvar);
	}
}

static void setupTerminal (void) {
	SetConsoleOutputCP(CP_UTF8);

	HANDLE console_output_handle = GetStdHandle(STD_ERROR_HANDLE);
	if (console_output_handle != INVALID_HANDLE_VALUE) {
		DWORD mode;

		if (GetConsoleMode(console_output_handle, &mode)) {
			mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
			if (SetConsoleMode(console_output_handle, mode)) {
				have_ansi_terminal = true;
			}
		}
	}
}
