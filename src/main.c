#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

#include "parse.h"
#include "arena.h"
#include "ir_gen.h"
#include "ansii.h"


Module module = {0};

typedef struct {
	const char *name;
	u32 flag;
} Name;

typedef enum {
	F_Unknown = -1,
	F_Version,
	F_Help,
	F_Standard,
	F_Crashing,
	F_Debug,
	F_Include,
	F_EmitIr,
	F_EmitAssembly,
} Flag;

static Name flags[] = {
	{"h", F_Help},
	{"help", F_Help},
	{"-help", F_Help},
	{"v", F_Version},
	{"version", F_Version},
	{"g", F_Debug},
	{"debug", F_Debug},
	{"std", F_Standard},
	{"crash", F_Crashing},
	{"I", F_Include},
	{"ir", F_EmitIr},
	{"as", F_EmitAssembly},
	{0}
};


const char *help_string = "Usage:\n"
	"  %s [options] file...\n"
	"\n"
	"Options:\n"
	" -v, -version    Print the compiler version.\n"
	" -g, -debug      Emit debug information (TODO).\n"
	" -std <version>  Select the used version of the C standard. Options:\n"
	"                   c89, c99, c11/c17, c23/latest, gnu, ms, lax.\n"
	"                   Default: lax.\n"
	" -I <path>       Add <path> as an include directory.\n"
	" -stdinc <path>  Add <path> as a standard library headers include directory.\n"
	" -D <def>        Define a preprocessor macro. <def> may be a name, which will be defined as ‘1’, or a name followed by ‘=’ and the intended replacement list.\n"
	" -nostdinc       Disable standard include paths.\n"
	" -nostdlib       Do not link to the standard library.\n"
	" -simple-types   Print types (in definition lists or error messages) in an easier-to-parse left-to-right syntax. (TODO)\n"
	"\n"
	"The following output options write to stdout by default, or may be followed by ‘=<FILNAME>’ to specify an output file. Default: ‘-as’.\n"
	" -o, -out        Generate an executable. (TODO)\n"
	" -c, -obj        Generate an object file. (TODO)\n"
	" -lib            Generate an archive. (TODO)\n"
	" -so, -dll       Generate a shared/dynamically linked library. (TODO)\n"
	" -as             Generate assembly code in the flat assembler format.\n"
	" -cpp            Print the preprocessed source. (TODO)\n"
	" -M, -deps       Print a Makefile-compatible dependency list of all #included files. (TODO)\n"
	" -decls          Print a list of top-level declarations in the code. (TODO)\n"
	" -all-decls      Print a list of all declarations in the code. (TODO)\n"
	" -std-decls      Print a list of all declarations in the code, including those from standard library headers. (TODO)\n"
	"                   Declaration lists are a sequence of lines consisting of ‘<filename>:<line>:<column>:<def-kind>:<name>:<type>’,\n"
	"                   where a non-top-level <name> is qualified by a ‘.’-separated path of its containers,\n"
	"                   and <def-kind> is either ‘type’, ‘macro’, ‘decl’, ‘tentative-def’ or ‘def’.\n"
	"\n"
	"For debugging the compiler:\n"
	" -ir             Print the intermediate representation.\n"
	" -crash          Crash when generating an error.\n"
	"\n";

static Name versions[] = {
	{"c89", Version_C89},
	{"c99", Version_C99},
	{"c11", Version_C17},
	{"c17", Version_C17},
	{"c23", Version_C23},
	{"latest", Version_C23},
	{"gnu", Version_GNU},
	{"ms", Version_MSVC},
	{"lax", Version_Lax},
	{0}
};

static i32 find(const char *str, const Name *names) {
	while (names->name) {
		if (strcmp(names->name, str) == 0)
			return names->flag;
		names++;
	}
	return -1;
}


void emitX64AsmSimple(FILE *out, Arena *arena, Module module);

bool had_output = false;
static const char *stdout_marker = "<stdout>";

static void setOut(const char **dest, char *f) {
	static bool had_stdout;
	if (f) {
		*dest = f;
	} else {
		if (had_stdout)
			generalFatal("Only one output option can write to stdout; specify a destination file by appending ‘=<FILENAME>’.\n");

		*dest = stdout_marker;
		had_stdout = true;
	}
	had_output = true;
}
static FILE *openOut(const char *name) {
	if (name == stdout_marker)
		return stdout;
	FILE *f = fopen(name, "w");
	if (f == NULL)
		generalFatal("Could not open output file %s\n", name);
	return f;
}


Target target_x64_linux_gcc = {
	.ptrdiff = {Kind_Basic, .basic = Int_long},
	.intptr = {Kind_Basic, .basic = Int_long},
	.typesizes = {
		[Int_bool] = I8,
		[Int_char] = I8,
		[Int_suchar] = I8,
		[Int_short] = I16,
		[Int_int] = I32,
		[Int_long] = I64,
		[Int_longlong] = I64,
	},
	.ptr_size = I64,
	.version = Version_GNU,
};

static String checkIncludePath(Arena *, const char *path);

int main (int argc, char **args) {
	String input = {0};
	Arena arena = create_arena(16 * 1024);
	const char *assembly_out = NULL;
	const char *ir_out = NULL;

	String sys_paths[] = {
		zString("musl/arch/x86_64/"),
		zString("musl/obj/include/"),
		zString("musl/include/"),
	};

	LIST(String) user_paths = {0};


	Options opt = {
		.target = target_x64_linux_gcc,
	};

	for (int i = 1; i < argc; i++) {
		if (args[i][0] == '-') {
			char *direct_arg = NULL;
			for (char *c = args[i]; *c; c++) {
				if (*c == '=') {
					*c = 0;
					direct_arg = c + 1;
					break;
				}
			}
			Flag flag = find(args[i] + 1, flags);
			switch (flag) {
			case F_Help:
				fprintf(stderr, help_string, args[0]);
				return 0;
			case F_Version:
				fprintf(stderr, "%s NECC Version 0.0%s\n", BOLD, RESET);
				return 0;
			case F_Crashing: opt.crash_on_error = true; break;
			case F_Debug: opt.gen_debug = true; break;
			case F_EmitIr: setOut(&ir_out, direct_arg); break;
			case F_EmitAssembly: setOut(&assembly_out, direct_arg); break;
			case F_Standard:
				i++;
				i32 v = find(args[i], versions);
				if (v == -1)
					fprintf(stderr, "%swarning: %sIgnoring unknown version name %s\n", YELLOW, RESET, args[i]);
				else
					opt.target.version = v;
				break;
			case F_Include:
				i++;
				PUSH(user_paths, checkIncludePath(&arena, args[i]));
				break;
			case F_Unknown:
				fprintf(stderr, "%swarning: %sIgnoring unknown flag %s\n", YELLOW, RESET, args[i]);
			}
		} else {
			input = zString(args[i]);
		}
	}

	if (input.ptr == NULL)
		generalFatal("Please supply a file name. (Use \"-h\" to show usage informaion.)\n");

	if (!had_output)
		assembly_out = stdout_marker;

	i32 i;
	for (i = input.len - 1; i >= 0; i--) {
		if (input.ptr[i] == '/')
			break;
	}
	PUSH(user_paths, ((String){i+1, input.ptr}));

	Paths paths = {
		.sys_include_dirs = ARRAY_SPAN(sys_paths),
		.user_include_dirs = {user_paths.len, user_paths.ptr},
	};


	Tokenization tokens = lex(&arena, input, paths, &target_x64_linux_gcc);

	parse(&arena, tokens, opt, &module);

	if (ir_out) {
		FILE *dest = openOut(ir_out);
		for (u32 i = 0; i < module.len; i++) {
			StaticValue val = module.ptr[i];
			if (val.is_public)
				fprintf(dest, "public ");

			if (val.def_state != Def_Defined) {
				fprintf(dest, "extern %.*s\n", STRING_PRINTAGE(val.name));
			} else if (val.def_kind == Static_Function) {
				fprintf(dest, "%s:\n", printDeclarator(&arena, val.type, val.name));
				printBlock(dest, val.function_entry, val.function_ir);
			} else {
				if (val.type.qualifiers & Static_Variable)
					fprintf(dest, "variable ");
				else
					fprintf(dest, "constant ");
				String name = val.name;
				if (name.len == 0)
					name = zString("[anon]");
				fprintf(dest, "%d (%.*s):\n", (int) i, STRING_PRINTAGE(name));

				bool is_string = true;
				String data = val.value_data;
				for (u32 i = 0; i < data.len - 1; i++) {
					if (data.ptr[i] < 32 || (uchar) data.ptr[i] >= 128)
						is_string = false;
				}
				is_string = is_string && data.ptr[data.len - 1] == 0;
				if (is_string) {
					fprintf(dest, "\"%.*s\"\n", STRING_PRINTAGE(data));
				} else {
					for (u32 i = 0; i < data.len; i++) {
						fprintf(dest, "%02hhx ", data.ptr[i]);
					}
					fprintf(dest, "\n");
				}
			}
		}
	}

	if (assembly_out)
		emitX64AsmSimple(openOut(assembly_out), &arena, module);


#ifndef NDEBUG
	free_arena(&arena);
	for (u32 i = 0; i < tokens.files.len; i++)
		free(tokens.files.ptr[i]);

	free(tokens.tokens);
	free(tokens.positions);
	free(tokens.files.ptr);
#endif
	return 0;
}

// TODO Do not allow regular files.
static bool isDirectory(const char *path) {
	FILE *f = fopen(path, "r");
	if (f == NULL)
		return false;
	fclose(f);
	return true;
}

static String checkIncludePath(Arena *arena, const char *path) {
	if (!isDirectory(path))
		generalFatal("could not open include path %s.\n", path);
	u32 len = strlen(path);
	if (path[len-1] == '/')
		return (String) {len, path};
	char *c = aalloc(arena, len + 2);
	memcpy(c, path, len);
	c[len] = '/';
	c[len+1] = 0;
	return (String) {len + 1, c};
}

