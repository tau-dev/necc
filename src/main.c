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
	F_EmitIr,
	F_Standard,
	F_Crashing,
	F_Debug,
	F_Include,
} Flag;

static Name flags[] = {
	{"h", F_Help},
	{"help", F_Help},
	{"-help", F_Help},
	{"v", F_Version},
	{"version", F_Version},
	{"ir", F_EmitIr},
	{"g", F_Debug},
	{"debug", F_Debug},
	{"std", F_Standard},
	{"crash", F_Crashing},
	{"I", F_Include},
	{0}
};


const char *help_string = "usage:\n"
	"  %s [flags] file...\n"
	"flags:\n"
	"-v, -version   Print the compiler version.\n"
	"-ir            Print the intermediate representation.\n"
	"-g, -debug     Emit debug informaion (TODO).\n"
	"-std <version> Select the used version of the C standard. Options:\n"
	"                 c89, c99, c11/c17, c23/latest, gnu, ms, lax.\n"
	"                 Default: lax.\n"
	"-I <path>      Add <path> as an include directory.\n"
	"-crash         Crash when generating an error (for debugging the compiler).\n"
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


int main (int argc, char **args) {
	bool emit_ir = false;
	char *input = NULL;
	Arena arena = create_arena(16 * 1024);

	String sys_paths[] = {
		zString("musl/arch/x86_64/"),
		zString("musl/obj/include/"),
		zString("musl/include/"),
	};

	LIST(String) user_paths = {0};
	PUSH(user_paths, STRING_EMPTY);

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

	ParseOptions opt = {
		.target = target_x64_linux_gcc,
	};

	for (int i = 1; i < argc; i++) {
		if (args[i][0] == '-') {
			Flag flag = find(args[i] + 1, flags);
			switch (flag) {
			case F_EmitIr: emit_ir = true; break;
			case F_Crashing: opt.crash_on_error = true; break;
			case F_Debug: opt.gen_debug = true; break;
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
				String path = zString(args[i]);
				if (path.ptr[path.len-1] != '/') {
					char *c = aalloc(&arena, path.len + 2);
					memcpy(c, path.ptr, path.len);
					c[path.len] = '/';
					c[path.len+1] = 0;
					path.ptr = c;
					path.len++;
				}
				PUSH(user_paths, path);
				break;
			case F_Help:
				fprintf(stderr, help_string, args[0]);
				return 0;
			case F_Version:
				fprintf(stderr, "%s NECC Version 0.0%s\n", BOLD, RESET);
				return 0;
			case F_Unknown:
				fprintf(stderr, "%swarning: %sIgnoring unknown flag %s\n", YELLOW, RESET, args[i]);
			}
		} else {
			input = args[i];
		}
	}

	if (input == NULL) {
		fprintf(stderr, "%s%serror:   %sPlease supply a file name. (Use \"-h\" to show usage informaion.)\n", BOLD, RED, RESET);
		return 1;
	}

	Paths paths = {
		.sys_include_dirs = ARRAY_SPAN(sys_paths),
		.user_include_dirs = {user_paths.len, user_paths.ptr},
	};


	Tokenization tokens = lex(&arena, input, paths, &target_x64_linux_gcc);

	parse(&arena, tokens, opt, &module);

// 	FILE *dest = fopen("a.out", "w");
	if (emit_ir) {
		for (u32 i = 0; i < module.len; i++) {
			StaticValue val = module.ptr[i];
			if (val.is_public)
				puts("public ");

			if (val.def_state != Def_Defined) {
				printf("extern %.*s\n", STRING_PRINTAGE(val.name));
			} else if (val.def_kind == Static_Function) {
				printf("%s:\n", printDeclarator(&arena, val.type, val.name));
				printBlock(val.function_entry, val.function_ir);
			} else {
				if (val.type.qualifiers & Static_Variable)
					puts("variable ");
				else
					puts("constant ");
				printf("%d:\n%.*s\n", (int) i, STRING_PRINTAGE(val.value_data));
			}
		}
	} else {
		emitX64AsmSimple(stdout, &arena, module);
	}


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












