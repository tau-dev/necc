#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

#include "parse.h"
#include "arena.h"
#include "ir_gen.h"
#include "ansii.h"
#include "analysis.h"


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
	F_Define,
	F_Include,
	F_Crashing,
	F_Debug,

	F_StdInc,
	F_NoStdInc,
	F_EmitIr,
	F_EmitAssembly,
	F_EmitExe,
	F_EmitObj,
	F_EmitDeps,
	F_EmitLocalDeps,

	F_OptSimple,
	F_OptStoreLoad,
	F_Werror,
} Flag;

static Name flags[] = {
	{"h", F_Help},
	{"help", F_Help},
	{"-help", F_Help},
	{"v", F_Version},
	{"version", F_Version},
	{"D", F_Define},
	{"def", F_Define},
	{"g", F_Debug},
	{"debug", F_Debug},
	{"std", F_Standard},
	{"crash", F_Crashing},
	{"I", F_Include},
	{"stdinc", F_StdInc},
	{"nostdinc", F_NoStdInc},

	{"ir", F_EmitIr},
	{"as", F_EmitAssembly},
	{"o", F_EmitExe},
	{"out", F_EmitExe},
	{"c", F_EmitObj},
	{"obj", F_EmitObj},
	{"M", F_EmitDeps},
	{"deps", F_EmitDeps},
	{"MM", F_EmitLocalDeps},
	{"localdeps", F_EmitLocalDeps},

	{"O", F_OptSimple},
	{"Ostore-load", F_OptStoreLoad},
	{"Werr", F_Werror},
	{"Werror", F_Werror},
	{0}
};


const char *help_string = "Usage:\n"
	"  %s [options] file...\n"
	"\n"
	"Options:\n"
	" -v/-version     Print the compiler version.\n"
	" -g/-debug       Emit debug information (TODO).\n"
	" -std <version>  Select the used version of the C standard. Options:\n"
	"                   c89, c99, c11/c17, c23/latest, gnu, ms, lax.\n"
	" -I <path>       Add <path> as an include directory.\n"
	" -stdinc <path>  Add <path> as a standard library headers include directory.\n"
	" -D/-def <macro> Define a preprocessor macro. <macro> should be the name of the macro, optionally followed by ‘=’ and the intended replacement list.\n"
	" -nostdinc       Disable default standard include paths.\n"
	" -nostdlib       Do not link to the standard library. (TODO)\n"
	" -simple-types   Print types (in declaration lists or error messages) in an easier-to-parse left-to-right syntax. (TODO)\n"
	" -Werr/-Werror   Fail the compilation if warnings are generated.\n"
	"\n"
	"The following output options write to stdout by default, or may be followed by ‘=<FILENAME>’ to specify an output file. Default: ‘-o=a.out’.\n"
	" -o/-out         Generate an executable.\n"
	" -c/-obj         Generate an object file.\n"
	" -lib            Generate an archive. (TODO)\n"
	" -so/-dll        Generate a shared/dynamically linked library. (TODO)\n"
	" -as             Generate assembly code in the flat assembler format.\n"
	" -cpp            Print the preprocessed source. (TODO)\n"
	" -M/-deps        Print a Makefile-compatible dependency list of all #included files.\n"
	" -MM/-localdeps  Same as above, but do not mention system header files.\n"
	" -decls          Print a list of top-level declarations in the code. (TODO)\n"
	" -all-decls      Print a list of all declarations in the code. (TODO)\n"
	" -std-decls      Print a list of all declarations in the code, including those from standard library headers. (TODO)\n"
	"                   Declaration lists are a sequence of lines consisting of ‘<filename>:<line>:<column>:<def-kind>:<name>:<type>’,\n"
	"                   where a non-top-level <name> is qualified by a ‘.’-separated path of its containers,\n"
	"                   and <def-kind> is either ‘decl’, ‘def’, ‘type’ or ‘macro’.\n"
	"\n"
	"Optimizations:\n"
	" -O              Perform optimizations: -Ostore-load.\n"
	" -Ostore-load    Fold simple store-load sequences.\n"
	"\n"
	"Options to assist in fixing compiler errors:\n"
	" -ir             (Output option) Print the intermediate representation.\n"
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


void emitX64AsmSimple(FILE *out, Arena *arena, Module module, const Target *target);

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
	.enum_int = Int_int,
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
	.int_size = I32,
	.version = Version_GNU,
};

static String checkIncludePath(Arena *, const char *path);
static void emitIr(Arena *arena, const char *ir_out, Module module);
static void emitDeps(const char *deps_out, const char *out_name, FileList files, bool emit_system_files);
static const char *concat(Arena *arena, ...);

int main (int argc, char **args) {
	String input = {0};
	Arena arena = create_arena(256 * 1024);

	Options opt = {
		.target = target_x64_linux_gcc,
		.warn_on_wrapping = true,
		.warn_char_subscript = true,
		.warn_compare = true,
	};

	const char *ir_out = NULL;
	const char *assembly_out = NULL;
	const char *obj_out = NULL;
	const char *exe_out = NULL;
	const char *deps_out = NULL;
	const char *localdeps_out = NULL;

	bool stdinc = true;
	bool opt_store_load = false;

	LexParams paths = {0};

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
			case F_EmitObj: setOut(&obj_out, direct_arg); break;
			case F_EmitExe: setOut(&exe_out, direct_arg); break;
			case F_EmitDeps: setOut(&deps_out, direct_arg); break;
			case F_EmitLocalDeps: setOut(&localdeps_out, direct_arg); break;
			case F_Standard: {
				const char *arg = direct_arg ? direct_arg : args[++i];
				i32 v = find(arg, versions);
				if (v == -1)
					fprintf(stderr, "%swarning: %sIgnoring unknown version name %s\n", YELLOW, RESET, args[i]);
				else
					opt.target.version = v;
			} break;
			case F_Include: {
				const char *arg = direct_arg ? direct_arg : args[++i];
				PUSH(paths.user_include_dirs, checkIncludePath(&arena, arg));
			} break;
			case F_Define:
				i++;
				PUSH(paths.command_line_macros, zstr(args[i]));
				break;
			case F_StdInc: {
				const char *arg = direct_arg ? direct_arg : args[++i];
				PUSH(paths.sys_include_dirs, checkIncludePath(&arena, arg));
			} break;
			case F_NoStdInc: stdinc = false; break;
			case F_OptSimple:
				opt_store_load = true;
				break;
			case F_OptStoreLoad: opt_store_load = true; break;
			case F_Werror: opt.error_on_warnings = true; break;
			case F_Unknown:
				fprintf(stderr, "%swarning: %sIgnoring unknown flag %s\n", YELLOW, RESET, args[i]);
			}
		} else {
			input = zstr(args[i]);
		}
	}

	if (!input.len)
		generalFatal("Please supply a file name. (Use \"-h\" to show usage information.)\n");

	if (!had_output)
		exe_out = "a.out";

// 	char tmp_obj[L_tmpnam] = {0};
// 	char tmp_asm[L_tmpnam] = {0};

	if (exe_out && !obj_out) {
// 		tmpnam(tmp_obj);
// 		obj_out = tmp_obj;
		obj_out = "/tmp/a.obj";
	}
	if (obj_out && !assembly_out) {
// 		tmpnam(tmp_asm);
// 		assembly_out = tmp_asm;
		assembly_out = "/tmp/a.s";
	}

	i32 i;
	for (i = input.len - 1; i >= 0; i--) {
		if (input.ptr[i] == '/')
			break;
	}
	String input_directory = {i+1, input.ptr};

	if (stdinc) {
		PUSH(paths.sys_include_dirs, zstr(MUSL_DIR "/arch/generic/"));
		PUSH(paths.sys_include_dirs, zstr(MUSL_DIR "/arch/x86_64/"));
		PUSH(paths.sys_include_dirs, zstr(MUSL_DIR "/obj/include/"));
		PUSH(paths.sys_include_dirs, zstr(MUSL_DIR "/include/"));
	}
	PUSH(paths.user_include_dirs, input_directory);


	PUSH(paths.system_macros, zstr("__STDC__"));
	// TODO
// 	predefine(arena, macros, "__DATE__", 1);
// 	predefine(arena, macros, "__TIME__", 1);
	if (opt.target.version & Features_C23) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=202301L"));
	} else if (opt.target.version & Features_C11) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=201710L"));
	} else if (opt.target.version & Features_C99) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=199901L"));
	}


	if (opt.target.version & Features_C99) {
		PUSH(paths.system_macros, zstr("__STDC_HOSTED__"));
	}

	if (opt.target.version & Features_C11) {
		PUSH(paths.system_macros, zstr("__STDC_ANALYZABLE__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_ATOMICS__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_COMPLEX__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_THREADS__"));
	}

	if (opt.target.version &Features_GNU_Extensions) {
		PUSH(paths.system_macros, zstr("unix"));
		PUSH(paths.system_macros, zstr("__unix__"));
		PUSH(paths.system_macros, zstr("__LITTLE_ENDIAN__"));
	}



	// The Real Work happens now.
	Tokenization tokens = lex(&arena, input, paths);

	const char *out_name = exe_out ? exe_out :
		obj_out ? obj_out : "a.out";
	if (deps_out)
		emitDeps(deps_out, out_name, tokens.files, true);
	if (localdeps_out)
		emitDeps(localdeps_out, out_name, tokens.files, false);

	if (!ir_out && !assembly_out && !obj_out && !exe_out)
		return 0;

	parse(&arena, tokens, &opt, &module);

	if (opt.emitted_warnings && opt.error_on_warnings)
		generalFatal("generated warnings");

	// Analyses and transformations
	for (u32 i = 0; i < module.len; i++) {
		StaticValue *val = &module.ptr[i];
		if (val->def_state == Def_Defined && val->def_kind == Static_Function) {
			Blocks linearized = {0};
			scheduleBlocksStraight(&arena, val->function_entry, &linearized);
			if (opt_store_load)
				innerBlockPropagate(val->function_ir, linearized);
			resolveCopies(val->function_ir);
			decimateIr(&val->function_ir, linearized);
			free(linearized.ptr);
		}
	}

	// Output
	if (ir_out) {
		emitIr(&arena, ir_out, module);
	}

	if (assembly_out) {
		FILE *dest = openOut(assembly_out);
		emitX64AsmSimple(dest, &arena, module, &target_x64_linux_gcc);
		fclose(dest);
	}
	if (obj_out) {
		assert(assembly_out);
		const char *cmd = concat(&arena, "fasm -m500000 \"", assembly_out, "\" \"", obj_out, "\"", 0);
		system(cmd);
	}
	if (exe_out) {
		assert(obj_out);
		const char *cmd = concat(&arena, "musl-gcc -static \"", obj_out, "\" -o", exe_out, 0);
		system(cmd);
	}

#ifndef NDEBUG
	free_arena(&arena, "code");
	for (u32 i = 0; i < tokens.files.len; i++)
		free(tokens.files.ptr[i]);
	for (u32 i = 0; i < module.len; i++) {
		StaticValue *val = &module.ptr[i];
		if (val->def_kind == Static_Variable)
			free(val->value_references.ptr);
		else
			free(val->function_ir.ptr);
	}

	free(tokens.symbols.ptr);
	free(tokens.tokens);
	free(tokens.positions);
	free(tokens.files.ptr);

	free(paths.user_include_dirs.ptr);
	free(paths.sys_include_dirs.ptr);
	free(paths.command_line_macros.ptr);
	free(paths.system_macros.ptr);
#endif
	return 0;
}


static String checkIncludePath (Arena *arena, const char *path) {
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


static void emitIr (Arena *arena, const char *ir_out, Module module) {
	FILE *dest = openOut(ir_out);
	for (u32 i = 0; i < module.len; i++) {
		StaticValue val = module.ptr[i];
		if (val.is_public)
			fprintf(dest, "public ");

		if (val.def_state != Def_Defined) {
			fprintf(dest, "extern %.*s\n", STRING_PRINTAGE(val.name));
		} else if (val.def_kind == Static_Function) {
			fprintf(dest, "%s:\n", printDeclarator(arena, val.type, val.name));
			printBlock(dest, val.function_entry, val.function_ir);
		} else {
			if (val.type.qualifiers & Static_Variable)
				fprintf(dest, "variable ");
			else
				fprintf(dest, "constant ");
			String name = val.name;
			if (name.len == 0)
				name = zstr("[anon]");
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
	fclose(dest);
}

static void emitDeps (const char *deps_out, const char *out_name, FileList files, bool emit_system_files) {
	const u32 max_line = 80;
	FILE *dest = openOut(deps_out);

	fprintf(dest, "%s:", out_name);
	u32 line_length = strlen(out_name) + 1;
	for (u32 i = 0; i < files.len; i++) {
		SourceFile *f = files.ptr[i];
		if (f->kind == Source_Regular || (f->kind == Source_StandardHeader && emit_system_files)) {
			u32 elem_length = f->path.len + f->name.len + 1;
			if (line_length + elem_length > max_line) {
				fprintf(dest, " \\\n ");
				line_length = 1;
			}
			fprintf(dest, " %.*s%.*s", STRING_PRINTAGE(f->path), STRING_PRINTAGE(f->name));
			line_length += elem_length;
		}
	}
	fprintf(dest, "\n");
	fclose(dest);
}

static const char *concat (Arena *arena, ...) {
	va_list args;
	va_start(args, arena);
	u32 len = 1;
	for (const char *str = va_arg(args, const char*); str; str = va_arg(args, const char*)) {
		len += strlen(str);
	}

	char *joined = aalloc(arena, len);
	char *insert = joined;
	va_end(args);
	va_start(args, arena);
	for (const char *str = va_arg(args, const char*); str; str = va_arg(args, const char*)) {
		u32 l = strlen(str);
		memcpy(insert, str, l);
		insert += l;
	}
	va_end(args);

	joined[len-1] = 0;
	return joined;
}
