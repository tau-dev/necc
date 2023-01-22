#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

// TODO The compiler itself should be completely platform-independent.
// Enable -run platform-dependently.
#include <unistd.h>

#include "parse.h"
#include "arena.h"
#include "ir_gen.h"
#include "ansi.h"
#include "analysis.h"
#include "emit.h"


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
	F_Run,

	F_OptSimple,
	F_OptStoreLoad,
	F_OptArith,
	F_OptMemreduce,
	F_OptMemreduceStrong,
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

	{"run", F_Run},
	{"ir", F_EmitIr},
	{"as", F_EmitAssembly},
	{"out", F_EmitExe},
	{"obj", F_EmitObj},
	{"M", F_EmitDeps},
	{"deps", F_EmitDeps},
	{"MM", F_EmitLocalDeps},
	{"localdeps", F_EmitLocalDeps},

	{"O", F_OptSimple},
	{"Oarith", F_OptArith},
	{"Omem0", F_OptStoreLoad},
	{"Omem1", F_OptMemreduce},
	{"Omem2", F_OptMemreduceStrong},
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
	" -out            Generate an executable.\n"
	" -obj            Generate an object file.\n"
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
	" -O              Perform optimizations: -Omem1, -Oarith.\n"
	" -Oarith         Apply some arithmetic simplifications.\n"
	" -Omem0    Fold simple store-load sequences.\n"
	" -Omem1          Perform block-local memory elisions.\n"
	" -Omem2          Perform whole-function memory elisions.\n"
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



bool had_output = false;
static const char *stdout_marker = "<stdout>";

static void setOut(const char **dest, char *f) {
	static bool had_stdout = false; // TODO Inits!
	if (f) {
		*dest = f;
	} else {
		if (had_stdout)
			generalFatal("Only one output option can write to stdout; specify a destination file by appending ‘=<FILENAME>’");

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
		generalFatal("Could not open output file %s", name);
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
	.valist_size = I32 + I32 + I64 + I64,
	.version = Version_GNU,
};

static String checkIncludePath(Arena *, const char *path);
static const char *concat(Arena *arena, ...);
static void emitDeps(const char *deps_out, const char *out_name, FileList files, bool emit_system_files);

int main (int argc, char **args) {
	String input = {0};
	Arena arena = create_arena(256 * 1024);

	Options options = {
		.target = target_x64_linux_gcc,
// 		.warn_on_wrapping = true,
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
	bool opt_memreduce = false;
	bool opt_arith = false;
	bool runit = false;

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
			case F_Crashing: options.crash_on_error = true; break;
			case F_Debug: options.emit_debug = true; break;
			case F_EmitIr: setOut(&ir_out, direct_arg); break;
			case F_EmitAssembly: setOut(&assembly_out, direct_arg); break;
			case F_EmitObj: setOut(&obj_out, direct_arg); break;
			case F_EmitExe: setOut(&exe_out, direct_arg); break;
			case F_EmitDeps: setOut(&deps_out, direct_arg); break;
			case F_EmitLocalDeps: setOut(&localdeps_out, direct_arg); break;
			case F_Run: runit = true; break;
			case F_Standard: {
				const char *arg = direct_arg ? direct_arg : args[++i];
				i32 v = find(arg, versions);
				if (v == -1)
					fprintf(stderr, "%swarning: %sIgnoring unknown version name %s\n", YELLOW, RESET, args[i]);
				else
					options.target.version = v;
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
			case F_Werror: options.error_on_warnings = true; break;
			case F_OptSimple:
				opt_memreduce = true;
				opt_arith = true;
				break;
			case F_OptStoreLoad: opt_store_load = true; break;
			case F_OptArith: opt_arith = true; break;
			case F_OptMemreduce: opt_memreduce = true; break;

			case F_OptMemreduceStrong:
			case F_Unknown:
				fprintf(stderr, "%swarning: %sIgnoring unknown flag %s\n", YELLOW, RESET, args[i]);
			}
		} else {
			input = zstr(args[i]);
		}
	}

	if (!input.len)
		generalFatal("Please supply a file name. (Use \"-h\" to show usage information.)");

	if (!had_output)
		exe_out = "./a.out";


	i32 i;
	for (i = input.len - 1; i >= 0; i--) {
		if (input.ptr[i] == '/')
			break;
	}
	i++;
	String input_directory = {i, input.ptr};
	String input_name = {input.len - i, input.ptr + i};

	const char *obj_name = obj_out;
	char obj_name_buf[1024];
	if (!obj_name) {
		obj_name = obj_name_buf;
		u32 len = input_name.len;
		if (input_name.ptr[len-2] == '.' && input_name.ptr[len-1] == 'c')
			len -= 2;
		memcpy(obj_name_buf, input_name.ptr, len);
		memcpy(obj_name_buf + len, ".o", 3);
	}

	char tmp_exe[L_tmpnam] = {0};
// 	char tmp_obj[L_tmpnam] = {0};
	char tmp_asm[L_tmpnam] = {0};

	if (runit && !exe_out) {
		tmpnam(tmp_exe);
		exe_out = tmp_exe;
	}

	if ((obj_out || exe_out) && !assembly_out) {
		tmpnam(tmp_asm);
		assembly_out = tmp_asm;
	}

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
	if (options.target.version & Features_C23) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=202301L"));
	} else if (options.target.version & Features_C11) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=201710L"));
	} else if (options.target.version & Features_C99) {
		PUSH(paths.system_macros, zstr("__STDC_VERSION__=199901L"));
	}


	if (options.target.version & Features_C99) {
		PUSH(paths.system_macros, zstr("__STDC_HOSTED__"));
	}

	if (options.target.version & Features_C11) {
		PUSH(paths.system_macros, zstr("__STDC_ANALYZABLE__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_ATOMICS__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_COMPLEX__"));
		PUSH(paths.system_macros, zstr("__STDC_NO_THREADS__"));
	}

	if (options.target.version & Features_GNU_Extensions) {
		PUSH(paths.system_macros, zstr("unix"));
		PUSH(paths.system_macros, zstr("__unix__"));
		PUSH(paths.system_macros, zstr("__LITTLE_ENDIAN__"));
	}



	// The Real Work happens now.
	Tokenization tokens = lex(&arena, input, paths);

	const char *out_name = exe_out ? exe_out : obj_name;
	if (deps_out)
		emitDeps(deps_out, out_name, tokens.files, true);
	if (localdeps_out)
		emitDeps(localdeps_out, out_name, tokens.files, false);

	if (!ir_out && !assembly_out && !obj_out && !exe_out)
		return 0;

	parse(&arena, tokens, &options, &module);

	if (options.emitted_warnings && options.error_on_warnings)
		generalFatal("generated warnings");

	// Analyses and transformations
	for (u32 i = 0; i < module.len; i++) {
		StaticValue *val = &module.ptr[i];
		if (val->def_state == Def_Defined && val->def_kind == Static_Function) {
			Blocks linearized = {0};
			scheduleBlocksStraight(&arena, val->function_ir.entry, &linearized);

			if (opt_store_load) {
				storeLoadPropagate(val->function_ir, linearized);
			}
			if (opt_memreduce) {
				innerBlockPropagate(val->function_ir, linearized);
			}
			if (opt_arith) {
				resolveCopies(val->function_ir, linearized);
				u16 *uses = malloc(val->function_ir.len * 2);
				calcUsage(val->function_ir, uses);
				arithSimplify(val->function_ir, uses);
				free(uses);
			}
			if (opt_store_load || opt_memreduce || opt_arith)
				resolveCopies(val->function_ir, linearized);

			// The output passes currently require decimated IR.
			decimateIr(&val->function_ir, linearized);
			free(linearized.ptr);
		}
	}

	EmitParams emit_params = {
		.arena = &arena,
		.module = module,
		.target = &options.target,
		.files = tokens.files,
		.emit_debug_info = options.emit_debug,
	};
	// Output
	if (ir_out) {
		emit_params.out = openOut(ir_out);
		emitIr(emit_params);
		fclose(emit_params.out);
	}

	if (assembly_out) {
		emit_params.out = openOut(assembly_out);
		emitX64AsmSimple(emit_params);
		fclose(emit_params.out);
	}

	if (obj_out) {
		assert(assembly_out);
		const char *cmd = concat(&arena, "musl-gcc -c -x assembler \"", assembly_out, "\" -o\"", obj_out, "\" > /dev/null", NULL);
		if (system(cmd))
			generalFatal("failed to assemble");
	}
	if (exe_out) {
		const char *cmd = concat(&arena, "musl-gcc -static -lm  -x assembler \"", obj_out ? obj_out : assembly_out, "\" -o", exe_out, NULL);
		if (system(cmd)) {
			if (obj_out)
				generalFatal("failed to link object files");
			else
				generalFatal("failed to assemble");
		}
	}

	if (runit) {
		u32 len = strlen(exe_out)+1;
		char *c = malloc(len);
		memcpy(c, exe_out, len);
		char *new_argv[2] = {c, NULL};
		execve(exe_out, new_argv, NULL);
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
		generalFatal("could not open include path %s", path);
	u32 len = strlen(path);
	if (path[len-1] == '/')
		return (String) {len, path};
	char *c = aalloc(arena, len + 2);
	memcpy(c, path, len);
	c[len] = '/';
	c[len+1] = 0;
	return (String) {len + 1, c};
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

