#include <stdio.h>
#include <stdbool.h>

// TODO The compiler itself should be completely platform-independent.
// Enable -run platform-dependently.
#include <unistd.h>

#define __STDC_WANT_LIB_EXT1__ 1
#include <time.h>

#include "parse.h"
#include "arena.h"
#include "ansi.h"
#include "analysis.h"
#include "emit.h"



typedef struct {
	const char *name;
	u32 flag;
	enum {
		Param_None,
		Param_Appended = 1,
		Param_After = 2,
		Param_Assigned = 4,
		Param_Regular = Param_Assigned | Param_After,
		Param_Any = Param_Appended | Param_After | Param_Assigned,
	} param;
} Name;

typedef enum {
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
	F_EmitPreproc,
	F_EmitAssembly,
	F_EmitExe,
	F_EmitObj,
	F_EmitDeps,
	F_EmitLocalDeps,
	F_EmitDecls,
	F_EmitAllDecls,
	F_EmitStdDecls,
	F_Run,
	F_LinkLib,

	F_GCC_Out,
	F_GCC_Obj,
	F_GCC_Flag,

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
	{"D", F_Define, Param_Any},
	{"def", F_Define, Param_Regular},
	{"g", F_Debug},
	{"debug", F_Debug},
	{"std", F_Standard, Param_Assigned | Param_After},
	{"crash", F_Crashing},
	{"I", F_Include, Param_Any},
	{"stdinc", F_StdInc, Param_Regular},
	{"nostdinc", F_NoStdInc},

	{"run", F_Run},
	{"ir", F_EmitIr, Param_Assigned},
	{"pp", F_EmitPreproc, Param_Assigned},
	{"as", F_EmitAssembly, Param_Assigned},
	{"out", F_EmitExe, Param_Assigned},
	{"obj", F_EmitObj, Param_Assigned},
	{"M", F_EmitDeps, Param_Assigned},
	{"deps", F_EmitDeps, Param_Assigned},
	{"MM", F_EmitLocalDeps, Param_Assigned},
	{"localdeps", F_EmitLocalDeps, Param_Assigned},
	{"decls", F_EmitDecls, Param_Assigned},
	{"all-decls", F_EmitAllDecls, Param_Assigned},
	{"std-decls", F_EmitStdDecls, Param_Assigned},

	{"l", F_LinkLib, Param_Any},
	{"o", F_GCC_Out, Param_Any},
	{"c", F_GCC_Obj},
	{"f", F_GCC_Flag, Param_Any},

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
	" -g/-debug       Emit debug information.\n"
	" -std <version>  Select the used version of the C standard. Options:\n"
	"                   c89/ansi, c99, c11/c17, c23/latest, gnu, ms, lax.\n"
	" -I <path>       Add <path> as a normal include directory—these are searched by #include directive with quotes, but not with angle brackets.\n"
	" -stdinc <path>  Add <path> as a standard-library-headers include directory—searched by either kind of directive, after normal include directories.\n"
	"                   Each is searched last-to-first.\n"
	" -D/-def <macro> Define a preprocessor macro. <macro> should be the name of the macro, optionally followed by ‘=’ and the intended replacement list.\n"
	" -nostdinc       Disable default standard include paths.\n"
	" -nostdlib       Do not link to the standard library. (TODO)\n"
	" -simple-types   Print types (in declaration lists or error messages) in an easier-to-parse left-to-right syntax. (TODO)\n"
	" -Werr/-Werror   Fail the compilation if warnings are generated.\n"
	"\n"
	"The following output options write to stdout by default (except for -out and -obj), or may be followed by ‘=<FILENAME>’ to specify an output file. Default: ‘-out=a.out’.\n"
	" -out            Generate an executable.\n"
	" -obj            Generate an object file.\n"
	" -lib            Generate an archive. (TODO)\n"
	" -so/-dll        Generate a shared/dynamically linked library. (TODO)\n"
	" -as             Generate assembly code in the GNU assembler format.\n"
	" -pp             Print the preprocessed source.\n"
	" -M/-deps        Print a Makefile-compatible dependency list of all #included files.\n"
	" -MM/-localdeps  Same as above, but do not mention system header files.\n"
	" -decls          Print a list of top-level declarations in the code.\n"
	" -all-decls      Print a list of all declarations in the code.\n"
	" -std-decls      Print a list of all declarations in the code, including those from standard library headers.\n"
	"                   Declaration lists are a sequence of lines consisting of ‘<filename>:<line>:<column>:<def-kind>:<name>:<type>’,\n"
	"                   where a non-top-level <name> is qualified by a ‘.’-separated path of its containers,\n"
	"                   and <def-kind> is either ‘decl’, ‘def’, ‘type’ or ‘macro’.\n"
	"\n"
	"For compatibility purposes, the -c and -o flags are also available, with the same semantics as in GCC.\n"
	"\n"
	"Optimizations: (very rudimentary for now)\n"
	" -O              Perform optimizations: -Omem1, -Oarith.\n"
	" -Oarith         Apply some arithmetic simplifications.\n"
	" -Omem0          Fold simple store-load sequences.\n"
	" -Omem1          Perform block-local memory elisions.\n"
	" -Omem2          Perform whole-function memory elisions.\n"
	"\n"
	"Options to assist in fixing compiler errors:\n"
	" -ir             Print the intermediate representation. This is an output option.\n"
	" -crash          Crash when generating an error.\n"
	"\n";

static Name versions[] = {
	{"ansi", Version_C89},
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

static const Name *find (const char *str, const Name *names) {
	while (names->name) {
		if (strcmp(names->name, str) == 0 || ((names->param & Param_Appended) && names->name[0] == str[0]))
			return names;
		names++;
	}
	return NULL;
}

static void closeOut(FILE *f) {
	if (f) fclose(f);
}



bool had_any_output = false;
static const char *default_output = "<stdout>";

static void setOut (const char **dest, const char *f) {
	static bool had_stdout = false;
	if (!f) {
		// FIXME This isn't right for -out and -obj, which do not go to stdout by default.
		if (had_stdout)
			generalFatal("Only one output option can write to stdout; specify a destination file by appending ‘=<FILENAME>’");

		f = default_output;
		had_stdout = true;
	}
	if (*dest)
		generalFatal("Cannot output the same thing to multiple files (%s and %s)", *dest, f);
	*dest = f;
	had_any_output = true;
}

static FILE *openOut (const char *name) {
	if (name == NULL)
		return NULL;
	if (name == default_output)
		return stdout;
	FILE *f = fopen(name, "w");
	if (f == NULL)
		generalFatal("could not open output file %s", name);
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
	.link_std = true,
};

static String checkIncludePath(Arena *, const char *path);
static const char *sourceWithSuffix(Arena *arena, String source, const char *suffix);
static const char *concat(Arena *arena, ...);
static void emitDeps(FILE *deps_out, const char *out_name, FileList files, bool emit_system_files);


typedef struct {
	const char *obj;
	const char *assembly;
	// These can be meaningfully concatenated for multiple compilation
	// units, so they stay open.
	FILE *ir;
	FILE *preproc;
	FILE *deps;
	FILE *localdeps;
	FILE *decls;
	FILE *all_decls;
	FILE *std_decls;

	bool optimize_store_load;
	bool optimize_memreduce;
	bool optimize_arith;

	Arena *arena;
	LexParams lex_params;
} CompileConfig;

static void compile(String input, CompileConfig comp);
static void setupPaths(LexParams *paths, bool stdinc);

int main (int argc, char **args) {
	Arena arena = create_arena(256 * 1024);

	Options options = {
		.target = target_x64_linux_gcc,
		.warn_char_subscript = true,
		.warn_compare = true,
	};

	const char *exe_out = NULL;
	const char *obj_out = NULL;

	const char *ir_out = NULL;
	const char *preproc_out = NULL;
	const char *assembly_out = NULL;
	const char *deps_out = NULL;
	const char *localdeps_out = NULL;
	const char *decls_out = NULL;
	const char *all_decls_out = NULL;
	const char *std_decls_out = NULL;

	const char *gcc_out = NULL;
	bool gcc_obj = false;

	bool stdinc = true;
	bool runit = false;
	bool had_double_dash = false;

	CompileConfig comp = {
		.arena = &arena,
	};
	LexParams paths = {0};
	StringList compile_inputs = {0};
	StringList link_inputs = {0};
	StringList link_libraries = {0};

	for (int i = 1; i < argc; i++) {
		if (args[i][0] == '-' && !had_double_dash) {
			if (args[i][1] == '-' && args[i][2] == 0) {
				had_double_dash = true;
				continue;
			}

			char *arg_param = NULL;
			for (char *c = args[i]; *c; c++) {
				if (*c == '=') {
					*c = 0;
					arg_param = c + 1;
					break;
				}
			}

			const Name *flag = find(args[i] + 1, flags);
			if (flag == NULL) {
				fprintf(stderr, "%swarning: %sIgnoring unknown flag %s.\n", YELLOW, RESET, args[i]);
				continue;
			}
			if ((flag->param & Param_Appended) && args[i][2])
				arg_param = args[i] + 2;
			if (arg_param && !(flag->param & Param_Assigned))
				generalFatal("flag -%s does not take an =<parameter>", flag->name);
			if (!arg_param && (flag->param & Param_After))
				arg_param = args[++i];

			switch ((Flag) flag->flag) {
			case F_Help:
				fprintf(stderr, help_string, args[0]);
				return 0;
			case F_Version:
				fprintf(stderr, "%s NECC Version 0.0%s\n", BOLD, RESET);
				return 0;
			case F_Crashing: options.crash_on_error = true; break;
			case F_Debug: options.emit_debug = true; break;
			case F_EmitIr: setOut(&ir_out, arg_param); break;
			case F_EmitPreproc: setOut(&preproc_out, arg_param); break;
			case F_EmitAssembly: setOut(&assembly_out, arg_param); break;
			case F_EmitObj: setOut(&obj_out, arg_param); break;
			case F_EmitExe: setOut(&exe_out, arg_param); break;
			case F_EmitDeps: setOut(&deps_out, arg_param); break;
			case F_EmitLocalDeps: setOut(&localdeps_out, arg_param); break;
			case F_EmitDecls: setOut(&decls_out, arg_param); break;
			case F_EmitAllDecls: setOut(&all_decls_out, arg_param); break;
			case F_EmitStdDecls: setOut(&std_decls_out, arg_param); break;
			case F_Run: runit = true; break;
			case F_LinkLib:
				PUSH(link_libraries, zstr(arg_param));
				break;

			case F_GCC_Out:
				gcc_out = arg_param;
				had_any_output = true;
				break;
			case F_GCC_Obj:
				gcc_obj = true;
				had_any_output = true;
				break;
			case F_GCC_Flag:
				break;

			case F_Standard: {
				if (!arg_param)
					generalFatal("supply a standard version to use");
				const Name *v = find(arg_param, versions);
				if (v)
					options.target.version = v->flag;
				else
					fprintf(stderr, "%swarning: %sIgnoring unknown version name %s.\n", YELLOW, RESET, arg_param);
			} break;
			case F_Include: {
				PUSH(paths.user_include_dirs, checkIncludePath(&arena, arg_param));
			} break;
			case F_Define: {
				if (!arg_param)
					generalFatal("supply a definition text for the macro");
				PUSH(paths.command_line_macros, zstr(arg_param));
			} break;
			case F_StdInc: {
				PUSH(paths.sys_include_dirs, checkIncludePath(&arena, arg_param));
			} break;
			case F_NoStdInc: stdinc = false; break;
			case F_Werror: options.error_on_warnings = true; break;
			case F_OptSimple:
				comp.optimize_memreduce = true;
				comp.optimize_arith = true;
				break;
			case F_OptStoreLoad: comp.optimize_store_load = true; break;
			case F_OptArith: comp.optimize_arith = true; break;
			case F_OptMemreduce: comp.optimize_memreduce = true; break;

			case F_OptMemreduceStrong:
				break;
			}
		} else {
			String name = zstr(args[i]);
			if (name.ptr[name.len-2] == '.' && name.ptr[name.len-1] == 'c') {
				PUSH(compile_inputs, name);
			} else {
				PUSH(link_inputs, name);
			}
		}
	}

	if (!compile_inputs.len && !link_inputs.len)
		generalFatal("Please supply a file name. (Use \"-h\" to show usage information)");

	if (compile_inputs.len > 1) {
		if (obj_out && obj_out != default_output)
			generalFatal("Cannot combine multiple inputs into one object file.");
		if (assembly_out && assembly_out != default_output)
			generalFatal("Cannot combine multiple inputs into one assembly file.");
	}

	if (gcc_obj)
		obj_out = gcc_out ? gcc_out : default_output;
	else if (gcc_out)
		exe_out = gcc_out;

	if (!had_any_output || exe_out == default_output)
		exe_out = "./a.out";



	char tmp_exe[L_tmpnam] = {0};

	// tmpnam is not atomic, so this may (very rarely) collide with
	// another process. tmpfile won't work as a replacement because this
	// file needs to be input into another process, and mkstemp is only
	// available on POSIX. Damn.
	if (runit && !exe_out) {
		tmpnam(tmp_exe);
		exe_out = tmp_exe;
	}


	if (decls_out) options.emit_decls = openOut(decls_out);
	if (all_decls_out) options.emit_all_decls = openOut(all_decls_out);
	if (std_decls_out) options.emit_std_decls = openOut(std_decls_out);
	options.any_decl_emit = decls_out || all_decls_out || std_decls_out;

	paths.options = &options;
	setupPaths(&paths, stdinc);

	comp.lex_params = paths;

	comp.ir = openOut(ir_out);
	comp.preproc = openOut(preproc_out);
	comp.deps = openOut(deps_out);
	comp.localdeps = openOut(localdeps_out);
	comp.decls = openOut(decls_out);
	comp.all_decls = openOut(all_decls_out);
	comp.std_decls = openOut(std_decls_out);

	foreach (i, compile_inputs) {
		String input = compile_inputs.ptr[i];

		comp.assembly = assembly_out;
		if ((obj_out || exe_out) && !assembly_out) {
			char *tmp_asm = aalloc(&arena, L_tmpnam+3);
			tmpnam(tmp_asm);
			u32 len = strlen(tmp_asm);
			memcpy(tmp_asm + len, ".s", 3);
			comp.assembly = tmp_asm;
		}

		// Cannot emit to stdout when we need to link.
		if ((obj_out || exe_out) && assembly_out == default_output)
			comp.assembly = sourceWithSuffix(&arena, input, ".s");


		comp.obj = obj_out;
		if (obj_out == default_output)
			comp.obj = sourceWithSuffix(&arena, input, ".o");

		if (comp.obj || comp.assembly)
			PUSH(link_inputs, zstr(comp.obj ? comp.obj : comp.assembly));

		compile(input, comp);
	}

	if (exe_out) {
		assert(link_inputs.len);
		DynString cmd = {0};
		strAppend(&cmd, zstr("gcc -static -lm -o '"));
		strAppend(&cmd, zstr(exe_out));
		foreach (i, link_inputs) {
			strAppend(&cmd, zstr("' '"));
			strAppend(&cmd, link_inputs.ptr[i]);
		}
		foreach (i, link_libraries) {
			strAppend(&cmd, zstr("' -l'"));
			strAppend(&cmd, link_libraries.ptr[i]);
		}

		strAppend(&cmd, (String) {2, "'"});

		if (system(cmd.ptr)) {
			if (obj_out)
				generalFatal("failed to link object files");
			else
				generalFatal("failed to assemble");
		}
	}

	closeOut(comp.ir);
	closeOut(comp.preproc);
	closeOut(comp.deps);
	closeOut(comp.localdeps);
	closeOut(comp.decls);
	closeOut(comp.all_decls);
	closeOut(comp.std_decls);

	if (runit) {
		u32 len = strlen(exe_out)+1;
		char *c = malloc(len);
		memcpy(c, exe_out, len);
		char *new_argv[2] = {c, NULL};
		execve(exe_out, new_argv, NULL);
		perror("");
		generalFatal("could not run the generated executabe");
	}

	free_arena(&arena, "code");
#ifndef NDEBUG
	free(paths.user_include_dirs.ptr);
	free(paths.sys_include_dirs.ptr);
	free(paths.command_line_macros.ptr);
	free(paths.system_macros.ptr);
#endif

	return 0;
}

static void freeTokens (Tokenization tokens) {
	// TODO Keep files open!
	foreach (i, tokens.files) {
		SourceFile *file = tokens.files.ptr[i];
		if (file) {
			assert(file->idx == i);
			free((char *)file->abs_name.ptr);
			free((char *)file->plain_name.ptr);
			free(file);
		}
	}
	free(tokens.symbols.ptr);
	free(tokens.list.tokens);
	free(tokens.list.positions);
	free(tokens.files.ptr);
}

static void compile(String input, CompileConfig comp) {
	Tokenization tokens = lex(comp.arena, input, comp.lex_params);

	const char *out_name = sourceWithSuffix(comp.arena, input, ".o");
	if (comp.deps)
		emitDeps(comp.deps, out_name, tokens.files, true);
	if (comp.localdeps)
		emitDeps(comp.localdeps, out_name, tokens.files, false);
	if (comp.preproc)
		emitPreprocessed(&tokens, comp.preproc);

	if (!comp.ir && !comp.assembly && !comp.obj &&
		!comp.decls && !comp.all_decls && !comp.std_decls)
	{
		freeTokens(tokens);
		return;
	}

	// TODO Preserve the old module to allow typechecking across
	// compilation units that will be linked.

	Module module = {0};
	Options *options = comp.lex_params.options;
	parse(comp.arena, tokens, options, &module);

	if (options->emitted_warnings && options->error_on_warnings)
		generalFatal("generated warnings");

	if (!comp.ir && !comp.assembly && !comp.obj) {
		freeTokens(tokens);
		return;
	}

	// Analyses and transformations
	foreach (i, module) {
		StaticValue *val = &module.ptr[i];
		if (val->def_state == Def_Defined && val->def_kind == Static_Function) {
			Blocks linearized = {0};
			scheduleBlocksStraight(comp.arena, val->function_ir.entry, &linearized);

			if (comp.optimize_store_load) {
				storeLoadPropagate(val->function_ir, linearized);
			}
			if (comp.optimize_memreduce) {
				innerBlockPropagate(val->function_ir, linearized);
			}
			if (comp.optimize_arith) {
				resolveCopies(val->function_ir, linearized);
				u16 *uses = malloc(val->function_ir.len * 2);
				calcUsage(val->function_ir, uses);
				arithSimplify(val->function_ir, uses);
				free(uses);
			}
			if (comp.optimize_store_load || comp.optimize_memreduce || comp.optimize_arith)
				resolveCopies(val->function_ir, linearized);
			// The output passes currently require decimated IR.
			decimateIr(&val->function_ir, linearized);
			free(linearized.ptr);
		}
	}

	EmitParams emit_params = {
		.arena = comp.arena,
		.module = module,
		.target = &options->target,
		.files = tokens.files,
		.emit_debug_info = options->emit_debug,
		.module_name = input,
	};
	// Output
	if (comp.ir) {
		emit_params.out = comp.ir;
		emitIr(emit_params);
	}

	if (comp.assembly) {
		emit_params.out = openOut(comp.assembly);
		emitX64AsmSimple(emit_params);
		closeOut(emit_params.out);
	}

	if (comp.obj) {
		assert(comp.assembly);
		const char *cmd = concat(comp.arena, "gcc -c -x assembler '", comp.assembly, "'"
				" -o'", comp.obj, "' > /dev/null", NULL);

		if (system(cmd))
			generalFatal("failed to assemble");
	}


	foreach (i, module) {
		StaticValue *val = &module.ptr[i];
		if (val->def_kind == Static_Variable)
			free(val->value_references.ptr);
		else
			free(val->function_ir.ptr);
	}

	freeTokens(tokens);
}


static void setupPaths(LexParams *paths, bool stdinc) {
	if (stdinc) {
		PUSH(paths->sys_include_dirs, zstr(GLIBC_DIR "/include/"));
		PUSH(paths->sys_include_dirs, zstr("/usr/include/"));
		PUSH(paths->sys_include_dirs, zstr(GLIBC_DIR "/include-fixed/"));
	}

	PUSH(paths->system_macros, zstr("__STDC__=1"));
	PUSH(paths->system_macros, zstr("__x86_64__=1"));
	PUSH(paths->system_macros, zstr("__STDC_IEC_559__=1"));


	time_t compilation_time = time(NULL);
#ifdef __STDC_LIB_EXT1__
	static char buf[26];
	ctime_s(buf, 26, &compilation_time);
	char *date_text = buf;
#else
	char *date_text = ctime(&compilation_time);
#endif

	static char date_macro_text[30] = {0};
	sprintf(date_macro_text, "__DATE__=\"%.*s %.*s\"",
		6, date_text+4,   // "Mmm dd" part
		4, date_text+20); // "yyyy" part
	PUSH(paths->system_macros, zstr(date_macro_text));

	static char time_macro_text[30] = {0};
	sprintf(time_macro_text, "__TIME__=\"%.*s\"", 8, date_text+11);  // "hh:mm:ss" part
	PUSH(paths->system_macros, zstr(time_macro_text));

	Options options = *paths->options;
	if (options.target.version & Features_C23) {
		PUSH(paths->system_macros, zstr("__STDC_VERSION__=202311L"));
		PUSH(paths->system_macros, zstr("__STDC_IEC_60559_BFP__=202311L"));
	} else if (options.target.version & Features_C11) {
		PUSH(paths->system_macros, zstr("__STDC_VERSION__=201710L"));
	} else if (options.target.version & Features_C99) {
		PUSH(paths->system_macros, zstr("__STDC_VERSION__=199901L"));
	}


	if (options.target.version & Features_C99) {
		PUSH(paths->system_macros, zstr("__STDC_HOSTED__=1"));
	}

	if (options.target.version & Features_C11) {
		PUSH(paths->system_macros, zstr("__STDC_ANALYZABLE__=1"));
		PUSH(paths->system_macros, zstr("__STDC_NO_ATOMICS__=1"));
		PUSH(paths->system_macros, zstr("__STDC_NO_COMPLEX__=1"));
		PUSH(paths->system_macros, zstr("__STDC_NO_THREADS__=1"));
	}

	if (options.target.version & Features_GNU_Extensions) {
		PUSH(paths->system_macros, zstr("unix=1"));
		PUSH(paths->system_macros, zstr("__unix__=1"));
		PUSH(paths->system_macros, zstr("__LITTLE_ENDIAN__=1"));
	}
}

static String checkIncludePath (Arena *arena, const char *path) {
	if (!isDirectory(path))
		generalFatal("could not access include path %s", path);
	u32 len = strlen(path);
	if (path[len-1] == '/')
		return (String) {len, path};
	char *c = aalloc(arena, len + 2);
	memcpy(c, path, len);
	c[len] = '/';
	c[len+1] = 0;
	return (String) {len + 1, c};
}

static const char *sourceWithSuffix (Arena *arena, String input, const char *suffix) {
	u32 len = input.len;
	u32 suffix_len = strlen(suffix) + 1;
	if (len > 2 && input.ptr[len-2] == '.' && input.ptr[len-1] == 'c')
		len -= 2;
	char *name = aalloc(arena, len + suffix_len);
	memcpy(name, input.ptr, len);
	memcpy(name + len, suffix, suffix_len);
	return name;
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

static void emitDeps (FILE *dest, const char *out_name, FileList files, bool emit_system_files) {
	const u32 max_line = 80;

	fprintf(dest, "%s:", out_name);
	u32 line_length = strlen(out_name) + 1;
	foreach (i, files) {
		SourceFile *f = files.ptr[i];
		if (f->kind == Source_Regular || (f->kind == Source_StandardHeader && emit_system_files)) {
			String name = sourceName(f);
			if (line_length + name.len > max_line) {
				fprintf(dest, " \\\n ");
				line_length = 1;
			}
			fprintf(dest, " %.*s", STRING_PRINTAGE(name));
			line_length += name.len + 1;
		}
	}
	fprintf(dest, "\n");
}

