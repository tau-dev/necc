#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

#include "parse.h"
#include "arena.h"
#include "ir_gen.h"


Module module = {0};

void emitX64AsmSimple(Arena *arena, Module module);

int main (int argc, char **args) {
	bool emit_ir = false;
	char *input = NULL;

	String sys_paths[] = {
		zString("/home/tau/foreign/lang/musl-1.2.3/arch/x86_64"),
		zString("/home/tau/foreign/lang/musl-1.2.3/obj/include/"),
		zString("/home/tau/foreign/lang/musl-1.2.3/include/"),
	};
	String user_paths[] = {
		zString("./"),
	};
	Paths paths = {
		.sys_include_dirs = ARRAY_SPAN(sys_paths),
		.user_include_dirs = ARRAY_SPAN(user_paths),
	};

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
		.version = Version_GNU,
	};

	for (int i = 1; i < argc; i++) {
		if (args[i][0] == '-') {
			char *flags = args[i] + 1;
			if (strcmp(flags, "ir") == 0) {
				emit_ir = true;
			} else if (strcmp(flags, "v") == 0) {
				printf(" NECC Version 0.0\n");
				return 0;
			} else if (strcmp(flags, "std") == 0) {
				if (!args[i+1]) {
					// ...
				}
			}
		} else {
			input = args[i];
		}
	}
	if (input == NULL) {
		printf("Please supply a file name.\n");
		return 1;
	}

	Arena arena = create_arena(16 * 1024);


	Tokenization tokens = lex(&arena, input, paths, &target_x64_linux_gcc);

	parse(&arena, tokens, target_x64_linux_gcc, &module);

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
		emitX64AsmSimple(&arena, module);
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












