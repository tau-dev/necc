#include "main.h"

#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

#include "parse.h"
#include "arena.h"
#include "ir_gen.h"


char *readAll (const char *filename) {
	FILE *f = fopen(filename, "rb");
	CHECK(f, "Failed to open file.")
	fseek(f, 0, SEEK_END);

	long fsize = ftell(f);
	CHECK(fsize > 0 && fsize != LONG_MAX, "Failed to open file.")
	fseek(f, 0, SEEK_SET);

	char *content = malloc(fsize + 1);
	CHECK(content, "Out of memory on reading file.")

	CHECK(fread(content, 1, fsize, f) == (size_t) fsize, "Failed to read file.")
	fclose(f);

	content[fsize] = 0;
	return content;
}

void emitX64AsmSimple(Arena *arena, Function *func, String name);

int main (int argc, char **args) {
	bool emit_ir = false;
	char *input = NULL;

	for (int i = 1; i < argc; i++) {
		if (args[i][0] == '-') {
			char *flags = args[i] + 1;
			if (strcmp(flags, "ir") == 0) {
				emit_ir = true;
			}
		} else {
			input = args[i];
		}
	}
	if (input == NULL) {
		printf("please supply file name");
		return 1;
	}

	char *code = readAll(input);
	Arena arena = create_arena(16 * 1024);

	parseFile(&arena, code);

	FILE *dest = fopen("a.out", "w");
	if (!emit_ir)
		print("use64\nformat ELF64\n\n");


	for (u32 i = 0; i < symbols.capacity; i++) {
		Symbol *s = symbols.content[i];
		if (s == NULL)
			continue;
		if (s->kind == Sym_Value && s->value.typ.kind == Kind_Function && s->value.function) {
			if (emit_ir) {
				printf("%s:\n", printDeclaration(&arena, s->value.typ, s->name));
				printIr(s->value.function);
				free(s->value.function->ir.ptr);
			} else {
				printf("public ");
				printString(s->name);
				printf("\n");
				printString(s->name);
				printf(":\n");
				emitX64AsmSimple(&arena, s->value.function, s->name);
			}
		}
	}

	fclose(dest);
	free(code);
	return 0;
}

