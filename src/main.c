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

typedef long(*func)(long);

int main (int argc, char **args) {
	CHECK(argc == 2, "Expected one argument.")
	(void) args;

	char *code = readAll(args[1]);
	Arena arena = create_arena(16 * 1024);

	parseFile(&arena, code);

	FILE *dest = fopen("a.out", "w");

	for (u32 i = 0; i < symbols.capacity; i++) {
		Symbol *s = symbols.content[i];
		if (s == NULL)
			continue;
		if (s->kind == Sym_Value && s->value.typ.kind == Kind_Function && s->value.function) {
			printf("%s:\n", printDeclaration(&arena, s->value.typ, s->name));
			printIr(s->value.function);
// 			emitX64(dest, &s->val.function);
			free(s->value.function->ir.ptr);
		}
	}

	fclose(dest);
	free(code);
	return 0;
}

