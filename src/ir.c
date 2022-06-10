#include "ir.h"

#include <stdarg.h>


// TODO The IR really is untyped, this belongs somewhere else.

u32 typeSize (Type t) {
	switch (t.kind) {
	case Kind_Basic:
		switch (t.basic) {
		case Basic_int:
			return 4;
		case Basic_char:
			return 1;
		}
		break;
	case Kind_Pointer:
		return 8;
	default: {}
	}
	return 0;
}


bool fnTypeEqual(FunctionType a, FunctionType b) {
	if (!typeEqual(*a.rettype, *b.rettype))
		return false;
	if (a.parameters.len != b.parameters.len)
		return false;

	for (u32 u = 0; u < a.parameters.len; u++)
		if (!typeEqual(a.parameters.ptr[u].type, b.parameters.ptr[u].type))
			return false;
	return true;
}

bool typeEqual(Type a, Type b) {
	if (a.kind != b.kind)
		return false;

	switch (a.kind) {
	case Kind_Function:
	case Kind_FunctionPtr:
		return fnTypeEqual(a.function, b.function);
	case Kind_Pointer:
		return typeEqual(*a.pointer, *b.pointer);
	case Kind_Basic:
		return a.basic == b.basic;
	default:
		printf("could not compare types, assuming equal\n");
		return true;
	}
}

void printto(char **insert, const char *end, char *fmt, ...) {
	if (*insert >= end)
		return;
	va_list args;
    va_start(args, fmt);
    int count = vsnprintf(*insert, end-*insert, fmt, args);
    va_end(args);
    if (count < 0) {
    	perror(NULL);
    	exit(1);
    }
    *insert += count;
}

#define MAX_TYPE_STRING_LENGTH 1024

char *printDeclaration(Arena *arena, Type t, String name) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	char *end = string + MAX_TYPE_STRING_LENGTH;

	printTypeBase(t, &pos, end);
	printto(&pos, end, " ");
	printTypeHead(t, &pos, end);
	if (name.len > 0 && pos + name.len + 1 < end) {
		memcpy(pos, name.ptr, name.len);
		pos += name.len;
	}
	printTypeTail(t, &pos, end);

	return string;
}
char *printType(Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	char *end = string + MAX_TYPE_STRING_LENGTH;

	printTypeBase(t, &pos, end);
	if (t.kind != Kind_Basic && t.kind != Kind_Void && t.kind != Kind_Struct && t.kind != Kind_Struct) {
		printto(&pos, end, " ");
		printTypeHead(t, &pos, end);
		printTypeTail(t, &pos, end);
	}

	return string;
}

void printTypeBase(Type t, char **pos, char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		printTypeBase(*t.pointer, pos, end); break;
	case Kind_Array:
		printTypeBase(*t.array.inner, pos, end); break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printTypeBase(*t.function.rettype, pos, end); break;
	case Kind_Void:
		printto(pos, end, "void");
		break;
	case Kind_Basic:
		switch (t.basic) {
		case Basic_int:
			printto(pos, end, "int");
			break;
		case Basic_char:
			printto(pos, end, "char");
			break;
		}
		break;
	case Kind_Struct:
		printto(pos, end, "struct {???}");
		break;
	case Kind_Enum:
		printto(pos, end, "enum {???}");
		break;
	}
}

void printTypeHead(Type t, char **pos, char *end) {
	// Is this quite right?
	if (t.storage == Storage_Extern)
		printto(pos, end, "static ");
	if (t.storage == Storage_Static)
		printto(pos, end, "static ");

	if (t.qualifiers & Qualifier_Const)
		printto(pos, end, "const ");
	if (t.qualifiers & Qualifier_Atomic)
		printto(pos, end, "atomic ");
	if (t.qualifiers & Qualifier_Volatile)
		printto(pos, end, "volatile ");
	if (t.qualifiers & Qualifier_Restrict)
		printto(pos, end, "restrict ");

	switch (t.kind) {
	case Kind_Pointer:
		printTypeHead(*t.pointer, pos, end);
		if (t.pointer->kind == Kind_Function || t.pointer->kind == Kind_Pointer)
			printto(pos, end, "(");
		printto(pos, end, "*");
		break;
	case Kind_Array:
		printTypeHead(*t.array.inner, pos, end);
		break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printTypeHead(*t.function.rettype, pos, end);
		break;
	default: {}
	}
}

void printTypeTail(Type t, char **pos, char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		if (t.pointer->kind == Kind_Function || t.pointer->kind == Kind_Pointer)
			printto(pos, end, ")");
		printTypeTail(*t.pointer, pos, end);
		break;
	case Kind_Array:
		printto(pos, end, "[%lu]", (unsigned long) t.array.count);
		printTypeTail(*t.array.inner, pos, end);
		break;
	case Kind_Function:
	case Kind_FunctionPtr:
		printto(pos, end, "(");
		for (size_t i = 0; i < t.function.parameters.len; i++) {
			printTypeBase(t.function.parameters.ptr[i].type, pos, end);
			String name = t.function.parameters.ptr[i].name;
			if (name.len > 0 && *pos + name.len + 1 < end) {
				printto(pos, end, " ");
				printTypeHead(t.function.parameters.ptr[i].type, pos, end);
				memcpy(*pos, name.ptr, name.len);
				*pos += name.len;
			} else {
				printTypeHead(t.function.parameters.ptr[i].type, pos, end);
			}
			printTypeTail(t.function.parameters.ptr[i].type, pos, end);
			if (i + 1 < t.function.parameters.len)
				printto(pos, end, ", ");
		}
		printto(pos, end, ")");
		printTypeTail(*t.function.rettype, pos, end);
		break;
	default: {}
	}
}

