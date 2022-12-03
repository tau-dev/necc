#include "types.h"
#include "ansii.h"
#include "util.h"
#include "parse.h"
// #include <stdarg.h>


// Used in the phrasings "supported with $version" and "target uses $version".

const char *version_names[Version_Lax + 1] = {
	[Version_C89] = "C89",
	[Version_C99] = "C99",
	// lol. The compiler does not care to distinguish between C11 and
	// C17; semantics always follow C17 (therefore the enum name;
	// __STDC_VERSION__ is 201710L), but all relevant features are
	// already present in C11.
	[Version_C17] = "C11",
	[Version_C23] = "C23",

	[Version_GNU] = "GNU extensions",
	[Version_MSVC] = "MSVC extensions",

	[Version_Lax] = "lax conformance checks",
};


// Size typeSize (Type t, const Target *target) {
// 	switch (t.kind) {
// 	default: unreachable;
// 	}
// }

// Result of 0 means incomplete type.
u32 typeSize (Type t, const Target *target) {
	switch (t.kind) {
	case Kind_Union_Named:
	case Kind_Struct_Named: {
		Type actual = t.nametagged->type;
		if (actual.kind == Kind_Void)
			return 0; // Incomplete
		if (t.kind == Kind_Struct_Named)
			assert(actual.kind == Kind_Struct);
		else
			assert(actual.kind == Kind_Union);
		return typeSize(actual, target);
	}
	case Kind_Union:
	case Kind_Struct: {
		u32 max_alignment = 1;
		u32 tightsize = 0;
		for (u32 i = 0; i < t.members.len; i++) {
			CompoundMember member = t.members.ptr[i];
			u32 align = typeAlignment(member.type, target);
			if (align > max_alignment)
				max_alignment = align;
			u32 end = member.offset + typeSize(member.type, target);
			if (end > tightsize)
				tightsize = end;
		}
		return ((tightsize + max_alignment - 1) / max_alignment) * max_alignment;
	}
	case Kind_Void:
		return 0;
	case Kind_Basic:
		assert(t.basic != (Int_char | Int_unsigned) && t.basic != (Int_bool | Int_unsigned));
		return target->typesizes[t.basic & ~Int_unsigned];
	case Kind_Pointer:
	case Kind_FunctionPtr:
		assert(target->typesizes[target->ptrdiff.basic] == target->ptr_size);
		return target->ptr_size;
	case Kind_Function:
		unreachable;
	default: assert(!"TODO Calculate other type sizes.");
	}
}


u32 typeAlignment (Type t, const Target *target) {
	u32 size = typeSize(t, target);
	return size < 8 ? size : 8;
}

u32 addMemberOffset (u32 *offset, Type t, const Target *target) {
	u32 alignment = typeAlignment(t, target);
	u32 aligned = ((*offset + alignment - 1) / alignment) * alignment;
	*offset = aligned + typeSize(t, target);
	return aligned;
}


bool fnTypeCompatible (FunctionType a, FunctionType b) {
	if (!typeCompatible(*a.rettype, *b.rettype))
		return false;
	if (a.parameters.len != b.parameters.len)
		return false;

	for (u32 u = 0; u < a.parameters.len; u++)
		if (!typeCompatible(a.parameters.ptr[u].type, b.parameters.ptr[u].type))
			return false;
	return true;
}

bool typeCompatible (Type a, Type b) {
	if (a.kind != b.kind) {
		if (b.kind == Kind_Enum) {
			Type tmp = a;
			a = b;
			b = tmp;
		}
		return a.kind == Kind_Enum && b.kind == Kind_Basic && b.basic == Int_int;
	}

	switch (a.kind) {
	case Kind_Function:
	case Kind_FunctionPtr:
		return fnTypeCompatible(a.function, b.function);
	case Kind_Pointer:
		return typeCompatible(*a.pointer, *b.pointer);
	case Kind_Basic:
		return a.basic == b.basic;
	case Kind_Enum:
		// TODO
		return true;
	case Kind_Union:
	case Kind_Struct:
		return a.members.ptr == b.members.ptr;
	case Kind_Struct_Named:
	case Kind_Union_Named:
		return a.nametagged == b.nametagged;
	default:
		printf("could not compare types, assuming equal\n");
		return true;
	}
}


#define MAX_TYPE_STRING_LENGTH 1024

char *printDeclarator (Arena *arena, Type t, String name) {
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

static void printComplete (char **pos, const char *end, Type t) {
	printTypeBase(t, pos, end);
	switch (t.kind) {
	case Kind_Basic:
	case Kind_Void:
	case Kind_Struct:
	case Kind_Union:
		break;
	default:
		printto(pos, end, " ");
		printTypeHead(t, pos, end);
		printTypeTail(t, pos, end);
		break;
	}
}

char *printType (Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	printComplete(&pos, string + MAX_TYPE_STRING_LENGTH, t);
	return string;

}

char *printTypeHighlighted (Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	const char *end = string + MAX_TYPE_STRING_LENGTH;
	printto(&pos, end, BOLD);
	printComplete(&pos, end - strlen(RESET), t);
	printto(&pos, end, RESET);
	return string;
}

void printTypeBase(Type t, char **pos, const char *end) {
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
	case Kind_Basic: {
		BasicType basic = t.basic;
		if (basic & Int_unsigned) {
			printto(pos, end, "unsigned ");
			basic &= ~Int_unsigned;
		} else if (basic == Int_suchar) {
			printto(pos, end, "signed ");
		}
		switch (basic) {
		case Int_bool:
			printto(pos, end, "_Bool"); break;
		case Int_char:
		case Int_suchar:
			printto(pos, end, "char"); break;
		case Int_short:
			printto(pos, end, "short"); break;
		case Int_int:
			printto(pos, end, "int"); break;
		case Int_long:
			printto(pos, end, "long"); break;
		case Int_longlong:
			printto(pos, end, "long long"); break;
		default: assert(!"illegal type value");
		}
	} break;
	case Kind_Struct:
		printto(pos, end, "struct {???}");
		break;
	case Kind_Enum:
		printto(pos, end, "enum {???}");
		break;
	}
}

void printTypeHead (Type t, char **pos, const char *end) {

	switch (t.kind) {
	case Kind_Pointer:
		printTypeHead(*t.pointer, pos, end);
		if (t.pointer->kind == Kind_Array || t.pointer->kind == Kind_FunctionPtr)
			printto(pos, end, "(");
		printto(pos, end, "*");
		break;
	case Kind_FunctionPtr:
		t.kind = Kind_Function;
		printTypeHead(t, pos, end);
		printto(pos, end, "(*");
		break;
	case Kind_Array:
		printTypeHead(*t.array.inner, pos, end);
		break;
	case Kind_Function:
		printTypeHead(*t.function.rettype, pos, end);
		break;
	default: {}
	}

	if (t.qualifiers & Qualifier_Const)
		printto(pos, end, "const ");
	if (t.qualifiers & Qualifier_Atomic)
		printto(pos, end, "atomic ");
	if (t.qualifiers & Qualifier_Volatile)
		printto(pos, end, "volatile ");
	if (t.qualifiers & Qualifier_Restrict)
		printto(pos, end, "restrict ");
}

void printTypeTail (Type t, char **pos, const char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		if (t.pointer->kind == Kind_Array || t.pointer->kind == Kind_FunctionPtr)
			printto(pos, end, ")");
		printTypeTail(*t.pointer, pos, end);
		break;
	case Kind_FunctionPtr:
		printto(pos, end, ")");
		t.kind = Kind_Function;
		printTypeTail(t, pos, end);
		break;
	case Kind_Array:
		printto(pos, end, "[%llu]", (unsigned long long) t.array.count);
		printTypeTail(*t.array.inner, pos, end);
		break;
	case Kind_Function:
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


