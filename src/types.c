#include "types.h"
#include "ansi.h"
#include "util.h"
#include "parse.h"
// #include <stdarg.h>

/*

Routines for analyzing and printing types.

*/


// Used in the phrasings "supported with $version" and "target uses $version".

const char *versionName(Version v) {
	switch (v) {
	case Version_C89:        return "C89";
	case Version_C99:        return "C99";
	case Version_C17:        return "C11";
	case Version_C23:        return "C23";
	case Version_GNU:        return "GNU";
	case Version_MSVC:       return "MSVC";
	case Version_Lax:        return "lax";
	default:                 return "???";
	}
}


Type resolveType (Type t) {
	return t.kind == Kind_Struct_Named || t.kind == Kind_Union_Named || t.kind == Kind_Enum_Named ?
		t.nametagged->type : t;
}

// Result of 0 means incomplete type.
u32 typeSize (Type t, const Target *target) {
	switch (t.kind) {
	case Kind_Enum_Named:
	case Kind_Union_Named:
	case Kind_Struct_Named: {
		Type actual = t.nametagged->type;
		if (actual.kind == Kind_Void)
			return 0; // Incomplete
		if (t.kind == Kind_Struct_Named)
			assert(actual.kind == Kind_Struct);
		else if (t.kind == Kind_Union_Named)
			assert(actual.kind == Kind_Union);
		else
			assert(actual.kind == Kind_Enum);
		return typeSize(actual, target);
	}
	case Kind_Union:
	case Kind_Struct: {
		u32 max_alignment = 1;
		u32 tightsize = 0;
		for (u32 i = 0; i < t.compound.members.len; i++) {
			CompoundMember member = t.compound.members.ptr[i];
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
	case Kind_Float:
		return floatSize(t.real);
	case Kind_Enum:
		return target->typesizes[Int_int];
	case Kind_Pointer:
	case Kind_FunctionPtr:
		assert(target->typesizes[target->ptrdiff.basic] == target->ptr_size);
		return target->ptr_size;
	case Kind_Function:
		unreachable;
	case Kind_Array:
		return t.array.count * typeSize(*t.array.inner, target);
	case Kind_UnsizedArray:
		return 0;
	case Kind_VLArray:
		unreachable;
	default: assert(!"TODO Calculate other type sizes.");
	}
}


u32 typeAlignment (Type t, const Target *target) {
	u32 size = typeSize(t, target);
	return size == 0 ? 1 :
			size < 8 ? size : 8;
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
	if (a.missing_prototype || b.missing_prototype)
		return true;
	if (a.parameters.len != b.parameters.len)
		return false;

	for (u32 u = 0; u < a.parameters.len; u++)
		if (!typeCompatible(a.parameters.ptr[u].type, b.parameters.ptr[u].type))
			return false;
	return true;
}

bool typeCompatible (Type a, Type b) {
	if (a.kind != b.kind) {
		// Clang and GCC behave like this, but I have not yet found the
		// relevant spec paragraph and there may be more nuance.
		if (isAnyArray(a) && isAnyArray(b)) {
			if (a.kind == Kind_Array && b.kind == Kind_Array)
				return a.array.count == b.array.count;
			return true;
		}
		if (b.kind == Kind_Enum || b.kind == Kind_Enum_Named) {
			Type tmp = a;
			a = b;
			b = tmp;
		}
		if (a.kind == Kind_Enum || a.kind == Kind_Enum_Named) {
			if (a.kind == Kind_Enum_Named) a = a.nametagged->type;
			return b.kind == Kind_Basic && b.basic == a.unnamed_enum.underlying;
		}
		return false;
	}

	switch (a.kind) {
	case Kind_Void:
		return true;
	case Kind_Function:
	case Kind_FunctionPtr:
		return fnTypeCompatible(a.function, b.function);
	case Kind_Pointer:
		return typeCompatible(*a.pointer, *b.pointer);
	case Kind_Basic:
		return a.basic == b.basic;
	case Kind_Float:
		return a.real == b.real;
	case Kind_Enum:
		return true;
	case Kind_Union:
	case Kind_Struct:
		return a.compound.members.ptr == b.compound.members.ptr;
	case Kind_Enum_Named:
	case Kind_Struct_Named:
	case Kind_Union_Named:
		return a.nametagged == b.nametagged;
	case Kind_Array:
		return a.array.count == b.array.count &&
				typeCompatible(*a.array.inner, *b.array.inner);
	default:
		unreachable;
	}
}


int rankDiff(BasicType a, BasicType b) {
	a &= ~Int_unsigned;
	b &= ~Int_unsigned;
	if ((a == Int_char && b == Int_suchar) || (a == Int_suchar && b == Int_char))
		return 0;
	else
		return a - b;
}


#define MAX_TYPE_STRING_LENGTH 1024

static void printComplete (char **pos, const char *end, Type t, String name) {
	printTypeBase(t, pos, end);
	switch (t.kind) {
	case Kind_Basic:
	case Kind_Float:
	case Kind_Void:
	case Kind_Struct:
	case Kind_Struct_Named:
	case Kind_Union:
	case Kind_Union_Named:
	case Kind_Enum:
	case Kind_Enum_Named:
		break;
	default:
		printto(pos, end, " ");
		printTypeHead(t, pos, end);
		printto(pos, end, "%.*s", STRING_PRINTAGE(name));
		printTypeTail(t, pos, end);
		break;
	}
}

char *printType (Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	printComplete(&pos, string + MAX_TYPE_STRING_LENGTH, t, STRING_EMPTY);
	return string;
}

char *printDeclarator (Arena *arena, Type t, String name) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	printComplete(&pos, string + MAX_TYPE_STRING_LENGTH, t, name);
	return string;
}


char *printTypeHighlighted (Arena *arena, Type t) {
	char *string = aalloc(arena, MAX_TYPE_STRING_LENGTH);
	char *pos = string;
	const char *end = string + MAX_TYPE_STRING_LENGTH;
	printto(&pos, end, BOLD);
	printComplete(&pos, end - strlen(RESET), t, STRING_EMPTY);
	printto(&pos, end, RESET);
	return string;
}

void printTypeUnnamed(char **pos, const char *end, SourceFile *source, unsigned long line, unsigned long column) {
	printto(pos, end, "[unnamed at %.*s line %lu column %lu]", STRING_PRINTAGE(source->name),
			(unsigned long) line, (unsigned long) column);
}

void printTypeBase(Type t, char **pos, const char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		printTypeBase(*t.pointer, pos, end); break;
	case Kind_Array:
	case Kind_VLArray:
	case Kind_UnsizedArray:
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
	case Kind_Float: {
		switch (t.real) {
		case Float_Single:
			printto(pos, end, "float"); break;
		case Float_Double:
			printto(pos, end, "double"); break;
		case Float_LongDouble:
			printto(pos, end, "long double"); break;
		default: assert(!"illegal type value");
		}
	} break;
	case Kind_Union:
		printto(pos, end, "union ");
		printTypeUnnamed(pos, end, t.compound.source, t.compound.line, t.compound.column);
		break;
	case Kind_Struct:
		printto(pos, end, "struct ");
		printTypeUnnamed(pos, end, t.compound.source, t.compound.line, t.compound.column);
		break;
	case Kind_Enum:
		printto(pos, end, "enum ");
		printTypeUnnamed(pos, end, t.unnamed_enum.source, t.unnamed_enum.line, t.unnamed_enum.column);
		break;
	case Kind_Struct_Named:
		printto(pos, end, "struct %.*s", STRING_PRINTAGE(t.nametagged->name));
		break;
	case Kind_Union_Named:
		printto(pos, end, "union %.*s", STRING_PRINTAGE(t.nametagged->name));
		break;
	case Kind_Enum_Named:
		printto(pos, end, "enum %.*s", STRING_PRINTAGE(t.nametagged->name));
		break;
	default:
		unreachable;
	}
}

void printTypeHead (Type t, char **pos, const char *end) {
	switch (t.kind) {
	case Kind_Pointer:
		printTypeHead(*t.pointer, pos, end);

		// TODO A Pointer should not actually ever point to a Function.
		if (t.pointer->kind == Kind_Array || t.pointer->kind == Kind_VLArray ||
			t.pointer->kind == Kind_FunctionPtr || t.pointer->kind == Kind_Function)
		{
			printto(pos, end, "(");
		}
		printto(pos, end, "*");
		break;
	case Kind_FunctionPtr:
		t.kind = Kind_Function;
		printTypeHead(t, pos, end);
#ifdef NDEBUG
		printto(pos, end, "(*");
#else
		printto(pos, end, "(fn*");
#endif
		break;
	case Kind_Array:
	case Kind_UnsizedArray:
	case Kind_VLArray:
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
		if (t.pointer->kind == Kind_Array || t.pointer->kind == Kind_VLArray ||
			t.pointer->kind == Kind_FunctionPtr || t.pointer->kind == Kind_Function)
		{
			printto(pos, end, ")");
		}
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
	case Kind_VLArray:
		printto(pos, end, "[val_%llu]", (unsigned long long) t.array.count);
		printTypeTail(*t.array.inner, pos, end);
		break;
	case Kind_UnsizedArray:
		printto(pos, end, "[]");
		printTypeTail(*t.array.inner, pos, end);
		break;
	case Kind_Function:
		printto(pos, end, "(");
		for (size_t i = 0; i < t.function.parameters.len; i++) {
			Declaration param = t.function.parameters.ptr[i];

			printComplete(pos, end, param.type, param.name ? param.name->name : STRING_EMPTY);

			if (i + 1 < t.function.parameters.len)
				printto(pos, end, ", ");
		}
		printto(pos, end, ")");
		printTypeTail(*t.function.rettype, pos, end);
		break;
	default: {}
	}
}


