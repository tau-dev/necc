#pragma once

#include "ir.h"

typedef struct Type Type;
typedef struct Declaration Declaration;

typedef LIST(Declaration) DeclList;

typedef enum {
	Int_bool,
	Int_char,
	Int_suchar, // signed or unsigned (plain char is neither!)
	Int_short,
	Int_int,
	Int_long,
	Int_longlong,
	Int_unsigned = 8,
} BasicType;

typedef enum {
	Kind_Void,
	Kind_Basic,
	Kind_Struct_Named,
	Kind_Struct,
	Kind_Union_Named,
	Kind_Union,
	Kind_Pointer,
	Kind_Enum,
	Kind_Array,
	Kind_VLArray,
	// Decays to Kind_FunctionPtr by rvalue() conversion. Handled
	// specially (for error messages) in lvalue usage.
	Kind_Function,
	// Same representation as a Function, avoiding a heap allocation
	// over a Pointer to Function, as Functions almost always decay.
	Kind_FunctionPtr,
} TypeKind;

enum {
	Storage_Unspecified,
	Storage_Auto,
	Storage_Register,
	Storage_Static,
	Storage_Static_Threadlocal,
	Storage_Extern,
	Storage_Extern_Threadlocal,
	Storage_Typedef,
};
enum {
	Qualifier_Const = 1,
	Qualifier_Volatile = 2,
	Qualifier_Restrict = 4,
	Qualifier_Atomic = 8,
};

typedef struct {
	Type *rettype;
	DeclList parameters;
	bool is_vararg;
} FunctionType;

typedef struct {
	Type *inner;
	u32 count; // IrRef for VLA (IR_REF_NONE for unspecified size), number otherwise.
} ArrayType;


// TODO struct and union also have a name tag and storage duration!
typedef struct CompoundMember CompoundMember;

typedef SPAN(CompoundMember) Members;

typedef struct NameTaggedType NameTaggedType;
typedef struct Type {
	u8 kind;
	u8 qualifiers;

	union {
		BasicType basic;
		FunctionType function;
		Type *pointer;
		ArrayType array;
		NameTaggedType *nametagged;

		// TODO Pre-calculate size, do not store these inline.
		Members members;
	};
} Type;

struct Function {
	FunctionType type;
	Block *entry;
	IrList ir;
};

typedef struct CompoundMember {
	Type type;
	String name;
	u32 offset;
} CompoundMember;

typedef struct Declaration {
	Type type;
	String name;
} Declaration;

typedef enum {
	Version_C89,
	Version_C99,
	Version_C17,
	Version_C23,
	Version_GNU,
	Version_MSVC,
	Version_Lax,
} Version;

typedef enum {
	Attribute_Deprecated = 1,
	Attribute_Fallthrough = 1 << 1,
	Attribute_MaybeUnused = 1 << 2,
	Attribute_Noreturn = 1 << 3,
	Attribute_Nodiscard = 1 << 4,
	Attribute_Unsequenced = 1 << 5,
	Attribute_Reproducible = 1 << 6,
} Attributes;
typedef struct Target Target;
extern const char *version_names[];

Type resolveType(Type t);
bool typeCompatible(Type a, Type b);
bool fnTypeEqual(FunctionType a, FunctionType b);
u32 typeSize(Type t, const Target *target);
u32 typeAlignment(Type t, const Target *target);
u32 addMemberOffset(u32 *offset, Type t, const Target *target);
bool isFlexibleArrayMember(Members m, u32 index);
int rankDiff(BasicType a, BasicType b);

char *printDeclarator(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
char *printTypeHighlighted(Arena *a, Type t);
void printTypeBase(Type t, char **insert, const char *end);
void printTypeHead(Type t, char **insert, const char *end);
void printTypeTail(Type t, char **insert, const char *end);



#define BASIC_VOID ((Type) {Kind_Void})
#define BASIC_INT ((Type) {Kind_Basic, .basic = Int_int})
// #define BASIC_CHAR ((Type) {Kind_Basic, .basic = Int_char})
#define INT_SIZE I32


