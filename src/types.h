#pragma once

#include "arena.h"

typedef struct Type Type;
typedef struct Declaration Declaration;
typedef struct Symbol Symbol;

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
	Float_Single,
	Float_Double,
	Float_LongDouble,
} FloatType;

typedef enum {
	Kind_Void,
	Kind_Basic,
	Kind_Float,
	Kind_Struct_Named,
	Kind_Struct,
	Kind_Union_Named,
	Kind_Union,
	Kind_Enum_Named,
	Kind_Enum,
	Kind_Pointer,
	Kind_Array,
	Kind_VLArray,
	// Decays to Kind_FunctionPtr by rvalue() conversion. Handled
	// specially (for error messages) in lvalue usage.
	Kind_Function,
	// Same representation as a Function, avoiding a heap allocation
	// over a Pointer to Function, as Functions almost always decay.
	Kind_FunctionPtr,
} TypeKind;


// Syntactically "storage-class specifiers".
enum {
	Storage_Unspecified,
	Storage_Auto,
	Storage_Register,
	Storage_Constexpr,
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


// TODO The only reason for inner Types not to be const* is that the
// declarator parser needs to do some crufty fix-ups. Make that better!

typedef struct {
	Type *rettype;
	DeclList parameters;
	bool is_vararg;
	bool missing_prototype;
} FunctionType;

typedef struct {
	Type *inner;
	u32 count; // IrRef for VLA (IDX_NONE for unkown size), number otherwise.
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
		FloatType real;
		FunctionType function;
		Type *pointer;
		ArrayType array;
		NameTaggedType *nametagged;

		// TODO Pre-calculate size, do not store these inline.
		Members members;
	};
} Type;


typedef struct CompoundMember {
	Type type;
	Symbol *name;
	u32 offset;
} CompoundMember;

typedef struct Declaration {
	Type type;
	Symbol *name;
} Declaration;

typedef enum {
// 	Features_C89 = 0x1,
	Features_C99 = 0x2,
	Features_C11 = 0x4,
	Features_C23 = 0x8,
	Features_GNU_Extensions = 0x10,
	Features_MSVC_Extensions = 0x20,

	Features_OldStyleDefinitions = 0x40,
	Features_DefaultInt = 0x80,
} Features;

typedef enum {
	Version_C89       = Features_OldStyleDefinitions | Features_DefaultInt,
	Version_C99       = Features_C99 | Features_OldStyleDefinitions,
	Version_C17       = Features_C99 | Features_C11 | Features_OldStyleDefinitions,
	Version_C23       = Features_C99 | Features_C11 | Features_C23,
	Version_GNU       = Version_C17 | Features_GNU_Extensions | Features_DefaultInt,
	Version_MSVC      = Version_C17 | Features_MSVC_Extensions,
	Version_Lax       = 0x7fff,
} Version;
extern const char *versionName(Version);

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

static inline bool isSignedOrUnsignedIntegerType (Type t) {
	return t.kind == Kind_Basic && t.basic != Int_char;
}
static inline bool isIntegerType (Type t) {
	return t.kind == Kind_Enum || t.kind == Kind_Enum_Named || t.kind == Kind_Basic;
}
static inline bool isArithmeticType (Type t) {
	return t.kind == Kind_Float || isIntegerType(t);
}
static inline bool isScalar (Type t) {
	return t.kind == Kind_Pointer || t.kind == Kind_Pointer || isArithmeticType(t);
}
static inline bool isUnknownSizeArray (Type t) {
	return t.kind == Kind_VLArray && t.array.count == IDX_NONE;
}

static inline u16 floatSize (FloatType f) {
	switch (f) {
	case Float_Single: return 4;
	case Float_Double: return 8;
	case Float_LongDouble: return 10;
	}
	unreachable;
}
