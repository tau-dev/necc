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

typedef enum Signedness {
	Unsigned,
	Signed,
} Signedness;


typedef enum {
	Float_Single,
	Float_Double,
	Float_LongDouble,
} FloatType;

typedef enum TypeKind {
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
	// Variably modified, possibly stack-allocated.
	Kind_VLArray,
	// Only used for declarations before initialization or, for
	// parameters, decay to pointer.
	Kind_UnsizedArray,
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
	// This is a qualifier grammatically but not semantically. Should probably not be here.
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


typedef struct CompoundMember CompoundMember;
typedef SPAN(CompoundMember) Members;

typedef struct UnnamedCompound {
	Members members;
	u32 line;
	u32 column;
	u32 alignment;
	u32 size;
	SourceFile *source;
} UnnamedCompound;

typedef struct UnnamedEnum {
	SPAN(Symbol*) members;
	BasicType underlying;
	u32 line;
	u32 column;
	SourceFile *source;
} UnnamedEnum;

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
		UnnamedCompound compound;
		UnnamedEnum unnamed_enum;
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

typedef struct Target Target;

Type resolveType(Type t);
bool typeCompatible(Type a, Type b);
bool fnTypeEqual(FunctionType a, FunctionType b);
u32 typeSize(Type t, const Target *target);
bool isIncomplete(Type t);
u32 typeAlignment(Type t, const Target *target);
u32 addMemberOffset(bool is_struct, u32 *current_size, u32 *alignment, Type t, const Target *target);
int rankDiff(BasicType a, BasicType b);

char *printDeclarator(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
char *printTypeHighlighted(Arena *a, Type t);
void printTypeBase(Type t, char **insert, const char *end);
void printTypeHead(Type t, char **insert, const char *end);
void printTypeTail(Type t, char **insert, const char *end);



#define BASIC_VOID ((Type) {Kind_Void})
#define BASIC_INT ((Type) {Kind_Basic, .basic = Int_int})
#define BASIC_LONG ((Type) {Kind_Basic, .basic = Int_long})
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
static inline bool isAnyArray (Type t) {
	return t.kind == Kind_Array || t.kind == Kind_VLArray || t.kind == Kind_UnsizedArray;
}
static inline Signedness typeSign (Type t) {
	if (t.kind == Kind_Pointer)
		return Unsigned;
	assert(t.kind == Kind_Basic);
	return t.basic & Int_unsigned ? Unsigned : Signed;
}

static inline u16 floatSize (FloatType f) {
	switch (f) {
	case Float_Single: return 4;
	case Float_Double: return 8;
	case Float_LongDouble: return 10;
	}
	unreachable;
}
