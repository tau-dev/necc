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
	Kind_Struct,
	Kind_Union,
	Kind_Function, // Only used for function declarations.
	Kind_FunctionPtr,
	Kind_Pointer,
	Kind_Enum,
	Kind_Array,
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
} FunctionType;

typedef struct {
	Type *inner;
	IrRef count;
} ArrayType;

typedef struct StructMember StructMember;

typedef SPAN(StructMember) StructType;

typedef struct Type {
	u8 kind;
	u8 qualifiers;

	union {
		BasicType basic;
		FunctionType function;
		Type *pointer;
		StructType structure;
		ArrayType array;
		// TODO union, enum, ??
	};
} Type;

struct Function {
	String name;
	FunctionType type;
	Block *entry;
	IrList ir;
	StringMap labels; // Maps labels to Blocks
};

typedef struct StructMember {
	Type type;
	String name;
	u32 offset;
} StructMember;

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

extern const char *version_names[];

typedef struct {
	Type ptrdiff;
	Type intptr;
	Size ptr_size;
	Size int_size;
	int typesizes[Int_unsigned];
	Version version;
} Target;

bool typeEqual(Type a, Type b);
bool fnTypeEqual(FunctionType a, FunctionType b);
Size typeSize(Type t, const Target *target);
u32 typeSizeBytes(Type t, const Target *target);
u32 typeAlignment(Type t, const Target *target);
u32 addMemberOffset(u32 *offset, Type t, const Target *target);
char *printDeclaration(Arena *a, Type t, String name);
char *printType(Arena *a, Type t);
char *printTypeHighlighted(Arena *a, Type t);
void printTypeBase(Type t, char **insert, const char *end);
void printTypeHead(Type t, char **insert, const char *end);
void printTypeTail(Type t, char **insert, const char *end);

#define BASIC_VOID ((Type) {Kind_Void})
#define BASIC_INT ((Type) {Kind_Basic, .basic = Int_int})
#define BASIC_CHAR ((Type) {Kind_Basic, .basic = Int_char})
#define INT_SIZE I32


