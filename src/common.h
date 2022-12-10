
typedef struct Target {
	Type ptrdiff;
	Type intptr;
	PrimitiveSize ptr_size;
	PrimitiveSize int_size;
	PrimitiveSize typesizes[Int_unsigned];
	Version version;
} Target;


typedef struct {
	Target target;
	bool crash_on_error;
	bool gen_debug;
} Options;
