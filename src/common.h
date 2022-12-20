
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

	bool warn_on_wrapping;
	bool warn_char_subscript;
	bool warn_compare;

	bool emitted_warnings;
	bool error_on_warnings;
} Options;
