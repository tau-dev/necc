
typedef struct {
	Arena *arena;
	Module module;
	const Target *target;
	String module_name;

	FILE *out;
	bool emit_debug_info;
	FileList files;
} EmitParams;

typedef enum {
	Param_MEM,
	Param_NONE = Param_MEM,
	Param_INTEGER,
	Param_SSE,
	// TODO There are more classes, such as SSEUP or X87.
} ParameterEightbyteClass;

typedef struct {
	// If 0, this is passed in memory.
	u8 count;
	// Number of eightbytes that are passed in the xmm registers; the remaining count-sse_count are integers.
	u8 sse_count;
	u8 registers[8];
} ParameterClass;

void emitX64AsmSimple(EmitParams params);
ParameterClass classifyParam(const Target *target, Type type);
void emitIr(EmitParams params);

