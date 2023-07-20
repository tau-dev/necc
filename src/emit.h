
typedef struct {
	Arena *arena;
	Module module;
	const Target *target;
	String module_name;

	FILE *out;
	bool emit_debug_info;
	FileList files;
} EmitParams;

void emitX64AsmSimple(EmitParams params);
void emitIr(EmitParams params);

