
typedef struct {
	Arena *arena;
	Module module;
	const Target *target;

	FILE *out;
	FILE *debug_out;
	FileList files;
} EmitParams;

void emitX64AsmSimple(EmitParams params);
void emitIr(EmitParams params);

