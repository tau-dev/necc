#include "analysis.h"
#include "types.h"
#include "parse.h"
#include "emit.h"


/*

Generates assembly in the flat assembler format.
C identifiers are prefixed with an underscore to disambiguate from
instructions & registers.

NOTE A StackAlloc of Constant size may be referenced across blocks,
to allow jumps over definitions. Probably need to allocate these in a
prepass.


*/



#define BUF 100000
// Maximum amount of data expected to be emitted by a single call to emit().
#define MAX_LINE 1000
static char buf[BUF];
static char *insert = buf;


typedef unsigned long ulong;
typedef unsigned long long ullong;

typedef u32 Storage;



#define STORAGE_SIZE_OFFSET 4
typedef enum {
	RCX = 0x000,
	RBX = 0x001,
	RSI = 0x002,
	RDI = 0x003,
	RBP = 0x004,
	R8  = 0x005,
	R9  = 0x006,
	R10 = 0x007,
	R11 = 0x008,
	R12 = 0x009,
	R13 = 0x00a,
	R14 = 0x00b,
	R15 = 0x00c,
// rsp is reserved as stack pointer, rax and rdx are used for intermediate values
	RDX = 0x00d,
	GENERAL_PURPOSE_REGS_END = RDX,
	RSP = 0x00e,
	RAX = 0x00f,

	RSIZE_BYTE  = 0x0 << STORAGE_SIZE_OFFSET,
	RSIZE_WORD  = 0x1 << STORAGE_SIZE_OFFSET,
	RSIZE_DWORD = 0x2 << STORAGE_SIZE_OFFSET,
	RSIZE_QWORD = 0x3 << STORAGE_SIZE_OFFSET,

	RSIZE_MASK = 0x3 << STORAGE_SIZE_OFFSET,

	STACK_BEGIN = 0x4 << STORAGE_SIZE_OFFSET,
} Register;


typedef struct {
	u32 storage;
	ParameterClass class;
	u8 registers[8];
} ParamInfo;

typedef LIST(Location) Locations;

typedef struct {
	u32 id;
	u32 inst;
} DebugMark;
// STYLE EEEEEW.
static LIST(DebugMark) line_marks;

typedef struct {
	Arena *arena;
	FILE *out;
	FILE *debug_out;
	u32 current_id;
	IrList ir;
	Module module;
	FileList files;
	bool emit_debug_info;

	StringMap *types;
	const Target *target;

	u16 *usage;
	Storage *storage;
	u32 stack_allocated;

	ParameterClass ret_class;
	bool is_vararg;

	u32 vaarg_gp_offset;
	u32 vaarg_fp_offset;
	u32 vaarg_overflow_args;
	u32 vaarg_reg_saves;
	SPAN(ParamInfo) param_info;
} Codegen;


#define CALLER_SAVED_COUNT (sizeof(caller_saved) / sizeof(caller_saved[0]))

const char *register_names[STACK_BEGIN] = {
	[RAX] = "al",
	[RCX] = "cl",
	[RDX] = "dl",
	[RBX] = "bl ",
	[RSI] = "sil",
	[RSP] = "spl",
	[RDI] = "dil",
	[RBP] = "bpl",
	[R8]  = "r8b",
	[R9]  = "r9b",
	[R10] = "r10b",
	[R11] = "r11b",
	[R12] = "r12b",
	[R13] = "r13b",
	[R14] = "r14b",
	[R15] = "r15b",

	[RAX + RSIZE_WORD] = "ax",
	[RCX + RSIZE_WORD] = "cx",
	[RDX + RSIZE_WORD] = "dx",
	[RBX + RSIZE_WORD] = "bx",
	[RSI + RSIZE_WORD] = "si",
	[RSP + RSIZE_WORD] = "sp",
	[RDI + RSIZE_WORD] = "di",
	[RBP + RSIZE_WORD] = "bp",
	[R8 + RSIZE_WORD]  ="r8w",
	[R9 + RSIZE_WORD]  ="r9w",
	[R10 + RSIZE_WORD] = "r10w",
	[R11 + RSIZE_WORD] = "r11w",
	[R12 + RSIZE_WORD] = "r12w",
	[R13 + RSIZE_WORD] = "r13w",
	[R14 + RSIZE_WORD] = "r14w",
	[R15 + RSIZE_WORD] = "r15w",

	[RAX + RSIZE_DWORD] = "eax",
	[RCX + RSIZE_DWORD] = "ecx",
	[RDX + RSIZE_DWORD] = "edx",
	[RBX + RSIZE_DWORD] = "ebx",
	[RSI + RSIZE_DWORD] = "esi",
	[RSP + RSIZE_DWORD] = "esp",
	[RDI + RSIZE_DWORD] = "edi",
	[RBP + RSIZE_DWORD] = "ebp",
	[R8 + RSIZE_DWORD]  ="r8d",
	[R9 + RSIZE_DWORD]  ="r9d",
	[R10 + RSIZE_DWORD] = "r10d",
	[R11 + RSIZE_DWORD] = "r11d",
	[R12 + RSIZE_DWORD] = "r12d",
	[R13 + RSIZE_DWORD] = "r13d",
	[R14 + RSIZE_DWORD] = "r14d",
	[R15 + RSIZE_DWORD] = "r15d",

	[RAX + RSIZE_QWORD] = "rax",
	[RCX + RSIZE_QWORD] = "rcx",
	[RDX + RSIZE_QWORD] = "rdx",
	[RBX + RSIZE_QWORD] = "rbx",
	[RSI + RSIZE_QWORD] = "rsi",
	[RSP + RSIZE_QWORD] = "rsp",
	[RDI + RSIZE_QWORD] = "rdi",
	[RBP + RSIZE_QWORD] = "rbp",
	[R8  + RSIZE_QWORD] = "r8",
	[R9  + RSIZE_QWORD] = "r9",
	[R10 + RSIZE_QWORD] = "r10",
	[R11 + RSIZE_QWORD] = "r11",
	[R12 + RSIZE_QWORD] = "r12",
	[R13 + RSIZE_QWORD] = "r13",
	[R14 + RSIZE_QWORD] = "r14",
	[R15 + RSIZE_QWORD] = "r15",
};

enum {
	RAX_4 = RAX + RSIZE_DWORD,
	RCX_4 = RCX + RSIZE_DWORD,
	RDX_4 = RDX + RSIZE_DWORD,
	RBX_4 = RBX + RSIZE_DWORD,
	RSI_4 = RSI + RSIZE_DWORD,
	RSP_4 = RSP + RSIZE_DWORD,
	RDI_4 = RDI + RSIZE_DWORD,
	RBP_4 = RBP + RSIZE_DWORD,
	R8_4 = R8 + RSIZE_DWORD,
	R9_4 = R9 + RSIZE_DWORD,
	R10_4 = R10 + RSIZE_DWORD,
	R11_4 = R11 + RSIZE_DWORD,
	R12_4 = R12 + RSIZE_DWORD,
	R13_4 = R13 + RSIZE_DWORD,
	R14_4 = R14 + RSIZE_DWORD,
	R15_4 = R15 + RSIZE_DWORD,

	RAX_8 = RAX + RSIZE_QWORD,
	RCX_8 = RCX + RSIZE_QWORD,
	RDX_8 = RDX + RSIZE_QWORD,
	RBX_8 = RBX + RSIZE_QWORD,
	RSI_8 = RSI + RSIZE_QWORD,
	RSP_8 = RSP + RSIZE_QWORD,
	RDI_8 = RDI + RSIZE_QWORD,
	RBP_8 = RBP + RSIZE_QWORD,
	R8_8  = R8  + RSIZE_QWORD,
	R9_8  = R9  + RSIZE_QWORD,
	R10_8 = R10 + RSIZE_QWORD,
	R11_8 = R11 + RSIZE_QWORD,
	R12_8 = R12 + RSIZE_QWORD,
	R13_8 = R13 + RSIZE_QWORD,
	R14_8 = R14 + RSIZE_QWORD,
	R15_8 = R15 + RSIZE_QWORD,
};


// Registers rbx, r12 - r15 are callee-save, rsp and rbp are special.
// On Windows, rsi and rdi and the lower 128 bits of XMM6-XMM15 are callee-save too!
const Register caller_saved[] = {
	RAX,
	RCX,
	RDX,
	RSI,
	RDI,
	R8,
	R9,
	R10,
	R11,
};

const int gp_parameter_regs[] = {
	RDI_8,
	RSI_8,
	RDX_8,
	RCX_8,
	R8_8,
	R9_8,
};
static const u32 gp_parameter_regs_count = sizeof(gp_parameter_regs) / sizeof(gp_parameter_regs[0]);
const u32 reg_save_area_size = 48 + 128;

static const char* debug_prelude =
	".set DW_TAG_compile_unit, 0x11\n"
	".set DW_TAG_subprogram, 0x2e\n"
	"\n"
	".set DW_AT_name, 0x3\n"
	".set DW_AT_comp_dir, 0x1b\n"
	".set DW_AT_language, 0x13\n"
	".set DW_AT_stmt_list, 0x10\n"
	".set DW_AT_decl_file, 0x3a\n"
	".set DW_AT_decl_line, 0x3b\n"
	".set DW_AT_decl_column,0x39\n"
	".set DW_AT_external,   0x3f\n"
	".set DW_AT_frame_base, 0x40\n"
	"\n"
	".set DW_AT_low_pc, 0x11\n"
	".set DW_AT_high_pc, 0x12\n"
	"\n"
	"\n"
	// The values for an attribute are constrained to one or more classes.
	// Forms are the particular encodings of the values.
	"\n"
	"# class address\n"
	".set DW_FORM_addr, 0x1\n"
	"\n"
	"# class constant\n"
	".set DW_FORM_data1, 0xb\n"
	".set DW_FORM_data2, 0x5\n"
	".set DW_FORM_data4, 0x6\n"
	".set DW_FORM_data8, 0x7\n"
	".set DW_FORM_sdata, 0xd # leb128\n"
	".set DW_FORM_udata, 0xf # leb128\n"
	"\n"
	"# class lineptr, rangelistptr or loclistptr\n"
	".set DW_FORM_sec_offset, 0x17 # offset into the appropriate section, 32/64 bits\n"
	"\n"
	"# class reference\n"
	".set DW_FORM_ref_addr, 0x10 # offset into .debug_info, section, 32/64 bits\n"
	"\n"
	"# class string\n"
	".set DW_FORM_string, 0x8 # null-terminated byte sequence\n"
	"\n"
	"# class flag\n"
	".set DW_FORM_flag, 0xc # single byte\n"
	"\n"
	"\n"
	".set DW_LANG_C99, 0x0c\n"
	".set DW_LANG_C11, 0x1d\n"
	"\n"
	"# TODO Would uleb also take multiple arguments?\n"
	".macro leb2_128 a,b\n"
	"	.uleb128 \\a\n"
	"	.uleb128 \\b\n"
	".endm\n"
	"\n"
	".section .debug_abbrev\n"
	".abbrevs:\n"
	"	.uleb128 1\n"
	"	.uleb128 DW_TAG_compile_unit\n"
	"	.byte 1 # has children\n"
	"# 	leb2_128 DW_AT_producer,DW_FORM_string\n"
	"	leb2_128 DW_AT_name,DW_FORM_string\n"
// 	"	leb2_128 DW_AT_comp_dir,DW_FORM_string\n"
	"	leb2_128 DW_AT_language,DW_FORM_data1\n"
	"	leb2_128 DW_AT_stmt_list,DW_FORM_sec_offset\n"
	"	leb2_128 DW_AT_low_pc,DW_FORM_addr\n"
	"	leb2_128 DW_AT_high_pc,DW_FORM_addr\n"
	"	leb2_128 0,0\n"
	"\n"
	"	.uleb128 2\n"
	"	.uleb128 DW_TAG_subprogram\n"
	"	.byte 0 # has no children\n"
	"	leb2_128 DW_AT_external,DW_FORM_flag\n"
	"	leb2_128 DW_AT_name,DW_FORM_string\n"
	"	leb2_128 DW_AT_decl_file,DW_FORM_data2\n"
	"	leb2_128 DW_AT_decl_line,DW_FORM_data2\n"
	"	leb2_128 DW_AT_decl_column,DW_FORM_data2\n"
	"	leb2_128 DW_AT_low_pc,DW_FORM_addr\n"
	"	leb2_128 DW_AT_high_pc,DW_FORM_addr\n"
	"	# leb2_128 DW_AT_frame_base,??\n"
	"	leb2_128 0,0\n"
	"\n"
	"	.uleb128 0\n"
	"\n"
	"\n"
	".set DW_LNS_copy, 1\n"
	".set DW_LNS_advance_pc, 2\n"
	".set DW_LNS_advance_line, 3\n"
	".set DW_LNS_set_file, 4\n"
	".set DW_LNE_end_sequence, 1\n"
	".set DW_LNE_set_address, 2\n"
	"\n"
	".set last_line, 1\n"
	".set last_pc, .exec_base\n"
	"\n"
	".macro line_inst line,pc\n"
	"	.byte DW_LNS_advance_line\n"
	"	.uleb128 \\line - last_line\n"
	"	.byte DW_LNS_advance_pc\n"
	"	.uleb128 \\pc - last_pc\n"
	"	.byte DW_LNS_copy\n"
	"\n"
	"	.set last_line, \\line\n"
	"	.set last_pc, \\pc\n"
	".endm\n"
	;

static void emitData(Codegen *, Module, String data, References);
static void emitDataLine(FILE *, u32 len, const char *data);
static void emitDataString(u32 len, const char *data);
static void emitDataRaw(u32 len, const char *data);
static void emitName(Module module, u32 id);
static void emitJump(Codegen *, const char *inst, const Block *dest);
static void emitFunctionForward(EmitParams, u32);
static void emitBlockForward(Codegen *, Blocks, u32);
static inline bool isNewLine(Codegen *c, u32 i);
static inline bool isNewFile(Codegen *c, u32 i);
static void emitInstForward(Codegen *c, IrRef i);
static inline u16 sizeOfRegister(Register size);
static inline Storage registerSize(u16 size);
static inline Register registerSized(Register, u16);
static const char *sizeOp(u16 size);
static const char *sizeSuffix(u16 size);
static const char *sizeFSuffix(u16 size);
static u16 valueSize(Codegen *, IrRef);
static void emit(Codegen *c, const char *fmt, ...);
static void emitString(String s);
static void emitZString(const char *);
static void emitInt(u64 i);
static void flushit(FILE *f);

int splice_dest_order (const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

static void emitLabel (Module module, u32 i) {
	emitZString(".align 8\n");
	emitName(module, i);
	emitZString(":\n");
}

void emitX64AsmSimple(EmitParams params) {
	Codegen globals = {
		.out = params.out,
		.arena = params.arena,
		.emit_debug_info = params.emit_debug_info,
		.target = params.target,
	};
	Module module = params.module;

	fprintf(params.out, ".intel_syntax\n\n");

	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.name.len == 0) {
			assert(reloc.def_state != Def_Undefined);
			assert(!reloc.is_public);
			continue;
		}
		if (reloc.def_state == Def_Undefined)
			; //fprintf(out, "extrn '%.*s' as _%.*s	; %lu\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name), (ulong) i);
		else if (reloc.is_public) {
			emitZString("\n.global ");
			emitName(module, i);

			*insert++ = '\n';

			if (insert > buf + (BUF - MAX_LINE))
				flushit(globals.out);
		}
	}

	emit(&globals, "\n\n");

	emit(&globals, ".text\n"
		".exec_base:");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state != Def_Undefined) {
			emitLabel(module, i);
			assert(reloc.type.kind == Kind_Function);
			emitFunctionForward(params, i);
			emit(&globals, ".S_end:\n", reloc.name);
		}
	}
	emit(&globals, ".exec_end:");

	emit(&globals, "\n\n"
		".data");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state != Def_Undefined)
		{
			emitLabel(module, i);
			emitData(&globals, module, reloc.value_data, reloc.value_references);
		}
	}

	emit(&globals, "\n\n"
		".section .rodata\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state != Def_Undefined)
		{
			emitLabel(module, i);
			emitData(&globals, module, reloc.value_data, reloc.value_references);
		}
	}
	(void)emitData;



	if (params.emit_debug_info) {
		emitZString(debug_prelude);
		emitZString("\n\n"
			".section .debug_info\n"
			"	.int .info_end - .info_start # unit_length\n"
			".info_start:\n"
			"	.short 4 # version\n"
			"	.int offset .abbrevs # debug_abbrev_offset\n"
			"	.byte 8 # address_size\n"
			"\n");
		flushit(params.out);
		emit(&globals,
			"	/* compilation unit: */\n"
			"	.uleb128 1\n"
			"	.string \"S\"",
			params.module_name);
		emitZString(
			"	.byte DW_LANG_C99\n"
			"	.int offset .lines\n"
			"	.quad .exec_base\n"
			"	.quad .exec_end\n");

		for (u32 i = 0; i < module.len; i++) {
			StaticValue reloc = module.ptr[i];
			if (reloc.def_kind == Static_Function && reloc.def_state != Def_Undefined) {
				String name = reloc.name;
				Location reloc_loc = reloc.function_ir.locations[0];
				emit(&globals,
					"	.uleb128 2\n"
					"	.byte I\n"
					"	.string \"S\"\n"
					"	.short I\n"
					"	.short I\n"
					"	.short I\n"
					"	.quad S\n"
					"	.quad .S_end\n",
					reloc.is_public, name,
					reloc_loc.file_id, reloc_loc.line, reloc_loc.column,
					name, name);

			}
		}

		emitZString(
			"	.uleb128 0\n"
			".info_end:\n"
			"\n"
			"\n"
			".section .debug_line\n"
			".lines:\n"
			"	.int .line_end - .line_start # unit_length\n"
			".line_start:\n"
			"	.short 4 # version\n"
			"	.int .line_data - .line_header\n"
			".line_header:\n"
			"	.byte 1 # minimum_instruction_length\n"
			"	.byte 1 # maximum_operations_per_instruction\n"
			"	.byte 1 # default_is_stmt\n"
			"	.byte 1 # line_base, for computing special ops.\n"
			"	.byte 1 # line_range, for computing special ops.\n"
			"	.byte 13 # opcode_base\n"
			"	.byte 0,1,1,1,1,0,0,0,1,0,0,1 # standard_opcode_lengths\n"
			"	# include_directories\n"
			"	.byte 0\n" // TODO
			"	# file_names\n");
		for (u32 i = 1; i < params.files.len; i++) {
			SourceFile *file = params.files.ptr[i];
			emit(&globals,
				"	.string \"SS\"\n"
				"	.uleb128 0\n"
				"	.uleb128 0\n"
				"	.uleb128 0\n",
				file->path, file->name);
		}
		emitZString(
			"	.byte 0\n"
			"\n"
			".line_data:\n"
			"	.byte 0,9 # extended opcode over 9 bytes\n"
			"	.byte DW_LNE_set_address\n"
			"	.quad .exec_base\n"
			);

		Location prev = {0};
		for (u32 i = 0; i < line_marks.len; i++) {
			DebugMark m = line_marks.ptr[i];
			Location loc = module.ptr[m.id].function_ir.locations[m.inst];

			if (loc.file_id != prev.file_id) {
				emitZString("	.byte DW_LNS_set_file\n");
				emit(&globals,
					"	.uleb128 I", loc.file_id);
			}
			if (loc.line != prev.line || loc.file_id != prev.file_id) {
				if (i == 0 || m.id != line_marks.ptr[i-1].id) {
					emit(&globals, "	line_inst I, S", loc.line, module.ptr[m.id].name);
				}
				emit(&globals, "	line_inst I, ..IiI", loc.line, m.id, m.inst);
			}
			prev = loc;
		}
		emitZString(
				"\n"
				"	.byte 0,9 # extended op\n"
				"	.byte DW_LNE_set_address\n"
				"	.quad .exec_end\n"
				"\n"
				"	.byte DW_LNS_advance_line\n"
				"	.uleb128 1\n"
				"	.byte 0,1 # extended op\n"
				"	.byte DW_LNE_end_sequence\n"
				"\n"
				"\n"
				".line_end:\n");
	}

	flushit(params.out);
}


static void emitName (Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	if (reloc.name.len) {
		if (reloc.parent_decl != IDX_NONE) {
			emitName(module, reloc.parent_decl);
			*insert++ = '.';
		}
		emitString(reloc.name);
	} else {
		emitZString("__");
		emitInt(id);
	}
}

static void emitData (Codegen *c, Module module, String data, References references) {
	if (references.len) {
		qsort(references.ptr, references.len, sizeof(references.ptr[0]),
			splice_dest_order);
		assert(!(references.len > 1 && references.ptr[0].splice_pos > references.ptr[1].splice_pos));
	}

	u32 pos = 0;
	for (u32 i = 0; i < references.len; i++) {
		Reference ref = references.ptr[i];
		assert(ref.splice_pos >= pos);
		emitDataLine(c->out, ref.splice_pos - pos, data.ptr + pos);

		StaticValue reloc = module.ptr[ref.source_id];
		if (reloc.name.len)
			emit(c, " .quad S~L", reloc.name, ref.offset);
		else
			emit(c, " .quad __I~L", ref.source_id, ref.offset);
		pos = ref.splice_pos + 8;
	}

	emitDataLine(c->out, data.len - pos, data.ptr + pos);
}



static inline bool isAlpha (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
static inline bool isDigit (char c) {
	return c >= '0' && c <= '9';
}
static inline bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}


static u32 roundUp(u32 x)  {
	return ((x + 7) / 8) * 8;
}

static void emitFunctionForward (EmitParams params, u32 id) {
	// Arena *arena, FILE *out, Module module, u32 id, const Target *target
	Module module = params.module;

	StaticValue *reloc = &module.ptr[id];
	Block *entry = reloc->function_ir.entry;
	ParameterClass ret_class = classifyParam(params.target, *reloc->type.function.rettype);
	if (ret_class.sse_count > 2 || ret_class.count - ret_class.count > 2)
		ret_class.count = 0;

	bool mem_return = ret_class.count == 0;
	bool is_vararg = reloc->type.function.is_vararg;


	Blocks linearized = {0};
	scheduleBlocksStraight(params.arena, entry, &linearized);
	assert(linearized.len);
	decimateIr(&reloc->function_ir, linearized);

	IrList ir = reloc->function_ir;

	StringMap types = {0};
	Codegen c = {
		.arena = params.arena,
		.out = params.out,
		.current_id = id,
		.emit_debug_info = params.emit_debug_info,
		.files = params.files,
		.target = params.target,

		.types = &types,

		.ret_class = ret_class,
		.is_vararg = is_vararg,
		.storage = calloc(ir.len, sizeof(Storage)),
		.module = module,
		.param_info = ALLOCN(params.arena, ParamInfo, ir.params.len),
		.ir = ir,
	};

	// Register-save area for the general purpose and the SSE parameter registers.
	if (is_vararg)
		c.stack_allocated += reg_save_area_size;

	// A memory return uses up the first parameter register for the
	// destination address.
	u32 gp_params = mem_return;
	u32 fp_params = 0;
	if (mem_return)
		c.stack_allocated += 8;

	// First mark parameters in order.
	for (u32 i = 0; i < ir.params.len; i++) {
		ParameterClass class = classifyParam(c.target, ir.params.ptr[i].type);
		u32 gp_count = class.count - class.sse_count;
		if (gp_count > gp_parameter_regs_count - gp_params || class.sse_count > 8 - fp_params)
			class.count = 0;

		c.param_info.ptr[i].class = class;
		if (class.count > 0) {
			c.param_info.ptr[i].storage = c.stack_allocated;
			c.stack_allocated += 8 * class.count;

			for (u32 j = 0; j < class.count; j++) {
				if (class.registers[j] == Param_INTEGER) {
					c.param_info.ptr[i].registers[j] = gp_params;
					gp_params++;
				} else {
					c.param_info.ptr[i].registers[j] = fp_params;
					fp_params++;
				}
			}
		}
	}


	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_Parameter)
			continue;
		c.storage[i] = c.stack_allocated;
		c.stack_allocated += roundUp(inst.size);

		if (inst.kind == Ir_StackAllocFixed)
			c.stack_allocated += roundUp(inst.alloc.size);
	}

	// Align stack to 16 bytes.
	c.stack_allocated = (c.stack_allocated + 15) / 16 * 16;


	// No need to do callee-saves because those registers are never touched.

	emit(&c, " push R", RBP_8);
	emit(&c, " mov R, R", RBP_8, RSP_8);
	emit(&c, " sub R, I", RSP_8, c.stack_allocated);


	if (mem_return)
		emit(&c, " mov qword ptr [R+I], R", RSP_8, is_vararg ? reg_save_area_size : 0, RDI_8);

	// 16 bytes for rbp and return address.
	u32 mem_params = c.stack_allocated + 16;

	// Copy regular parameters to the stack.
	for (u32 i = 0; i < c.param_info.len; i++) {
		ParamInfo info = c.param_info.ptr[i];
		if (info.class.count) {
			for (u32 j = 0; j < info.class.count; j++) {
				u32 size = ir.params.ptr[i].size - j*8;
				if (size > 8) size = 8;

				if (info.class.registers[j] == Param_INTEGER) {
					Register reg = gp_parameter_regs[info.registers[j]];
					emit(&c, " mov Z [R+I], R", sizeOp(size), RSP_8, info.storage + 8*j, registerSized(reg, size));
				} else {
					assert(info.class.registers[j] == Param_SSE);
					emit(&c, " movsZ [R+I], %xmmI", sizeFSuffix(size), RSP_8, info.storage + 8*j, (u32) info.registers[j]);
				}
			}
		} else {
			c.param_info.ptr[i].storage = mem_params;
			mem_params += roundUp(ir.params.ptr[i].size);
		}
	}

	if (is_vararg) {
		c.vaarg_reg_saves = 0;
		c.vaarg_gp_offset = gp_params * 8;
		c.vaarg_fp_offset = 48 + fp_params * 8;
		c.vaarg_overflow_args = mem_params;

		// Copy out the registers into the register save area.
		for (u32 i = gp_params; i < gp_parameter_regs_count; i++)
			emit(&c, " mov qword ptr [R+I], R", RSP_8, i*8, registerSized(gp_parameter_regs[i], I64));
		emit(&c, " test R, R", RAX, RAX);
		emit(&c, " je ..vaarg_no_float_args_I", id);
		for (u32 i = 0; i < 8; i++)
			emit(&c, " movaps xmmword ptr [R+I], %xmmI", RSP_8, 48 + i*16, i);
		emit(&c, "..vaarg_no_float_args_I:", id);
	}


	// Initialize the StackAllocs.
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_StackAllocFixed) {
			emit(&c, " lea R, [R+I]", RAX_8, RSP_8, c.storage[i]+8);
			emit(&c, " mov #, R", i, RAX_8);
		}
	}

	for (u32 i = 0; i < linearized.len; i++) {
		emitBlockForward(&c, linearized, i);
	}
	free(linearized.ptr);
	emit(&c, "");



	free(c.storage);
	mapFree(c.types);
}


static void copyTo (Codegen *c, Register to_addr, i32 to_offset, Register from_addr, i32 from_offset, u16 size) {
	i32 offset = 0;
	to_addr = registerSized(to_addr, 8);
	from_addr = registerSized(from_addr, 8);
	while (size - offset > 8) {
		emit(c, " mov %r8, qword ptr [R~I]", from_addr, offset+from_offset);
		emit(c, " mov qword ptr [R~I], %r8", to_addr, offset+to_offset);
		offset += 8;
	}

	// This will break for non-power-of-two remainders.
	Register tmp = registerSized(R8, size - offset);
	const char *op = sizeOp(size - offset);
	emit(c, " mov R, Z [R~I]", tmp, op, from_addr, offset+from_offset);
	emit(c, " mov Z [R~I], R", op, to_addr, offset+to_offset, tmp);
}


static Register loadTo (Codegen *c, Register reg, IrRef i) {
	Register dest = registerSized(reg, c->ir.ptr[i].size);
	emit(c, " mov R, #", dest, i);
	return dest;
}


static void triple (Codegen *c, const char *inst, u16 size, IrRef lhs, IrRef rhs, IrRef dest) {
	Register rax = loadTo(c, RAX, lhs);
	emit(c, " ZZ R, #", inst, sizeSuffix(size), rax, rhs);
	emit(c, " mov #, R", dest, rax);
}

static void ftriple (Codegen *c, const char *inst, u16 size, IrRef lhs, IrRef rhs, IrRef dest) {
	const char *suff = sizeFSuffix(size);
	emit(c, " movsZ %xmm1, #", suff, lhs);
	emit(c, " ZZ %xmm1, #", inst, suff, rhs);
	emit(c, " movsZ #, %xmm1", suff, dest);
}


// This backend performs _no_ register allocation.
static void emitBlockForward (Codegen *c, Blocks blocks, u32 i) {
	Block *block = blocks.ptr[i];
	Block *next = i + 1 == blocks.len ? NULL : blocks.ptr[i+1];
	const u32 visit_id = 1;
	if (block->visited == visit_id) return;
	block->visited = visit_id;

	assert(block->exit.kind != Exit_None);


	emit(c, "..I_I_S:", c->current_id, block->id, block->label);

	IrRefList false_phis = {0};

	for (IrRef ref = block->first_inst; ref < block->inst_end; ref++) {
		if (c->emit_debug_info) {
			if (isNewLine(c, ref) || isNewFile(c, ref)) {
				emit(c, "..IiI:", c->current_id, ref);
				DebugMark mark = {.id = c->current_id, .inst = ref};
				PUSH(line_marks, mark);
			}
		}
		emitInstForward(c, ref);

		Inst inst = c->ir.ptr[ref];
		if (inst.kind == Ir_PhiOut && inst.phi_out.on_false != IDX_NONE)
			PUSH(false_phis, ref);
	}

	Exit exit = block->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		if (exit.unconditional != next)
			emitJump(c, "jmp", exit.unconditional);
		break;
	case Exit_Branch: {
		emit(c, " test #, -1", exit.branch.condition);
		emitJump(c, "jnz", exit.branch.on_true);


		for (u32 k = 0; k < false_phis.len; k++) {
			Inst inst  = c->ir.ptr[false_phis.ptr[k]];
			emit(c, " mov #, R",
				inst.phi_out.on_false, loadTo(c, RAX, inst.phi_out.source));
		}

		if (exit.branch.on_false != next)
			emitJump(c, "jmp", exit.branch.on_false);
	} break;
	case Exit_Switch: {
		assert(!false_phis.len);
		Cases cases = exit.switch_.cases;

		for (u32 i = 0; i < cases.len; i++) {
			emit(c, " cmp #, L", exit.switch_.value, cases.ptr[i].value & 0xffffffff);
			emitJump(c, "je", cases.ptr[i].dest);
		}
		if (exit.switch_.default_case != next)
			emitJump(c, "jnz", exit.switch_.default_case);
	} break;
	case Exit_Return:
		if (exit.ret != IDX_NONE) {
			if (c->ret_class.count == 0) {
				emit(c, " mov R, qword ptr [R+I]", RAX_8, RSP_8, c->is_vararg ? reg_save_area_size : 0);
				copyTo(c, RAX, 0, RSP, c->storage[exit.ret], valueSize(c, exit.ret));
			} else {
				u32 gp_returns = 0;
				u32 fp_returns = 0;
				for (u32 j = 0; j < c->ret_class.count; j++) {
					u32 size = valueSize(c, exit.ret) - 8*j;
					if (size > 8) size = 8;
					u32 src = c->storage[exit.ret] + 8*j;
					if (c->ret_class.registers[j] == Param_INTEGER) {
						int reg = gp_returns == 0 ? RAX_8 : RDX_8;
						emit(c, " mov R, Z [R+I]", registerSized(reg, size), sizeOp(size), RSP_8, src);
						gp_returns++;
					} else {
						assert(c->ret_class.registers[j] == Param_SSE);
						emit(c, " movsZ %xmmI, [R+I]", sizeFSuffix(size), fp_returns, RSP_8, src);
						fp_returns++;
					}
				}
			}
		}
// 		emit(c, " add rsp, I", c->stack_allocated);
		emit(c, " mov R, R", RSP_8, RBP_8);
		emit(c, " pop R", RBP_8);
		emit(c, " ret");
		break;
	case Exit_None: unreachable;
	}

	free(false_phis.ptr);
}


static void emitInstForward(Codegen *c, IrRef i) {
	assert(i != IDX_NONE);

	Inst inst = c->ir.ptr[i];

	switch ((InstKind) inst.kind) {
	case Ir_Add: triple(c, "add", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Sub: triple(c, "sub", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Mul: triple(c, "imul", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Div:
	case Ir_Mod: {
		loadTo(c, RAX, inst.binop.lhs);
		emit(c, " xor R, R", RDX_8, RDX_8);

		emit(c, " divZ #", sizeSuffix(inst.size), inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
		emit(c, " mov #, R", i, registerSized(inst.kind == Ir_Div ? RAX : RDX, inst.size));
	} break;
	case Ir_SDiv:
	case Ir_SMod: {
		loadTo(c, RAX, inst.binop.lhs);
		switch (inst.size) {
		case 1: emit(c, " cbw"); break;
		case 2: emit(c, " cwd"); break;
		case 4: emit(c, " cdq"); break;
		case 8: emit(c, " cqo"); break;
		default: unreachable;
		}

		emit(c, " idivZ #", sizeSuffix(inst.size), inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(inst.kind == Ir_SDiv ? RAX : RDX, inst.size));
	} break;
	case Ir_FAdd: ftriple(c, "adds", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FSub: ftriple(c, "subs", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FMul: ftriple(c, "muls", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FDiv: ftriple(c, "divs", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FMod: unreachable;

	case Ir_BitOr: triple(c, "or", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitXor: triple(c, "xor", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitAnd: triple(c, "and", inst.size, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitNot: {
		Register reg = loadTo(c, RAX, inst.unop);
		emit(c, " not R", reg);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_Equals:
	case Ir_FEquals:
	case Ir_LessThan:
	case Ir_SLessThan:
	case Ir_FLessThan:
	case Ir_LessThanOrEquals:
	case Ir_SLessThanOrEquals:
	case Ir_FLessThanOrEquals: {
		if (inst.kind == Ir_FLessThan || inst.kind == Ir_FLessThanOrEquals || inst.kind == Ir_FEquals) {
			u16 sz = c->ir.ptr[inst.binop.lhs].size;
			emit(c, " movsZ %xmm1, #", sizeFSuffix(sz), inst.binop.lhs);
			emit(c, " comisZ %xmm1, #", sizeFSuffix(sz), inst.binop.rhs);
		} else {
			emit(c, " cmp #, R", inst.binop.lhs, loadTo(c, RAX, inst.binop.rhs));
		}
		switch (inst.kind) {
			case Ir_LessThan:
			case Ir_FLessThan:
				emit(c, " setb R", RAX); break;
			case Ir_SLessThan:
				emit(c, " setl R", RAX); break;
			case Ir_LessThanOrEquals:
			case Ir_FLessThanOrEquals:
				emit(c, " setbe R", RAX); break;
			case Ir_SLessThanOrEquals:
				emit(c, " setle R", RAX); break;
			case Ir_Equals:
			case Ir_FEquals:
				emit(c, " sete R", RAX); break;
		}
		emit(c, " movzx R, R", RSI_8, RAX);
		emit(c, " mov #, R", i, registerSized(RSI, inst.size));
	} break;
	case Ir_ShiftLeft: {
		loadTo(c, RCX, inst.binop.rhs);
		Register shiftee = loadTo(c, R8, inst.binop.lhs);
		emit(c, " xor R, R", RAX_8, RAX_8);
		emit(c, " shld R, R, R", shiftee, registerSized(RAX, inst.size), RCX);
		emit(c, " mov #, R", i, shiftee);
	} break;
	case Ir_ShiftRight: {
		loadTo(c, RCX, inst.binop.rhs);
		Register shiftee = loadTo(c, R8, inst.binop.lhs);
		emit(c, " xor R, R", RAX_8, RAX_8);
		emit(c, " shrd R, R, R", shiftee, registerSized(RAX, inst.size), RCX);
		emit(c, " mov #, R", i, shiftee);
		loadTo(c, RCX, inst.binop.rhs);
	} break;
	case Ir_Truncate: {
		Register reg = registerSized(RAX, inst.size);
		emit(c, " mov R, [R+I]", reg, RSP_8, c->storage[inst.unop]);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_SignExtend: {
		Register reg = registerSized(RAX, inst.size);
		bool dw = inst.size == 8 && valueSize(c, inst.unop) == 4;
		emit(c, " movsxZ R, #", (dw ? "d" : ""), reg, inst.unop);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_ZeroExtend: {
		emit(c, " xor R, R", RAX_8, RAX_8);
		emit(c, " mov R, #", registerSized(RAX, valueSize(c, inst.unop)), inst.unop);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
	} break;
	case Ir_Store: {
		copyTo(c, loadTo(c, RAX, inst.mem.address), 0, RSP, c->storage[inst.mem.source], inst.size);
	} break;
	case Ir_Load: {
		copyTo(c, RSP, c->storage[i], loadTo(c, RAX, inst.mem.address), 0, inst.size);
	} break;
	case Ir_Access: {
		copyTo(c, RSP, c->storage[i],
			RSP, c->storage[inst.binop.lhs] + inst.binop.rhs, inst.size);
	} break;
	case Ir_Parameter: {
		ParamInfo info = c->param_info.ptr[inst.unop];
		c->storage[i] = info.storage;
	} break;
	case Ir_Constant:
		assert(inst.size <= 8);
		if (inst.size > 4 && inst.constant > INT32_MAX) {
			// Wrapping unsigned to signed is undefined, but whatever.
			emit(c, " mov Z [R+I], ~I", sizeOp(4), RSP_8, c->storage[i], (i32) inst.constant);
			emit(c, " mov Z [R+I], ~I", sizeOp(4), RSP_8, c->storage[i] + 4, (i32) (inst.constant >> 32));
		} else {
			emit(c, " mov #, ~I", i, (i32) inst.constant);
		}
		break;
	case Ir_StackAllocFixed:
		break;
	case Ir_StackAllocVLA:
		unreachable;
	case Ir_Reloc:
		assert(inst.size == 8);

		emitZString(" mov %rax, offset flat:");
		emitName(c->module, inst.reloc.id);
		emit(c, "~L", inst.reloc.offset);

		emit(c, " mov #, R", i,  RAX_8);
		break;
	case Ir_PhiOut: {
		if (inst.phi_out.on_true != IDX_NONE) {
			copyTo(c, RSP, c->storage[inst.phi_out.on_true],
				RSP, c->storage[inst.phi_out.source],
				valueSize(c, inst.phi_out.source));
		}
	} break;
	case Ir_Copy: break;
	case Ir_PhiIn: break;
	case Ir_StackDeallocVLA: break;
	case Ir_FCast: {
		u32 source = c->ir.ptr[inst.unop].size;
		const char *fsuff = sizeFSuffix(inst.size);
		emit(c, " cvtsZ2sZ %xmm1, #", sizeFSuffix(source), fsuff, inst.unop);
		emit(c, " movsZ #, %xmm1", fsuff, i);
	} break;
	case Ir_UIntToFloat: // TODO This is a hack, must be handled differently.
	case Ir_SIntToFloat: {
		const char *fsuff = sizeFSuffix(inst.size);
		loadTo(c, RAX, inst.unop);

		emit(c, " cvtsi2sZ %xmm1, R", fsuff, RAX_8);
		emit(c, " movsZ #, %xmm1", fsuff, i);
	} break;
	case Ir_FloatToUInt:
	case Ir_FloatToSInt: {
		const char *fsuff = sizeFSuffix(c->ir.ptr[inst.unop].size);

		emit(c, " movsZ %xmm0, [R+I]", fsuff, RSP_8, c->storage[inst.unop]);
		emit(c, " cvttsZ2si R, %xmm0", fsuff, RAX_8);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
	} break;
	case Ir_VaStart: {
		loadTo(c, RAX, inst.binop.lhs);
		// State of affairs: rhs should be the last parameter, but it is currently ignored.

		emit(c, " mov dword ptr [R], I", RAX_8, c->vaarg_gp_offset);
		emit(c, " mov dword ptr [R+4], I", RAX_8, c->vaarg_fp_offset);
		emit(c, " lea R, [R+I]", RDX_8, RSP_8, c->vaarg_overflow_args);
		emit(c, " mov qword ptr [R+8], R", RAX_8, RDX_8);
		emit(c, " lea R, [R+I]", RDX_8, RSP_8, c->vaarg_reg_saves);
		emit(c, " mov qword ptr [R+16], R", RAX_8, RDX_8);
	} break;
	case Ir_VaArg: {
		Type type = AUX_DATA(Type, c->ir, inst.unop_const.offset);
		ParameterClass class = classifyParam(c->target, type);

		// Put the va_list's address into RAX.
		loadTo(c, RAX, inst.unop);

		if (class.count) {
			u32 gp_count = class.count - class.sse_count;

			if (gp_count) {
				// Test if all INTEGER parts would fit
				emit(c, " mov R, dword ptr [R]", RDX_4, RAX_8);
				emit(c, " cmp R, I", RDX_4, 48 - gp_count*8);
				emit(c, " ja .vaarg_I_overflowarg", i);
			}

			if (class.sse_count) {
				// Test if all SSE parts would fit
				emit(c, " mov R, dword ptr [R+4]", RDX_4, RAX_8);
				emit(c, " cmp R, I", RDX_4, reg_save_area_size - class.sse_count*8);
				emit(c, " ja .vaarg_I_overflowarg", i);
			}

			for (u32 j = 0; j < class.count; j++) {
				// Load the register offset to EDX and update it.
				if (class.registers[j] == Param_INTEGER) {
					emit(c, " mov R, dword ptr [R]", RDX_4, RAX_8);
					emit(c, " add dword ptr [R], 8", RAX_8);
				} else {
					emit(c, " mov R, dword ptr [R+4]", RDX_4, RAX_8);
					emit(c, " add dword ptr [R+4], 16", RAX_8);
				}
				// Add it to the reg_save_area.
				emit(c, " add R, qword ptr [R+16]", RDX_8, RAX_8);

				emit(c, " mov R, qword ptr [R]", RDX_8, RDX_8);
				emit(c, " mov qword ptr [R+I], R", RSP_8, c->storage[i] + j*8, RDX_8);
			}
			emit(c, " jmp .vaarg_I_done", i);

			emit(c, ".vaarg_I_overflowarg:", i);
		}
		emit(c, " mov R, qword ptr [R+8]", RDX_8, RAX_8);
		emit(c, " add qword ptr [R+8], I", RAX_8, inst.size);
		copyTo(c, RSP, c->storage[i], RDX, 0, inst.size);

		emit(c, ".vaarg_I_done:", i);
	} break;
	case Ir_Call: {
		Call call = AUX_DATA(Call, c->ir, inst.call.data);
		ArgumentSpan args = call.arguments;
		ParameterClass ret_class = classifyParam(c->target, call.rettype);
		if (ret_class.sse_count > 2 || ret_class.count - ret_class.count > 2)
			ret_class.count = 0;

		bool memory_return = ret_class.count == 0;

		if (memory_return)
			emit(c, " lea R, #", RDI_8, i);

		// STYLE All kinds of copypasta between parameter taking and
		// argument passing. Need to unify caller and callee definitions
		// per ABI. PERFORMANCE Could cache ABI calculations in the Type
		// data of the function prototype, for example.

		u32 gp_params = memory_return;
		u32 fp_params = 0;

		u32 stack_memory = 0;
		ParameterClass *classes = malloc(args.len * sizeof(*classes));
		for (u32 i = 0; i < args.len; i++) {
			Argument arg = args.ptr[i];
			u16 param_size = valueSize(c, arg.arg_inst);
			ParameterClass class = classifyParam(c->target, arg.type);

			u32 gp_count = class.count - class.sse_count;
			if (gp_count > gp_parameter_regs_count - gp_params || class.sse_count > 8 - fp_params)
				class.count = 0;
			classes[i] = class;
			if (class.count) {
				for (u32 j = 0; j < class.count; j++) {
					u32 size = param_size - j*8;
					if (size > 8) size = 8;

					// TODO Do small arguments need to be zero-extended?
					if (class.registers[j] == Param_INTEGER) {
						Register reg = registerSized(gp_parameter_regs[gp_params], size);
						emit(c, " mov R, Z [R+I]", reg, sizeOp(size), RSP_8, c->storage[arg.arg_inst] + 8*j);
						gp_params++;
					} else {
						assert(class.registers[j] == Param_SSE);
						emit(c, " movsZ %xmmI, [R+I]", sizeFSuffix(size), fp_params, RSP_8, c->storage[arg.arg_inst] + 8*j);
						fp_params++;
					}
				}
			} else {
				stack_memory += param_size;
			}
		}

		u32 stack_filled = 0;
		for (u32 i = 0; i < args.len; i++) {
			if (classes[i].count == 0) {
				IrRef arg = args.ptr[i].arg_inst;
				u16 param_size = valueSize(c, arg);
				copyTo(c, RSP, -(i32)stack_memory + stack_filled, RSP, c->storage[arg], param_size);
				stack_filled += param_size;
			}
		}

		assert(stack_filled == stack_memory);

		if (stack_memory)
			emit(c, " sub R, I", RSP_8, stack_memory);

		if (inst.properties & Prop_Call_Vararg)
			emit(c, " mov R, I", RAX_4, fp_params);

		emit(c, " call qword ptr [R+I]", RSP_8, c->storage[inst.call.function_ptr] + stack_memory);
		if (stack_memory)
			emit(c, " add R, I", RSP_8, stack_memory);

		if (!memory_return && inst.size) {
			u32 gp_returns = 0;
			u32 fp_returns = 0;
			for (u32 j = 0; j < ret_class.count; j++) {
				u32 dest = c->storage[i] + 8*j;
				u32 size = inst.size - 8*j;
				if (size > 8) size = 8;

				if (ret_class.registers[j] == Param_INTEGER) {
					int reg = gp_returns == 0 ? RAX_8 : RDX_8;
					emit(c, " mov Z [R+I], R", sizeOp(size), RSP_8, dest, registerSized(reg, size));
					gp_returns++;
				} else {
					assert(ret_class.registers[j] == Param_SSE);
					emit(c, " movsZ [R+I], %xmmI", sizeFSuffix(size), RSP_8, dest, fp_returns);
					fp_returns++;
				}
			}
		}
	} break;
	}
}


static inline bool isNewLine (Codegen *c, u32 i) {
	return i == 0 || c->ir.locations[i].line != c->ir.locations[i-1].line;
}

static inline bool isNewFile (Codegen *c, u32 i) {
	return i == 0 || c->ir.locations[i].line != c->ir.locations[i-1].line;
}



// static void emitFunctionLineInfo (Arena *arena, FILE *out, Module module, StaticValue *reloc, const Target *target) {
// 	ir = reloc->function_ir;


// 	for (u32 i = 0; i < ir.len; i++) {
// 		Inst inst = ir.ptr[i];
// 		Location loc = ir.locations[i];
// 		if (i == 0 || ir.locations[i].line != loc.line) || ir.locations[i].file_id != loc.file_id) {
// 			emit(c, "S.__fIlIcI:", reloc.name, loc.file_id, loc.line, loc.column);
// 		}
// 	}
// }


static void emitJump (Codegen *c, const char *inst, const Block *dest) {
	emit(c, " Z ..I_I_S", inst, c->current_id, dest->id, dest->label);
}

// static bool isSpilled(Storage s) {
// 	return s >= STACK_BEGIN;
// }

static inline u16 sizeOfRegister (Register reg) {
	switch (reg & RSIZE_MASK) {
	case RSIZE_BYTE:
		return 1;
	case RSIZE_WORD:
		return 2;
	case RSIZE_DWORD:
		return 4;
	case RSIZE_QWORD:
		return 8;
	default: unreachable;
	}
}

static inline Storage registerSize (u16 size) {
	assert(size);
	if (size == 1)
		return RSIZE_BYTE;
	if (size == 2)
		return RSIZE_WORD;
	if (size <= 4)
		return RSIZE_DWORD;
	if (size <= 8)
		return RSIZE_QWORD;
	unreachable;
}

static inline Register registerSized (Register stor, u16 size) {
	return (stor & ~RSIZE_MASK) | registerSize(size);
}

static const char *sizeOp (u16 size) {
	if (size == 1)
		return "byte ptr";
	if (size == 2)
		return "word ptr";
	if (size <= 4)
		return "dword ptr";
	if (size <= 8)
		return "qword ptr";
	unreachable;
}

static const char *sizeSuffix (u16 size) {
	if (size == 1)
		return "b";
	if (size == 2)
		return "w";
	if (size <= 4)
		return "d";
	if (size <= 8)
		return "q";
	return NULL;
}
static const char *sizeFSuffix (u16 size) {
	switch (size) {
	case 4: return "s";
	case 8: return "d";
	}
	unreachable;
}
static u16 valueSize (Codegen *c, IrRef ref) {
	return c->ir.ptr[ref].size;
}

static inline bool printable(char c) {
	return c >= ' ' && c <= '~' && c != '"';
}


// Spending a bit of time to compress the assembly is totally worth it.
static void emitDataLine (FILE *out, u32 len, const char *data) {
	u32 raw_begin;
	u32 string_begin;
	u32 i = 0;

	while (i < len) {
		u32 end = i + (buf + BUF - insert) / 5 - 20;
		assert(end - i < BUF);
		if (end > len) end = len;

		raw_begin = i;
		while (i < end && !printable(data[i])) i++;
		string_begin = i;
		while (i < end && printable(data[i])) i++;

		if (i - string_begin < 3) {
			emitDataRaw(i - raw_begin, data + raw_begin);
		} else {
			emitDataRaw(string_begin - raw_begin, data + raw_begin);
			emitDataString(i - string_begin, data + string_begin);
		}

		if (insert > buf + (BUF - MAX_LINE))
			flushit(out);
	}
}

static void emitDataString (u32 len, const char *data) {
	emitZString(" .ascii \"");
	for (u32 i = 0; i < len; i++) {
		if (data[i] == '\\' || data[i] == '\"')
			*insert++ = '\\';
		*insert++ = data[i];
	}
	*insert++ = '\"';
	*insert++ = '\n';
}

static void emitDataRaw (u32 len, const char *data) {
	assert(len * 5 < BUF);
	if (len == 0)
		return;

	static const char hexchars[] = "0123456789abcdef";

	memcpy(insert, " .byte ", 7);
	insert += 7;
	u32 pos = 0;
	while (pos < len) {
		uchar c = data[pos++];
		if (c > 9) {
			*insert++ = '0';
			*insert++ = 'x';
		}
		if (c/16) {
			*insert++ = hexchars[c/16];
			*insert++ = hexchars[c%16];
		} else {
			*insert++ = hexchars[c%16];
		}
		*insert++ = ',';
	}
	insert[-1] = '\n';
}


/*

Formatting:

Z const char *
S String
~ signed
I 32 bit
L 64 bit
# IrRef
R Register
H hex char

*/


static void emit (Codegen *c, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	for (const char *p = fmt; *p; p++) {
		switch (*p) {
		case 'Z': {
			emitZString(va_arg(args, const char *));
		} break;
		case 'S': {
			String str = va_arg(args, String);
			if (str.len)
				memcpy(insert, str.ptr, str.len);
			insert += str.len;
		} break;
		case 'R': {
			*insert++ = '%';
			const char *name = register_names[va_arg(args, Register)];
			u32 len = strlen(name);
			memcpy(insert, name, len);
			insert += len;
		} break;
		case '#': {
			IrRef ref = va_arg(args, IrRef);
			u32 size = c->ir.ptr[ref].size;
			if (size <= 8) {
			const char *size_op = sizeOp(c->ir.ptr[ref].size);
				u32 len = strlen(size_op);
				memcpy(insert, size_op, len);
				insert += len;
			}
			memcpy(insert, " [%rsp+", 7);
			insert += 7;
			emitInt(c->storage[ref]);
			*insert++ = ']';
		} break;
		case 'I':
			emitInt(va_arg(args, u32));
			break;
		case 'L':
			emitInt(va_arg(args, u64));
			break;
		case '~': {
			p++;
			if (*p == 'I') {
				i32 val = va_arg(args, i32);
				if (val >= 0) {
					*insert++ = '+';
					emitInt(val);
				} else {
					*insert++ = '-';
					emitInt(-(i64)val);
				}
			} else {
				assert(*p == 'L');
				i64 val = va_arg(args, i64);
				if (val >= 0) {
					*insert++ = '+';
					emitInt(val);
				} else {
					// TODO Will break for I64_MIN, which can not be inverted.
					*insert++ = '-';
					emitInt(-val);
				}
			}
		} break;
		default:
			assert(!(*p >= 'A' && *p <= 'Z'));
			if (*p == '\\') p++;
			*insert++ = *p;
		}
	}
	*insert++ = '\n';
	va_end(args);

	if (insert > buf + (BUF - MAX_LINE))
		flushit(c->out);
}

static void flushit (FILE *f) {
	(void) f;
	fwrite(buf, 1, insert-buf, f);
	insert = buf;
}

// PERFORMANCE This consumes an obscene 7% of execution time.
static void emitInt (u64 i) {
	if (i < 9) {
		*insert++ = '0' + i;
		return;
	}

	u64 place = i < 10000U ? 10000U : 10000000000000000000ULL;
	while (i / place == 0) place /= 10;

	while (place != 0) {
		*insert++ = '0' + (i / place) % 10;
		place /= 10;
	}
}

static void emitString (String s) {
	memcpy(insert, s.ptr, s.len);
	insert += s.len;
}
static void emitZString (const char *s) {
	u32 len = strlen(s);
	memcpy(insert, s, len);
	insert += len;
}



static void setEightbyteClass(ParameterClass *dest, u8 offset, ParameterEightbyteClass class) {
	u8 *part = &dest->registers[offset / 8];
	if (class == Param_SSE && *part != Param_INTEGER)
		*part = Param_SSE;
	else
		*part = Param_INTEGER;
}

static void classifyMembers(const Target *target, ParameterClass *dest, u8 offset, Type type) {
	assert(typeSize(type, target) <= 64);

	switch (type.kind) {
	case Kind_Void:
	case Kind_Basic:
	case Kind_Enum:
	case Kind_Enum_Named:
	case Kind_Pointer:
	case Kind_Function:
	case Kind_FunctionPtr:
		setEightbyteClass(dest, offset, Param_INTEGER);
		return;
	case Kind_Float:
		setEightbyteClass(dest, offset, Param_SSE);
		return;
	case Kind_Struct_Named:
	case Kind_Union_Named:
		type = type.nametagged->type;
		FALLTHROUGH;
	case Kind_Struct:
	case Kind_Union: {
		Members m = type.compound.members;
		for (u32 i = 0; i < m.len; i++)
			classifyMembers(target, dest, offset + m.ptr[i].offset, m.ptr[i].type);
	} return;
	case Kind_Array: {
		u32 begin = offset / 8;
		u32 end = (offset + typeSize(type, target) + 7) / 8;
		for (u32 i = begin; i < end; i++) {
			classifyMembers(target, dest, i * 8, *type.array.inner);
		}
	} return;
	case Kind_VLArray:
	case Kind_UnsizedArray:
		unreachable;
	}
}

ParameterClass classifyParam(const Target *target, Type type) {
	switch (type.kind) {
	case Kind_Void:
	case Kind_Basic:
	case Kind_Enum:
	case Kind_Enum_Named:
	case Kind_Pointer:
	case Kind_FunctionPtr:
		return (ParameterClass) {.count = 1, .registers[0] = Param_INTEGER};
	case Kind_Float:
		return (ParameterClass) {.count = 1, .registers[0] = Param_SSE};
	case Kind_Struct_Named:
	case Kind_Union_Named:
		type = type.nametagged->type;
		FALLTHROUGH;
	case Kind_Struct:
	case Kind_Union:
	case Kind_Array: {
		u32 size = typeSize(type, target);
		if (size > 64)
			return (ParameterClass) {.count = 0};
		ParameterClass res = {.count = (size + 7) / 8};
		classifyMembers(target, &res, 0, type);
		if (res.count > 2 && res.registers[0] != Param_SSE)
			res.count = 0;
		return res;
	}
	case Kind_Function:
	case Kind_VLArray:
	case Kind_UnsizedArray:
		unreachable;
	}
	unreachable;
}


