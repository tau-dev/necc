#include "../analysis.h"
#include "../types.h"
#include "../parse.h"
#include "../emit.h"


/*

Generates assembly for the GNU assembler.
C identifiers are prefixed with an underscore to disambiguate from
instructions & registers.

*/



typedef unsigned long ulong;
typedef unsigned long long ullong;


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
	RDX = 0x00d,
	RSP = 0x00e,
	RAX = 0x00f,

	RSIZE_BYTE  = 0x0 << STORAGE_SIZE_OFFSET,
	RSIZE_WORD  = 0x1 << STORAGE_SIZE_OFFSET,
	RSIZE_DWORD = 0x2 << STORAGE_SIZE_OFFSET,
	RSIZE_QWORD = 0x3 << STORAGE_SIZE_OFFSET,

	RSIZE_MASK = 0x3 << STORAGE_SIZE_OFFSET,

	STACK_BEGIN = 0x4 << STORAGE_SIZE_OFFSET,
} Register;

typedef enum {
	XMM = 0x00,
	YMM = 0x20,
	ZMM = 0x40,
	VRSIZE_MASK = 0x60,
} VecRegister;

typedef struct Mem {
	i32 offset;
	u8 reg;
	u8 index_reg;
	u8 index_scale;
} Mem;

typedef struct {
	enum {
		Location_Register,
		Location_VecRegister,
		Location_Memory,
		Location_Relocation,
		Location_Constant,
		Location_LongConstant,
	} kind;

	union {
		Register reg;
		VecRegister vec_reg;
		Mem memory;
		Relocation relocation;
		i32 constant;
		u64 long_constant;
	};
} Storage;

typedef enum {
	Cond_Overflow,
	Cond_NoOverflow,

	Cond_ULessThan, Cond_Carry = Cond_ULessThan,
	Cond_UGreaterThanOrEquals, Cond_NoCarry = Cond_UGreaterThanOrEquals,

	Cond_Equal, Cond_Zero = Cond_Equal,
	Cond_NotEqual, Cond_NotZero = Cond_NotEqual,

	Cond_UGreaterThan,
	Cond_ULessThanOrEquals,

	Cond_Sign,
	Cond_NoSign,
	Cond_ParityEven,
	Cond_ParityOdd,

	Cond_SLessThan,
	Cond_SGreaterThanOrEquals,
	Cond_SGreaterThan,
	Cond_SLessThanOrEquals,
} Condition;

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
ParameterClass classifyParam(const Target *target, Type type);



typedef struct {
	i32 storage;
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
	i32 *storage;

	ParameterClass ret_class;
	i32 return_pointer_storage;
	bool is_vararg;

	u32 vaarg_gp_offset;
	u32 vaarg_fp_offset;
	i32 vaarg_overflow_args;
	i32 vaarg_reg_saves;
	SPAN(ParamInfo) param_info;
} Codegen;



#define RAX_4 (RAX + RSIZE_DWORD)
#define RCX_4 (RCX + RSIZE_DWORD)
#define RDX_4 (RDX + RSIZE_DWORD)
#define RBX_4 (RBX + RSIZE_DWORD)
#define RSI_4 (RSI + RSIZE_DWORD)
#define RSP_4 (RSP + RSIZE_DWORD)
#define RDI_4 (RDI + RSIZE_DWORD)
#define RBP_4 (RBP + RSIZE_DWORD)
#define R8_4  (R8 + RSIZE_DWORD)
#define R9_4  (R9 + RSIZE_DWORD)
#define R10_4 (R10 + RSIZE_DWORD)
#define R11_4 (R11 + RSIZE_DWORD)
#define R12_4 (R12 + RSIZE_DWORD)
#define R13_4 (R13 + RSIZE_DWORD)
#define R14_4 (R14 + RSIZE_DWORD)
#define R15_4 (R15 + RSIZE_DWORD)

#define RAX_8 (RAX + RSIZE_QWORD)
#define RCX_8 (RCX + RSIZE_QWORD)
#define RDX_8 (RDX + RSIZE_QWORD)
#define RBX_8 (RBX + RSIZE_QWORD)
#define RSI_8 (RSI + RSIZE_QWORD)
#define RSP_8 (RSP + RSIZE_QWORD)
#define RDI_8 (RDI + RSIZE_QWORD)
#define RBP_8 (RBP + RSIZE_QWORD)
#define R8_8  (R8  + RSIZE_QWORD)
#define R9_8  (R9  + RSIZE_QWORD)
#define R10_8 (R10 + RSIZE_QWORD)
#define R11_8 (R11 + RSIZE_QWORD)
#define R12_8 (R12 + RSIZE_QWORD)
#define R13_8 (R13 + RSIZE_QWORD)
#define R14_8 (R14 + RSIZE_QWORD)
#define R15_8 (R15 + RSIZE_QWORD)

#define CALLER_SAVED_COUNT (sizeof(caller_saved) / sizeof(caller_saved[0]))

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


typedef enum {
	IAdd,
	ISub,
	IIMul,
	IOr,
	IXor,
	IAnd,
	IDiv,
	IIDiv,
	IShl,
	IShr,
	ICmp,
	ITest,
	ILea,
	IBtc,
	IComis,

	IRet,
} BasicInst;


static const u32 gp_parameter_regs_count = sizeof(gp_parameter_regs) / sizeof(gp_parameter_regs[0]);
const i32 reg_save_area_size = 48 + 128;


typedef enum {
	SectionStrtab,
	SectionDebugLine,
	SectionDebugAbbrev,
	SectionDebugInfo,
	SectionText,
	SectionData,
	SectionRodata,
	Section_COUNT,
} ElfSection;

static char *section_names[] = {
	".strtab",
	".debug_line",
	".debug_abbrev",
	".debug_info",
	".text",
	".data",
	".rodata",
};


#define DW_TAG_compile_unit 0x11
#define DW_TAG_subprogram 0x2e

#define DW_AT_name        0x3
#define DW_AT_comp_dir    0x1b
#define DW_AT_language    0x13
#define DW_AT_stmt_list   0x10
#define DW_AT_decl_file   0x3a
#define DW_AT_decl_line   0x3b
#define DW_AT_decl_column 0x39
#define DW_AT_external    0x3f
#define DW_AT_frame_base  0x40

#define DW_AT_low_pc 0x11
#define DW_AT_high_pc 0x12


// The values for an attribute are constrained to one or more classes.
// Forms are the particular encodings of the values.

// class address
#define DW_FORM_addr 0x1

// class constant
#define DW_FORM_data1 0xb
#define DW_FORM_data2 0x5
#define DW_FORM_data4 0x6
#define DW_FORM_data8 0x7
#define DW_FORM_sdata 0xd /* leb128 */
#define DW_FORM_udata 0xf /* leb128 */

// class lineptr, rangelistptr or loclistptr
#define DW_FORM_sec_offset 0x17 /* offset into the appropriate section, 32/64 bits */

// class reference
#define DW_FORM_ref_addr 0x10 /* offset into .debug_info, section, 32/64 bits */

// class string
#define DW_FORM_string 0x8 /* null-terminated byte sequence */

// class flag
#define DW_FORM_flag 0xc /* single byte */

#define DW_LANG_C99 0x0c
#define DW_LANG_C11 0x1d

#define DW_LNS_advance_line 3
#define DW_LNS_set_file 4
#define DW_LNE_end_sequence 1
#define DW_LNE_set_address 2
#define DW_LNS_prologue_end 10

static const char* debug_prelude =
	".set DW_LNS_copy, 1\n"
	".set DW_LNS_advance_pc, 2\n"
	".set DW_LNS_advance_line, 3\n"
	"\n"
	".set last_line, 1\n"
	".set last_pc, .exec_base\n"
	"\n"
	".macro line_inst line,pc\n"
	"	.byte DW_LNS_advance_line\n"
	"	.sleb128 \\line - last_line\n"
	"	.byte DW_LNS_advance_pc\n"
	"	.sleb128 \\pc - last_pc\n"
	"	.byte DW_LNS_copy\n"
	"\n"
	"	.set last_line, \\line\n"
	"	.set last_pc, \\pc\n"
	".endm\n"
	;

static void emitData(Codegen *, Module, String data, References);
static void emitDataLine(Codegen *, u32 len, const char *data);
static void emitDataString(Codegen *c, u32 len, const char *data);
static void emitDataRaw(u32 len, const char *data);
static void emitName(Codegen *, Module mod, u32 id);
static void emitFunctionForward(EmitParams, u32);
static void emitBlockForward(Codegen *, Blocks, u32);
static inline bool isNewLine(Codegen *c, u32 i);
static inline bool isNewFile(Codegen *c, u32 i);
static void emitInstForward(Codegen *c, IrRef i);
static inline u16 sizeOfRegister(Register size);
static inline Register registerSize(u16 size);
static inline Register registerSized(Register, u16);
static const char *sizeSuffix(u16 size);
static const char *sizeFSuffix(u16 size);
static u16 valueSize(Codegen *, IrRef);
static void emit(Codegen *c, const char *fmt, ...);
static void emitString(Codegen *, String s);
static void emitZString(Codegen *, const char *);
static void emitInt(Codegen *, u64 i);
static void emitIntSigned(Codegen *, i64 i);
static void flushit(FILE *f);

#include "encode_fasm.h"

int splice_dest_order (const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

static void emitLabel (Codegen *c, Module mod, u32 i) {
	emitZString(c, ".align 8\n");
	emitName(c, mod, i);
	emitZString(c, ":\n");
}

void emitX64AsmSimple (EmitParams params) {
	Codegen globals = {
		.out = params.out,
		.arena = params.arena,
		.emit_debug_info = params.emit_debug_info,
		.target = params.target,
	};
	Codegen *c = &globals;
	insert = buf;
	Module module = params.module;

// 	fprintf(params.out, ".intel_syntax\n\n");

	foreach (i, module) {
		StaticValue reloc = module.ptr[i];
		if (reloc.name.len == 0) {
			assert(reloc.def_state != Def_Undefined);
			assert(!reloc.is_public);
			continue;
		}
		if (reloc.def_state == Def_Undefined)
			; //fprintf(out, "extrn '%.*s' as _%.*s	; %lu\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name), (ulong) i);
		else if (reloc.is_public) {
			emitZString(c, "\n.global ");
			emitName(c, module, i);

			*insert++ = '\n';

			if (insert > buf + (BUF - MAX_LINE))
				flushit(globals.out);
		}
	}

	startSection(c, SectionText);
	emitZString(c, ".exec_base:\n");
	foreach (i, module) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state != Def_Undefined) {
			emitLabel(c, module, i);
			assert(reloc.type.kind == Kind_Function);
			emitFunctionForward(params, i);
			emit(c, ".S_end:\n", reloc.name);
		}
	}
	emit(c, ".exec_end:");

	startSection(c, SectionData);
	foreach (i, module) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state != Def_Undefined)
		{
			emitLabel(c, module, i);
			emitData(c, module, reloc.value_data, reloc.value_references);
		}
	}

	startSection(c, SectionRodata);
	foreach (i, module) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state != Def_Undefined)
		{
			emitLabel(c, module, i);
			emitData(c, module, reloc.value_data, reloc.value_references);
		}
	}
	(void)emitData;



	if (false) {
		startSection(c, SectionDebugAbbrev);
		writeUleb128(c, 1);
		writeUleb128(c, DW_TAG_compile_unit);
		writeU8(c, 1); // has children
			// 	leb2_128 DW_AT_producer,DW_FORM_string
		writeUleb128Pair(c, DW_AT_name,DW_FORM_string);
		// writeUleb128Pair(c, DW_AT_comp_dir,DW_FORM_string);
		writeUleb128Pair(c, DW_AT_language,DW_FORM_data1);
		writeUleb128Pair(c, DW_AT_stmt_list,DW_FORM_sec_offset);
		writeUleb128Pair(c, DW_AT_low_pc,DW_FORM_addr);
		writeUleb128Pair(c, DW_AT_high_pc,DW_FORM_addr);
		writeUleb128Pair(c, 0,0);
		writeUleb128(c, 2);
		writeUleb128(c, DW_TAG_subprogram);
		writeU8(c, 0); // has no children
		writeUleb128Pair(c, DW_AT_external,DW_FORM_flag);
		writeUleb128Pair(c, DW_AT_name,DW_FORM_string);
		writeUleb128Pair(c, DW_AT_decl_file,DW_FORM_data2);
		writeUleb128Pair(c, DW_AT_decl_line,DW_FORM_data2);
		writeUleb128Pair(c, DW_AT_decl_column,DW_FORM_data2);
		writeUleb128Pair(c, DW_AT_low_pc,DW_FORM_addr);
		writeUleb128Pair(c, DW_AT_high_pc,DW_FORM_addr);
			// "	# leb2_128 DW_AT_frame_base,??\n"
		writeUleb128Pair(c, 0,0);
		writeUleb128(c, 0);
		emitZString(c, debug_prelude);

		startSection(c, SectionDebugInfo);
		emitZString(c,
			"	.int .info_end - .info_start # unit_length\n"
			".info_start:\n"
			"	.short 4 # version\n"
			"	.int 0 # debug_abbrev_offset\n");
		writeU8(c, 8); // address_size
		flushit(params.out);

		/* compilation unit: */
		writeUleb128(c, 1);
		writeString(c, params.module_name);
		writeU8(c, DW_LANG_C99);
		emitZString(c,
			"	.int .lines\n"
			"	.quad .exec_base\n"
			"	.quad .exec_end\n");

		foreach (i, module) {
			StaticValue reloc = module.ptr[i];
			if (reloc.def_kind == Static_Function && reloc.def_state != Def_Undefined) {
				String name = reloc.name;
				Location reloc_loc = reloc.function_ir.locations[0];
				writeUleb128(c,   2);
				writeU8(c,        reloc.is_public);
				writeString(c,    name);
				writeU16(c,       reloc_loc.file_id);
				writeU16(c,       reloc_loc.line);
				writeU16(c,       reloc_loc.column);
				writeSymbolRef(c,    name);
				writeSymbolEndRef(c, name);
			}
		}

		writeUleb128(c, 0);
		emitZString(c, ".info_end:\n");
		startSection(c, SectionDebugLine);

		emitZString(c,
			".lines:\n"
			"	.int .line_end - .line_start # unit_length\n"
			".line_start:\n"
			"	.short 4 # version\n"
			"	.int .line_data - .line_header\n"
			".line_header:\n");

		writeU8(c, 1); // minimum_instruction_length
		writeU8(c, 1); // maximum_operations_per_instruction
		writeU8(c, 1); // default_is_stmt
		writeU8(c, 1); // line_base, for computing special ops.
		writeU8(c, 1); // line_range, for computing special ops.
		writeU8(c, 13); // opcode_base
		writeU8(c, 0); // standard_opcode_lengths
		writeU8(c, 1);
		writeU8(c, 1);
		writeU8(c, 1);
		writeU8(c, 1);
		writeU8(c, 0);
		writeU8(c, 0);
		writeU8(c, 0);
		writeU8(c, 1);
		writeU8(c, 0);
		writeU8(c, 0);
		writeU8(c, 1);

		// include_directories
		writeU8(c, 0); // T

		// file_names
		for (u32 i = 1; i < params.files.len; i++) {
			SourceFile *file = params.files.ptr[i];
			writeString(c, file->abs_name);
			writeUleb128(c, 0);
			writeUleb128(c, 0);
			writeUleb128(c, 0);
		}
		writeU8(c, 0);
		emitZString(c, ".line_data:\n");
		writeU8(c, 0); writeU8(c, 9); // extended opcode over 9 bytes
		writeU8(c, DW_LNE_set_address);
		emitZString(c, "	.quad .exec_base\n");

		Location prev = {0};
		foreach (i, line_marks) {
			DebugMark m = line_marks.ptr[i];
			Location loc = module.ptr[m.id].function_ir.locations[m.inst];

			if (loc.file_id != prev.file_id) {
				writeU8(c, DW_LNS_set_file);
				writeUleb128(c, loc.file_id);
			}
			if (loc.line != prev.line || loc.file_id != prev.file_id) {
				if (i == 0 || m.id != line_marks.ptr[i-1].id) {
					emit(c, "	line_inst I, S", loc.line, module.ptr[m.id].name);
					writeU8(c, DW_LNS_prologue_end);
				}
				emit(c, "	line_inst I, ..IiI", loc.line, m.id, m.inst);
			}
			prev = loc;
		}
		emitZString(c, "\n");
		writeU8(c, 0); writeU8(c, 9); // extended op
		writeU8(c, DW_LNE_set_address);
		emitZString(c, "	.quad .exec_end\n");
		writeU8(c, DW_LNS_advance_line);
		emitString(c, zstr("	.sleb128 1\n"));
		writeU8(c, 0); writeU8(c, 1); // extended op
		writeU8(c, DW_LNE_end_sequence);
		emitZString(c, ".line_end:\n");
	}
	free(line_marks.ptr);
	line_marks.len = 0;
	line_marks.capacity = 0;
	line_marks.ptr = 0;
	flushit(params.out);
}


static void emitName (Codegen *c, Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	if (reloc.name.len) {
		if (reloc.parent_decl != IDX_NONE) {
			emitName(c, module, reloc.parent_decl);
			*insert++ = '.';
		}
		emitString(c, reloc.name);
	} else {
		emitZString(c, "__");
		emitInt(c, id);
	}
}

static void emitData (Codegen *c, Module module, String data, References references) {
	if (references.len) {
		qsort(references.ptr, references.len, sizeof(references.ptr[0]),
			splice_dest_order);
		assert(!(references.len > 1 && references.ptr[0].splice_pos > references.ptr[1].splice_pos));
	}

	u32 pos = 0;
	foreach (i, references) {
		Reference ref = references.ptr[i];
		assert(ref.splice_pos >= pos);
		emitDataLine(c, ref.splice_pos - pos, data.ptr + pos);

		StaticValue reloc = module.ptr[ref.source_id];
		if (reloc.name.len)
			emit(c, " .quad S~L", reloc.name, ref.offset);
		else
			emit(c, " .quad __I~L", ref.source_id, ref.offset);
		pos = ref.splice_pos + 8;
	}

	emitDataLine(c, data.len - pos, data.ptr + pos);
}


static void movCI (Codegen *c, u32 val, IrRef dest) {
	movCM(c, c->ir.ptr[dest].size, val, (Mem) {c->storage[dest], RBP_8});
}
static Register movIR (Codegen *c, IrRef src, Register dest) {
	u16 size = c->ir.ptr[src].size;
	movMR(c, size, (Mem) {c->storage[src], RBP_8}, dest);
	return registerSized(dest, size);
}
static void movRI (Codegen *c, Register src, IrRef dest) {
	u16 size = c->ir.ptr[dest].size;
	movRM(c, size, src, (Mem) {c->storage[dest], RBP_8});
}

static void movIF (Codegen *c, IrRef src, VecRegister dest) {
	movMF(c, valueSize(c, src), (Mem) {c->storage[src], RBP_8}, dest);
}
static void movFI (Codegen *c, VecRegister src, IrRef dest) {
	movFM(c, valueSize(c, dest), src, (Mem) {c->storage[dest], RBP_8});
}


static void genIR (Codegen *c, BasicInst inst, IrRef src, Register dest) {
	genMR(c, valueSize(c, src), inst, (Mem) {c->storage[src], RBP}, dest);
}
static void genRI (Codegen *c, BasicInst inst, Register src, IrRef dest) {
	genRM(c, valueSize(c, dest), inst, src, (Mem) {c->storage[dest], RBP});
}
static void genCI (Codegen *c, BasicInst inst, i32 val, IrRef dest) {
	genCM(c, valueSize(c, dest), inst, val, (Mem) {c->storage[dest], RBP});
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
		.return_pointer_storage = -8,
		.ir = ir,
	};
	u32 stack_allocated = 0;

	// Register-save area for the general purpose and the SSE parameter registers.
	if (is_vararg) {
		stack_allocated += reg_save_area_size;
		c.return_pointer_storage -= reg_save_area_size;
	}

	// A memory return uses up the first parameter register for the
	// destination address.
	u32 gp_params = mem_return;
	u32 fp_params = 0;
	if (mem_return) {
		stack_allocated += 8;
	}

	// First mark parameters in order.
	foreach (i, ir.params) {
		ParameterClass class = classifyParam(c.target, ir.params.ptr[i].type);
		u32 gp_count = class.count - class.sse_count;
		if (gp_count > gp_parameter_regs_count - gp_params || class.sse_count > 8 - fp_params)
			class.count = 0;

		c.param_info.ptr[i].class = class;
		if (class.count > 0) {
			stack_allocated += 8 * class.count;
			c.param_info.ptr[i].storage = -(i32)stack_allocated;

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


	foreach (i, ir) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_Parameter)
			continue;
		stack_allocated += roundUp(inst.size);
		if (inst.kind == Ir_StackAllocFixed)
			stack_allocated += roundUp(inst.alloc.size);

		c.storage[i] = -(i32)stack_allocated;
	}

	// Align stack to 16 bytes.
	stack_allocated = (stack_allocated + 15) / 16 * 16;


	// No need to do callee-saves because those registers are never touched.

	push(&c, I64, RBP);
	movRR(&c, I64, RSP, RBP);
	genCR(&c, I64, ISub, stack_allocated, RSP);

	if (mem_return)
		movRM(&c, I64, RDI, (Mem) {c.return_pointer_storage, RBP_8});



	// 16 bytes for rbp and return address.
	u32 mem_params_offset = 16;

	// Copy regular parameters to the stack.
	foreach (i, c.param_info) {
		ParamInfo info = c.param_info.ptr[i];
		if (info.class.count) {
			for (u32 j = 0; j < info.class.count; j++) {
				u32 size = ir.params.ptr[i].size - j*8;
				if (size > 8) size = 8;

				if (info.class.registers[j] == Param_INTEGER) {
					Register reg = gp_parameter_regs[info.registers[j]];
					movRM(&c, size, reg, (Mem) {info.storage + 8*j, RBP_8});
				} else {
					assert(info.class.registers[j] == Param_SSE);
					movFM(&c, size, info.registers[j], (Mem) {info.storage + 8*j, RBP_8});
				}
			}
		} else {
			c.param_info.ptr[i].storage = mem_params_offset;
			mem_params_offset += roundUp(ir.params.ptr[i].size);
		}
	}

	if (is_vararg) {
		c.vaarg_reg_saves = -reg_save_area_size;
		c.vaarg_gp_offset = gp_params * 8;
		c.vaarg_fp_offset = 48 + fp_params * 8;
		c.vaarg_overflow_args = mem_params_offset;

		// Copy out the registers into the register save area.
		for (u32 i = gp_params; i < gp_parameter_regs_count; i++)
			movRM(&c, I64, gp_parameter_regs[i], (Mem) {-reg_save_area_size + i*8, RBP_8});
		testRR(&c, I32, RAX, RAX);
		Label no_float_args = newLabel(&c, "vaarg_no_float_args", id);
		jccL(&c, Cond_Equal, no_float_args);
		for (u32 i = 0; i < 8; i++)
			emit(&c, " movaps F, ~I(R)", XMM+i, -reg_save_area_size + 48 + i*16, RBP_8);
		placeLabel(&c, no_float_args);
	}


	// Initialize the StackAllocs.
	foreach (i, ir) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_StackAllocFixed) {
			genMR(&c, I64, ILea, (Mem) {c.storage[i]+8, RBP}, RAX);
			movRI(&c, RAX_8, i);
		}
	}

	foreach (i, linearized) {
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
		movMR(c, I64, (Mem) {offset+from_offset, from_addr}, R10);
		movRM(c, I64, R10, (Mem) {offset+to_offset, to_addr});
		offset += 8;
	}

	// FIXME This will break for non-power-of-two remainders.
	size -= offset;
	movMR(c, size, (Mem) {offset+from_offset, from_addr}, R10);
	movRM(c, size, R10, (Mem) {offset+to_offset, to_addr});
}

static void triple (Codegen *c, BasicInst inst, IrRef lhs, IrRef rhs, IrRef dest) {
	movIR(c, lhs, RAX);
	genIR(c, inst, rhs, RAX);
	movRI(c, RAX, dest);
}

static void ftriple (Codegen *c, const char *inst, IrRef lhs, IrRef rhs, IrRef dest) {
	const char *suff = sizeFSuffix(c->ir.ptr[dest].size);
	movIF(c, lhs, XMM+1);
	emit(c, " ZZ #, F", inst, suff, rhs, XMM+1);
	movFI(c, XMM+1, dest);
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
			jmpB(c, exit.unconditional);
		break;
	case Exit_Branch: {
		genCI(c, ITest, -1, exit.branch.condition);
		jccB(c, Cond_NotEqual, exit.branch.on_true);

		foreach (k, false_phis) {
			Inst inst  = c->ir.ptr[false_phis.ptr[k]];
			movRI(c, movIR(c, inst.phi_out.source, RAX),
				inst.phi_out.on_false);
		}

		if (exit.branch.on_false != next)
			jmpB(c, exit.branch.on_false);
	} break;
	case Exit_Switch: {
		assert(!false_phis.len);
		Cases cases = exit.switch_.cases;

		foreach (i, cases) {
			u32 size = valueSize(c, exit.switch_.value);
			if (size == 8) {
				emit(c, " movabsq $L, R", cases.ptr[i].value, RAX_8);
				genRI(c, ICmp, RAX, exit.switch_.value);
			} else {
				assert(size <= 4);
				u64 val = cases.ptr[i].value & 0xffffffff;
				if (val > INT32_MAX) val -= UINT32_MAX;
				genCI(c, ICmp, val, exit.switch_.value);
			}
			jccB(c, Cond_Equal, cases.ptr[i].dest);
		}
		if (exit.switch_.default_case != next)
			jccB(c, Cond_NotEqual, exit.switch_.default_case);
	} break;
	case Exit_Return:
		if (exit.ret != IDX_NONE) {
			if (c->ret_class.count == 0) {
				movMR(c, I64, (Mem) {c->return_pointer_storage, RBP_8}, RAX);
				copyTo(c, RAX, 0, RBP, c->storage[exit.ret], valueSize(c, exit.ret));
			} else {
				u32 gp_returns = 0;
				u32 fp_returns = 0;
				for (u32 j = 0; j < c->ret_class.count; j++) {
					u32 size = valueSize(c, exit.ret) - 8*j;
					if (size > 8) size = 8;
					i32 src = c->storage[exit.ret] + 8*j;
					if (c->ret_class.registers[j] == Param_INTEGER) {
						int reg = gp_returns == 0 ? RAX_8 : RDX_8;
						movMR(c, size, (Mem) {src, RBP}, reg);
						gp_returns++;
					} else {
						assert(c->ret_class.registers[j] == Param_SSE);
						movMF(c, size, (Mem) {src, RBP}, XMM+fp_returns);
						fp_returns++;
					}
				}
			}
		}
		movRR(c, I64, RBP, RSP);
		pop(c, I64, RBP);
		gen(c, I64, IRet);
		break;
	case Exit_None: unreachable;
	}

	free(false_phis.ptr);
}

static char *ir_names[] = {
	[Ir_Reloc] = "Reloc",
	[Ir_Constant] = "Constant",
	[Ir_Call] = "Call",
	[Ir_Parameter] = "Parameter",
	[Ir_PhiOut] = "PhiOut",
	[Ir_PhiIn] = "PhiIn",
	[Ir_StackAllocFixed] = "StackAllocFixed",
	[Ir_StackAllocVLA] = "StackAllocVLA",
	[Ir_StackDeallocVLA] = "StackDeallocVLA",
	[Ir_Copy] = "Copy",
	[Ir_Load] = "Load",
	[Ir_Store] = "Store",

	[Ir_Access] = "Access",
	[Ir_Truncate] = "Truncate",
	[Ir_SignExtend] = "SignExtend",
	[Ir_ZeroExtend] = "ZeroExtend",
	[Ir_UIntToFloat] = "UIntToFloat",
	[Ir_SIntToFloat] = "SIntToFloat",
	[Ir_FloatToSInt] = "FloatToSInt",
	[Ir_FloatToUInt] = "FloatToUInt",

	[Ir_Add] = "Add",
	[Ir_Sub] = "Sub",
	[Ir_Mul] = "Mul",
	[Ir_Div] = "Div",
	[Ir_SDiv] = "SDiv",
	[Ir_Mod] = "Mod",
	[Ir_SMod] = "SMod",

	[Ir_FAdd] = "FAdd",
	[Ir_FSub] = "FSub",
	[Ir_FMul] = "FMul",
	[Ir_FDiv] = "FDiv",
	[Ir_FMod] = "FMod",
	[Ir_FCast] = "FCast",

	[Ir_BitAnd] = "BitAnd",
	[Ir_BitOr] = "BitOr",
	[Ir_BitNot] = "BitNot",
	[Ir_BitXor] = "BitXor",
	[Ir_LessThan] = "LessThan",
	[Ir_SLessThan] = "SLessThan",
	[Ir_FLessThan] = "FLessThan",
	[Ir_LessThanOrEquals] = "LessThanOrEquals",
	[Ir_SLessThanOrEquals] = "SLessThanOrEquals",
	[Ir_FLessThanOrEquals] = "FLessThanOrEquals",
	[Ir_Equals] = "Equals",
	[Ir_FEquals] = "FEquals",
	[Ir_ShiftLeft] = "ShiftLeft",
	[Ir_ShiftRight] = "ShiftRight",

	[Ir_VaStart] = "VaStart",
	[Ir_VaArg] = "VaArg",
};

static void emitInstForward (Codegen *c, IrRef i) {
	assert(i != IDX_NONE);

	Inst inst = c->ir.ptr[i];

// 	emitZString(c, "# ");
// 	emitZString(c, ir_names[inst.kind]);
// 	emitZString(c, "\n");

	switch ((InstKind) inst.kind) {
	case Ir_Add: triple(c, IAdd, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Sub: triple(c, ISub, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Mul: triple(c, IIMul, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Div:
	case Ir_Mod: {
		movIR(c, inst.binop.lhs, RAX);
		genRR(c, I32, IXor, RDX, RDX);

		emit(c, " divZ #", sizeSuffix(inst.size), inst.binop.rhs);
		movRI(c, registerSized(inst.kind == Ir_Div ? RAX : RDX, inst.size), i);
	} break;
	case Ir_SDiv:
	case Ir_SMod: {
		movIR(c, inst.binop.lhs, RAX);
		switch (inst.size) {
		case 1: emit(c, " cbw"); break;
		case 2: emit(c, " cwd"); break;
		case 4: emit(c, " cdq"); break;
		case 8: emit(c, " cqo"); break;
		default: unreachable;
		}

		emit(c, " idivZ #", sizeSuffix(inst.size), inst.binop.rhs);
		movRI(c, registerSized(inst.kind == Ir_SDiv ? RAX : RDX, inst.size), i);
	} break;
	case Ir_FAdd: ftriple(c, "adds", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FSub: ftriple(c, "subs", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FMul: ftriple(c, "muls", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FDiv: ftriple(c, "divs", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_FMod: unreachable;

	case Ir_BitOr: triple(c, IOr, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitXor: triple(c, IXor, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitAnd: triple(c, IAnd, inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitNot: {
		Register reg = movIR(c, inst.unop, RAX);
		emit(c, " not R", reg);
		movRI(c, reg, i);
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
			u16 sz = valueSize(c, inst.binop.lhs);
			movIF(c, inst.binop.lhs, XMM+1);
			emit(c, " comisZ #, F", sizeFSuffix(sz), inst.binop.rhs, XMM+1);
		} else {
			movIR(c, inst.binop.rhs, RAX);
			genRI(c, ICmp, RAX, inst.binop.lhs);
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
		emit(c, " movzx R, R", RAX, RSI_8);
		movRI(c, registerSized(RSI, inst.size), i);
	} break;
	case Ir_ShiftLeft: {
		movIR(c, inst.binop.rhs, RCX);
		movIR(c, inst.binop.lhs, R8);
		genRR(c, inst.size, IShl, RCX, R8);
		movRI(c, R8, i);
	} break;
	case Ir_ShiftRight: {
		movIR(c, inst.binop.rhs, RCX);
		movIR(c, inst.binop.lhs, R8);
		genRR(c, inst.size, IShr, RCX, R8);
		movRI(c, R8, i);
	} break;
	case Ir_Truncate: {
		movMR(c, inst.size, (Mem) {c->storage[inst.unop], RBP_8}, RAX);
		movRI(c, RAX, i);
	} break;
	case Ir_SignExtend: {
		Register reg = registerSized(RAX, inst.size);
		bool dw = inst.size == 8 && valueSize(c, inst.unop) == 4;
		emit(c, " movsxZ #, R", (dw ? "d" : ""), inst.unop, reg);
		movRI(c, reg, i);
	} break;
	case Ir_ZeroExtend: {
		genRR(c, I32, IXor, RAX, RAX);
		movIR(c, inst.unop, registerSized(RAX, valueSize(c, inst.unop)));
		movRI(c, registerSized(RAX, inst.size), i);
	} break;
	case Ir_Store: {
		copyTo(c, movIR(c, inst.mem.address, RAX), 0, RBP, c->storage[inst.mem.source], inst.size);
	} break;
	case Ir_Load: {
		copyTo(c, RBP, c->storage[i], movIR(c, inst.mem.address, RAX), 0, inst.size);
	} break;
	case Ir_Access: {
		copyTo(c, RBP, c->storage[i],
			RBP, c->storage[inst.binop.lhs] + inst.binop.rhs, inst.size);
	} break;
	case Ir_Parameter: {
		ParamInfo info = c->param_info.ptr[inst.unop];
		c->storage[i] = info.storage;
	} break;
	case Ir_Constant:
		assert(inst.size <= 8);
		if (inst.size > 4 && inst.constant > INT32_MAX) {
			movCM(c, I32, inst.constant, (Mem) {c->storage[i], RBP_8});
			movCM(c, I32, (inst.constant >> 32), (Mem) {c->storage[i] + 4, RBP_8});
		} else {
			movCI(c, inst.constant, i);
		}
		break;
	case Ir_StackAllocFixed:
		break;
	case Ir_StackAllocVLA: {
		genMR(c, I64, ISub, (Mem) {c->storage[inst.unop], RBP}, RSP);
		movRI(c, RSP_8, i);
	} break;
	case Ir_StackDeallocVLA: {
		movIR(c, c->ir.ptr[inst.unop].unop, RSP_8);
	} break;
	case Ir_Reloc:
		assert(inst.size == 8);

		emitZString(c, " lea ");
		emitName(c, c->module, inst.reloc.id);
		emit(c, "~L(%rip), R", inst.reloc.offset, RAX_8);

		movRI(c, RAX_8, i);
		break;
	case Ir_PhiOut: {
		if (inst.phi_out.on_true != IDX_NONE) {
			copyTo(c, RBP, c->storage[inst.phi_out.on_true],
				RBP, c->storage[inst.phi_out.source],
				valueSize(c, inst.phi_out.source));
		}
	} break;
	case Ir_Copy: break;
	case Ir_PhiIn: break;
	case Ir_FCast: {
		u32 source = valueSize(c, inst.unop);
		const char *fsuff = sizeFSuffix(inst.size);
		emit(c, " cvtsZ2sZ #, F", sizeFSuffix(source), fsuff, inst.unop, XMM+1);
		movFI(c, XMM+1, i);
	} break;
	case Ir_UIntToFloat: {
		const char *fsuff = sizeFSuffix(inst.size);

		u32 src_size = valueSize(c, inst.unop);

		if (src_size == 8) {
			// x86 doesn't have a 64 bit int->float instruction, so we
			// use this polyfill, taken from GCC output.

			movIR(c, inst.unop, RAX);
			testRR(c, I64, RAX, RAX);

			Label negative = newLabel(c, "u2f_negative", i);
			Label done = newLabel(c, "u2f_done", i);
			jccL(c, Cond_Sign, negative);
			// When no sign bit is set, we can use the signed instruction.
			emit(c, " cvtsi2sZ R, F", fsuff, RAX_8, XMM+0);
			jmpL(c, done);

			placeLabel(c, negative);
			// Divide RAX by two, rounding to odd.
			movRR(c, I64, RAX, RDI);
			genCR(c, I64, IAnd, 1, RDI);
			emit(c, " shrq R", RAX_8);
			genRR(c, I64, IOr, RDI, RAX);

			// Now convert normally and multiply by 2. Now the lowest
			// bit would be lost, but it cannot be represented in the
			// mantissa anyways.
			emit(c, " cvtsi2sZ R, F", fsuff, RAX_8, XMM+0);
			emit(c, " addsZ F, F", fsuff, XMM+0, XMM+0);

			placeLabel(c, done);
		} else {
			genRR(c, I32, IXor, RAX, RAX);
			movIR(c, inst.unop, RAX);
			emit(c, " cvtsi2sZ R, F", fsuff, RAX_8, XMM+0);
		}
		movFI(c, XMM+0, i);
	} break;
	case Ir_SIntToFloat: {
		const char *fsuff = sizeFSuffix(inst.size);

		u32 src_size = valueSize(c, inst.unop);
		if (src_size != 8)
			emit(c, " movsxZ #, R", (src_size == 4 ? "d" : ""), inst.unop, RAX_8);
		else
			movIR(c, inst.unop, RAX_8);

		emit(c, " cvtsi2sZ R, F", fsuff, RAX_8, XMM+1);
		movFI(c, XMM+1, i);
	} break;
	case Ir_FloatToUInt: {
		u32 float_size = valueSize(c, inst.unop);
		const char *fsuff = sizeFSuffix(float_size);
		u32 dest_size = valueSize(c, inst.unop);

		if (dest_size == 8) {
			// Same problem as with Ir_UIntToFloat.

			// Load INT64_MAX as float.
			if (float_size == 4)
				movCR(c, I32, 0x5f000000, RAX);
			else
				emit(c, " movabsq $0x43e0000000000000, R", RAX_8);
			emit(c, " movd R, F", RAX_8, XMM+1);

			Label big = newLabel(c, "f2u_big", i);
			Label done = newLabel(c, "f2u_done", i);

			movMF(c, float_size, (Mem) {c->storage[inst.unop], RBP_8}, XMM+0);
			emit(c, " comisZ F, F", fsuff, XMM+1, XMM+0);
			jccL(c, Cond_UGreaterThanOrEquals, big);

			// When below INT64_MAX, we can use the signed instruction.
			emit(c, " cvttsZ2siq F, R", fsuff, XMM+0, RAX_8);
			jmpL(c, done);

			placeLabel(c, big);
			// Otherwise, subtract INT64_MAX as float, convert, set sign bit.
			emit(c, " subsZ F, F", fsuff, XMM+1, XMM+0);
			emit(c, " cvttsZ2siq F, R", fsuff, XMM+0, RAX_8);
			genCR(c, I64, IBtc, 63, RAX);

			placeLabel(c, done);
		} else {
			movMF(c, float_size, (Mem) {c->storage[inst.unop], RBP_8}, XMM+0);
			emit(c, " cvttsZ2si F, R", fsuff, XMM+0, RAX_8);
		}
		movRI(c, registerSized(RAX, inst.size), i);
	} break;
	case Ir_FloatToSInt: {
		u32 float_size = valueSize(c, inst.unop);
		const char *fsuff = sizeFSuffix(float_size);

		movMF(c, float_size, (Mem) {c->storage[inst.unop], RBP_8}, XMM+0);
		emit(c, " cvttsZ2si F, R", fsuff, XMM+0, RAX_8);
		movRI(c, registerSized(RAX, inst.size), i);
	} break;
	case Ir_VaStart: {
		movIR(c, inst.binop.lhs, RAX);
		// State of affairs: rhs should be the last parameter, but it is currently ignored.

		movCM(c, I32, c->vaarg_gp_offset, (Mem) {0, RAX_8});
		movCM(c, I32, c->vaarg_fp_offset, (Mem) {4, RAX_8});
		genMR(c, I64, ILea, (Mem) {c->vaarg_overflow_args, RBP}, RDX_8);
		movRM(c, I64, RDX, (Mem) {8, RAX_8});
		genMR(c, I64, ILea, (Mem) {c->vaarg_reg_saves, RBP}, RDX_8);
		movRM(c, I64, RDX, (Mem) {16, RAX_8});
	} break;
	case Ir_VaArg: {
		Type type = AUX_DATA(Type, c->ir, inst.unop_const.offset);
		ParameterClass class = classifyParam(c->target, type);

		// Put the va_list's address into RAX.
		movIR(c, inst.unop, RAX);

		Label done = newLabel(c, "vaarg_done", i);
		if (class.count) {
			u32 gp_count = class.count - class.sse_count;
			Label overflowarg = newLabel(c, "vaarg_overflowarg", i);

			if (gp_count) {
				// Test if all INTEGER parts would fit
				movMR(c, I32, (Mem) {0, RAX_8}, RDX);
				genCR(c, I32, ICmp, 48 - gp_count*8, RDX);
				jccL(c, Cond_UGreaterThan, overflowarg);
			}

			if (class.sse_count) {
				// Test if all SSE parts would fit
				movMR(c, I32, (Mem) {4, RAX_8}, RDX);
				genCR(c, I32, ICmp, reg_save_area_size - class.sse_count*8, RDX);
				jccL(c, Cond_UGreaterThan, overflowarg);
			}

			for (u32 j = 0; j < class.count; j++) {
				// Load the register offset to EDX and update it.
				if (class.registers[j] == Param_INTEGER) {
					movMR(c, I32, (Mem) {0, RAX_8}, RDX);
					genCM(c, I32, IAdd, 8, (Mem) {0, RAX});
				} else {
					movMR(c, I32, (Mem) {4, RAX_8}, RDX);
					genCM(c, I64, IAdd, 16, (Mem) {4, RAX});
				}
				// Add it to the reg_save_area.
				genMR(c, I64, IAdd, (Mem) {16, RAX}, RDX);

				movMR(c, I64, (Mem) {0, RDX_8}, RDX);
				movRM(c, I64, RDX, (Mem) {c->storage[i] + j*8, RBP_8});
			}
			jmpL(c, done);
			placeLabel(c, overflowarg);
		}
		movMR(c, I64, (Mem) {8, RAX_8}, RDX);
		genCM(c, I64, IAdd, inst.size, (Mem) {8, RAX});
		copyTo(c, RBP, c->storage[i], RDX, 0, inst.size);

		if (class.count)
			placeLabel(c, done);
	} break;
	case Ir_Call: {
		Call call = AUX_DATA(Call, c->ir, inst.call.data);
		ArgumentSpan args = call.arguments;
		ParameterClass ret_class = classifyParam(c->target, call.rettype);
		if (ret_class.sse_count > 2 || ret_class.count - ret_class.count > 2)
			ret_class.count = 0;

		bool memory_return = ret_class.count == 0;

		if (memory_return)
			genMR(c, I64, ILea, (Mem) {c->storage[i], RBP}, RDI);

		// STYLE All kinds of copypasta between parameter taking and
		// argument passing. Need to unify caller and callee definitions
		// per ABI. PERFORMANCE Could cache ABI calculations in the Type
		// data of the function prototype, for example.

		u32 gp_params = memory_return;
		u32 fp_params = 0;

		u32 arg_stack_memory = 0;
		ParameterClass *classes = malloc(args.len * sizeof(*classes));
		foreach (i, args) {
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
						movMR(c, size, (Mem) {c->storage[arg.arg_inst] + 8*j, RBP_8}, gp_parameter_regs[gp_params]);
						gp_params++;
					} else {
						assert(class.registers[j] == Param_SSE);
						movMF(c, size, (Mem) {c->storage[arg.arg_inst] + 8*j, RBP_8}, XMM+fp_params);
						fp_params++;
					}
				}
			} else {
				arg_stack_memory += param_size;
			}
		}
		arg_stack_memory = (arg_stack_memory + 15) / 16 * 16;

		u32 stack_filled = 0;
		foreach (i, args) {
			if (classes[i].count == 0) {
				IrRef arg = args.ptr[i].arg_inst;
				u16 param_size = valueSize(c, arg);
				copyTo(c, RSP, -(i32)arg_stack_memory + stack_filled, RBP, c->storage[arg], param_size);
				stack_filled += param_size;
			}
		}

		if (arg_stack_memory)
			genCR(c, I64, ISub, arg_stack_memory, RSP);

		if (inst.properties & Prop_Call_Vararg)
			movCR(c, I32, fp_params, RAX);

		emit(c, " call *~I(R)", c->storage[inst.call.function_ptr], RBP_8);
		if (arg_stack_memory)
			genCR(c, I64, IAdd, arg_stack_memory, RSP);

		if (!memory_return && inst.size) {
			u32 gp_returns = 0;
			u32 fp_returns = 0;
			for (u32 j = 0; j < ret_class.count; j++) {
				i32 dest_offset = c->storage[i] + 8*j;
				u32 size = inst.size - 8*j;
				if (size > 8) size = 8;

				if (ret_class.registers[j] == Param_INTEGER) {
					int reg = gp_returns == 0 ? RAX_8 : RDX_8;
					movRM(c, size, reg, (Mem) {dest_offset, RBP_8});
					gp_returns++;
				} else {
					assert(ret_class.registers[j] == Param_SSE);
					movFM(c, size, XMM+fp_returns, (Mem) {dest_offset, RBP_8});
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

static inline Register registerSize (u16 size) {
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
		foreach (i, m)
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


