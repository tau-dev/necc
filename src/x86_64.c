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
	u8 registers;
	Register reg1;
	Register reg2;
} ParamInfo;

typedef LIST(Location) Locations;

typedef struct {
	Arena *arena;
	FILE *out;
	FILE *debug_out;
	u32 current_id;
	IrList ir;
	Module module;
	FileList files;
	bool emit_stabs;

	StringMap *types;
	const Target *target;

	u16 *usage;
	Storage *storage;
	u32 stack_allocated;
	bool is_memory_return;
	u32 vaarg_gp_offset;
	u32 vaarg_reg_saves;
	u32 vaarg_overflow_args;
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

// Registers rbx, r12 - r15 are callee-save, rsp and rbp are special.
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

const Register parameter_regs[] = {
	RDI,
	RSI,
	RDX,
	RCX,
	R8,
	R9,
};
static const u32 parameter_regs_count = sizeof(parameter_regs) / sizeof(parameter_regs[0]);



static void emitData(Codegen *, Module, String data, References);
static void emitDataLine(FILE *, u32 len, const char *data);
static void emitDataString(u32 len, const char *data);
static void emitDataRaw(u32 len, const char *data);
static u32 typeDebugData(Codegen *, Type t);
static void emitName(Module module, u32 id);
static void emitJump(Codegen *, const char *inst, const Block *dest);
static void emitFunctionForward(EmitParams, u32);
static void emitBlockForward(Codegen *, Blocks, u32);
static void emitStabsLoc(Codegen *c, IrRef i);
static void emitInstForward(Codegen *c, IrRef i);
static inline u16 sizeOfRegister(Register size);
static inline Storage registerSize(u16 size);
static inline Register registerSized(Register, u16);
static const char *sizeOp(u16 size);
static const char *sizeSuffix(u16 size);
static const char *sizeFSuffix(u16 size);
static bool isMemory(u16 size);
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
// 	FILE *out, FILE *debug_out, Arena *arena, Module module, const Target *target;
	Codegen globals = {
		.out = params.out,
		.arena = params.arena,
		.emit_stabs = params.emit_debug_info,
		.target = params.target,
	};
	Module module = params.module;

	fprintf(params.out, ".intel_syntax noprefix\n\n");

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
	if (params.emit_debug_info) {
		emit(&globals, ".stabs \"S\",100,0,0,..text_begin",
			params.module_name);
	}
	emit(&globals, ".text\n"
		"..text_begin:");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state) {
			emitLabel(module, i);
			assert(reloc.type.kind == Kind_Function);
			emitFunctionForward(params, i);
		}
	}

	emit(&globals, "\n\n"
		".data");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
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
			&& reloc.def_state)
		{
			emitLabel(module, i);
			emitData(&globals, module, reloc.value_data, reloc.value_references);
		}
	}
	(void)emitData;

	flushit(params.out);
}

static void emitName (Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	if (reloc.name.len) {
		if (reloc.parent_decl != IDX_NONE) {
			emitName(module , reloc.parent_decl);
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

typedef struct TypeData {
	String view;
	Type type;
	u32 id;
} TypeData;

static void emitType(Codegen *, Type, u32 id);

static u32 typeDebugData (Codegen *c, Type t) {
	String view = {sizeof(t), (char *)&t};
	void **entry = mapGetOrCreate(c->types, view);
	if (*entry)
		return ((TypeData *)*entry)->id;

	*entry = ALLOC(c->arena, TypeData);
	u32 id = c->types->used;

	TypeData *data = *entry;
	data->id = id;
	data->type = t;
	data->view = (String) {sizeof(data->type), (char *) &data->type};

	emitType(c, t, id);
	return id;
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

// PERFORMANCE This is another absurd 14% of run time. Don't see a great
// way to accelerate it tho...
static void emitType (Codegen *c, Type type, u32 id) {
	Type res = resolveType(type);
	switch (res.kind) {
	case Kind_Union:
	case Kind_Struct:
		for (u32 i = 0; i < res.compound.members.len; i++)
			typeDebugData(c, res.compound.members.ptr[i].type);
		break;
	case Kind_Pointer:
		typeDebugData(c, *type.pointer);
		break;
	case Kind_Array:
	case Kind_VLArray:
	case Kind_UnsizedArray:
		typeDebugData(c, *type.array.inner);
		break;
	case Kind_Function:
	case Kind_FunctionPtr:
		typeDebugData(c, *type.function.rettype);
		for (u32 i = 0; i < type.function.parameters.len; i++) {
			typeDebugData(c, type.function.parameters.ptr[i].type);
		}
		break;
	case Kind_Basic:
	case Kind_Float:
	case Kind_Void:
	case Kind_Enum:
		break;
	default: unreachable;
	}

	emitZString(".stabs \"");

	char *name = printType(c->arena, type);
	emitZString(name);
	emitZString(":t");
	emitInt(id);
	emitZString("=");

	switch (res.kind) {
	case Kind_Enum:
		emitZString("-1");// TODO Portablility
		break;
	case Kind_Array:
	case Kind_VLArray:
	case Kind_UnsizedArray:
		if (res.kind == Kind_UnsizedArray || (res.kind == Kind_Array && res.array.count == 0)) {
			emitZString("A");
			emitInt(typeDebugData(c, *type.array.inner));
			break;
		}
		emitZString("ar-1;0;");
		if (res.kind == Kind_Array)
			emitInt(type.array.count);
		else
			emitZString("-1");
		emitZString(";");
		emitInt(typeDebugData(c, *type.array.inner));
		break;
	case Kind_FunctionPtr:
		*insert++ = '*';
		FALLTHROUGH;
	case Kind_Function: {
		*insert++ = 'f';
		emitInt(typeDebugData(c, *type.function.rettype));

		if (!type.function.missing_prototype) {
			*insert++ = ',';
			u32 count = type.function.parameters.len;
			emitInt(count);
			*insert++ = ';';
			for (u32 i = 0; i < type.function.parameters.len; i++) {
				Declaration decl = type.function.parameters.ptr[i];
				if (decl.name) {
					emitString(decl.name->name);
				} else {
					emitZString("__param_");
					emitInt(i);
				}
				*insert++ = ':';
				emitInt(typeDebugData(c, decl.type));
				emitZString(",1;");
			}
		}
		*insert++ = ';';

	} break;
	case Kind_Pointer:
		emitZString("*");
		emitInt(typeDebugData(c, *type.pointer));
		break;
	case Kind_Struct:
	case Kind_Union:
		emitZString(res.kind == Kind_Struct ? "s" : "u");
		emitInt(typeSize(res, c->target));
		for (u32 i = 0; i < res.compound.members.len; i++) {
			CompoundMember m = res.compound.members.ptr[i];
			emitString(m.name->name);
			emitZString(":");
			emitInt(typeDebugData(c, m.type));
			emitZString(",");
			emitInt(m.offset * 8);
			emitZString(",");
			emitInt(typeSize(m.type, c->target) * 8);
			emitZString(";");
		}
		emitZString(";");
		break;
	case Kind_Void:
		emitZString("-11"); break;
	case Kind_Float:
		switch (type.real) {
		case Float_Single: emitZString("-12"); break;
		case Float_Double: emitZString("-13"); break;
		case Float_LongDouble: emitZString("-14"); break;
		}
		break;
	case Kind_Basic:
		switch ((int) type.basic) {
		case Int_bool: emitZString("-5"); break;
		case Int_char: emitZString("-2"); break;
		case Int_suchar | Int_unsigned: emitZString("-5"); break;
		case Int_suchar: emitZString("-6"); break;
		case Int_short: emitZString("-3"); break;
		case Int_int: emitZString("-1"); break;
		case Int_long: emitZString("-4"); break;
		case Int_longlong: emitZString("-31"); break;
		case Int_short | Int_unsigned: emitZString("-7"); break;
		case Int_int | Int_unsigned: emitZString("-8"); break;
		case Int_long | Int_unsigned: emitZString("-10"); break;
		case Int_longlong | Int_unsigned: emitZString("-32"); break;
		default: unreachable;
		}
		break;
	default:
		unreachable;
	}
	emitZString("\",128,0,0,0\n");

	if (insert > buf + (BUF - MAX_LINE))
		flushit(c->out);
}

static u32 roundUp(u32 x)  {
	return ((x + 7) / 8) * 8;
}

static void emitFunctionForward (EmitParams params, u32 id) {
	// Arena *arena, FILE *out, Module module, u32 id, const Target *target
	Module module = params.module;

	StaticValue *reloc = &module.ptr[id];
	Block *entry = reloc->function_ir.entry;
	bool mem_return = isMemory(typeSize(*reloc->type.function.rettype, params.target));
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
		.emit_stabs = params.emit_debug_info,
		.files = params.files,
		.target = params.target,

		.types = &types,

		.is_memory_return = mem_return,
		.storage = calloc(ir.len, sizeof(Storage)),
		.module = module,
		.param_info = ALLOCN(params.arena, ParamInfo, ir.params.len),
		.ir = ir,
	};



	// A memory return uses up the first parameter register for the
	// destination address.
	u32 normal_params_found = mem_return;
	c.stack_allocated = 8 * mem_return;

	// First mark parameters in order.
	for (u32 i = 0; i < ir.params.len; i++) {
		u32 size = ir.params.ptr[i].size;
		bool bigg = size > 8;
		bool is_register = !isMemory(size) && normal_params_found + bigg < parameter_regs_count;
		c.param_info.ptr[i].registers = is_register ? 1 + bigg : 0;

		if (is_register) {
			u32 reduced_size = bigg ? 8 : size;
			c.param_info.ptr[i].reg1 = registerSized(parameter_regs[normal_params_found], reduced_size);
			normal_params_found++;

			if (bigg) {
				c.param_info.ptr[i].reg2 = registerSized(parameter_regs[normal_params_found], size - reduced_size);
				normal_params_found++;
			}

			c.param_info.ptr[i].storage = c.stack_allocated;
			c.stack_allocated += roundUp(size);
		}
	}

	if (is_vararg)
		c.stack_allocated = 48;


	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_Parameter)
			continue;
		c.storage[i] = c.stack_allocated;
		c.stack_allocated += roundUp(inst.size);

		if (inst.kind == Ir_StackAllocFixed)
			c.stack_allocated += roundUp(inst.alloc.size);
	}


	emit(&c, " push rbp");
	emit(&c, " mov rbp, rsp");
	emit(&c, " sub rsp, I", c.stack_allocated);

	if (c.is_memory_return)
		emit(&c, " mov qword ptr [rsp], rdi");

	// rbp and return address
	u32 mem_params = c.stack_allocated + 16;

	for (u32 i = 0; i < c.param_info.len; i++) {
		ParamInfo info = c.param_info.ptr[i];
		if (info.registers) {
			emit(&c, " mov Z [rsp+I], R", sizeOp(sizeOfRegister(info.reg1)), info.storage, info.reg1);

			if (info.registers == 2)
				emit(&c, " mov Z [rsp+I], R", sizeOp(sizeOfRegister(info.reg2)), info.storage + 8, info.reg2);
		} else {
			c.param_info.ptr[i].storage = mem_params;
			mem_params += roundUp(ir.params.ptr[i].size);
		}
	}

	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];

		if (inst.kind == Ir_StackAllocFixed) {
			emit(&c, " lea rax, [rsp+I]", c.storage[i]+8);
			emit(&c, " mov #, rax", i);
		}
	}

	if (is_vararg) {
		c.vaarg_reg_saves = 0;
		c.vaarg_gp_offset = normal_params_found * 8;
		c.vaarg_overflow_args = mem_params;
		for (u32 i = normal_params_found; i < parameter_regs_count; i++) {
			emit(&c, " mov qword ptr [rsp+I], R", i*8, registerSized(parameter_regs[i], I64));
		}
	}

	for (u32 i = 0; i < linearized.len; i++) {
		emitBlockForward(&c, linearized, i);
	}
	free(linearized.ptr);
	emit(&c, "");




	if (c.emit_stabs) {
		emit(&c, ".stabs \"S:ZI\",36,0,0,S\n", reloc->name,
			reloc->is_public ? "F" : "f",
			typeDebugData(&c, *reloc->type.function.rettype), reloc->name);
		for (u32 i = 0; i < ir.len; i++) {
			Inst inst = ir.ptr[i];

			if (inst.kind == Ir_StackAllocFixed) {
				if (c.emit_stabs && inst.alloc.decl_data != IDX_NONE) {
					Declaration decl = AUX_DATA(Declaration, ir, inst.alloc.decl_data);
					emit(&c, ".stabs \"S:I\",128,0,0,~I", decl.name->name, typeDebugData(&c, decl.type),
						(i32) (c.storage[i]+8) - c.stack_allocated);
				}
			}
		}
	}

	free(c.storage);
	mapFree(c.types);
}


static void copyTo (Codegen *c, Register to_addr, i32 to_offset, Register from_addr, i32 from_offset, u16 size) {
	i32 offset = 0;
	to_addr = registerSized(to_addr, 8);
	from_addr = registerSized(from_addr, 8);
	while (size - offset > 8) {
		emit(c, " mov r8, qword ptr [R~I]", from_addr, offset+from_offset);
		emit(c, " mov qword ptr [R~I], r8", to_addr, offset+to_offset);
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


static bool loadMaybeBigTo (Codegen *c, Register reg1, Register reg2, IrRef i) {
	u16 size = valueSize(c, i);
	bool bigg = size > 8;

	u32 reduced_size = bigg ? 8 : size;
	emit(c, " mov R, Z [rsp+I]", registerSized(reg1, reduced_size), sizeOp(reduced_size), c->storage[i]);

	if (bigg) {
		u32 rest = size - reduced_size;
		emit(c, " mov R, Z [rsp+I]", registerSized(reg2, rest), sizeOp(rest), c->storage[i] + 8);
	}

	return bigg;
}


static void triple (Codegen *c, const char *inst, u16 size, IrRef lhs, IrRef rhs, IrRef dest) {
	Register rax = loadTo(c, RAX, lhs);
	emit(c, " ZZ R, #", inst, sizeSuffix(size), rax, rhs);
	emit(c, " mov #, R", dest, rax);
}

static void ftriple (Codegen *c, const char *inst, u16 size, IrRef lhs, IrRef rhs, IrRef dest) {
	const char *suff = sizeFSuffix(size);
	emit(c, " movsZ xmm1, #", suff, lhs);
	emit(c, " ZZ xmm1, #", inst, suff, rhs);
	emit(c, " movsZ #, xmm1", suff, dest);
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
	if (c->emit_stabs && block->first_inst < block->inst_end)
		emitStabsLoc(c, block->first_inst);

	IrRefList false_phis = {0};

	for (IrRef ref = block->first_inst; ref < block->inst_end; ref++) {
		if (c->emit_stabs && ref > block->first_inst)
			emitStabsLoc(c, ref);
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
			if (c->is_memory_return) {
				emit(c, " mov rax, qword ptr [rsp]");
				copyTo(c, RAX, 0, RSP, c->storage[exit.ret], valueSize(c, exit.ret));
			} else {
				loadMaybeBigTo(c, RAX, RDX, exit.ret);
			}
		}
// 		emit(c, " add rsp, I", c->stack_allocated);
		emit(c, " mov rsp, rbp");
		emit(c, " pop rbp");
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
	case Ir_SDiv: {
		emit(c, " xor rdx, rdx");
		loadTo(c, RAX, inst.binop.lhs);

		const char *op = inst.kind == Ir_Div ? "div" : "idiv";
		emit(c, " ZZ #", op, sizeSuffix(inst.size), inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
	} break;
	case Ir_Mod:
	case Ir_SMod: {
		emit(c, " xor rdx, rdx");
		loadTo(c, RAX, inst.binop.lhs);

		const char *op = inst.kind == Ir_Mod ? "div" : "idiv";
		emit(c, " ZZ #", op, sizeSuffix(inst.size), inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(RDX, inst.size));
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
			emit(c, " movsZ xmm1, #", sizeFSuffix(sz), inst.binop.lhs);
			emit(c, " comisZ xmm1, #", sizeFSuffix(sz), inst.binop.rhs);
		} else {
			emit(c, " cmp #, R", inst.binop.lhs, loadTo(c, RAX, inst.binop.rhs));
		}
		switch (inst.kind) {
			case Ir_LessThan:
			case Ir_FLessThan:
				emit(c, " setb al"); break;
			case Ir_SLessThan:
				emit(c, " setl al"); break;
			case Ir_LessThanOrEquals:
			case Ir_FLessThanOrEquals:
				emit(c, " setbe al"); break;
			case Ir_SLessThanOrEquals:
				emit(c, " setle al"); break;
			case Ir_Equals:
			case Ir_FEquals:
				emit(c, " sete al"); break;
		}
		emit(c, " movzx rsi, al");
		emit(c, " mov #, R", i, registerSized(RSI, inst.size));
	} break;
	case Ir_ShiftLeft: {
		loadTo(c, RCX, inst.binop.rhs);
		Register shiftee = loadTo(c, R8, inst.binop.lhs);
		emit(c, " xor rax, rax");
		emit(c, " shld R, R, cl", shiftee, registerSized(RAX, inst.size));
		emit(c, " mov #, R", i, shiftee);
	} break;
	case Ir_ShiftRight: {
		loadTo(c, RCX, inst.binop.rhs);
		Register shiftee = loadTo(c, R8, inst.binop.lhs);
		emit(c, " xor rax, rax");
		emit(c, " shrd R, R, cl", shiftee, registerSized(RAX, inst.size));
		emit(c, " mov #, R", i, shiftee);
		loadTo(c, RCX, inst.binop.rhs);
	} break;
	case Ir_Truncate: {
		Register reg = registerSized(RAX, c->ir.ptr[i].size);
		emit(c, " mov R, [rsp+I]", reg, c->storage[inst.unop]);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_SignExtend: {
		Register reg = registerSized(RSI, inst.size);
		emit(c, " movsxZ R, #",
			(valueSize(c, inst.unop) > 2 ? "d" : ""), reg, inst.unop);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_ZeroExtend: {
		emit(c, " xor rax, rax");
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
			emit(c, " mov Z [rsp+I], ~I", sizeOp(4), c->storage[i], (i32) inst.constant);
			emit(c, " mov Z [rsp+I], ~I", sizeOp(4), c->storage[i] + 4, (i32) (inst.constant >> 32));
// 		} else if (inst.size == 4) {
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

		emitZString(" mov rax, offset flat:");
		emitName(c->module, inst.reloc.id);
		emit(c, "~L", inst.reloc.offset);

		emit(c, " mov #, rax", i);
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
		emit(c, " cvtsZ2sZ xmm1, #", sizeFSuffix(source), fsuff, inst.unop);
		emit(c, " movsZ #, xmm1", fsuff, i);
	} break;
	case Ir_UIntToFloat: // TODO This is a hack, must be handled differently.
	case Ir_SIntToFloat: {
		const char *fsuff = sizeFSuffix(inst.size);
		loadTo(c, RAX, inst.unop);

		emit(c, " cvtsi2sZ xmm1, rax", fsuff);
		emit(c, " movsZ #, xmm1", fsuff, i);
	} break;
	case Ir_FloatToUInt:
	case Ir_FloatToSInt: {
		const char *fsuff = sizeFSuffix(c->ir.ptr[inst.unop].size);

		emit(c, " movsZ xmm0, [rsp+I]", fsuff, c->storage[inst.unop]);
		emit(c, " cvttsZ2si rax, xmm0", fsuff);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
	} break;
	case Ir_VaStart: {
		loadTo(c, RAX, inst.binop.lhs);
		// State of affairs: rhs should be the last parameter, but it is currently ignored.

		emit(c, " mov dword ptr [rax], I", c->vaarg_gp_offset);
		emit(c, " mov dword ptr [rax+4], 48");
		emit(c, " lea rdx, [rsp+I]", c->stack_allocated + 8);
		emit(c, " mov qword ptr [rax+8], rdx");
		emit(c, " lea rdx, [rsp+I]", c->vaarg_reg_saves);
		emit(c, " mov qword ptr [rax+16], rdx");
	} break;
	case Ir_VaArg: {
		loadTo(c, RAX, inst.unop);
		if (!isMemory(inst.size)) {
			emit(c, " xor rdx, rdx");
			emit(c, " mov edx, dword ptr [rax]");
			emit(c, " cmp edx, I", inst.size <= 8 ? 48 : 40);
			emit(c, " jae .vaarg_I_overflowarg", i);

			emit(c, " add rdx, qword ptr [rax+16]");
			emit(c, " add dword ptr [rax], I", inst.size <= 8 ? 8 : 16);
			emit(c, " jmp .vaarg_I_done", i);

			emit(c, ".vaarg_I_overflowarg:", i);
		}
		emit(c, " mov rdx, qword ptr [rax+8]");
		emit(c, " add qword ptr [rax+8], I", inst.size);

		emit(c, ".vaarg_I_done:", i);
		copyTo(c, RSP, c->storage[i], RDX, 0, inst.size);
	} break;
	case Ir_Call: {
		Call call = AUX_DATA(Call, c->ir, inst.call.data);
		ValuesSpan args = call.arguments;
		bool memory_return = isMemory(inst.size);

// 		emit(c, "	; call");
		u32 param_slot = memory_return;
		if (memory_return)
			emit(c, " lea rdi, #", i);

		// STYLE All kinds of copypasta between parameter taking and
		// argument passing. Need to unify caller and callee definitions
		// per ABI.
		u32 stack_memory = 0;
		for (u32 p = 0; p < args.len; p++) {
			u32 size = valueSize(c, args.ptr[p]);
			bool bigg = size > 8;
			if (isMemory(size) || param_slot + bigg >= parameter_regs_count)
				stack_memory += size;
			else
				param_slot += 1 + bigg;
		}

		i32 stack_mem_pos = -stack_memory;

		param_slot = memory_return;
		for (u32 p = 0; p < args.len; p++) {
			IrRef param = args.ptr[p];
			u16 param_size = valueSize(c, param);
			bool bigg = param_size > 8;
			if (isMemory(param_size) || param_slot + bigg >= parameter_regs_count) {
				copyTo(c, RSP, stack_mem_pos, RSP, c->storage[param], param_size);
				stack_mem_pos += param_size;
			} else if (bigg) {
				loadMaybeBigTo(c, parameter_regs[param_slot], parameter_regs[param_slot+1], param);
				param_slot += 2;
			} else {
				loadTo(c, parameter_regs[param_slot], param);
				param_slot += 1;
			}
		}
		assert(stack_mem_pos == 0);


		if (stack_memory)
			emit(c, " sub rsp, I", stack_memory);

		// Necessary for va-arg calls. TODO Mark up call instructions with that info.
		emit(c, " mov eax, 0", stack_memory);
// 		emit(c, " mov r10, qwor[rsp+I]", stack_memory);
		emit(c, " call qword ptr [rsp+I]", c->storage[inst.call.function_ptr] + stack_memory);
		if (stack_memory)
			emit(c, " add rsp, I", stack_memory);

		if (!memory_return) {
			bool bigg = inst.size > 8;
			u32 reduced = bigg ? 8 : inst.size;
			emit(c, " mov Z [rsp+I], R", sizeOp(reduced), c->storage[i], registerSized(RAX, reduced));
			if (bigg) {
				emit(c, " mov Z [rsp+I], R", sizeOp(inst.size - 8), c->storage[i]+8, registerSized(RDX, inst.size - 8));
			}
		}
	} break;
	}
}

static void emitStabsLoc (Codegen *c, u32 i) {
	Location loc = c->ir.locations[i];

	bool labeled = false;
	if (i == 0 || c->ir.locations[i-1].file_id != loc.file_id) {
		SourceFile *src = c->files.ptr[loc.file_id];
		emit(c, ".stabs \"SS\",132,0,0,..IiI",
			src->path, src->name, c->current_id, i);
		labeled = true;
	}
	if (i == 0 || c->ir.locations[i-1].line != loc.line) {
		emit(c, ".stabn 68,0,I,..IiI",
			loc.line, c->current_id, i);
		labeled = true;
	}

	if (labeled)
		emit(c, "..IiI:", c->current_id, i);
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

static inline Register registerSized(Register stor, u16 size) {
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

static bool isMemory(u16 size) {
	return size > 16;
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
	memcpy(insert, data, len);
	insert += len;
	memcpy(insert, "\"\n", 2);
	insert += 2;
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
			memcpy(insert, " [rsp+", 6);
			insert += 6;
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
