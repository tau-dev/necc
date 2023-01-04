#include "analysis.h"
#include "types.h"
#include "parse.h"


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

typedef struct {
	Arena *arena;
	FILE *out;
	FILE *debugout;
	IrList ir;
	Module module;

	u16 *usage;
	Storage *storage;
	IrRef used_registers[GENERAL_PURPOSE_REGS_END];
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
static void emitName(Codegen *, Module module, u32 id);
static void emitJump(Codegen *, const char *inst, const Block *dest);
static void emitFunctionForward(Arena *, FILE *, Module, StaticValue *, const Target *);
static void emitBlockForward(Codegen *, Block *);
static void emitInstForward(Codegen *c, IrRef i);
static inline Storage registerSize(u16 size);
static inline Register registerSized(Register, u16);
static const char *sizeOp(u16 size);
static bool isMemory(u16 size);
static u16 valueSize(Codegen *, IrRef);
static void emit(Codegen *c, const char *fmt, ...);
static void flushit(FILE *f);

int splice_dest_order(const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

void emitX64AsmSimple(FILE *out, Arena *arena, Module module, const Target *target) {
	Codegen globals = {.out = out};
#ifndef NDEBUG
	globals.debugout = stderr;
#endif
	fprintf(out, "use64\nformat ELF64\n");

	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.name.len == 0) {
			assert(reloc.def_state != Def_Undefined);
			assert(!reloc.is_public);
			continue;
		}
		if (reloc.def_state == Def_Undefined)
			fprintf(out, "extrn '%.*s' as _%.*s	; %lu\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name), (ulong) i);
		else if (reloc.is_public)
			fprintf(out, "public _%.*s as '%.*s'\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name));
	}

	emit(&globals, "\n\n"
	     "section '.text' executable");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state) {
			emit(&globals, "_S:", reloc.name);
			assert(reloc.type.kind == Kind_Function);
			emitFunctionForward(arena, out, module, &module.ptr[i], target);
		}
	}

	emit(&globals, "\n\n"
	     "section '.data' writeable");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(&globals, module, i);
			emitData(&globals, module, reloc.value_data, reloc.value_references);
		}
	}

	emit(&globals, "\n\n"
	     "section '.rodata'\n"
	     "db 0"); // Ugly hack, the linker complains about “section `.rodata' type changed to PROGBITS” otherwise
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(&globals, module, i);
			emitData(&globals, module, reloc.value_data, reloc.value_references);
		}
	}
	(void)emitData;
	flushit(out);
}

static void emitName (Codegen *c, Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	emit(c, "align 8");
	if (reloc.name.len)
		emit(c, "_S: ; I bytes", reloc.name, reloc.value_data.len);
	else
		emit(c, "__I: ; I bytes", id, reloc.value_data.len);
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
			emit(c, " dq _S~L", reloc.name, ref.offset);
		else
			emit(c, " dq __I~L", ref.source_id, ref.offset);
		pos = ref.splice_pos + 8;
	}

	emitDataLine(c->out, data.len - pos, data.ptr + pos);
}

static u32 roundUp(u32 x)  {
	return ((x + 7) / 8) * 8;
}

static void emitFunctionForward (Arena *arena, FILE *out, Module module, StaticValue *reloc, const Target *target) {
// 	return;
	Block *entry = reloc->function_ir.entry;
	bool mem_return = isMemory(typeSize(*reloc->type.function.rettype, target));
	bool is_vararg = reloc->type.function.is_vararg;


	Blocks linearized = {0};
	scheduleBlocksStraight(arena, entry, &linearized);
	assert(linearized.len);
	decimateIr(&reloc->function_ir, linearized);
// 	calcUsage(ir, c.usage);
	free(linearized.ptr);

	IrList ir = reloc->function_ir;

	Codegen c = {
		.arena = arena,
		.out = out,
		.is_memory_return = mem_return,
		.storage = calloc(ir.len, sizeof(Storage)),
		.module = module,
		.param_info = ALLOCN(arena, ParamInfo, ir.params.len),
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

		if (inst.kind == Ir_StackAllocFixed) {
			ir.ptr[i].alloc.known_offset = c.stack_allocated;
			c.stack_allocated += roundUp(inst.alloc.size);
		}
		if (inst.kind == Ir_Parameter)
			continue;
		c.storage[i] = c.stack_allocated;
		c.stack_allocated += roundUp(inst.size);
	}


	emit(&c, " sub rsp, I", c.stack_allocated);

	if (c.is_memory_return)
		emit(&c, " mov [rsp], rdi");

	u32 mem_params = c.stack_allocated + 8;

	for (u32 i = 0; i < c.param_info.len; i++) {
		ParamInfo info = c.param_info.ptr[i];
		if (info.registers) {
			emit(&c, " mov [rsp+I], R", info.storage, info.reg1);

			if (info.registers == 2)
				emit(&c, "mov [rsp+I], R", info.storage + 8, info.reg2);
		} else {
			c.param_info.ptr[i].storage = mem_params;
			mem_params += roundUp(ir.params.ptr[i].size);
		}
	}

	if (is_vararg) {
		c.vaarg_reg_saves = 0;
		c.vaarg_gp_offset = normal_params_found * 8;
		c.vaarg_overflow_args = mem_params;
		for (u32 i = normal_params_found; i < parameter_regs_count; i++) {
			emit(&c, " mov [rsp+I], R", i*8, registerSized(parameter_regs[i], I64));
		}
	}

	emitBlockForward(&c, entry);
	emit(&c, "");

	free(c.storage);
}


static void copyTo (Codegen *c, Register to_addr, i32 to_offset, Register from_addr, i32 from_offset, u16 size) {
	i32 offset = 0;
	to_addr = registerSized(to_addr, 8);
	from_addr = registerSized(from_addr, 8);
	while (size - offset > 8) {
		emit(c, " mov r8, [R~I]", from_addr, offset+from_offset);
		emit(c, " mov [R~I], r8", to_addr, offset+to_offset);
		offset += 8;
	}

	// This will break for non-power-of-two remainders.
	Register tmp = registerSized(R8, size - offset);
	emit(c, " mov R, [R~I]", tmp, from_addr, offset+from_offset);
	emit(c, " mov [R~I], R", to_addr, offset+to_offset, tmp);
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
	emit(c, " mov R, #", registerSized(reg1, reduced_size), i);

	if (bigg)
		emit(c, " mov R, [rsp+I]", registerSized(reg2, size - reduced_size), c->storage[i] + 8);

	return bigg;
}


static void triple (Codegen *c, const char *inst, IrRef lhs, IrRef rhs, IrRef dest) {
	Register rax = loadTo(c, RAX, lhs);
	emit(c, " Z R, #", inst, rax, rhs);
	emit(c, " mov #, R", dest, rax);
}

// This backend performs _no_ register allocation.
static void emitBlockForward (Codegen *c, Block *block) {
	const u32 visit_id = 1;
	if (block->visited == visit_id) return;
	block->visited = visit_id;

	assert(block->exit.kind != Exit_None);


	emit(c, ".S_I:", block->label, block->id);

	IrRefList false_phis = {0};

	for (u32 i = block->first_inst; i < block->inst_end; i++) {
		IrRef ref = i;

		emitInstForward(c, ref);

		Inst inst = c->ir.ptr[ref];
		if (inst.kind == Ir_PhiOut && inst.phi_out.on_false != IR_REF_NONE)
			PUSH(false_phis, ref);
	}

	Exit exit = block->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		if (exit.unconditional->visited == visit_id)
			emitJump(c, "jmp", exit.unconditional);

		emitBlockForward(c, exit.unconditional);
		break;
	case Exit_Branch: {
		emit(c, " test #, -1", exit.branch.condition);
		emitJump(c, "jnz", exit.branch.on_true);


		for (u32 k = 0; k < false_phis.len; k++) {
			Inst inst  = c->ir.ptr[false_phis.ptr[k]];
			emit(c, " mov #, R	; phi-out",
				inst.phi_out.on_false, loadTo(c, RAX, inst.phi_out.source));
		}

		if (exit.branch.on_false->visited == visit_id)
			emitJump(c, "jmp", exit.branch.on_false);

		emitBlockForward(c, exit.branch.on_false);
		emitBlockForward(c, exit.branch.on_true);
	} break;
	case Exit_Switch: {
		assert(!false_phis.len);
		Cases cases = exit.switch_.cases;

		for (u32 i = 0; i < cases.len; i++) {
			emit(c, " cmp #, L", exit.switch_.value, cases.ptr[i].value);
			emitJump(c, "je", cases.ptr[i].dest);
		}
		emitJump(c, "jnz", exit.switch_.default_case);
		for (u32 i = 0; i < cases.len; i++) {
			emitBlockForward(c, cases.ptr[i].dest);
		}
		emitBlockForward(c, exit.switch_.default_case);
	} break;
	case Exit_Return:
		if (exit.ret != IR_REF_NONE) {
			if (c->is_memory_return) {
				emit(c, " mov rax, [rsp+I]", c->stack_allocated);
				copyTo(c, RAX, 0, RSP, c->storage[exit.ret], valueSize(c, exit.ret));
			} else {
				loadMaybeBigTo(c, RAX, RDX, exit.ret);
			}
		}
		emit(c, " add rsp, I", c->stack_allocated + (c->is_memory_return ? 8 : 0));
		emit(c, " ret");
		break;
	case Exit_None: unreachable;
	}

	free(false_phis.ptr);
}


static void emitInstForward(Codegen *c, IrRef i) {
	assert(i != IR_REF_NONE);

	Inst inst = c->ir.ptr[i];

	switch ((InstKind) inst.kind) {
	case Ir_Add: triple(c, "add", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Sub: triple(c, "sub", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_SMul: triple(c, "imul", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_Mul:
	case Ir_Div:
	case Ir_SDiv: {
		emit(c, " xor rdx, rdx");
		loadTo(c, RAX, inst.binop.lhs);

		const char *op = inst.kind == Ir_Mul ? "mul" :
				inst.kind == Ir_Div ? "div" : "idiv";
		emit(c, " Z #", op, inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(RAX, inst.size));
	} break;
	case Ir_Mod:
	case Ir_SMod: {
		emit(c, " xor rdx, rdx");
		loadTo(c, RAX, inst.binop.lhs);

		const char *op = inst.kind == Ir_Mod ? "div" : "idiv";
		emit(c, " Z #", op, inst.binop.rhs);
		emit(c, " mov #, R", i, registerSized(RDX, inst.size));
	} break;
	case Ir_BitOr: triple(c, "or", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitXor: triple(c, "xor", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitAnd: triple(c, "and", inst.binop.lhs, inst.binop.rhs, i); break;
	case Ir_BitNot: {
		Register reg = loadTo(c, RAX, inst.unop);
		emit(c, " not R", reg);
		emit(c, " mov #, R", i, reg);
	} break;
	case Ir_LessThan: {
		emit(c, " cmp #, R", inst.binop.lhs, loadTo(c, RAX, inst.binop.rhs));
		emit(c, " setl al");
		emit(c, " movzx rsi, al");
		emit(c, " mov #, R", i, registerSized(RSI, inst.size));
	} break;
	case Ir_LessThanOrEquals: {
		emit(c, " cmp #, R", inst.binop.lhs, loadTo(c, RAX, inst.binop.rhs));
		emit(c, " setle al");
		emit(c, " movzx rsi, al");
		emit(c, " mov #, R", i, registerSized(RSI, inst.size));
	} break;
	case Ir_Equals: {
		emit(c, " cmp #, R", inst.binop.lhs, loadTo(c, RAX, inst.binop.rhs));
		emit(c, " sete al");
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
	case Ir_Store:
	case Ir_StoreVolatile: {
		copyTo(c, loadTo(c, RAX, inst.mem.address), 0, RSP, c->storage[inst.mem.source], inst.size);
	} break;
	case Ir_Load:
	case Ir_LoadVolatile: {
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
		emit(c, " lea rax, [rsp+I]	; alloc", inst.alloc.known_offset);
		emit(c, " mov #, rax", i);
		break;
	case Ir_StackAllocVLA:
		unreachable;
	case Ir_Reloc:
		assert(inst.size == 8);
		String name = c->module.ptr[inst.reloc.id].name;
		if (name.len)
			emit(c, " mov #, _S~L", i, name, inst.reloc.offset);
		else
			emit(c, " mov #, __I~L", i, inst.reloc.id, inst.reloc.offset);
		break;
	case Ir_PhiOut: {
		if (inst.phi_out.on_true != IR_REF_NONE) {
			copyTo(c, RSP, c->storage[inst.phi_out.on_true],
				RSP, c->storage[inst.phi_out.source],
				valueSize(c, inst.phi_out.source));
		}
	} break;
	case Ir_Copy: break;
	case Ir_PhiIn: break;
	case Ir_StackDeallocVLA: break;
	case Ir_IntToFloat:
	case Ir_FloatToInt: {
		assert(!"TODO codegen float stuff");
	} break;
	case Ir_VaStart: {
		loadTo(c, RAX, inst.binop.lhs);
		// State of affairs: rhs should be the last parameter, but it is currently ignored.

		emit(c, " mov dword [rax], I", c->vaarg_gp_offset);
		emit(c, " mov dword [rax+4], 48");
		emit(c, " lea rdx, [rsp+I]", c->stack_allocated + 8);
		emit(c, " mov qword [rax+8], rdx");
		emit(c, " lea rdx, [rsp+I]", c->vaarg_reg_saves);
		emit(c, " mov qword [rax+16], rdx");
	} break;
	case Ir_VaArg: {
		loadTo(c, RAX, inst.unop);
		if (!isMemory(inst.size)) {
			emit(c, " xor rdx, rdx");
			emit(c, " mov edx, dword [rax]");
			emit(c, " cmp edx, I", inst.size <= 8 ? 48 : 40);
			emit(c, " jae .vaarg_I_overflowarg", i);

			emit(c, " add rdx, qword [rax+16]");
			emit(c, " add dword [rax], I", inst.size <= 8 ? 8 : 16);
			emit(c, " jmp .vaarg_I_done", i);

			emit(c, ".vaarg_I_overflowarg:", i);
		}
		emit(c, " mov rdx, qword [rax+8]");
		emit(c, " add qword [rax+8], I", inst.size);

		emit(c, ".vaarg_I_done:", i);
		copyTo(c, RSP, c->storage[i], RDX, 0, inst.size);
	} break;
	case Ir_Call: {
		ValuesSpan params = inst.call.parameters;
		bool memory_return = isMemory(inst.size);

		emit(c, "	; call");
		u32 param_slot = memory_return;
		if (memory_return)
			emit(c, " lea rdi, #", i);

		// STYLE All kinds of copypasta between parameter taking and
		// argument passing. Need to unify caller and callee definitions
		// per ABI.
		u32 stack_memory = 0;
		for (u32 p = 0; p < params.len; p++) {
			u32 size = valueSize(c, params.ptr[p]);
			bool bigg = size > 8;
			if (isMemory(size) || param_slot + bigg >= parameter_regs_count)
				stack_memory += size;
			else
				param_slot += 1 + bigg;
		}

		i32 stack_mem_pos = -stack_memory;

		param_slot = memory_return;
		for (u32 p = 0; p < params.len; p++) {
			IrRef param = params.ptr[p];
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
		emit(c, " call qword [rsp+I]", c->storage[inst.call.function_ptr] + stack_memory);
		if (stack_memory)
			emit(c, " add rsp, I", stack_memory);

		if (!memory_return) {
			bool bigg = inst.size > 8;
			emit(c, " mov #, R", i, registerSized(RAX, bigg ? 8 : inst.size));
			if (bigg) {
				emit(c, " mov [rsp+I], R", c->storage[i]+8, registerSized(RDX, inst.size - 8));
			}
		}
	} break;
	}
}

static void emitJump (Codegen *c, const char *inst, const Block *dest) {
	emit(c, " Z .S_I", inst, dest->label, dest->id);
}

// static bool isSpilled(Storage s) {
// 	return s >= STACK_BEGIN;
// }

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
		return "byte";
	if (size == 2)
		return "word";
	if (size <= 4)
		return "dword";
	if (size <= 8)
		return "qword";
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
	memcpy(insert, " db \"", 5);
	insert += 5;
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

	memcpy(insert, " db ", 4);
	insert += 4;
	u32 pos = 0;
	while (pos < len) {
		uchar c = data[pos++];
		if (c > 9 || c == 0)
			*insert++ = '0';
		if (c/16) {
			*insert++ = hexchars[c/16];
			*insert++ = hexchars[c%16];
		} else if (c % 16) {
			*insert++ = hexchars[c%16];
		}
		if (c > 9)
			*insert++ = 'h';
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


static void emitInt(u64 i);


static void emit (Codegen *c, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	for (const char *p = fmt; *p; p++) {
		switch (*p) {
		case 'Z': {
			const char *str = va_arg(args, const char *);
			u32 len = strlen(str);
			if (len)
				memcpy(insert, str, len);
			insert += len;
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
			const char *size = sizeOp(c->ir.ptr[ref].size);
			u32 len = strlen(size);
			memcpy(insert, size, len);
			insert += len;
			memcpy(insert, " [rsp+", 6);
			insert += 6;
			emitInt(c->storage[ref]);
			memcpy(insert, "]", 1);
			insert += 1;
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

static void emitInt (u64 i) {
	if (i < 9) {
		*insert++ = '0' + i;
		return;
	}

	u64 place = i < 1000000U ? 100000U : 10000000000000000000ULL;
	while (i / place == 0) place /= 10;

	while (place != 0) {
		*insert++ = '0' + (i / place) % 10;
		place /= 10;
	}
}
