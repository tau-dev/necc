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

typedef unsigned long ulong;
typedef unsigned long long ullong;

// Register, or stack offset + STACK_BEGIN
typedef u16 Storage;

typedef SPAN(Storage) Storages;


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
	Arena *arena;
	FILE *out;
	IrList ir;
	Module module;

	ValuesSpan lifetimes;
	Storages storage;
	IrRef used_registers[GENERAL_PURPOSE_REGS_END];
	u32 stack_allocated;
	bool is_memory_return;
} Codegen;


const int caller_saved[] = {
	RCX,
	RSI,
	RDI,
	RBP,
	R8,
	R9,
	R10,
	R11,
};

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



static void emitData(FILE *, Module, String data, References);
static void emitName (FILE *, Module module, u32 id);
static void emitJump(FILE *, const char *inst, const Block *dest);
static void emitFunctionForward(Arena *, FILE *, Module, StaticValue, const Target *);
static void emitBlockForward(Codegen *, Block *);
static inline Storage registerSize(u16 size);
static inline const char *registerSized(Register, u16);
static const char *sizeOp(u16 size);
static bool isMemory(u16 size);
static u16 valueSize(Codegen *, IrRef);
static const char *valueName(Codegen *, IrRef);
static const char *storageName(Codegen *, Storage);

int splice_dest_order(const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

void emitX64AsmSimple(FILE *out, Arena *arena, Module module, const Target *target) {
	fprintf(out, "use64\n"
	     "format ELF64\n");

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

	fprintf(out, "\n\n"
	     "section '.text' executable\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state) {
			fprintf(out, "_%.*s:\n", STRING_PRINTAGE(reloc.name));
			assert(reloc.type.kind == Kind_Function);
			emitFunctionForward(arena, out, module, reloc, target);
		}
	}

	fprintf(out, "\n\n"
	     "section '.data' writeable\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(out, module, i);
			emitData(out, module, reloc.value_data, reloc.value_references);
		}
	}

	fprintf(out, "\n\n"
	     "section '.rodata'\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(out, module, i);
			emitData(out, module, reloc.value_data, reloc.value_references);
		}
	}
	printf("\n");
}

static void emitName (FILE *out, Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	fprintf(out, "align 8\n");
	// STYLE Copypasta from valueName
	if (reloc.name.len)
		fprintf(out, "_%.*s:\n", STRING_PRINTAGE(reloc.name));
	else
		fprintf(out, "__%lu:\n", (ulong) id);
}

static void emitData (FILE *out, Module module, String data, References references) {
	if (references.len) {
		qsort(references.ptr, references.len, sizeof(references.ptr[0]),
			splice_dest_order);
	}

	u32 pos = 0;
	for (u32 r = 0; r < references.len; r++) {
		Reference ref = references.ptr[r];
		if (pos < ref.splice_pos) {
			fprintf(out, " db 0%hhxh", data.ptr[pos]);
			pos++;
			while (pos < references.ptr[r].splice_pos) {
				fprintf(out, ",0%hhxh", data.ptr[pos]);
				pos++;
			}
			fprintf(out, "\n");
		}
		fprintf(out, " dq %.*s %+lld\n", STRING_PRINTAGE(module.ptr[ref.source_id].name), (long long) ref.offset);
		pos += 8;
	}

	if (pos < data.len) {
		fprintf(out, " db 0%hhxh", data.ptr[pos]);
		pos++;
		while (pos < data.len) {
			fprintf(out, ",0%hhxh", data.ptr[pos]);
			pos++;
		}
		fprintf(out, "\n");
	}
}

static u32 roundUp(u32 x)  {
	return ((x + 7) / 8) * 8;
}

static void emitFunctionForward (Arena *arena, FILE *out, Module module, StaticValue reloc, const Target *target) {
	IrList ir = reloc.function_ir;
	Block *entry = reloc.function_entry;
	Codegen c = {
		.arena = arena,
		.out = out,
		.ir = ir,
		.is_memory_return = isMemory(typeSize(*reloc.type.function.rettype, target)),
		.lifetimes = (ValuesSpan) ALLOCN(arena, IrRef, ir.len),
		.storage = (Storages) ALLOCN(arena, Storage, ir.len),
		.module = module,
	};

	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		switch (inst.kind) {
		case Ir_StackAlloc:
			ir.ptr[i].alloc.known_offset = c.stack_allocated;
			Inst sz = ir.ptr[inst.alloc.size];
			assert(sz.kind == Ir_Constant);
			c.stack_allocated += roundUp(sz.constant);
			break;
		case Ir_Parameter:
			if (isMemory(inst.size)) {
				continue;
			}
			break;
		}

		switch (inst.kind) {
		case Ir_Store:
		case Ir_StackDealloc:
			break;
		default:
			c.storage.ptr[i] = c.stack_allocated;
			c.stack_allocated += roundUp(inst.size);
		}
	}

	u32 mem_params = c.stack_allocated + 8;
	for (u32 i = 0; i < ir.len; i++) {
		Inst inst = ir.ptr[i];
		if (inst.kind == Ir_Parameter && isMemory(inst.size)) {
			c.storage.ptr[i] = mem_params;
			mem_params += inst.size;
		}
	}

	fprintf(c.out, " add rsp, %lu\n", (ulong) c.stack_allocated);
	if (c.is_memory_return)
		fprintf(c.out, " mov r15, rdi\n");
	emitBlockForward(&c, entry);
}

const Register parameter_regs[] = {
	RDI,
	RSI,
	RDX,
	RCX,
	R8,
	R9,
};

static const u32 parameter_regs_count = sizeof(parameter_regs) / sizeof(parameter_regs[0]);

static void copyTo (Codegen *c, const char *to, u32 to_offset, const char *from, u32 from_offset, u16 size) {
	ulong offset = 0;
	while (size - offset > 8) {
		fprintf(c->out, " mov r8, [%s+%lu]\n", from, offset+from_offset);
		fprintf(c->out, " mov [%s+%lu], r8 \n", to, offset+to_offset);
		offset += 8;
	}
	const char *tmp = registerSized(R8, size - offset);
	fprintf(c->out, " mov %s, [%s+%lu]\n", tmp, from, offset+from_offset);
	fprintf(c->out, " mov [%s+%lu], %s \n", to, offset+to_offset, tmp);
}


static const char *loadTo (Codegen *c, Register reg, IrRef i) {
	const char *dest = registerSized(reg, c->ir.ptr[i].size);
	fprintf(c->out, " mov %s, %s\n", dest, storageName(c, c->storage.ptr[i]));
	return dest;
}


static void triple (Codegen *c, const char *msg, IrRef lhs, IrRef rhs, IrRef dest) {
	const char *rax = loadTo(c, RAX, lhs);
	fprintf(c->out, " %s %s, %s\n mov %s, %s\n", msg, rax, valueName(c, rhs), valueName(c, dest), rax);
}

// This backend performs _no_ register allocation.
static void emitBlockForward (Codegen *c, Block *block) {
	if (block->visited)
		return;
	assert(block->exit.kind != Exit_None);

	block->visited = true;

	// param instructions will all appear in the first block.
	u32 parameters_found = c->is_memory_return;

	fprintf(c->out, ".%.*s%u:\n", STRING_PRINTAGE(block->label), (u32) (intptr_t) block);

	if (block->first_inst == 0) { // entry
		fprintf(c->out, " sub rsp, %d\n", (int) c->stack_allocated);
	}
	LIST(IrRef) false_phis = {0};

	for (u32 i = block->first_inst; i <= block->last_inst; i++) {
		Inst inst = c->ir.ptr[i];

		switch ((InstKind) inst.kind) {
		case Ir_Add: triple(c, "add", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_Sub: triple(c, "sub", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_Mul: triple(c, "imul", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_Div:
			fprintf(c->out, " xor rdx, rdx\n");
			loadTo(c, RAX, inst.binop.lhs);
			fprintf(c->out, " idiv %s %s\n mov %s, %s\n",
					sizeOp(inst.size), valueName(c, inst.binop.rhs), valueName(c, i),
					registerSized(RAX, inst.size));
			break;
		case Ir_BitOr: triple(c, "or", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_BitXor: triple(c, "xor", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_BitAnd: triple(c, "and", inst.binop.lhs, inst.binop.rhs, i); break;
		case Ir_BitNot: {
			const char *reg = loadTo(c, RAX, inst.unop);
			fprintf(c->out, "not %s\n"
			     " mov %s, %s\n", reg, valueName(c, i), reg);

		} break;
		case Ir_LessThan: {
			fprintf(c->out, " cmp %s, %s\n"
			     " setl al\n"
			     " movzx rsi, al\n"
			     " mov %s, %s\n",
			     loadTo(c, RAX, inst.binop.lhs),
			     valueName(c, inst.binop.rhs),
			     valueName(c, i),
			     registerSized(RSI, inst.size));
		} break;
		case Ir_LessThanOrEquals: {
			fprintf(c->out, " cmp %s, %s\n"
			     " setle al\n"
			     " movzx rsi, al\n"
			     " mov %s, %s\n",
			     loadTo(c, RAX, inst.binop.lhs),
			     valueName(c, inst.binop.rhs),
			     valueName(c, i),
			     registerSized(RSI, inst.size));
		} break;
		case Ir_Equals: {
			fprintf(c->out, " cmp %s, %s\n"
			     " sete al\n"
			     " movzx rsi, al\n"
			     " mov %s, %s\n",
			     loadTo(c, RAX, inst.binop.lhs),
			     valueName(c, inst.binop.rhs),
			     valueName(c, i),
			     registerSized(RSI, inst.size));
		} break;
		case Ir_Truncate: {
			const char *reg = registerSized(RAX, c->ir.ptr[i].size);
			fprintf(c->out, " mov %s, %s\n", reg, valueName(c, inst.unop));
			fprintf(c->out, " mov %s %s, %s\n", sizeOp(c->ir.ptr[i].size), valueName(c, i), reg);
		} break;
		case Ir_SignExtend: {
			const char *reg = registerSized(RSI, inst.size);
			fprintf(c->out, " movsx%s %s, %s %s\n mov %s, %s\n",
				(valueSize(c, inst.unop) > 2 ? "d" : ""), reg, sizeOp(valueSize(c, inst.unop)), valueName(c, inst.unop),
				valueName(c, i), reg);
		} break;
		case Ir_ZeroExtend: {
			fprintf(c->out, " xor rax, rax\n mov %s, %s\n mov %s, %s\n",
				registerSized(RAX, valueSize(c, inst.unop)),  valueName(c, inst.unop),
				valueName(c, i), registerSized(RAX, inst.size));
		} break;

		case Ir_Store: {
			copyTo(c, loadTo(c, RAX, inst.binop.lhs), 0, "rsp", c->storage.ptr[inst.binop.rhs], inst.size);
		} break;
		case Ir_Load: {
			copyTo(c, "rsp", c->storage.ptr[i], loadTo(c, RAX, inst.unop), 0, inst.size);
		} break;
		case Ir_Access: {
			copyTo(c, "rsp", c->storage.ptr[i],
				"rsp", c->storage.ptr[inst.binop.lhs] + inst.binop.lhs, inst.size);
		} break;
		case Ir_Parameter: {
			if (!isMemory(inst.size)) {
				assert(parameters_found < parameter_regs_count);

				bool bigg = inst.size > 8;
				u32 sizea = bigg ? 8 : inst.size;
				fprintf(c->out, " mov [rsp+%lu], %s	; param\n", (ulong) c->storage.ptr[i],
					registerSized(parameter_regs[parameters_found], sizea));
				parameters_found++;
				if (bigg) {
					assert(parameters_found < parameter_regs_count);
					fprintf(c->out, " mov [rsp+%lu], %s	; param\n", (ulong) c->storage.ptr[i] + 8,
						registerSized(parameter_regs[parameters_found], inst.size - sizea));
					parameters_found++;
				}
			}
		} break;
		case Ir_Constant:
			fprintf(c->out, " mov %s %s, %llu\n", sizeOp(inst.size),
					valueName(c, i), (ullong) inst.constant);
			break;
		case Ir_StackAlloc:
			fprintf(c->out, " mov %s %s, %lu	; alloc\n", sizeOp(inst.size),
					valueName(c, i), (ulong) inst.alloc.known_offset);
			break;
		case Ir_Reloc:
			assert(inst.size == 8);
			fprintf(c->out, " mov qword %s, ", valueName(c, i));
			String name = c->module.ptr[inst.reloc.id].name;
			if (name.len)
				fprintf(c->out, "_%.*s", STRING_PRINTAGE(name));
			else
				fprintf(c->out, "__%lu", (ulong) inst.reloc.id);
			fprintf(c->out, "%+lld\n", (long long) inst.reloc.offset);
			break;
		case Ir_PhiOut: {
			if (inst.phi_out.on_true != IR_REF_NONE) {
				copyTo(c, "rsp", c->storage.ptr[inst.phi_out.on_true],
					"rsp", c->storage.ptr[inst.phi_out.source],
					valueSize(c, inst.phi_out.source));
			}
			if (inst.phi_out.on_false != IR_REF_NONE)
				PUSH(false_phis, i);
		} break;
		case Ir_PhiIn: break;
		case Ir_StackDealloc: break;
		case Ir_IntToFloat:
		case Ir_FloatToInt: {
			assert(!"TODO codegen float stuff");
		} break;
		case Ir_Call: {
			ValuesSpan params = inst.call.parameters;

			u32 param_slot = 0;
			for (u32 p = 0; p < params.len; p++) {
				assert(param_slot < parameter_regs_count);
				u16 sz = valueSize(c, params.ptr[p]);
				assert(!isMemory(sz));

				bool bigg = sz > 8;
				u32 sza = bigg ? 8 : sz;
				fprintf(c->out, "  mov %s, [rsp+%lu]\n",
					registerSized(parameter_regs[param_slot], sza),
					(ulong) c->storage.ptr[i]);
				param_slot++;
				if (bigg) {
					assert(param_slot < parameter_regs_count);
					fprintf(c->out, "  mov %s, [rsp+%lu]\n",
						registerSized(parameter_regs[param_slot], sz - sza),
						(ulong) c->storage.ptr[i] + 8);
					param_slot++;
				}
			}
			fprintf(c->out, "  call qword %s\n", valueName(c, inst.call.function_ptr));
			fprintf(c->out, "  mov %s, %s\n", valueName(c, i),
				registerSized(RAX, inst.size));
		} break;
		}
	}

	Exit exit = block->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		if (exit.unconditional->visited)
			emitJump(c->out, "jmp", exit.unconditional);

		emitBlockForward(c, exit.unconditional);
		break;
	case Exit_Branch: {
		const char *condition = valueName(c, exit.branch.condition);
		fprintf(c->out, " test dword %s, -1\n", condition);
		emitJump(c->out, "jnz", exit.branch.on_true);


		for (u32 k = 0; k < false_phis.len; k++) {
			Inst inst  = c->ir.ptr[false_phis.ptr[k]];
			fprintf(c->out, " mov %s, %s	; phi-out\n",
				valueName(c, inst.phi_out.on_false),
				loadTo(c, RAX, inst.phi_out.source));
		}
		if (exit.branch.on_false->visited)
			emitJump(c->out, "jmp", exit.branch.on_false);

		emitBlockForward(c, exit.branch.on_false);
		emitBlockForward(c, exit.branch.on_true);
	} break;
	case Exit_Return:
		if (exit.ret != IR_REF_NONE) {
			if (c->is_memory_return) {
				copyTo(c, "r15", 0, "rsp", c->storage.ptr[exit.ret], valueSize(c, exit.ret));
				fprintf(c->out, " mov rax, r15\n");
			} else {
				u32 ret_size = valueSize(c, exit.ret);
				if (ret_size > 8) {
					assert(!"TODO !!Big returns!!");
				} else {
					fprintf(c->out, " mov %s, %s\n",
							registerSized(RAX, ret_size),
							valueName(c, exit.ret));
				}
			}
		}
		fprintf(c->out, " ret %lu\n\n", (ulong) c->stack_allocated);
		break;
	case Exit_None: unreachable;
	}

	free(false_phis.ptr);
}

static void emitJump(FILE *out, const char *inst, const Block *dest) {
	fprintf(out, " %s .%.*s%u\n\n", inst, STRING_PRINTAGE(dest->label), (u32) (intptr_t) dest);
}

// static bool isSpilled(Storage s) {
// 	return s >= STACK_BEGIN;
// }

static inline Storage registerSize(u16 size) {
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


static inline const char *registerSized(Register stor, u16 size) {
	return register_names[(stor & ~RSIZE_MASK) | registerSize(size)];
}

static const char *sizeOp(u16 size) {
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

static u16 valueSize(Codegen *c, IrRef ref) {
	return c->ir.ptr[ref].size;
}

static bool isMemory(u16 size) {
	return size > 16;
}

static const char *valueName(Codegen *c, IrRef ref) {
	assert(ref != IR_REF_NONE);
	return storageName(c, c->storage.ptr[ref]);
}

#define BUF 100
static char buf[BUF] = {0};
static const char *storageName(Codegen *c, Storage store) {
	snprintf(buf, BUF, "[rsp+%lu]", (ulong) store);
	return adupez(c->arena, buf);
}
