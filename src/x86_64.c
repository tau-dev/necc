#include "analysis.h"
#include "types.h"
#include "parse.h"

//    Emits the flat assembler format.


// NOTE A StackAlloc of Constant size may be referenced across blocks,
// to allow jumps over definitions. Probably need to allocate these in a
// prepass.


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



static void emitData(FILE *out, Module, String data, References);
static void emitName (Module module, u32 id);
static bool isSpilled(Storage);
static void emitFunction(Arena *, FILE *, Module, IrList, Block *entry);
static void emitBlock(Codegen *, Block *);
static inline Storage storageSize(u16 size);
static inline Storage storageSized(Storage, u16);
static inline Storage storageUnsized(Storage);
static Storage regalloc(IrRef next_used_registers[GENERAL_PURPOSE_REGS_END], u16 size, IrRef);
static const char *sizeOp(u16 size);
static u16 valueSize(Codegen *, IrRef);
static const char *valueName(Codegen *, IrRef);
static const char *storageName(Codegen *, Storage);

int splice_dest_order(const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

void emitX64AsmSimple(FILE *out, Arena *arena, Module module) {
	puts("use64\n"
	     "format ELF64\n");

	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_state == Def_Undefined)
			fprintf(out, "extrn '%.*s' as _%.*s\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name));
		else if (reloc.is_public)
			fprintf(out, "public _%.*s as '%.*s'\n", STRING_PRINTAGE(reloc.name), STRING_PRINTAGE(reloc.name));
	}

	puts("\n\n"
	     "section '.text' executable\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state) {
			fprintf(out, "_%.*s:\n", STRING_PRINTAGE(reloc.name));
			assert(reloc.type.kind == Kind_Function);
			emitFunction(arena, out, module, reloc.function_ir, reloc.function_entry);
		}
	}

	puts("\n\n"
	     "section '.data' writeable");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(module, i);
			emitData(out, module, reloc.value_data, reloc.value_references);
		}
	}

	puts("\n\n"
	     "section '.rodata'");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			emitName(module, i);
			emitData(out, module, reloc.value_data, reloc.value_references);
		}
	}
	printf("\n");
}

static void emitName (Module module, u32 id) {
	StaticValue reloc = module.ptr[id];
	printf("align 8\n");
	// STYLE Copypasta from valueName
	if (reloc.name.len)
		printf("_%.*s:\n", STRING_PRINTAGE(reloc.name));
	else
		printf("__%lu:\n", (unsigned long) id);
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





static void emitFunction (Arena *arena, FILE *out, Module module, IrList ir, Block *entry) {
	Codegen c = {
		.arena = arena,
		.out = out,
		.ir = ir,
		.lifetimes = (ValuesSpan) ALLOCN(arena, IrRef, ir.len),
		.storage = (Storages) ALLOCN(arena, Storage, ir.len),
		.module = module,
	};
	for (u32 i = 0; i < ir.len; i++) {
		if (ir.ptr[i].kind == Ir_StackAlloc) {
			Inst inst = ir.ptr[i];
			assert(ir.ptr[inst.alloc.size].kind == Ir_Constant);

			ir.ptr[i].alloc.known_offset = c.stack_allocated;
			c.stack_allocated += ir.ptr[inst.alloc.size].constant;
		} else {
			c.storage.ptr[i] = 0;
		}
	}
	for (u32 i = 0; i < GENERAL_PURPOSE_REGS_END; i++) {
		c.used_registers[i] = IR_REF_NONE;
	}
	calcLifetimes(ir, c.lifetimes);
	fprintf(out, " push rbx\n push r12\n push r13\n push r14\n push r15\n");
	emitBlock(&c, entry);
}

// INTEGER arguments use in order %rdi %rsi (%rdx->%rbx) %rcx %r8 %r9.
const u8 parameter_regs[] = {
	RDI,
	RSI,
	RDX,
	RCX,
	R8,
	R9,
};

static void emitBlock(Codegen *c, Block *block) {
	if (block->visited || block->exit.kind == Exit_None)
		return;
	block->visited = true;
	IrRef next_used_registers[GENERAL_PURPOSE_REGS_END];

	// param instructions will all appear in the first block.
	u32 parameters_found = 0;

	fprintf(c->out, ".%s:\n ", block->label.ptr);
	if (block->first_inst == 0) { // entry
		fprintf(c->out, "sub rsp, %d\n ", (int) c->stack_allocated);
	}

	for (u32 i = block->first_inst; i <= block->last_inst; i++) {
		Inst inst = c->ir.ptr[i];
		for (u32 r = 0; r < GENERAL_PURPOSE_REGS_END; r++) {
			IrRef inst = c->used_registers[r];
			if (inst == IR_REF_NONE || c->lifetimes.ptr[inst] <= i) {
				next_used_registers[r] = IR_REF_NONE;
			} else
				next_used_registers[r] = inst;
		}

		switch ((InstKind) inst.kind) {
		case Ir_Add: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			fprintf(c->out, "mov rax, %s\n"
			     " add rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Sub: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			fprintf(c->out, "mov rax, %s\n"
			     " sub rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Mul: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			fprintf(c->out, "mov rax, %s\n"
			     " imul rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Div: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			if (c->ir.ptr[inst.binop.rhs].kind == Ir_Constant) {
				Storage tmp = regalloc(next_used_registers, I64, i);
				assert(!isSpilled(tmp));
				const char *imm_reg = storageName(c, tmp);
				fprintf(c->out, "mov %s, %s\n ", imm_reg, rhs);
				next_used_registers[storageUnsized(tmp)] = IR_REF_NONE;
				rhs = imm_reg;
			}
			const char *dest = storageName(c, slot);
			// TODO rdx will screw this up
			fprintf(c->out, "xor rdx, rdx\n"
			     " mov rax, %s\n"
			     " idiv %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_BitOr: {
			fprintf(stderr, "TODO codegen or");
		} break;
		case Ir_BitXor: {
			fprintf(stderr, "TODO codegen xor");
		} break;
		case Ir_BitAnd: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			fprintf(c->out, "mov rax, %s\n"
			     " and rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_BitNot: {
			fprintf(stderr, "TODO codegen not");
		} break;
		case Ir_LessThan: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);

			fprintf(c->out, "cmp %s, %s\n"
			     " setl al\n"
			     " movzx %s, al	; less than", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_LessThanOrEquals: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			fprintf(c->out, "cmp %s, %s\n"
			     " setle al\n"
			     " movzx %s, al	; less than or equals", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Equals: {
			Storage slot = regalloc(next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			fprintf(c->out, "cmp %s, %s\n"
			     " sete al\n"
			     " movzx %s, al	; less than or equals", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Access: {
			Storage slot = regalloc(next_used_registers, inst.size, i);

			fprintf(stderr, "TODO Access");

			c->storage.ptr[i] = slot;
		} break;
		case Ir_Truncate: {
			Storage slot = regalloc(next_used_registers, inst.size, i);

			Inst src = c->ir.ptr[inst.unop];
			assert(src.size >= inst.size);
			// STYLE Much hackyness
			const char *src_name;
			if (src.kind == Ir_Constant || src.kind == Ir_Reloc) {
				src_name = valueName(c, inst.unop);
			} else {
				Storage src_storage = storageSized(c->storage.ptr[inst.unop], inst.size);
				src_name = storageName(c, src_storage);
			}

			fprintf(c->out, "mov %s, %s", storageName(c, slot), src_name);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_SignExtend: {
			Storage slot = regalloc(next_used_registers, inst.size, i);

			fprintf(c->out, "movsx %s, %s", storageName(c, slot), valueName(c, inst.unop));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_ZeroExtend: {
			Storage slot = regalloc(next_used_registers, inst.size, i);

			fprintf(c->out, "movzx %s, %s", storageName(c, slot), valueName(c, inst.unop));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_StackAlloc: {
			Storage slot = regalloc(next_used_registers, I64, i);
			fprintf(c->out, "lea %s, [rsp+%d]	; alloc", storageName(c, slot), inst.alloc.known_offset);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Store: {
			if (isSpilled(c->storage.ptr[i])) {
				fprintf(stderr, "TODO stack spillage");
			} else {
				const char *dest = valueName(c, inst.binop.lhs);
				const char *src = valueName(c, inst.binop.rhs);
				fprintf(c->out, "mov %s [%s], %s	; store", sizeOp(valueSize(c, inst.binop.rhs)), dest, src);
			}
		} break;
		case Ir_Load: {
			if (isSpilled(c->storage.ptr[i])) {
				fprintf(stderr, "TODO stack spillage");
			} else {
				Storage slot = regalloc(next_used_registers, I64, i);
				fprintf(c->out, "mov %s, [%s]	; load", storageName(c, slot), valueName(c, inst.unop));
				c->storage.ptr[i] = slot;
			}
		} break;
		case Ir_Parameter: {
			u8 reg = parameter_regs[parameters_found];
			if (parameters_found == 2) {
				// rdx must be available for intermediate storage
				fprintf(c->out, "mov rbx, rdx");
				reg = RBX;
			}
			c->storage.ptr[i] = reg + storageSize(inst.size);
			next_used_registers[storageUnsized(reg)] = i;
			parameters_found++;
		} break;
		case Ir_Phi: {
			assert(!"TODO codegen phi");
		} break;
		case Ir_IntToFloat:
		case Ir_FloatToInt: {
			assert(!"TODO codegen float stuff");
		} break;
		case Ir_Constant:
		case Ir_Reloc:
		case Ir_StackDealloc:
			memcpy(c->used_registers, next_used_registers, sizeof(next_used_registers));
			continue;
		case Ir_Call: {
// 			u16 stack_param_allocated = 0;
// 			u16 stack_offsets[CALLER_SAVED_COUNT];

			for (u8 r = 0; r < CALLER_SAVED_COUNT; r++) {
				u8 reg = caller_saved[r];
				IrRef inst = c->used_registers[reg];
				if (inst == IR_REF_NONE || isSpilled(c->storage.ptr[inst]))
					continue;
// 				stack_offsets[r] = stack_param_allocated;
				fprintf(c->out, "push %s\n ", storageName(c, c->storage.ptr[inst]));
// 				stack_param_allocated += c->ir.ptr[inst].size;
			}
			Storage slot = regalloc(next_used_registers, I64, i);
			ValuesSpan params = inst.call.parameters;

			for (u32 p = 0; p < params.len; p++) {
				u8 r = storageSized(parameter_regs[p], valueSize(c, params.ptr[p]));
				fprintf(c->out, "mov %s, %s\n ", register_names[r], valueName(c, params.ptr[p]));
			}
			fprintf(c->out, "call %s\n ", valueName(c, inst.call.function_ptr));

			for (i8 r = CALLER_SAVED_COUNT - 1; r >= 0; r--) {
				u8 reg = caller_saved[r];
				IrRef inst = c->used_registers[reg];
				if (inst == IR_REF_NONE || isSpilled(c->storage.ptr[inst]))
					continue;
				fprintf(c->out, "pop %s\n ", storageName(c, c->storage.ptr[inst]));
// 				stack_param_allocated -= c->ir.ptr[inst].size;
			}
			fprintf(c->out, "mov %s, rax", storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		}

		fprintf(c->out, "\n ");

		memcpy(c->used_registers, next_used_registers, sizeof(next_used_registers));
	}

	Exit exit = block->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		if (exit.unconditional->visited)
			fprintf(c->out, "jmp %s\n\n", exit.unconditional->label.ptr);
		emitBlock(c, exit.unconditional);
		break;
	case Exit_Branch: {
		const char *condition = valueName(c, exit.branch.condition);
		fprintf(c->out, "test %s, -1\n", condition);
		fprintf(c->out, " jnz .%s\n", exit.branch.on_true->label.ptr);
		if (exit.branch.on_false->visited)
			fprintf(c->out, " jmp .%s\n\n", exit.branch.on_false->label.ptr);
		emitBlock(c, exit.branch.on_false);
		emitBlock(c, exit.branch.on_true);
	} break;
	case Exit_Return:
		if (exit.ret != IR_REF_NONE)
			fprintf(c->out, "mov %s, %s\n ",
					storageName(c, storageSized(RAX, valueSize(c, exit.ret))),
					valueName(c, exit.ret));
		fprintf(c->out, "add rsp, %llu	; return\n pop r15\n pop r14\n pop r13\n pop r12\n pop rbx\n ret\n\n", (unsigned long long) c->stack_allocated);
		break;
	case Exit_None: unreachable;
	}
}

static Storage regalloc(IrRef next_used_registers[GENERAL_PURPOSE_REGS_END], u16 size, IrRef inst) {
	for (u32 i = 0; i < GENERAL_PURPOSE_REGS_END; i++) {
		if (next_used_registers[i] == IR_REF_NONE) {
			next_used_registers[i] = inst;
			return i + storageSize(size);
		}
	}
	fprintf(stderr, "Cannot spill registers yet.");
	return 0;
}

static bool isSpilled(Storage s) {
	return s >= STACK_BEGIN;
}

static inline Storage storageSize(u16 size) {
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


static inline Storage storageSized(Storage stor, u16 size) {
	if (stor >= STACK_BEGIN)
		return stor;
	return (stor & ~RSIZE_MASK) | storageSize(size);
}

static inline Storage storageUnsized(Storage stor) {
	if (stor >= STACK_BEGIN)
		return stor;
	return stor & ~RSIZE_MASK;
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

static const char *valueName(Codegen *c, IrRef ref) {
	Inst inst = c->ir.ptr[ref];
	const int max_num_chars = 24;
	if (inst.kind == Ir_Constant) {
		char *name = aalloc(c->arena, max_num_chars);
		snprintf(name, max_num_chars, "%llu", (unsigned long long) inst.constant);
		return name;
	} else if (inst.kind == Ir_Reloc) {
		String name = c->module.ptr[inst.reloc.id].name;
		char *term;
		if (name.len) {
			term = aalloc(c->arena, name.len + max_num_chars);
			term[0] = '_';
			memcpy(term+1, name.ptr, name.len);
			term[name.len+1] = 0;
		} else {
			term = aalloc(c->arena, max_num_chars * 2);
			snprintf(term, max_num_chars, "__%lu", (unsigned long) inst.reloc.id);
		}
		snprintf(term + strlen(term), max_num_chars, "%+lld", (long long) inst.reloc.offset);
		return term;
	}
	return storageName(c, c->storage.ptr[ref]);
}

static inline u32 storageStackOffset(Storage s) {
	return s - STACK_BEGIN;
}

static inline Storage stackOffsetStorage(u16 offset) {
	return STACK_BEGIN + offset;
}

static const char *storageName(Codegen *c, Storage store) {
	if (isSpilled(store)) {
		char *name = aalloc(c->arena, 32);
		snprintf(name, 32, "[rsp+%lu]", (unsigned long) storageStackOffset(store));
		return name;
	}
	return register_names[store];
}
