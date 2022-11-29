#include "analysis.h"
#include "types.h"
#include "parse.h"

//    Emits the flat assembler format.

// TODO A StackAlloc of Constant size may be referenced across blocks,
// to allow jumps over definitions. Probably need to allocate these in a
// prepass.

#define ON_STACK 255

typedef struct {
	u8 reg;
	u8 size;
	u16 stack_offset;
} Storage;

typedef SPAN(Storage) Storages;

typedef enum {
	RCX,
	RBX,
	RSI,
	RDI,
	RBP,
	R8,
	R9,
	R10,
	R11,
	R12,
	R13,
	R14,
	R15,
// rsp is reserved as stack pointer, rax and rdx are used for intermediate values
	GENERAL_PURPOSE_REGS_END,
	RDX = GENERAL_PURPOSE_REGS_END,
	RAX,
	RSP
} Register;

typedef struct {
	Arena *arena;
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

const char *register_names[16][4] = {
	[RAX] = {"al", "ax", "eax", "rax"},
	[RCX] = {"cl", "cx", "ecx", "rcx"},
	[RDX] = {"dl", "dx", "edx", "rdx"},
	[RBX] = {"bl ", "bx", "ebx", "rbx"},
	[RSI] = {"sil", "si", "esi", "rsi"},
	[RSP] = {"spl", "sp", "esp", "rsp"},
	[RDI] = {"dil", "di", "edi", "rdi"},
	[RBP] = {"bpl", "bp", "ebp", "rbp"},
	[R8]  = {"r8b", "r8w", "r8d", "r8"},
	[R9]  = {"r9b", "r9w", "r9d", "r9"},
	[R10] = {"r10b", "r10w", "r10d", "r10"},
	[R11] = {"r11b", "r11w", "r11d", "r11"},
	[R12] = {"r12b", "r12w", "r12d", "r12"},
	[R13] = {"r13b", "r13w", "r13d", "r13"},
	[R14] = {"r14b", "r14w", "r14d", "r14"},
	[R15] = {"r15b", "r15w", "r15d", "r15"},
};



static void emitData(Module, String data, References);
static void emitFunction(Arena *, Module, IrList, Block *entry);
static void emitBlock(Codegen *, Block *);
static Storage regalloc(u8 freed, IrRef next_used_registers[GENERAL_PURPOSE_REGS_END], u8 size, IrRef);
static const char *valueName(Codegen *, IrRef);
static const char *storageName(Codegen *, Storage);

int splice_dest_order(const void *a, const void *b) {
	return (i32) ((Reference*) a)->splice_pos - (i32) ((Reference*) b)->splice_pos;
}

void emitX64AsmSimple(Arena *arena, Module module) {
	puts("use64\n"
	     "format ELF64\n\n");

	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_state == Def_Undefined)
			printf("extern %.*s\n", STRING_PRINTAGE(reloc.name));
		if (reloc.is_public)
			printf("public %.*s\n", STRING_PRINTAGE(reloc.name));
	}

	puts("\n\n"
	     "section '.text'\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Function && reloc.def_state) {
			printf("%.*s:\n", STRING_PRINTAGE(reloc.name));
			assert(reloc.type.kind == Kind_Function);
			emitFunction(arena, module, reloc.function_ir, reloc.function_entry);
		}
	}

	puts("\n\n"
	     "section '.data'\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& !(reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			printf("%.*s:\n", STRING_PRINTAGE(reloc.name));
			emitData(module, reloc.value_data, reloc.value_references);
		}
	}

	puts("\n\n"
	     "section '.rodata'\n");
	for (u32 i = 0; i < module.len; i++) {
		StaticValue reloc = module.ptr[i];
		if (reloc.def_kind == Static_Variable
			&& (reloc.type.qualifiers & Qualifier_Const)
			&& reloc.def_state)
		{
			printf("%.*s:\n", STRING_PRINTAGE(reloc.name));
			emitData(module, reloc.value_data, reloc.value_references);
		}
	}
}

static void emitData (Module module, String data, References references) {
	qsort(references.ptr, references.len, sizeof(references.ptr[0]),
		splice_dest_order);

	u32 pos = 0;
	for (u32 r = 0; r < references.len; r++) {
		Reference ref = references.ptr[r];
		if (pos < ref.splice_pos)
			printf(" db ");
		while (pos < references.ptr[r].splice_pos) {
			printf("%hhu,", data.ptr[pos]);
// 			printf("0%hhxh,", data.ptr[pos]);
			pos++;
		}
		printf(" dq %.*s %+lld\n", STRING_PRINTAGE(module.ptr[ref.source_id].name), (long long) ref.offset);
		pos += 8;
	}

	if (pos < data.len)
		printf(" db ");
	while (pos < data.len) {
		printf("0%hhxh, ", data.ptr[pos]);
		pos++;
	}
}

static void emitFunction (Arena *arena, Module module, IrList ir, Block *entry) {
	Codegen c = {
		.arena = arena,
		.ir = ir,
		.lifetimes = (ValuesSpan) ALLOCN(arena, IrRef, ir.len),
		.storage = (Storages) ALLOCN(arena, Storage, ir.len),
		.module = module,
	};
	for (u32 i = 0; i < ir.len; i++) {
		if (ir.ptr[i].kind == Ir_StackAlloc) {
			c.storage.ptr[i].stack_offset = c.stack_allocated;
			c.stack_allocated += 8;//ir.ptr[i].unop;
		} else {
			c.storage.ptr[i] = (Storage) {0};
		}
	}
	for (u32 i = 0; i < GENERAL_PURPOSE_REGS_END; i++) {
		c.used_registers[i] = IR_REF_NONE;
	}
	calcLifetimes(ir, c.lifetimes);
	printf(" push rbx\n push r12\n push r13\n push r14\n push r15\n");
	emitBlock(&c, entry);
}

// INTEGER arguments use in order %rdi %rsi (%rdx->%rbx) %rcx %r8 %r9.
const u8 parameter_regs[] = { RDI, RSI, RDX, RCX, R8, R9 };

static void emitBlock(Codegen *c, Block *block) {
	if (block->visited || block->exit.kind == Exit_None)
		return;
	block->visited = true;
	IrRef next_used_registers[GENERAL_PURPOSE_REGS_END];

	// param instructions will all appear in the first block.
	u32 parameters_found = 0;

	printf(".%s:\n ", block->label.ptr);
	if (block->first_inst == 0) { // entry
		printf("sub rsp, %d\n ", (int) c->stack_allocated);
	}

	for (u32 i = block->first_inst; i <= block->last_inst; i++) {
		Inst inst = c->ir.ptr[i];
		u8 freed = -1;
		for (u32 r = 0; r < GENERAL_PURPOSE_REGS_END; r++) {
			IrRef inst = c->used_registers[r];
			if (inst == IR_REF_NONE || c->lifetimes.ptr[inst] <= i) {
				freed = r;
				next_used_registers[r] = IR_REF_NONE;
			} else
				next_used_registers[r] = inst;
		}

		switch ((InstKind) inst.kind) {
		case Ir_Add: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			printf("mov rax, %s\n"
			     " add rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Sub: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			printf("mov rax, %s\n"
			     " sub rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Mul: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			printf("mov rax, %s\n"
			     " imul rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Div: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			if (c->ir.ptr[inst.binop.rhs].kind == Ir_Constant) {
				Storage tmp = regalloc(-1, next_used_registers, I64, i);
				assert(tmp.reg != ON_STACK);
				const char *imm_reg = storageName(c, tmp);
				printf("mov %s, %s\n ", imm_reg, rhs);
				next_used_registers[tmp.reg] = IR_REF_NONE;
				rhs = imm_reg;
			}
			const char *dest = storageName(c, slot);
			// TODO rdx will screw this up
			printf("xor rdx, rdx\n"
			     " mov rax, %s\n"
			     " idiv %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_BitOr: {
			printf("TODO codegen or");
		} break;
		case Ir_BitXor: {
			printf("TODO codegen xor");
		} break;
		case Ir_BitAnd: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			const char *dest = storageName(c, slot);
			printf("mov rax, %s\n"
			     " and rax, %s\n"
			     " mov %s, rax", lhs, rhs, dest);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_BitNot: {
			printf("TODO codegen not");
		} break;
		case Ir_LessThan: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);

			printf("cmp %s, %s\n"
			     " setl al\n"
			     " movzx %s, al	; less than", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_LessThanOrEquals: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			printf("cmp %s, %s\n"
			     " setle al\n"
			     " movzx %s, al	; less than or equals", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Equals: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			const char *lhs = valueName(c, inst.binop.lhs);
			const char *rhs = valueName(c, inst.binop.rhs);
			printf("cmp %s, %s\n"
			     " sete al\n"
			     " movzx %s, al	; less than or equals", lhs, rhs, storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Truncate: {
			Storage slot = regalloc(freed, next_used_registers, inst.size, i);

			Inst src = c->ir.ptr[inst.unop];
			// STLY Much hackyness
			const char *src_name;
			if (src.kind == Ir_Constant || src.kind == Ir_Reloc) {
				src_name = valueName(c, inst.unop);
			} else {
				Storage src_storage = c->storage.ptr[inst.unop];
				assert(src_storage.size >= inst.size);
				src_storage.size = inst.size;
				src_name = storageName(c, src_storage);
			}

			printf("mov %s, %s", storageName(c, slot), src_name);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_SignExtend: {
			Storage slot = regalloc(freed, next_used_registers, inst.size, i);

			printf("movsx %s, %s", storageName(c, slot), valueName(c, inst.unop));
		} break;
		case Ir_ZeroExtend: {
			Storage slot = regalloc(freed, next_used_registers, inst.size, i);

			printf("movzx %s, %s", storageName(c, slot), valueName(c, inst.unop));
		} break;

		case Ir_StackAlloc: {
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			printf("lea %s, [rsp+%d]	; alloc", storageName(c, slot), c->storage.ptr[i].stack_offset);
			c->storage.ptr[i] = slot;
		} break;
		case Ir_Store: {
			if (c->storage.ptr[i].reg == ON_STACK) {
				printf("TODO stack spillage");
			} else {
				const char *dest = valueName(c, inst.binop.lhs);
				const char *src = valueName(c, inst.binop.rhs);
				printf("mov [%s], %s	; store", dest, src);
			}
		} break;
		case Ir_Load: {
			if (c->storage.ptr[i].reg == ON_STACK) {
				printf("TODO stack spillage");
			} else {
				Storage slot = regalloc(freed, next_used_registers, I64, i);
				printf("mov %s, [%s]	; load", storageName(c, slot), valueName(c, inst.unop));
				c->storage.ptr[i] = slot;
			}
		} break;
		case Ir_Parameter: {
			u8 reg = parameter_regs[parameters_found];
			if (parameters_found == 2) {
				// rdx must be available for intermediate storage
				printf("mov rbx, rdx");
				reg = 1;
			}
			c->storage.ptr[i] = (Storage) {reg, I64};
			next_used_registers[reg] = i;
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
			u16 stack_param_allocated = 0;
			for (u8 r = 0; r < CALLER_SAVED_COUNT; r++) {
				u8 reg = caller_saved[r];
				IrRef inst = c->used_registers[reg];
				if (inst == IR_REF_NONE)
					continue;
				c->storage.ptr[inst] = (Storage){ON_STACK, I64, stack_param_allocated};
				printf("mov [rsp-%d], %s\n ", (int) stack_param_allocated, register_names[reg][I64]);
				stack_param_allocated += 8;
			}
			Storage slot = regalloc(freed, next_used_registers, I64, i);
			ValuesSpan params = inst.call.parameters;

			for (u32 p = 0; p < params.len; p++) {
				u8 r = parameter_regs[p];
				printf("mov %s, %s\n ", register_names[r][I64], valueName(c, params.ptr[p]));
			}
			printf("call %s\n ", valueName(c, inst.call.function_ptr));

			for (u8 r = 0; r < CALLER_SAVED_COUNT; r++) {
				u8 reg = caller_saved[r];
				IrRef inst = c->used_registers[reg];
				if (inst == IR_REF_NONE)
					continue;
				printf("mov %s, [rsp-%d]\n ", register_names[reg][I64], (int) c->storage.ptr[inst].stack_offset);
				c->storage.ptr[inst] = (Storage){reg, I64};
				stack_param_allocated -= 8;
			}
			printf("mov %s, rax", storageName(c, slot));
			c->storage.ptr[i] = slot;
		} break;
		}

		printf("\n ");

		memcpy(c->used_registers, next_used_registers, sizeof(next_used_registers));
	}

	Exit exit = block->exit;
	switch (exit.kind) {
	case Exit_Unconditional:
		if (exit.unconditional->visited)
			printf("jmp %s\n\n", exit.unconditional->label.ptr);
		emitBlock(c, exit.unconditional);
		break;
	case Exit_Branch: {
		const char *condition = valueName(c, exit.branch.condition);
		printf("test %s, -1\n", condition);
		printf(" jnz .%s\n", exit.branch.on_true->label.ptr);
		if (exit.branch.on_false->visited)
			printf(" jmp .%s\n\n", exit.branch.on_false->label.ptr);
		emitBlock(c, exit.branch.on_false);
		emitBlock(c, exit.branch.on_true);
	} break;
	case Exit_Return:
		if (exit.ret != IR_REF_NONE)
			printf("mov rax, %s\n ", valueName(c, exit.ret));
		printf("add rsp, %llu	; return\n pop r15\n pop r14\n pop r13\n pop r12\n pop rbx\n ret\n\n", (unsigned long long) c->stack_allocated);
		break;
	case Exit_None: unreachable;
	}
}

static Storage regalloc(u8 freed, IrRef next_used_registers[GENERAL_PURPOSE_REGS_END], u8 size, IrRef inst) {
	if (freed != (u8) -1) {
		next_used_registers[freed] = inst;
		return (Storage) {freed, size};
	}
	for (u32 i = 0; i < GENERAL_PURPOSE_REGS_END; i++) {
		if (next_used_registers[i] == IR_REF_NONE) {
			next_used_registers[i] = inst;
			return (Storage) {i, size};
		}
	}
	printf("Cannot spill registers yet.");
	return (Storage) {0};
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
			memcpy(term, name.ptr, name.len);
			term[name.len] = 0;
		} else {
			term = aalloc(c->arena, max_num_chars * 2);
			snprintf(term, max_num_chars, "__%lu", (unsigned long) inst.reloc.id);
		}
		snprintf(term + strlen(term), max_num_chars, "%+lld", (long long) inst.reloc.offset);
		return term;
	}
	return storageName(c, c->storage.ptr[ref]);
}

static const char *storageName(Codegen *c, Storage store) {
	if (store.reg == ON_STACK) {
		char *name = aalloc(c->arena, 32);
		snprintf(name, 32, "[rsp+%d]", store.stack_offset);
		return name;
	}
	return register_names[store.reg][store.size];
}
