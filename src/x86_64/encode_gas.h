// typedef struct Codegen Codegen;
static void emit(Codegen *c, const char *fmt, ...);


#define BUF 100000
// Maximum amount of data expected to be emitted by a single call to emit().
#define MAX_LINE 1000
static char buf[BUF];
static char *insert;


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
	[R8 + RSIZE_WORD]  = "r8w",
	[R9 + RSIZE_WORD]  = "r9w",
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
	[R8 + RSIZE_DWORD]  = "r8d",
	[R9 + RSIZE_DWORD]  = "r9d",
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


static const char *xmm_names[] = {
	"%xmm",
	"%ymm",
	"%zmm",
};

static const char *condition_names[] = {
	[Cond_Overflow] = "o",
	[Cond_NoOverflow] = "no",
	[Cond_ULessThan] = "b",
	[Cond_UGreaterThanOrEqual] = "ae",
	[Cond_Equal] = "e",
	[Cond_NotEqual] = "ne",
	[Cond_ULessThanOrEqual] = "be",
	[Cond_UGreaterThan] = "a",
	[Cond_Sign] = "s",
	[Cond_NoSign] = "ns",
	[Cond_ParityEven] = "pe",
	[Cond_ParityOdd] = "po",
	[Cond_SLessThan] = "l",
	[Cond_SGreaterThanOrEqual] = "ge",
	[Cond_SLessThanOrEqual] = "le",
	[Cond_SGreaterThan] = "g",
};




static void push (Codegen *c, u16 size, Register r) {
	emit(c, " push R", registerSized(r, size));
}
static void pop (Codegen *c, u16 size, Register r) {
	emit(c, " pop R", registerSized(r, size));
}

static void movRR (Codegen *c, u16 size, Register src, Register dest) {
	emit(c, " mov R, R", registerSized(src, size), registerSized(dest, size));
}

static void movRM (Codegen *c, u16 size, Register src, Mem mem) {
	emit(c, " movZ R, M", sizeSuffix(size), registerSized(src, size), mem);
}
static void movMR (Codegen *c, u16 size, Mem mem, Register dest) {
	emit(c, " movZ M, R", sizeSuffix(size), mem, registerSized(dest, size));
}
static void movCM (Codegen *c, u16 size, u32 val, Mem mem) {
	emit(c, " movZ $I, M", sizeSuffix(size), val, mem);
}
static void movCR (Codegen *c, u16 size, u32 val, Register dest) {
	if (size == I16) size = I32; // IMM16 is slow to decode.
	emit(c, " movZ $I, R", sizeSuffix(size), val, registerSized(dest, size));
}

static void movFM (Codegen *c, u16 size, VecRegister src, Mem mem) {
	emit(c, " movsZ F, M", sizeFSuffix(size), src, mem);
}
static void movMF (Codegen *c, u16 size, Mem mem, VecRegister dest) {
	emit(c, " movsZ M, F", sizeFSuffix(size), mem, dest);
}
static void movRF (Codegen *c, u16 size, Register src, VecRegister dest) {
	emit(c, " movZ R, F", size == 4 ? "d" : "q", registerSized(src, size), dest);
}



static void testRR (Codegen *c, u16 size, Register a, Register b) {
	emit(c, " test R, R", registerSized(a, size), registerSized(b, size));
}

static const String inst_names[] = {
	[IAdd] =  STR_LITERAL("add"),
	[ISub] =  STR_LITERAL("sub"),
	[IIMul] = STR_LITERAL("imul"),
	[IDiv] =  STR_LITERAL("div"),
	[IIDiv] = STR_LITERAL("idiv"),
	[IOr] =   STR_LITERAL("or"),
	[IXor] =  STR_LITERAL("xor"),
	[IAnd] =  STR_LITERAL("and"),
	[IShl] =  STR_LITERAL("shl"),
	[IShr] =  STR_LITERAL("shr"),
	[ICmp] =  STR_LITERAL("cmp"),
	[ITest] = STR_LITERAL("test"),
	[ILea] =  STR_LITERAL("lea"),
	[IBtc] =  STR_LITERAL("btc"),

	[ISetO] =    STR_LITERAL("seto"),
	[ISetNO] =   STR_LITERAL("setno"),
	[ISetULT] =  STR_LITERAL("setb"),
	[ISetUGTE] = STR_LITERAL("setuae"),
	[ISetE] =    STR_LITERAL("sete"),
	[ISetNE] =   STR_LITERAL("setne"),
	[ISetULTE] = STR_LITERAL("setbe"),
	[ISetUGT] =  STR_LITERAL("setua"),
	[ISetS] =    STR_LITERAL("sets"),
	[ISetNS] =   STR_LITERAL("setns"),
	[ISetPE] =   STR_LITERAL("setpe"),
	[ISetPO] =   STR_LITERAL("setpo"),
	[ISetSLT] =  STR_LITERAL("setl"),
	[ISetSGTE] = STR_LITERAL("setge"),
	[ISetSLTE] = STR_LITERAL("setle"),
	[ISetSGT] =  STR_LITERAL("setg"),

	[IJmpO] =    STR_LITERAL("jmpo"),
	[IJmpNO] =   STR_LITERAL("jmpno"),
	[IJmpULT] =  STR_LITERAL("jmpb"),
	[IJmpUGTE] = STR_LITERAL("jmpuae"),
	[IJmpE] =    STR_LITERAL("jmpe"),
	[IJmpNE] =   STR_LITERAL("jmpne"),
	[IJmpULTE] = STR_LITERAL("jmpbe"),
	[IJmpUGT] =  STR_LITERAL("jmpua"),
	[IJmpS] =    STR_LITERAL("jmps"),
	[IJmpNS] =   STR_LITERAL("jmpns"),
	[IJmpPE] =   STR_LITERAL("jmppe"),
	[IJmpPO] =   STR_LITERAL("jmppo"),
	[IJmpSLT] =  STR_LITERAL("jmpl"),
	[IJmpSGTE] = STR_LITERAL("jmpsge"),
	[IJmpSLTE] = STR_LITERAL("jmpsle"),
	[IJmpSGT] =  STR_LITERAL("jmpsg"),

	[IRet] =  STR_LITERAL("ret"),
	[IMovzxb] =      STR_LITERAL("movzb"),
	[IMovzxw] =     STR_LITERAL("movzw"),
	[IMovsxb] =      STR_LITERAL("movsb"),
	[IMovsxw] =     STR_LITERAL("movsw"),
	[IMovsxd] =     STR_LITERAL("movsxd"),
	[IAdds] =       STR_LITERAL("adds"),
	[ISubs] =       STR_LITERAL("subs"),
	[IMuls] =       STR_LITERAL("muls"),
	[IDivs] =       STR_LITERAL("divs"),
	[IComis] =      STR_LITERAL("comis"),
	[ICvts2d] =     STR_LITERAL("cvtss2sd"),
	[ICvtd2s] =     STR_LITERAL("cvtsd2ss"),
	[ICvtsi2ss] =   STR_LITERAL("cvtsi2ss"),
	[ICvtsi2sd] =   STR_LITERAL("cvtsi2sd"),
	[ICvttss2si] =  STR_LITERAL("cvttss2si"),
	[ICvttss2siq] = STR_LITERAL("cvttss2siq"),
	[ICvttsd2si] =  STR_LITERAL("cvttsd2si"),
	[ICvttsd2siq] = STR_LITERAL("cvttsd2siq"),
};

static const char *sizeSuffix (u16 size) {
	if (size == 1)
		return "b";
	if (size == 2)
		return "w";
	if (size <= 4)
		return "l";
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
static void emitRegister (Codegen *c, Register reg) {
	(void) c;
	*insert++ = '%';
	const char *name = register_names[reg];
	u32 len = strlen(name);
	memcpy(insert, name, len);
	insert += len;
}

static void emitStorage (Codegen *c, u16 size, Storage s) {
	switch (s.kind) {
	case Location_VecRegister: {
		const char *name = xmm_names[s.vec_reg >> 5];
		u32 len = strlen(name);
		memcpy(insert, name, len);
		insert += len;
		emitInt(c, s.vec_reg & ~VRSIZE_MASK);
	} break;
	case Location_Register: {
		emitRegister(c, registerSized(s.reg, size));
	} break;
	case Location_Memory: {
		Mem mem = s.memory;
		emitIntSigned(c, mem.offset);
		*insert++ = '(';
		emitRegister(c, registerSized(mem.reg, I64));
		if (mem.index_scale) {
			*insert++ = ',';
			emitRegister(c, registerSized(mem.index_reg, I64));
			*insert++ = ',';
			emitInt(c, mem.index_scale);
		}
		*insert++ = ')';
	} break;
	case Location_Relocation: {
		emitName(c, c->module, s.relocation.id);
		emitIntSigned(c, s.relocation.offset);
		emitString(c, (String) STR_LITERAL("(%rip)"));
	} break;
	case Location_Constant: {
		*insert++ = '$';
		i32 val = s.constant;
		if (size < 4) {
			assert(size);
			val = val & ((1 << (8 * size)) - 1);
		}
		emitIntSigned(c, val);
	} break;
	case Location_LongConstant:
		*insert++ = '$';
		emitInt(c, s.long_constant);
		break;
	}
}

static bool isSized (BasicInst inst) {
	switch (inst) {
	case IAdd:
	case ISub:
	case IIMul:
	case IDiv:
	case IIDiv:
	case IOr:
	case IXor:
	case IAnd:
	case IShl:
	case IShr:
	case ICmp:
	case ITest:
	case ILea:
	case IBtc:
	case IMovzxb:
	case IMovzxw:
	case IMovsxb:
	case IMovsxw:
		return true;

	case IMovsxd:
	case ISetO:
	case ISetNO:
	case ISetULT:
	case ISetUGTE:
	case ISetE:
	case ISetNE:
	case ISetULTE:
	case ISetUGT:
	case ISetS:
	case ISetNS:
	case ISetPE:
	case ISetPO:
	case ISetSLT:
	case ISetSGTE:
	case ISetSLTE:
	case ISetSGT:
	case IJmpO:
	case IJmpNO:
	case IJmpULT:
	case IJmpUGTE:
	case IJmpE:
	case IJmpNE:
	case IJmpULTE:
	case IJmpUGT:
	case IJmpS:
	case IJmpNS:
	case IJmpPE:
	case IJmpPO:
	case IJmpSLT:
	case IJmpSGTE:
	case IJmpSLTE:
	case IJmpSGT:
	case IRet:
	case IAdds:
	case ISubs:
	case IMuls:
	case IDivs:
	case IComis:
	case ICvts2d:
	case ICvtd2s:
	case ICvtsi2ss:
	case ICvtsi2sd:
	case ICvttss2si:
	case ICvttss2siq:
	case ICvttsd2si:
	case ICvttsd2siq:
		return false;
	}
	unreachable;
}

static bool isFloat (BasicInst inst) {
	switch (inst) {
	case IAdds:
	case ISubs:
	case IMuls:
	case IDivs:
	case IComis:
		return true;
	default:
		return false;
	}
	unreachable;
}

static void genS (Codegen *c, u16 size, BasicInst inst, Storage src) {
	*insert++ = ' ';
	emitString(c, inst_names[inst]);
	if (isSized(inst))
		*insert++ = sizeSuffix(size)[0];
	else if (isFloat(inst))
		*insert++ = size == 4 ? 's' : 'd';
	*insert++ = ' ';
	emitStorage(c, size, src);
	*insert++ = '\n';

	if (insert > buf + (BUF - MAX_LINE))
		flushit(c->out);
}

static void genSS (Codegen *c, u16 size, BasicInst inst, Storage src, Storage dest) {
	*insert++ = ' ';
	emitString(c, inst_names[inst]);
	if (isSized(inst))
		*insert++ = sizeSuffix(size)[0];
	else if (isFloat(inst))
		*insert++ = size == 4 ? 's' : 'd';
	*insert++ = ' ';
	u32 src_size = size;
	switch (inst) {
	case IMovzxb:
	case IMovsxb:
		src_size = 1; break;
	case IMovzxw:
	case IMovsxw:
		src_size = 2; break;
	case IMovsxd:
		src_size = 4; break;
	default: break;
	}
	emitStorage(c, src_size, src);
	*insert++ = ',';
	*insert++ = ' ';
	emitStorage(c, size, dest);
	*insert++ = '\n';

	if (insert > buf + (BUF - MAX_LINE))
		flushit(c->out);
}

static void genSR (Codegen *c, u16 size, BasicInst inst, Storage src, Register dest) {
	genSS(c, size, inst, src, (Storage) {Location_Register, .reg = dest});
}
static void genSM (Codegen *c, u16 size, BasicInst inst, Storage src, Mem dest) {
	genSS(c, size, inst, src, (Storage) {Location_Memory, .memory = dest});
}
static void genSF (Codegen *c, u16 size, BasicInst inst, Storage src, VecRegister dest) {
	genSS(c, size, inst, src, (Storage) {Location_VecRegister, .vec_reg = dest});
}

static void genCR (Codegen *c, u16 size, BasicInst inst, i32 val, Register dest) {
	genSR(c, size, inst, (Storage) {Location_Constant, .constant = val}, dest);
}
static void genCM (Codegen *c, u16 size, BasicInst inst, i32 val, Mem dest) {
	genSM(c, size, inst, (Storage) {Location_Constant, .constant = val}, dest);
}
static void genCF (Codegen *c, u16 size, BasicInst inst, i32 val, VecRegister dest) {
	genSF(c, size, inst, (Storage) {Location_Constant, .constant = val}, dest);
}
static void genLR (Codegen *c, u16 size, BasicInst inst, u64 val, Register dest) {
	genSR(c, size, inst, (Storage) {Location_LongConstant, .long_constant = val}, dest);
}
static void genLM (Codegen *c, u16 size, BasicInst inst, u64 val, Mem dest) {
	genSM(c, size, inst, (Storage) {Location_LongConstant, .long_constant = val}, dest);
}

static void genRR (Codegen *c, u16 size, BasicInst inst, Register src, Register dest) {
	genSR(c, size, inst, (Storage) {Location_Register, .reg = src}, dest);
}
static void genRM (Codegen *c, u16 size, BasicInst inst, Register src, Mem dest) {
	genSM(c, size, inst, (Storage) {Location_Register, .reg = src}, dest);
}
static void genRF (Codegen *c, u16 size, BasicInst inst, Register src, VecRegister dest) {
	genSF(c, size, inst, (Storage) {Location_Register, .reg = src}, dest);
}

static void genFR (Codegen *c, u16 size, BasicInst inst, VecRegister src, Register dest) {
	genSR(c, size, inst, (Storage) {Location_VecRegister, .vec_reg = src}, dest);
}
static void genFM (Codegen *c, u16 size, BasicInst inst, VecRegister src, Mem dest) {
	genSM(c, size, inst, (Storage) {Location_VecRegister, .vec_reg = src}, dest);
}
static void genFF (Codegen *c, u16 size, BasicInst inst, VecRegister src, VecRegister dest) {
	genSF(c, size, inst, (Storage) {Location_VecRegister, .vec_reg = src}, dest);
}


static void genMR (Codegen *c, u16 size, BasicInst inst, Mem src, Register dest) {
	genSR(c, size, inst, (Storage) {Location_Memory, .memory = src}, dest);
}
static void genMM (Codegen *c, u16 size, BasicInst inst, Mem src, Mem dest) {
	genSM(c, size, inst, (Storage) {Location_Memory, .memory = src}, dest);
}
static void genMF (Codegen *c, u16 size, BasicInst inst, Mem src, VecRegister dest) {
	genSF(c, size, inst, (Storage) {Location_Memory, .memory = src}, dest);
}

static void genM (Codegen *c, u16 size, BasicInst inst, Mem src) {
	genS(c, size, inst, (Storage) {Location_Memory, .memory = src});
}
static void genR (Codegen *c, u16 size, BasicInst inst, Register src) {
	genS(c, size, inst, (Storage) {Location_Register, .reg = src});
}
static void gen (Codegen *c, u16 size, BasicInst inst) {
	emit(c, " SZ", inst_names[inst], sizeSuffix(size));
}

static void jmpB (Codegen *c, const Block *dest) {
	emit(c, " jmp ..I_I_S", c->current_id, dest->id, dest->label);
}
static void jccB (Codegen *c, Condition cond, const Block *dest) {
	emit(c, " jZ ..I_I_S", condition_names[cond], c->current_id, dest->id, dest->label);
}

typedef struct {
	String name;
	u32 func;
	u32 id;
} Label;

static Label newLabel (Codegen *c, const char *name, u32 id) {
	(void) c;
	return (Label) {zstr(name), c->current_id, id};
}
static void placeLabel (Codegen *c, Label l) {
	emit(c, "..S_I_I:", l.name, l.func, l.id);
}
static void jmpL (Codegen *c, Label l) {
	emit(c, " jmp ..S_I_I", l.name, l.func, l.id);
}
static void jccL (Codegen *c, Condition cond, Label l) {
	emit(c, " jZ ..S_I_I", condition_names[cond], l.name, l.func, l.id);
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



static void startSection (Codegen *c, ElfSection sec) {
	emit(c, "\n\n.section Z", section_names[sec]);
}



static void emitString (Codegen *c, String s) {
	(void) c;
	if (s.len == 0) return;
	memcpy(insert, s.ptr, s.len);
	insert += s.len;
}

static void emitZString (Codegen *c, const char *s) {
	(void) c;
	size_t len = strlen(s);
	memcpy(insert, s, len);
	insert += len;
}

static void writeU8 (Codegen *c, u8 byte) {
	emitString(c, zstr("	.byte "));
	emitInt(c, byte);
	emitString(c, zstr("\n"));
}

static void writeU16 (Codegen *c, u16 val) {
	emitString(c, zstr("	.short "));
	emitInt(c, val);
	emitString(c, zstr("\n"));
}
static void writeU32 (Codegen *c, u32 val) {
	emitString(c, zstr("	.int "));
	emitInt(c, val);
	emitString(c, zstr("\n"));
}
static void writeU64 (Codegen *c, u64 val) {
	emitString(c, zstr("	.quad "));
	emitInt(c, val);
	emitString(c, zstr("\n"));
}
static void writeUleb128 (Codegen *c, u64 a) {
	emitString(c, zstr("	.uleb128 "));
	emitInt(c, a);
	emitString(c, zstr("\n"));
}
static void writeSleb128 (Codegen *c, i64 a) {
	emitString(c, zstr("	.sleb128 "));
	emitIntSigned(c, a);
	emitString(c, zstr("\n"));
}
static void writeUleb128Pair (Codegen *c, u64 a, u64 b) {
	writeUleb128(c, a);
	writeUleb128(c, b);
}
static void writeString (Codegen *c, String val) {
	emitString(c, zstr("	.string \""));
	emitString(c, val);
	emitString(c, zstr("\"\n"));
}
static void writeSymbolRef (Codegen *c, String val) {
	emitString(c, zstr("	.quad "));
	emitString(c, val);
	emitString(c, zstr("\n"));
}
static void writeSymbolEndRef (Codegen *c, String val) {
	emitString(c, zstr("	.quad ."));
	emitString(c, val);
	emitString(c, zstr("_end\n"));
}

static void startSymbol (Codegen *c, String name) {
	emitString(c, zstr("."));
	emitString(c, name);
	emitString(c, zstr(":\n"));
}
static void endSymbol (Codegen *c, String name) {
	emitString(c, zstr("."));
	emitString(c, name);
	emitString(c, zstr("_end:\n"));
}

static inline bool printable(char c) {
	return c >= ' ' && c <= '~' && c != '"';
}

// Spending a bit of time to compress the assembly is totally worth it.
static void emitDataLine (Codegen *c, u32 len, const char *data) {
	FILE *out = c->out;
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
			emitDataString(c, i - string_begin, data + string_begin);
		}

		if (insert > buf + (BUF - MAX_LINE))
			flushit(out);
	}
}

static void emitDataString (Codegen *c, u32 len, const char *data) {
	emitZString(c, " .ascii \"");
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



// PERFORMANCE This consumes an obscene 7% of execution time.
static void emitInt (Codegen *c, u64 i) {
	(void) c;
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

static void emitIntSigned (Codegen *c, i64 val) {
	(void) c;
	if (val >= 0) {
		*insert++ = '+';
		emitInt(c, val);
	} else {
		*insert++ = '-';
		assert(val != INT64_MIN);
		emitInt(c, -(i64)val);
	}
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
			emitZString(c, va_arg(args, const char *));
		} break;
		case 'S': {
			String str = va_arg(args, String);
			if (str.len)
				memcpy(insert, str.ptr, str.len);
			insert += str.len;
		} break;
		case 'F': {
			VecRegister reg = va_arg(args, VecRegister);
			const char *name = xmm_names[reg >> 5];
			u32 len = strlen(name);
			memcpy(insert, name, len);
			insert += len;
			emitInt(c, reg & ~VRSIZE_MASK);
		} break;
		case 'R': {
			emitRegister(c, va_arg(args, Register));
		} break;
		case 'M': {
			Mem mem = va_arg(args, Mem);
			emitIntSigned(c, mem.offset);
			*insert++ = '(';
			emitRegister(c, registerSized(mem.reg, I64));
			*insert++ = ')';
		} break;
		case '#': {
			IrRef ref = va_arg(args, IrRef);
			emitIntSigned(c, c->storage[ref]);
			memcpy(insert, "(%rbp)", 6);
			insert += 6;
		} break;
		case 'I':
			emitInt(c, va_arg(args, u32));
			break;
		case 'L':
			emitInt(c, va_arg(args, u64));
			break;
		case '~': {
			p++;
			if (*p == 'I') {
				emitIntSigned(c, va_arg(args, i32));
			} else {
				assert(*p == 'L');
				emitIntSigned(c, va_arg(args, i64));
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

