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


static const char *xmm_names[] = {
	"%xmm",
	"%ymm",
	"%zmm",
};

static const char *condition_names[] = {
	[Cond_Overflow] = "o",
	[Cond_NoOverflow] = "no",
	[Cond_ULessThan] = "b",
	[Cond_UGreaterThanOrEquals] = "ae",
	[Cond_Equal] = "e",
	[Cond_NotEqual] = "ne",
	[Cond_ULessThanOrEquals] = "be",
	[Cond_UGreaterThan] = "a",
	[Cond_Sign] = "s",
	[Cond_NoSign] = "ns",
	[Cond_ParityEven] = "pe",
	[Cond_ParityOdd] = "po",
	[Cond_SLessThan] = "l",
	[Cond_SGreaterThanOrEquals] = "ge",
	[Cond_SLessThanOrEquals] = "le",
	[Cond_SGreaterThan] = "g",
};



typedef struct Mem {
	Storage reg;
	i32 offset;
} Mem;

static void push (Codegen *c, Register r) {
	emit(c, " push R", r);
}

static void movRR (Codegen *c, Register src, Register dest) {
	emit(c, " mov R, R", src, dest);
}

static void movRM (Codegen *c, Register src, Mem mem) {
	emit(c, " movZ R, ~I(R)", sizeSuffix(sizeOfRegister(src)), src, mem.offset, mem.reg);
}
static void movMR (Codegen *c, Mem mem, Register dest) {
	emit(c, " movZ ~I(R), R", sizeSuffix(sizeOfRegister(dest)), mem.offset, mem.reg, dest);
}
static void movCM (Codegen *c, u32 val, Register size, Mem mem) {
	emit(c, " movZ $I, ~I(R)", sizeSuffix(sizeOfRegister(size)), val, mem.offset, mem.reg);
}
static void movCR (Codegen *c, u32 val, Register dest) {
	emit(c, " movZ $I, R", sizeSuffix(sizeOfRegister(dest)), val, dest);
}

static void movFR (Codegen *c, VecRegister src, Register dest) {
	emit(c, " movsZ F, R", sizeFSuffix(dest), src, dest);
}
static void movFM (Codegen *c, VecRegister src, u16 size, Mem mem) {
	emit(c, " movsZ F, ~I(R)", sizeFSuffix(size), src, mem.offset, mem.reg);
}
static void movMF (Codegen *c, Mem mem, VecRegister dest, u16 size) {
	emit(c, " movsZ ~I(R), F", sizeFSuffix(size), mem.offset, mem.reg, dest);
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


static void testRR (Codegen *c, Storage a, Storage b) {
	emit(c, " test R, R", a, b);
}

static void subRC (Codegen *c, Register dest, i32 val) {
	emit(c, " sub $~I, R", val, dest);
}

static void emitString (Codegen *c, String s) {
	(void) c;
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
	emitString(c, zstr("	.quad ."));
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
			*insert++ = '%';
			const char *name = register_names[va_arg(args, Register)];
			u32 len = strlen(name);
			memcpy(insert, name, len);
			insert += len;
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

