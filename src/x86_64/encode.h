// typedef struct Codegen Codegen;
static void emit(Codegen *c, const char *fmt, ...);

typedef struct Mem {
	Storage reg;
	i32 offset;
} Mem;

static void push (Codegen *c, Storage r) {
	emit(c, " push R", r);
}

static void movRR (Codegen *c, Storage src, Storage dest) {
	emit(c, " mov R, R", src, dest);
}

static void subRC (Codegen *c, Storage dest, i32 val) {
	emit(c, " sub $~I, R", val, dest);
}
static void movRM (Codegen *c, Storage src, Mem mem) {
	emit(c, " movZ R, ~I(R)", sizeSuffix(sizeOfRegister(src)), src, mem.offset, mem.reg);
}
static void movMR (Codegen *c, Mem mem, Storage dest) {
	emit(c, " movZ ~I(R), R", sizeSuffix(sizeOfRegister(dest)), mem.offset, mem.reg, dest);
}
static void testRR (Codegen *c, Storage a, Storage b) {
	emit(c, " test R, R", a, b);
}

