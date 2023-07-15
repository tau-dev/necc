C_SRCS := $(wildcard src/*.c)
C_HDRS := $(wildcard src/*.h)

C_FLAGS := -lm -std=c11 -Werror -Wall -Wextra -Wno-unused-command-line-argument -pedantic -Wpedantic -Wno-missing-field-initializers \
	-DMUSL_DIR=\"$(MUSL_DIR)\"
# C_DBG_FLAGS := -g -O0
C_DBG_FLAGS := -g -O0 -fsanitize=address -fsanitize=undefined
# C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O3 -flto=auto -DNDEBUG
C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O1

OBJS := $(patsubst src/%.c,bin/%.o,$(C_SRCS))
SELFHOST_OBJS := $(patsubst src/%.c,selfhost/%.o,$(C_SRCS))
SELFSELF_OBJS := $(patsubst src/%.c,selfself/%.o,$(C_SRCS))

REQUIRED_DIRS := bin tests/bin selfhost selfself

debug: bin/necc-dbg

release: bin/necc

self: selfhost/necc
	@echo
selfs: selfself/necc

test: bin/necc-dbg bin/necc tests/runner self
all: debug release selfs

	@./tests/runner


tests/runner: tests/runner.c tests/util.c
	$(CC) $< $(C_FLAGS) $(C_DBG_FLAGS) -std=gnu17 -o $@

bin/necc-dbg: $(OBJS)
	$(CC) $^ $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/%.o: src/%.c $(C_HDRS)
	$(CC) -c $< $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/necc: $(C_SRCS) $(C_HDRS)
	$(CC) $(C_SRCS) $(C_FLAGS) $(C_REL_FLAGS) -o $@

selfhost/necc: $(SELFHOST_OBJS)
	musl-gcc -static -g $^ -lm -o $@

selfhost/%.o: src/%.c $(C_HDRS) bin/necc-dbg
	./bin/necc-dbg $< -g -obj=$@ -std gnu -def MUSL_DIR=\"$(MUSL_DIR)\"


selfself/necc: $(SELFSELF_OBJS)
	musl-gcc -static -g $^ -lm -o $@
	diff selfhost/ selfself/
	# TODO Add error message here: "Inherited codegen error: self-compiled executable self-compiling does not yield the same executable."

selfself/%.o: src/%.c $(C_HDRS) selfhost/necc
	./selfhost/necc $< -g -obj=$@ -std gnu -def MUSL_DIR=\"$(MUSL_DIR)\"



.PHONY: debug release re clean printvars \
		self run run-rel gdb lldb all

clean:
	@rm -f -r bin/*
	@rm -f -r selfhost/*
	@rm -f -r tests/bin/*
	@mkdir -p $(REQUIRED_DIRS)

re: clean debug

printvars:
	@echo "Sources: $(C_SRCS)"
	@echo "Objs: $(OBJS)"
	@echo "Headers: $(C_HDRS)"

lldb: bin/necc-dbg
	@lldb ./bin/necc-dbg -- test.c

gdb: bin/necc-dbg
	@gdb --args $^ test.c

$(shell mkdir -p $(REQUIRED_DIRS))
