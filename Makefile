C_SRCS := $(wildcard src/*.c)
C_HDRS := $(wildcard src/*.h)

C_FLAGS := -lm -std=c11 -Werror -Wall -Wextra -Wno-unused-command-line-argument -pedantic -Wpedantic -Wno-missing-field-initializers \
	-DMUSL_DIR=\"/home/tau/foreign/lang/musl-1.2.3\"
# C_DBG_FLAGS := -g -O0
C_DBG_FLAGS := -g -O0 -fsanitize=address -fsanitize=undefined
# C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O3 -flto=auto -DNDEBUG
C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O1

OBJS := $(patsubst src/%.c,bin/%.o,$(C_SRCS))
SELFHOST_OBJS := $(patsubst src/%.c,selfhost/%.o,$(C_SRCS))

REQUIRED_DIRS := bin tests/bin selfhost

debug: bin/necc-dbg

release: bin/necc

self: selfhost/necc

test: bin/necc-dbg bin/necc tests/runner
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
	musl-gcc -static $^ -lm -o $@

selfhost/%.o: src/%.c $(C_HDRS) bin/necc-dbg
	./bin/necc-dbg $< -c=$@ -std gnu -Werror -def MUSL_DIR=\"/home/tau/foreign/lang/musl-1.2.3\"



.PHONY: debug release re clean printvars \
		self run run-rel gdb lldb

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
