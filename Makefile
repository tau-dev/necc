C_TESTS := $(wildcard tests/*.c)
C_SRCS := $(wildcard src/*.c)
C_HDRS := $(wildcard src/*.h)

C_FLAGS := -lm -std=c11 -Werror -Wall -Wextra -Wno-unused-command-line-argument -pedantic -Wpedantic -Wno-missing-field-initializers
# C_DBG_FLAGS := -g -O0
C_DBG_FLAGS := -g -O0 -fsanitize=address -fsanitize=undefined
# C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O3 -flto=auto -DNDEBUG
C_REL_FLAGS := -fstrict-aliasing -g -w -fno-omit-frame-pointer -O1

OBJS := $(patsubst src/%.c,bin/%.o,$(C_SRCS))

REQUIRED_DIRS := bin tests/bin

debug: bin/necc-dbg

release: bin/necc

run: debug
	@./bin/necc-dbg test.c

test: tests/bin/main
	@./tests/bin/main && echo "Tests passed." || echo "Tests failed."



tests/bin/main: $(C_TESTS) $(filter-out bin/main.c.o, $(OBJS))
	$(CC) $^ $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/necc-dbg: $(OBJS)
	$(CC) $^ $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/%.o: src/%.c $(C_HDRS)
	$(CC) -c $< $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/necc: $(C_SRCS) $(C_HDRS)
	$(CC) $(C_SRCS) $(C_FLAGS) $(C_REL_FLAGS) -o $@



.PHONY: debug release re clean printvars \
		run run-rel gdb lldb

clean:
	@rm -f -r bin/*
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

