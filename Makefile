C_TESTS := $(wildcard tests/*.c)
C_SRCS := $(wildcard src/*.c)
C_HDRS := $(wildcard src/*.h)

C_FLAGS := -lm -std=c11 -Werror -Wall -Wextra -Wno-unused-command-line-argument -pedantic -Wpedantic -Wno-missing-field-initializers
C_DBG_FLAGS := -g -O0 -fsanitize=address -fsanitize=undefined
# C_DBG_FLAGS := -g -O0
C_REL_FLAGS := -O3 -fstrict-aliasing -flto -DNDEBUG -w

OBJS := $(patsubst src/%.c,bin/%.o,$(C_SRCS))

REQUIRED_DIRS := bin tests/bin

debug: bin/nic-dbg

release: bin/nic

run: debug
	@./bin/nic-dbg test.c

test: tests/bin/main
	@./tests/bin/main && echo "Tests passed." || echo "Tests failed."



tests/bin/main: $(C_TESTS) $(filter-out bin/main.c.o, $(OBJS))
	$(CC) $^ $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/nic-dbg: $(OBJS)
	$(CC) $^ $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/%.o: src/%.c $(C_HDRS)
	$(CC) -c $< $(C_FLAGS) $(C_DBG_FLAGS) -o $@

bin/nic: $(C_SRCS) $(C_HDRS)
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

lldb: bin/nic-dbg
	@lldb ./bin/nic-dbg -- test.c

gdb: bin/nic-dbg
	@gdb --args $^ test.c

$(shell mkdir -p $(REQUIRED_DIRS))

