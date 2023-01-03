.POSIX:

ROOT=../../..
SCCPREFIX=$(ROOT)
CC=$(ROOT)/bin/scc

all:
	@CC=$(CC) ./chktest.sh scc-tests.lst

tests:
	@$(MAKE) |\
	grep -v TODO |\
	grep FAIL >/dev/null &&\
	printf '[FAIL]' || printf '[PASS]'; printf '\tcc/execute\n'

clean:
	rm -f a.out *.s *.s *.as *.o *.ir *.qbe *core* test.log tests.h tmp_*.c
