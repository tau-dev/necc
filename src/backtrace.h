#include <libunwind.h>
#define PRINT_STACK_TRACE do { \
	unw_context_t ctx; \
	unw_getcontext(&ctx); \
	print_stack_trace(&ctx); \
} while (0)

int print_stack_trace(ucontext_t *context);

