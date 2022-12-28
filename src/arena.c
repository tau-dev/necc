#include <stdio.h>
#include <stdalign.h>

#include "arena.h"

#define BLOCK_HEADER sizeof(ArenaBlock*)


/*

Provides functions to allocate quickly from a memory pool.

*/

struct ArenaBlock {
	ArenaBlock *next;
	char data[];
};

Arena create_arena(size_t block_size) {
	assert(block_size > 0);
// #ifdef NDEBUG
// 	return (Arena) {
// 		.last_used = block_size,
// 		.block_size = block_size,
// 	};
// #else
	// Use a block size of 0 so that each allocation is separate, for easier debugging.
	// PERFORMANCE This appears to be faster than bundled allocations,
	// which is a bit bizarre. Investigate.
	return (Arena) {.last_used = 1};
// #endif
}

static u64 allocations = 0;
void *aalloc(Arena* arena, size_t size) {
	assert(arena->last_used);
	size_t alignment = alignof(max_align_t);
	size = (size + alignment - 1) / alignment * alignment;

	if (arena->last_used + size >= arena->block_size) {
		u32 next_size = size >= arena->block_size ? size : arena->block_size;
		ArenaBlock *new = malloc(next_size + BLOCK_HEADER);
		if (!new) {
			puts("ERROR: Out ouf memory on arena extension.");
			exit(EXIT_FAILURE);
		}
		arena->total_used += next_size + BLOCK_HEADER;
		new->next = arena->last_block;

		arena->last_block = new;
		arena->last_used = 0;
		allocations++;
	}
	void *res = arena->last_block->data + arena->last_used;
	arena->last_used += size;
	return res;
}

char *adupe(Arena *arena, String s) {
	char *d = aalloc(arena, s.len);
	memcpy(d, s.ptr, s.len);
	return d;
}

char *adupez(Arena *arena, const char *c) {
	return adupe(arena, (String) {strlen(c)+1, c});
}


void free_arena(Arena* arena, const char *name) {
#ifndef NDEBUG
	fprintf(stderr, "Arena freeing %llu KiB of %s\n", (unsigned long long) arena->total_used / 1024, name);
	fprintf(stderr, "%llu calls to malloc so far.\n", (unsigned long long) allocations);
#endif
	while (arena->last_block != NULL) {
		ArenaBlock *next = arena->last_block->next;
		free(arena->last_block);
		arena->last_block = next;
	}
}
