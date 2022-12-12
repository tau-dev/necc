#include <stdio.h>
#include <stdalign.h>

#include "arena.h"

#define BLOCK_HEADER sizeof(void*)


/*

Provides functions to allocate quickly from a memory pool.

*/

struct ArenaBlock {
	ArenaBlock *next;
	char data[];
};

Arena create_arena(size_t block_size) {
	assert(block_size > 0);
	return (Arena) {
		.last_used = block_size,
		.block_size = block_size,
	};
}

void *aalloc(Arena* arena, size_t size) {
	assert(size <= arena->block_size);
	size_t alignment = alignof(max_align_t);
	size = (size + alignment - 1) / alignment * alignment;

	if (arena->last_used + size >= arena->block_size) {
		ArenaBlock *new = malloc(arena->block_size + BLOCK_HEADER);
		if (!new) {
			puts("ERROR: Out ouf memory on arena extension.");
			exit(EXIT_FAILURE);
		}
		new->next = arena->last_block;

		arena->last_block = new;
		arena->last_used = 0;
	}

	void *res = arena->last_block->data + arena->last_used;
	arena->last_used += size;
	return res;
}

void free_arena(Arena* arena) {
	while (arena->last_block != NULL) {
		ArenaBlock *next = arena->last_block->next;
		free(arena->last_block);
		arena->last_block = next;
	}
}
