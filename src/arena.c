#include <stdio.h>

// Dear MSVC team,
// didn't you promise to be better and finally adhere to standards?
// Sincerely,
// A Concerned Programmer
#ifndef _MSC_VER
#include <stdalign.h>
#endif

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
#ifdef NDEBUG
	return (Arena) {
		.last_used = block_size,
		.block_size = block_size,
	};
#else
	return (Arena) {.last_used = 1};
#endif
}

void *aalloc(Arena* arena, size_t size) {
	if (size == 0)
		return NULL;

#ifdef _MSC_VER
	size_t alignment = 16; // Sod it. 
#else
	size_t alignment = _Alignof(max_align_t);
#endif
	size = (size + alignment - 1) / alignment * alignment;

	if (arena->last_used + size > arena->block_size) {
		size_t next_size = size >= arena->block_size ? size : arena->block_size;
		ArenaBlock *new = malloc(next_size + BLOCK_HEADER);
		if (!new) {
			puts("ERROR: Out ouf memory on arena extension.");
			exit(EXIT_FAILURE);
		}
		arena->total_used += next_size + BLOCK_HEADER;
		new->next = arena->last_block;

		arena->last_block = new;
		arena->last_used = 0;
		arena->block_count++;
	}
	void *res = arena->last_block->data + arena->last_used;
	arena->last_used += size;
	arena->allocs++;
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

typedef unsigned long long ullong;
void free_arena(Arena* arena, const char *name) {
	(void) name;
// #ifndef NDEBUG
// 	fprintf(stderr, "Arena freeing %llu KiB of %s in %llu blocks from %llu aallocs\n",
// 		(ullong) arena->total_used / 1024, name,
// 		(ullong) arena->block_count, (ullong) arena->allocs);
// #endif
	while (arena->last_block != NULL) {
		ArenaBlock *next = arena->last_block->next;
		free(arena->last_block);
		arena->last_block = next;
	}
}
