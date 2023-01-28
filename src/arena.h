#pragma once

#include <stddef.h>
#include "util.h"

typedef struct ArenaBlock ArenaBlock;

typedef struct Arena {
	ArenaBlock *last_block;

	size_t total_used;

	size_t last_used;
	size_t block_size;

	u64 block_count;
	u64 allocs;
} Arena;

Arena create_arena(size_t block_size);
void *aalloc(Arena*, size_t size);
char *adupe(Arena*, String s);
char *adupez(Arena*, const char *s);
void free_arena(Arena*, const char *name);

#define ALLOC(arena, type) ((type*) aalloc((arena), sizeof (type)))
#define ALLOCN(arena, type, count) { .len = (count), .ptr = aalloc((arena), (count) * sizeof (type)) }
