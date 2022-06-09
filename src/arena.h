#pragma once

#include <stddef.h>
#include "util.h"

typedef struct ArenaBlock ArenaBlock;

typedef struct Arena {
	ArenaBlock *last_block;

	size_t last_used;
	size_t block_size;
} Arena;

Arena create_arena(size_t block_size);
void *aalloc(Arena*, size_t size);
void free_arena(Arena*);

#define ALLOC(arena, type) (type*) aalloc((arena), sizeof (type))
#define ALLOCN(arena, type, count) { .len = (count), .ptr = aalloc((arena), (count) * sizeof (type)) }
