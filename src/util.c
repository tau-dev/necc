#include "util.h"
#include "wyhash.h"
#include "stdio.h"
#include "ansii.h"

#define SLOT_UNUSED 0
#define SLOT_TOMBSTONE 128
#define FOUND_NONE ((u32) -1)
#define MAX_LOAD_PERCENTAGE 70


bool eql (const char* a, String b) {
	return strlen(a) == b.len && memcmp(a, b.ptr, b.len) == 0;
}

bool startsWith (const char* a, String b) {
	return memcmp(a, b.ptr, b.len) == 0;
}

String zString (const char *s) {
	return (String) {strlen(s), s};
}

static void printErr (SourceFile source, u32 offset, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vprintErr(source, offset, msg, args);
    va_end(args);
}

SourceFile *readAllAlloc (String path, String filename) {
	char *filename_z = malloc(path.len + filename.len + 1);
	if (filename_z) {
		memcpy(filename_z, path.ptr, path.len);
		memcpy(filename_z + path.len, filename.ptr, filename.len);
		filename_z[path.len + filename.len] = 0;
		FILE *f = fopen(filename_z, "r");
		if (f) {
			if (fseek(f, 0, SEEK_END) == 0) {
				long count = ftell(f);
				if (count >= 0 && fseek(f, 0, SEEK_SET) == 0) {
					char *data = malloc(sizeof(SourceFile) + count+1);
					char *content = data + sizeof(SourceFile);
					if (data) {
						size_t got = fread(content, 1, count, f);
						const char *nullbyte = memchr(content, 0, got);
						if (nullbyte) {
							SourceFile source = { filename, {got, content} };
							printErr(source, nullbyte - content, "file should not contain a null byte");
						} else if (got == (size_t)count) {
							content[count] = 0;
							free(filename_z);
							fclose(f);
							SourceFile *result = (SourceFile*) data;
							result->name = filename;
							result->content = (String) {count, content};
							result->idx = 0;
							return result;
						}
						free(data);
					}
				}
			}
			fclose(f);
		}
		free(filename_z);
	}
	return NULL;
}

void printString (String s) {
	fwrite(s.ptr, 1, s.len, stdout);
}

u64 strHash (String str) {
	return wyhash(str.ptr, str.len, 1337, _wyp);
}

typedef struct {
	u32 line;
	u32 col;
} SourceLocation;

SourceLocation findSourcePos(const char *source, const char *pos) {
	u32 line = 1;
	u32 col = 1;
	while (source < pos) {
		if (*source == '\n') {
			line++;
			col = 1;
		} else {
			col++;
		}
		source++;
	}
	return (SourceLocation) {line, col};
}

void vprintErr (SourceFile source, u32 offset, const char *msg, va_list vlist) {
	SourceLocation loc = findSourcePos(
			source.content.ptr, source.content.ptr + offset);

	fwrite(source.name.ptr, source.name.len, 1, stderr);
	fprintf(stderr, ":%lu:%lu: " RED "error: " RESET,
			(unsigned long) loc.line, (unsigned long) loc.col);

    vfprintf(stderr, msg, vlist);
    printf(".\n");
}

static u32 find (StringMap *map, u64 hash, String str) {
	if (map->used == 0)
		return FOUND_NONE;

	const u8 *const headers = map->headers;
	const u32 length = map->capacity;
	String **keys = (String **) map->content;

	u32 index = hash & (length - 1);
	u8 marker = hash >> 56;
	if (marker == SLOT_UNUSED || marker == SLOT_TOMBSTONE)
		marker++;

	u32 i;
	for (i = index; i < length; i++) {
		if (headers[i] == SLOT_UNUSED)
			return FOUND_NONE;
		if (headers[i] != marker)
			continue;

		if (SPAN_EQL(*keys[i], str))
			return i;
	}

	for (i = 0; i < index; i++) {
		if (headers[i] == SLOT_UNUSED)
			return FOUND_NONE;
		if (headers[i] != marker)
			continue;

		if (SPAN_EQL(*keys[i], str))
			return i;
	}
	return FOUND_NONE;
}

static void grow (StringMap *);

static void **insertNew (StringMap *map, u64 hash) {
	u8 *const headers = map->headers;
	const u32 length = map->capacity;

	u32 index = hash & (length - 1);
	u8 marker = hash >> 56;
	if (marker == SLOT_UNUSED || marker == SLOT_TOMBSTONE)
		marker++;

	u32 i = index;
	while (i < length && headers[i] != SLOT_UNUSED && headers[i] != SLOT_TOMBSTONE)
		i++;

	if (i == length && length != 0) {
		i = 0;
		while (i < index && headers[i] != SLOT_UNUSED && headers[i] != SLOT_TOMBSTONE)
			i++;
	}

	map->used++;
	if (map->used * 100 >= map->capacity * MAX_LOAD_PERCENTAGE) {
		grow(map);
		return insertNew(map, hash);
	} else {
		headers[i] = marker;
		return &map->content[i];
	}
}

static void grow (StringMap *map) {
	u32 new_capacity = map->capacity == 0 ?
		8 :
		map->capacity * 2;

	StringMap new_map = (StringMap) {
		.headers = calloc(new_capacity, 1), // TODO Catch OOM
		.content = calloc(new_capacity, sizeof(void *)),
		.capacity = new_capacity,
		.used = 0,
	};

	for (u32 i = 0; i < map->capacity; i++) {
		if (map->content[i] == NULL)
			continue;
		u64 hash = strHash(*(String *) map->content[i]);
		*insertNew(&new_map, hash) = map->content[i];
	}
	free(map->headers);
	free(map->content);

	*map = new_map;
}

void **mapGetOrCreate (StringMap *map, String str) {
	u64 hash = strHash(str);

	u32 found = find(map, hash, str);
	if (found != FOUND_NONE)
		return &map->content[found];
	else
		return insertNew(map, hash);
}

void *mapGet (StringMap *map, String str) {
	u32 i = find(map, strHash(str), str);
	if (i == FOUND_NONE)
		return NULL;
	return map->content[i];
}

void *mapRemove (StringMap *map, String str) {
	u32 i = find(map, strHash(str), str);
	if (i == FOUND_NONE)
		return NULL;

	void *prev = map->content[i];
	map->headers[i] = SLOT_TOMBSTONE;
	map->content[i] = NULL;
	map->used--;
	return prev;
}


void mapFree (StringMap *map) {
	free(map->headers);
	map->headers = NULL;
	free(map->content);
	map->content = NULL;
}
