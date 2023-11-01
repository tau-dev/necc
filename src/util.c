#include "util.h"
#include "wyhash.h"
#include "stdio.h"
#include "ansi.h"
#ifdef __unix__
#include <sys/stat.h>
#include <unistd.h>
#endif

#define SLOT_UNUSED 0
#define SLOT_TOMBSTONE 128
#define FOUND_NONE ((u32) -1)
#define MAX_LOAD_PERCENTAGE 70

/*

Provides functions for strings, hash maps and error reporting.

*/

const char *ifTerminal(const char *val) {
#ifdef __unix__
	if (isatty(2))
		return val;
#endif
	return "";
}

// TODO Do not allow regular files.
bool isDirectory(const char *path) {
	assert(path);
#ifdef __unix__
	struct stat path_stat;
	if (stat(path, &path_stat) == -1)
		return false;
	return S_ISDIR(path_stat.st_mode);
#endif
	return !isFile(path);
}

// TODO Do not allow regular files.
bool isFile(const char *path) {
#ifdef __unix__
	struct stat path_stat;
	if (stat(path, &path_stat) == -1)
		return false;
	return S_ISREG(path_stat.st_mode);
#endif
	FILE *f = fopen(path, "r");
	if (f == NULL)
		return false;
	fclose(f);
	return true;
}

bool eql (const char* a, String b) {
	// PERFORMANCE Remove strlen
	return strlen(a) == b.len && memcmp(a, b.ptr, b.len) == 0;
}

bool startsWith (const char* a, String b) {
	return memcmp(a, b.ptr, b.len) == 0;
}

String zstr (const char *s) {
	return (String) {strlen(s), s};
}

void *mdupe(const void *data, size_t len) {
	void *new = malloc(len);
	memcpy(new, data, len);
	return new;
}


static void printError (SourceFile source, Location loc, const char *msg, ...) {
	printErr(source, loc);
	va_list args;
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	fprintf(stderr, ".\n");
	exit(1);
}


static SourceFile *readFileData (FILE *f, long count, String filename) {
	char *data = malloc(sizeof(SourceFile) + count+2);
	if (!data) return NULL;
	char *content = data + sizeof(SourceFile);

	size_t got = fread(content, 1, count, f);
	const char *nullbyte = memchr(content, 0, got);

	SourceFile source = { filename, .content = {got, content} };
	if (nullbyte) {
		Location null_loc = {0, 1, 1};
		printError(source, null_loc, "file should not contain a null byte");
	} else if (got == (size_t)count) {
		// Discard BOM.
		if (count >= 3 && (uchar) content[0] == 0xef && (uchar) content[1] == 0xbb && (uchar) content[2] == 0xbf) {
			count -= 3;
			content += 3;
		}
		content[count] = 0;
		content[count+1] = 0; // The lexer sometimes wants to skip two characters at once.
		source.content = (String) {count, content};

		SourceFile *result = (SourceFile*) data;
		*result = source;
		return result;
	}
	free(data);
	return NULL;
}

SourceFile *readAllAlloc (String filename) {
	assert(filename.ptr[filename.len] == 0);

	SourceFile *result = NULL;
	FILE *f = NULL;

	if (isFile(filename.ptr))
		f = fopen(filename.ptr, "r");
	if (f) {
		if (fseek(f, 0, SEEK_END) == 0) {
			long count = ftell(f);
			if (count >= 0 && fseek(f, 0, SEEK_SET) == 0)
				result = readFileData(f, count, filename);
		}
		fclose(f);
	}
	return result;
}

String sourceName (SourceFile *source) {
	return source->plain_name.ptr ? source->plain_name : source->abs_name;
}

u64 strHash (String str) {
	return wyhash(str.ptr, str.len, 1337, _wyp);
}

typedef struct {
	u32 line;
	u32 col;
} SourceLocation;

SourceLocation findSourcePos (const char *source, const char *pos) {
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

void strAppend (DynString *str, String a) {
	if (str->len + a.len > str->capacity) {
		str->capacity = str->capacity * 3 / 2 + a.len;
		str->ptr = realloc(str->ptr, str->capacity);
	}
	if (a.len)
		memcpy(str->ptr + str->len, a.ptr, a.len);
	str->len += a.len;
}

static Log plainLevel (Log l) { return l & ~Log_Fatal & ~Log_Noexpand; }

void printMsg (Log level, SourceFile source, Location loc) {
	assert(loc.file_id == source.idx);
	switch (source.kind) {
	case Source_SystemDefinedMacro:
		fprintf(stderr, "%s<system defined macro>%s %.*s:\t", BOLD, RESET, STR_PRINTAGE(source.plain_name));
		break;
	case Source_CommandLineMacro:
		fprintf(stderr, "%s<command-line defined macro>%s %.*s:\t", BOLD, RESET, STR_PRINTAGE(source.plain_name));
		break;
	default: {
		String name = sourceName(&source);
		fprintf(stderr, "%s%.*s:%lu:%lu:%s\t", BOLD, STR_PRINTAGE(name),
			(unsigned long) loc.line, (unsigned long) loc.column, RESET);
	}
	}

	const char *const messages[3] = {
		[Log_Err]  = "error:   ",
		[Log_Warn] = "warning: ",
		[Log_Info] = "info:    ",
	};
	const char *const highlights[3] = {
		[Log_Err]  = REDBOLD,
		[Log_Warn] = YELLOW,
		[Log_Info] = CYAN,

	};
	fprintf(stderr, "%s%s%s", highlights[plainLevel(level)], messages[plainLevel(level)], RESET);
}

void printErr (SourceFile source, Location loc) {
	printMsg(Log_Err, source, loc);
}
void printWarn (SourceFile source, Location loc) {
	printMsg(Log_Warn, source, loc);
}
void printInfo (SourceFile source, Location loc) {
	printMsg(Log_Info, source, loc);
}

void generalFatal(const char *msg, ...) {
	fprintf(stderr, "%s%serror:   %s", BOLD, RED, RESET);
	va_list args;
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	fprintf(stderr, ".\n");
	exit(1);
}


void printto (char **insert, const char *end, const char *fmt, ...) {
	if (*insert >= end)
		return;
	va_list args;
	va_start(args, fmt);
	int count = vsnprintf(*insert, end-*insert, fmt, args);
	va_end(args);
	if (count < 0) {
		perror(NULL);
		exit(1);
	}
	*insert += count;
}



static u32 find (const StringMap *map, u64 hash, String str) {
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

void *mapGet (const StringMap *map, String str) {
	u32 i = find(map, strHash(str), str);
	if (i == FOUND_NONE)
		return NULL;
	return map->content[i];
}

void mapRemove (StringMap *map, void **entry) {
	if (!entry) return;
	u32 i = entry - map->content;

	map->headers[i] = SLOT_TOMBSTONE;
	map->content[i] = NULL;
	map->used--;
}


void mapFree (StringMap *map) {
	free(map->headers);
	map->headers = NULL;
	free(map->content);
	map->content = NULL;
	map->used = 0;
	map->capacity = 0;
}
