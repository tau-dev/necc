#pragma once

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>


#define SPAN(type) struct { size_t len; type *ptr; }
typedef SPAN(const char) String;
typedef SPAN(char) MutableString;
#define SPAN_EQL(a, b) (sizeof(*(a).ptr) == sizeof(*(b).ptr) && (a).len == (b).len && memcmp((a).ptr, (b).ptr, sizeof(*(a).ptr) * (a).len) == 0)
// #define STRING(text) (String) { .len = strlen(text), .ptr = text }
#define STRING_EMPTY ((String) {0})
#define ARRAY_SPAN(arr) {sizeof(arr)/sizeof((arr)[0]), (arr)}

// Generate arguments for printf("%.*s", ...);
#define STRING_PRINTAGE(str) ((int) (str).len), ((str).ptr)


#define LIST(type) struct { size_t capacity; size_t len; type *ptr; }
// TODO Catch OOM
#define LIST_INITIAL_CAPACITY 16
#define MAKE_LIST(type) { .capacity = LIST_INITIAL_CAPACITY, .ptr = calloc(LIST_INITIAL_CAPACITY, sizeof(type)) }

// #define COPY_LIST_TO(list, dest) (assert((dest).len == (list).len), memcpy((dest).ptr, (list).content, (list).len * sizeof(*(list).content)))
#define PUSH(list, value) \
	do { \
		if ((list).len >= (list).capacity) {\
			(list).ptr = realloc((list).ptr, sizeof(*(list).ptr) * ((list).len * 3 / 2 + 4)); \
			if ((list).ptr == NULL) {\
				puts("ERROR: Out of memory on list growth."); \
				exit(EXIT_FAILURE); \
			} \
		} \
		(list).ptr[(list).len] = (value); \
		(list).len++; \
	} while (0);
#define PUSH_A(arena, list, value) \
	do { \
		if ((list).len >= (list).capacity) {\
			void *new = aalloc(arena, sizeof(*(list).ptr) * ((list).len * 2 + 4)); \
			if ((list).ptr) memcpy(new, (list).ptr, sizeof(*(list).ptr) * (list).len); \
			(list).ptr = new; \
		} \
		(list).ptr[(list).len] = (value); \
		(list).len++; \
	} while (0);

#define POP(list) (assert((list).len > 0), (list).ptr[--(list).len])



// #define CHECK(a, msg) do { if (!(a)) {puts("error: " msg); exit(EXIT_FAILURE); } } while(0)
#define unreachable (assert(!"unreachable"))



#if (__STDC_VERSION__ >= 201904L) || (defined(_MSC_VER) && (_MSC_VER >= 1911) && (_MSVC_LANG >= 201703L))
#define FALLTHROUGH [[fallthrough]]
#define nodiscard [[nodiscard]]
#elif defined(__GNUC__)
#define FALLTHROUGH __attribute__((fallthrough))
#define nodiscard __attribute__((warn_unused_result))
#else
#define FALLTHROUGH
#define nodiscard
#endif



#define uchar uint8_t
#define u8 uint8_t
#define u16 uint16_t
#define u32 uint32_t
#define u64 uint64_t
#define i8 int8_t
#define i16 int16_t
#define i32 int32_t
#define i64 int64_t

u64 strHash(String);

typedef struct {
	String name; // non-owning
	String path; // non-owning

	String content;
	u32 idx;
	u32 included_count;
	bool is_standard_header;
} SourceFile;

typedef struct {
	u8 *headers;

	// Array of pointers to structs whose first item must be the
	// key String. (Upcast allowed by 6.7.2.1-13)
	void **content;
	u32 used;
	u32 capacity;
} StringMap;



void printto (char **insert, const char *end, const char *fmt, ...);

void **mapGetOrCreate(StringMap *, String);
void *mapGet(const StringMap *, String);
void *mapRemove(StringMap *, String);
void mapFree(StringMap *);

bool eql(const char *, String);
bool startsWith(const char *, String);
String zString(const char *);
SourceFile *readAllAlloc (String source, String filename);


typedef enum {
	Log_Err,
	Log_Warn,
	Log_Info,
	Log_Noexpand = 0x100,
	Log_Fatal = 0x200,
} Log;

void printMsg(Log, SourceFile, u32);
void printErr(SourceFile, u32);
void printWarn(SourceFile, u32);
void printInfo(SourceFile, u32);
void generalFatal(const char *msg, ...);
