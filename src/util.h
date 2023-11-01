#pragma once
#if defined(__unix__)
#define HAVE_POSIX 1
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 500
#endif


#if defined(_WIN32)
#define HAVE_WINDOWS 1
#endif


#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>


#define SPAN(type) struct { size_t len; type *ptr; }
#define SPAN_EQL(a, b) (sizeof(*(a).ptr) == sizeof(*(b).ptr) && (a).len == (b).len && memcmp((a).ptr, (b).ptr, sizeof(*(a).ptr) * (a).len) == 0)
#define ARRAY_SPAN(arr) {sizeof(arr)/sizeof((arr)[0]), (arr)}


#define LIST(type) struct { size_t capacity; size_t len; type *ptr; }
// TODO Catch OOM
#define LIST_INITIAL_CAPACITY 16
#define MAKE_LIST(type) { .capacity = LIST_INITIAL_CAPACITY, .ptr = calloc(LIST_INITIAL_CAPACITY, sizeof(type)) }

// #define COPY_LIST_TO(list, dest) (assert((dest).len == (list).len), memcpy((dest).ptr, (list).content, (list).len * sizeof(*(list).content)))
#define PUSH(list, value) \
	do { \
		if ((list).len >= (list).capacity) {\
			(list).capacity = (list).len * 3 / 2 + 4; \
			(list).ptr = realloc((list).ptr, sizeof(*(list).ptr) * (list).capacity); \
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
		if ((list).len >= (list).capacity) { \
			(list).capacity = (list).len * 2 + 4; \
			void *new = aalloc(arena, sizeof(*(list).ptr) * (list).capacity); \
			if ((list).ptr) memcpy(new, (list).ptr, sizeof(*(list).ptr) * (list).len); \
			(list).ptr = new; \
		} \
		(list).ptr[(list).len] = (value); \
		(list).len++; \
	} while (0);


#define POP(list) (assert((list).len > 0), (list).ptr[--(list).len])
#define LAST(list) ((list).ptr[(list).len-1])
#define foreach(i, list) for (u32 i = 0; i < (list).len; i++)

#define STRING_EMPTY ((String) {0})
// Generate arguments for printf("%.*s", ...);
#define STR_PRINTAGE(str) ((int) (str).len), ((str).ptr)

typedef SPAN(const char) String;
typedef SPAN(char) MutableString;
typedef LIST(String) StringList;
typedef LIST(char) DynString;

void strAppend(DynString *str, String s);



// #define CHECK(a, msg) do { if (!(a)) {puts("error: " msg); exit(EXIT_FAILURE); } } while(0)
#define unreachable (assert(!"unreachable"))


#if (__STDC_VERSION__ >= 201904L) || (defined(_MSC_VER) && (_MSC_VER >= 1911) && (_MSVC_LANG >= 201703L))
 #define FALLTHROUGH [[fallthrough]]
 #define nodiscard [[nodiscard]]
 #define STATIC_ASSERT static_assert
#elif defined(__GNUC__)
 #define FALLTHROUGH __attribute__((fallthrough))
 #define nodiscard __attribute__((warn_unused_result))
#else
 #define FALLTHROUGH
 #define nodiscard
#endif



#if __STDC_VERSION__ >= 201100L
#define STATIC_ASSERT static_assert
#else
#define STATIC_ASSERT(x,y) extern int _lul
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

#define IDX_NONE ((u32) -1)


u64 strHash(String);

typedef enum {
	Source_Regular,
	Source_StandardHeader,

	// For these special files, the name field holds the macro name and the
	// path field holds the macro contents.
	Source_CommandLineMacro,
	Source_SystemDefinedMacro,
} SourceKind;

// Lines and columns are 1-based in accordance with most editors.
typedef struct {
	u32 file_id;
	u32 line;
	u32 column;
} Location;

typedef struct {
	String abs_name; // owning
	String plain_name; // owning, possibly null
	String plain_path; // non-owning

	String content; // non-owning, possibly co-allocated

	// Index in the owning Tokenization's FileList. 0 is invalid, the
	// root file is 1, predefined macros come after that, then inclulded
	// files.
	u32 idx;
	// ID of the include directory this was found in. Required for
	// #include_next.
	u16 next_include_index;

	u32 included_count;
	SourceKind kind;
} SourceFile;

typedef struct {
	u8 *headers;

	// Array of pointers to structs whose first item must be the
	// key String. (Upcast allowed by 6.7.2.1-17)
	void **content;
	u32 used;
	u32 capacity;
} StringMap;




void printto (char **insert, const char *end, const char *fmt, ...);

void **mapGetOrCreate(StringMap *, String);
void *mapGet(const StringMap *, String);
void mapRemove(StringMap *, void **entry);
void mapFree(StringMap *);

bool eql(const char *, String);
bool startsWith(const char *, String);
String zstr(const char *);
void *mdupe(const void *data, size_t len);

SourceFile *readAllAlloc(String path_owning);
String sourceName(SourceFile *source);
bool isDirectory(const char *path);
bool isFile(const char *path);


typedef enum {
	Log_Err,
	Log_Warn,
	Log_Info,

	Log_Noexpand = 0x100,
	Log_Fatal = 0x200,
} Log;

void printMsg(Log, SourceFile, Location);
void printErr(SourceFile, Location);
void printWarn(SourceFile, Location);
void printInfo(SourceFile, Location);
void generalFatal(const char *msg, ...);
