#include <stdio.h>
#include "util.h"
#include "lex.h"

/*

Helper functions to display various state — currently
just Strings and Tokenizations.

*/

static LIST(char) buf;

static void append(String s) {
	if (buf.len + s.len + 1 > buf.capacity) {
		buf.capacity += s.len + 1;
		buf.ptr = realloc(buf.ptr, buf.capacity);
	}
	memcpy(buf.ptr + buf.len, s.ptr, s.len);
	buf.len += s.len;
	buf.ptr[buf.len] = 0;
}

static void appendz(const char *c) {
	append(zString(c));
}


const char *sz(String s) {
	buf.len = 0;
	append(s);
	return buf.ptr;
}

void tokz(Token t) {
	if (t.kind == Tok_Identifier) {
		appendz("‘");
		append(t.val.identifier);
		appendz("’");
	} else if (t.kind == Tok_String) {
		appendz("\"");
		append(t.val.identifier);
		appendz("\"");
	} else if (t.kind == Tok_Integer) {
		appendz("[int]");
	} else {
		const char *c = tokenName(t.kind);
		if (c) {
			appendz(c);
		} else {
			appendz("[???]");
		}
	}
}

const char *tz(Token t) {
	buf.len = 0;
	tokz(t);
	return buf.ptr;
}

const int ctx_len = 8;
const char *lexz(Tokenization t, const Token *current, const Token *parse_pos, u32 count) {
	u32 offset = 0;
	if (current && current > t.tokens + ctx_len)
		offset = current - t.tokens - ctx_len;
	buf.len = 0;

	u32 end = (count && offset + count < t.count) ? offset + count : t.count;
	for (u32 i = offset; i < end; i++) {
		if (t.tokens + i == current)
			appendz(" >MAIN> ");
		if (t.tokens + i == parse_pos)
			appendz(" >NEXT> ");
		tokz(t.tokens[i]);
		appendz(" ");
	}
	return buf.ptr;
}

