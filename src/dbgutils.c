#include <stdio.h>
#include "util.h"
#include "lex_preproc.h"

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
	append(zstr(c));
}


const char *sz(String s) {
	buf.len = 0;
	append(s);
	return buf.ptr;
}

void tokz(Token t) {
	switch (t.kind) {
	case Tok_Identifier:
		appendz("‘");
		append(t.val.symbol->name);
		appendz("’");
		break;
	case Tok_String:
		appendz("\"");
		append(t.val.symbol->name);
		appendz("\"");
		break;
	case Tok_Int:
		appendz("[int]");
		break;
	case Tok_UInt:
		appendz("[uint]");
		break;
	case Tok_Long:
		appendz("[long]");
		break;
	case Tok_ULong:
		appendz("[ulong]");
		break;
	default: {
		const char *c = tokenName(t.kind);
		if (c) {
			appendz(c);
		} else {
			char buf[16] = {0};
			snprintf(buf, 16, "[unkown:%d]", (int) t.kind);
			appendz(buf);
		}
	}
	}
}

const char *tz(Token t) {
	buf.len = 0;
	tokz(t);
	return buf.ptr;
}

const char *lexz(Tokenization t, const Token *current, const Token *parse_pos, u32 count) {
	int ctx_len = 8;
	if (count) ctx_len = (count + 1) / 2;

	i32 begin = (current ? current - t.tokens : t.count) - ctx_len;
	u32 end = begin + 2*ctx_len;
	if (end > t.count) {
		begin -= end - t.count;
		end = t.count;
	}
	if (begin < 0) begin = 0;
	buf.len = 0;

	for (u32 i = begin; i < end; i++) {
		if (t.tokens + i == current)
			appendz(" >MAIN> ");
		if (t.tokens + i == parse_pos)
			appendz(" >NEXT> ");
		tokz(t.tokens[i]);
		appendz(" ");
	}
	return buf.ptr;
}

