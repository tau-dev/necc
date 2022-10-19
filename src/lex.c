#include <stdbool.h>
#include <stdio.h>

#include "lex.h"
#include "arena.h"
#include "ansii.h"

bool isSpace(char);
bool isAlpha(char c);
bool isDigit(char c);
bool isAlnum(char c);

typedef enum {
	Space_None,
	Space_Regular,
	Space_Linebreak
} SpaceClass;

#define KEYWORDS Tok_Key_Last - Tok_Key_First + 1
struct { char *name; TokenKind key; } names[KEYWORDS] = {
// 	{"or", Keyword_Or},
// 	{"and", Keyword_And},
	{"if", Tok_Key_If},
	{"else", Tok_Key_Else},
	{"goto", Tok_Key_Goto},
	{"while", Tok_Key_While},
	{"do", Tok_Key_Do},
	{"for", Tok_Key_For},
	{"return", Tok_Key_Return},
	{"typedef", Tok_Key_Typedef},
};

Token fromWord (String word) {
	for (int i = 0; i < KEYWORDS; ++i) {
		if (eql(names[i].name, word)) {
			Token ret = {0};
			ret.kind = names[i].key;
			return ret;
		}
	}
	return (Token) {Tok_Identifier, {.identifier = word}};
}


_Noreturn void lexerror(SourceFile source, const char *pos, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vprintErr(source, pos - source.content.ptr, msg, args);
    va_end(args);
// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}


Token getToken (const char **p) {
	Token tok = {0};
	const char *pos = *p;

	switch (pos[0]) {
	case '\0': tok.kind = Tok_EOF; pos--; break;
	case '(': tok.kind = Tok_OpenParen; break;
	case ')': tok.kind = Tok_CloseParen; break;
	case '{': tok.kind = Tok_OpenBrace; break;
	case '}': tok.kind = Tok_CloseBrace; break;
	case '[': tok.kind = Tok_OpenBracket; break;
	case ']': tok.kind = Tok_CloseBracket; break;
	case ';': tok.kind = Tok_Semicolon; break;
	case ',': tok.kind = Tok_Comma; break;
	case ':': tok.kind = Tok_Colon; break;
	case '^': tok.kind = Tok_Hat; break;
	case '~': tok.kind = Tok_Tilde; break;
	case '<':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_LessEquals;
		} else if (pos[1] == '<') {
			pos++;
			tok.kind = Tok_DoubleLess;
		} else {
			tok.kind = Tok_Less;
		}
		break;
	case '>':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_GreaterEquals;
		} else if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_DoubleGreater;
		} else {
			tok.kind = Tok_Greater;
		}
		break;
	case '+':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_PlusEquals;
		} else {
			tok.kind = Tok_Plus;
		}
		break;
	case '-':
		if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_Arrow;
		} else if (pos[1] == '=') {
			tok.kind = Tok_MinusEquals;
		} else {
			tok.kind = Tok_Minus;
		}
		break;
	case '*':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_AsteriskEquals;
		} else {
			tok.kind = Tok_Asterisk;
		}
		break;
	case '/':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_SlashEquals;
		} else {
			tok.kind = Tok_Slash;
		}
		break;
	case '=':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_DoubleEquals;
		} else {
			tok.kind = Tok_Equals;
		}
		break;
	case '&':
		if (pos[1] == '&') {
			pos++;
			tok.kind = Tok_DoubleAmpersand;
		} else {
			tok.kind = Tok_Ampersand;
		}
		break;
	case '|':
		if (pos[1] == '|') {
			pos++;
			tok.kind = Tok_DoublePipe;
		} else {
			tok.kind = Tok_Pipe;
		}
		break;
	case '#':
		if (pos[1] == '#') {
			pos++;
			tok.kind = Tok_PreprocConcatenate;
		} else {
			pos++;
			while (pos[0] == ' ') pos++;
			const char *start = pos;
			while (!isSpace(pos[0])) pos++;
			tok = (Token) { Tok_PreprocDirective, { .identifier = {pos - start, start} } };
		}
		break;
	default:
		if (isAlpha(pos[0])) {
			const char *start = pos;
			while (isAlnum(*pos))
				pos++;

			tok = fromWord((String) {pos - start, start});
			pos--;
		} else if (isDigit(pos[0])) {
			const char *start = pos;

			while (isDigit(*pos))
				pos++;

			if (*pos == '.') {
				pos++;
				while (isDigit(*pos))
					pos++;
				if (*pos == 'e' || *pos == 'E')
					pos++;
				if (*pos == '-' || *pos == '+')
					pos++;
				while (isDigit(*pos))
					pos++;

				tok = (Token) {Tok_Real, {.real = strtod(start, NULL)}};
			} else {
				tok = (Token) {Tok_Integer, {.integer = strtoll(start, NULL, 10)}};
			}

			pos--;
		} else {
			// TODO...
		}
	}
	pos++;
	*p = pos;
	return tok;
}

typedef struct MacroToken {
	Token tok;
	u32 source_pos;
	u8 parameter;
	u8 preceded_by_space;
} MacroToken;

typedef struct {
	String name;
	u16 source_ref;
	SPAN(MacroToken) tokens;
	SPAN(String) parameters;
	bool being_replaced;
} Macro;

static void freeMacro(Macro *m);
static SpaceClass tryGobbleSpace(SourceFile source, const char **p);

Tokenization lex (const char *filename) {
	typedef struct {
		u16 file;
		const char *pos;
	} Inclusion;

	Arena macro_arena = create_arena(2048);
	Tokenization t = {0};

	SourceFile source = { zString(filename), readAllAlloc(zString(filename)) };
	if (source.content.ptr == NULL) {
		fprintf(stderr, RED "error: " RESET "could not open file \"%s\"\n", filename);
		exit(1);
	}
	PUSH(t.files, source);

	StringMap macros = {0};
	StringMap macro_parameters = {0};
	LIST(Inclusion) includes_stack = {0};

	const char *pos = source.content.ptr;

	u32 tokens_capactity = 0;
	u16 current_source_ref = 0;
	u32 i = 0;

	while (*pos) {
		bool file_begin = pos == source.content.ptr;
		bool line_begin;
		Token tok;
		const char *begin;
		while (true) {
			line_begin = tryGobbleSpace(source, &pos) == Space_Linebreak || file_begin;
			begin = pos;
			tok = getToken(&pos);
			if (tok.kind != Tok_EOF || current_source_ref == 0)
				break;
			Inclusion inc = POP(includes_stack);
			current_source_ref = inc.file;
			pos = inc.pos;
			source = t.files.ptr[current_source_ref];
		}

		u32 source_pos = begin - source.content.ptr;

		if (tok.kind == Tok_PreprocDirective) {
			if (!line_begin)
				lexerror(source, begin, "a preprocessor directive must be the first token of a line");
			String directive = tok.val.identifier;
			while (pos[0] == ' ') pos++;

			if (eql("define", directive)) {
				const char *start = pos;
				if (!isAlpha(pos[0]))
					lexerror(source, pos, "expected a macro identifier starting with a letter or underscore");
				pos++;
				while (pos[0] != ' ' && pos[0] != '(' && pos[0] != 0) {
					if (!isAlnum(pos[0]))
						lexerror(source, pos, "macro identifier may only contain alphanumeric characters");
					pos++;
				}
				String name = {pos - start, pos};
				void **entry = mapGetOrCreate(&macros, name);
				if (*entry != NULL)
					printf("redefinition!\n");

				Macro *mac = ALLOC(&macro_arena, Macro);
				*entry = &mac->name;
				*mac = (Macro) {name, current_source_ref};

				u32 parameters = 0;
				if (pos[0] == '(') {
					pos++;
					const char *p = pos;
					while (true) {
						tryGobbleSpace(source, &p); // TODO Warn on newline?
						Token t = getToken(&p);
						if (t.kind != Tok_Identifier)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
						parameters++;
						tryGobbleSpace(source, &p);
						t = getToken(&p);
						if (t.kind == Tok_CloseParen)
							break;
						else if (t.kind != Tok_Comma)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
					}
					mac->parameters.ptr = aalloc(&macro_arena, sizeof(String) * parameters);
					mac->parameters.len = parameters;

					for (u32 i = 0; i < parameters; i++) {
						tryGobbleSpace(source, &pos);
						Token t = getToken(&pos);
						assert(t.kind == Tok_Identifier);
						String name = t.val.identifier;
						mac->parameters.ptr[i] = name;
						void **entry = mapGetOrCreate(&macro_parameters, name);
						if (*entry)
							lexerror(source, name.ptr, "parameters may not be duplicated");
						*entry = &mac->parameters.ptr[i];
						tryGobbleSpace(source, &pos);
						t = getToken(&pos);
					}
				}

				LIST(MacroToken) macro_assembly = {0};
				while (true) {
					SpaceClass space = tryGobbleSpace(source, &pos);
					if (space == Space_Linebreak)
						break;
					MacroToken t = {getToken(&pos), pos - source.content.ptr, 0, space == Space_Regular};
					if (t.tok.kind == Tok_EOF)
						lexerror(source, pos, "macro definition must end before end of source file");
					if (parameters && t.tok.kind == Tok_Identifier) {
						String *param = mapGet(&macro_parameters, t.tok.val.identifier);
						if (param)
							t.parameter = param - mac->parameters.ptr;
					}
					PUSH(macro_assembly, t);
				}
				mac->tokens.ptr = macro_assembly.ptr;
				mac->tokens.len = macro_assembly.len;

				if (parameters)
					mapFree(&macro_parameters);
			} else if (eql("include", directive)) {
				// TODO Preprocessor replacements on the arguments to #include (6.10.2.4)
				char delimiter;
				if (pos[0] == '\"')
					delimiter = '\"';
				else if (pos[0] == '<')
					delimiter = '>';
				else
					lexerror(source, pos, "#include expects <FILENAME> or \"FILENAME\"");

				pos++;
				const char *begin = pos;
				while (*pos != delimiter) {
					if (*pos == '\0') {
						lexerror(source, pos, "#include expects <FILENAME> or \"FILENAME\"");
					}
					pos++;
				}

				String includefilename = {pos - begin, begin};
				// TODO Deduplicate inclusions
				SourceFile new_source = { includefilename, readAllAlloc(includefilename) };
				if (new_source.content.ptr == NULL)
					lexerror(source, begin, "could not open include file");
				pos++;
				PUSH(includes_stack, ((Inclusion) {current_source_ref, pos}));
				source = new_source;
				PUSH(t.files, source);
				current_source_ref = t.files.len - 1;
				pos = source.content.ptr;
			} else {
				lexerror(source, pos, "unknown preprocessor directive");
			}
			continue;
		}

		if (tok.kind == Tok_Identifier) {
			Macro *macro = mapGet(&macros, tok.val.identifier);
			if (macro) {
				if (macro->parameters.len)
					lexerror(source, pos, "replacement of function-like macros not yet supported");

				u32 count = macro->tokens.len;

				if (i + count > tokens_capactity) {
					// CLEANUP Copypasta
					tokens_capactity = tokens_capactity == tokens_capactity * 3 / 2 + count;
					t.tokens = realloc(t.tokens, sizeof(t.tokens[0]) * tokens_capactity);
					t.positions = realloc(t.positions, sizeof(t.positions[0]) * tokens_capactity);
				}
				for (u32 r = 0; r < count; i++) {
					MacroToken macro_tok = macro->tokens.ptr[r];
					t.tokens[i + r] = macro_tok.tok;
					t.positions[i + r] = (TokenPosition) {
						.source_file_offset = source_pos,
						.source_file_ref = current_source_ref,
						.macro_file_offset = macro_tok.source_pos,
						.macro_file_ref = macro->source_ref,
					};
				}
				i += count;
				continue;
			}
		}

		if (i >= tokens_capactity) {
			tokens_capactity = tokens_capactity == 0 ? 4 : tokens_capactity * 3 / 2;
			t.tokens = realloc(t.tokens, sizeof(t.tokens[0]) * tokens_capactity);
			t.positions = realloc(t.positions, sizeof(t.positions[0]) * tokens_capactity);
		}
		t.tokens[i] = tok;
		t.positions[i] = (TokenPosition) {source_pos, current_source_ref};
		i++;

		if (tok.kind == Tok_EOF && current_source_ref == 0)
			break;
	}

	t.tokens_count = i;

	free(includes_stack.ptr);
	for (u32 i = 0; i < macros.capacity; i++) {
		if (macros.content[i])
			freeMacro((Macro*)macros.content[i]);
	}
	free_arena(&macro_arena);
	mapFree(&macros);
	return t;
}



const char *token_names[Tok_EOF+1] = {
	[Tok_Identifier] = "identifier",
	[Tok_Integer] = "integer literal",
	[Tok_Real] = "floating point literal",
	[Tok_String] = "string literal",

	[Tok_OpenParen] = BOLD "(" RESET,
	[Tok_CloseParen] = BOLD ")" RESET,
	[Tok_OpenBrace] = BOLD "{" RESET,
	[Tok_CloseBrace] = BOLD "}" RESET,
	[Tok_OpenBracket] = BOLD "[" RESET,
	[Tok_CloseBracket] = BOLD "]" RESET,

	[Tok_Semicolon] = BOLD ";" RESET,
	[Tok_Comma] = BOLD "," RESET,
	[Tok_Colon] = BOLD ":" RESET,

	[Tok_Equals] = BOLD "=" RESET,
	[Tok_DoubleEquals] = BOLD "==" RESET,
	[Tok_Arrow] = BOLD "->" RESET,
	[Tok_Plus] = BOLD "+" RESET,
	[Tok_PlusEquals] = BOLD "+=" RESET,
	[Tok_Minus] = BOLD "-" RESET,
	[Tok_MinusEquals] = BOLD "-=" RESET,
	[Tok_Asterisk] = BOLD "*" RESET,
	[Tok_AsteriskEquals] = BOLD "*=" RESET,
	[Tok_Slash] = BOLD "/" RESET,
	[Tok_SlashEquals] = BOLD "/=" RESET,
	[Tok_Less] = BOLD "<" RESET,
	[Tok_DoubleLess] = BOLD "<<" RESET,
	[Tok_LessEquals] = BOLD "<=" RESET,
	[Tok_Greater] = BOLD ">" RESET,
	[Tok_DoubleGreater] = BOLD ">>" RESET,
	[Tok_GreaterEquals] = BOLD ">=" RESET,
	[Tok_Pipe] = BOLD "|" RESET,
	[Tok_DoublePipe] = BOLD "||" RESET,
	[Tok_Ampersand] = BOLD "&" RESET,
	[Tok_DoubleAmpersand] = BOLD "&&" RESET,
	[Tok_Hat] = BOLD "^" RESET,
	[Tok_Tilde] = BOLD "~" RESET,

	[Tok_Key_If] = BOLD "if" RESET,
	[Tok_Key_Else] = BOLD "else" RESET,
	[Tok_Key_Goto] = BOLD "goto" RESET,
	[Tok_Key_While] = BOLD "while" RESET,
	[Tok_Key_Do] = BOLD "do" RESET,
	[Tok_Key_For] = BOLD "for" RESET,
	[Tok_Key_Return] = BOLD "return" RESET,
// 	[Tok_Key_Define] = BOLD "#define" RESET,
	[Tok_Key_Typedef] = BOLD "typedef" RESET,

	[Tok_EOF] = "end of file",
};

// PERFORMANCE This function may be critical for the parse.
static SpaceClass tryGobbleSpace(SourceFile source, const char **p) {
	const char *pos = *p;
	SpaceClass spacing = Space_Regular;
	while (true) {
		if (pos[0] == '\\' && pos[1] == '\n') {
			pos += 2;
		} else if (pos[0] == '/' && pos[1] == '/') {
			while (pos[0] && pos[0] != '\n') pos++;
		} else if (pos[0] == '/' && pos[1] == '*') {
			const char *begin = pos;
			while (pos[0] && !(pos[0] == '*' && pos[1] == '/')) pos++;
			if (!pos[0])
				lexerror(source, begin, "comment must end before end of file");
			pos += 2;
		} {
			if (!isSpace(pos[0])) {
				if (pos == *p)
					return Space_None;
				*p = pos;
				return spacing;
			}
			if (pos[0] == '\n')
				spacing = Space_Linebreak;
			pos++;
		}
	}
}

static void freeMacro(Macro *m) {
	free(m->tokens.ptr);
	free(m->parameters.ptr);
}

static char name[1024] = {0};

const char *tokenName(TokenKind kind) {
	strcpy(name, "unidentified token, ID (hacky, only the last emitted one is certain): ");
	char *end = name + strlen(name);

	if (kind >= 0 && kind <= Tok_EOF && token_names[kind])
		return token_names[kind];
	else {
		sprintf(end, "%d", kind);
		return name;
	}
}

bool isSpace(char c) {
	switch (c) {
		case ' ':
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case '\t':
			return true;
		default:
			return false;
	}
}

bool isAlpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

bool isAlnum(char c) {
	return isAlpha(c) || isDigit(c);
}

