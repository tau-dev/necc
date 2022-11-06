#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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
	Space_Linebreak,
} SpaceClass;


typedef enum {
	If_Else,
	If_ElseIf,
	If_End,
} IfClass;

typedef struct {
	char *name;
	TokenKind key;
} Keyword;

// TODO Separate out keywords by version.
// TODO Sort and binary-search.
#define KEYWORDS Tok_Key_Last - Tok_Key_First + 1
Keyword names[KEYWORDS] = {
// 	{"or", Keyword_Or},
// 	{"and", Keyword_And},
	{"if", Tok_Key_If},
	{"else", Tok_Key_Else},
	{"goto", Tok_Key_Goto},
	{"while", Tok_Key_While},
	{"do", Tok_Key_Do},
	{"for", Tok_Key_For},
	{"return", Tok_Key_Return},
	{"break", Tok_Key_Break},
	{"default", Tok_Key_Default},
	{"sizeof", Tok_Key_Sizeof},
	{"_Alignof", Tok_Key_Alignof},
	{"_Alignas", Tok_Key_Alignas},
	{"typeof", Tok_Key_Typeof},
	{"typeof_unqual", Tok_Key_TypeofUnqual},
	{"_Generic", Tok_Key_Generic},

	{"struct", Tok_Key_Struct},
	{"enum", Tok_Key_Enum},
	{"union", Tok_Key_Union},
	{"_Bool", Tok_Key_Bool},
	{"char", Tok_Key_Char},
	{"int", Tok_Key_Int},
	{"void", Tok_Key_Void},
	{"float", Tok_Key_Float},
	{"double", Tok_Key_Double},

	{"long", Tok_Key_Long},
	{"short", Tok_Key_Short},
	{"signed", Tok_Key_Signed},
	{"unsigned", Tok_Key_Unsigned},
	{"const", Tok_Key_Const},
	{"volatile", Tok_Key_Volatile},
	{"restrict", Tok_Key_Restrict},
	{"_Atomic", Tok_Key_Atomic},
	{"typedef", Tok_Key_Typedef},
	{"auto", Tok_Key_Auto},
	{"register", Tok_Key_Register},
	{"static", Tok_Key_Static},
	{"extern", Tok_Key_Extern},
	{"_Thread_local", Tok_Key_Threadlocal},
};

int keyword_cmp(const void *a, const void *b) {
	return strcmp(((Keyword*)a)->name, ((Keyword*)b)->name);
}

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


_Noreturn void lexerror (SourceFile source, const char *pos, const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vprintErr(source, pos - source.content.ptr, msg, args);
    va_end(args);
// #ifndef NDEBUG
// 	PRINT_STACK_TRACE;
// #endif
	exit(1);
}

// TODO Trigraphs (trivial) and digraphs (annoying)
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
	case '.':
		if (pos[1] == '.' && pos[2] == '.') {
			pos += 2;
			tok.kind = Tok_TripleDot;
		} else {
			tok.kind = Tok_Dot;
		}
		break;
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
		} else if (pos[1] == '+') {
			pos++;
			tok.kind = Tok_DoublePlus;
		} else {
			tok.kind = Tok_Plus;
		}
		break;
	case '-':
		if (pos[1] == '>') {
			pos++;
			tok.kind = Tok_Arrow;
		} else if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_MinusEquals;
		} else if (pos[1] == '-') {
			pos++;
			tok.kind = Tok_DoubleMinus;
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
	case '!':
		if (pos[1] == '=') {
			pos++;
			tok.kind = Tok_BangEquals;
		} else {
			tok.kind = Tok_Bang;
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
			pos--;
		}
		break;
	case '\"': {
		pos++;
		const char *begin = pos;
		while (*pos != '\"') {
			if (*pos == '\\') pos++;
			pos++;
		}
		// TODO Error reporting
		tok = (Token) { Tok_String, { .string = {pos - begin, begin} } };
	} break;
	default:
		if (isAlpha(pos[0])) {
			const char *start = pos;
			while (isAlnum(*pos))
				pos++;

			tok = fromWord((String) {pos - start, start});
			pos--;
		} else if (isDigit(pos[0]) || pos[0] == '.') {
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
				if (pos[0] == 'l' || pos[1] == 'L') {
					if (pos[1] == 'l' || pos[1] == 'L') {
						pos += 2;
						tok.val.int_type = Int_longlong;
					} else {
						pos++;
						tok.val.int_type = Int_long;
					}
				} else {
					tok.val.int_type = Int_int;
				}
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
	// 0 if this token is not a parameter, 1+(index into parameters) if it is.
	u8 parameter;
	u8 preceded_by_space;
} MacroToken;

typedef struct {
	String name;
	// If the macro is currently being replaced, this points at the unexpanded argument.
	// The storage can put here because only one replacement of the macro is possible at a time.
	Tokenization current_argument;
} Parameter;

typedef struct {
	String name;
	u16 source_ref;
	SPAN(MacroToken) tokens;
	SPAN(Parameter) parameters;
	bool is_function_like;
	bool is_vararg;
	bool being_replaced;
} Macro;

// typedef struct {
// 	Macro *m;
// 	SPAN(SPAN(MacroToken)) parameters;
// } MacroExpansion;

typedef LIST(MacroToken) ExpansionBuffer;

static void expandInto (Arena *arena, Tokenization *dest, Tokenization *source, const StringMap *macros);
static void ensureCapacity (Tokenization *t, u32 required);
static void appendOneToken(Tokenization *t, Token tok, TokenPosition pos);
static SpaceClass tryGobbleSpace(SourceFile source, const char **p);
static void skipToEndIf(SourceFile source, const char **p);
static IfClass skipToElseIfOrEnd(SourceFile source, const char **p);
static bool gobbleSpaceToNewline(const char **p);
static bool evalPreprocExpression(SourceFile source, StringMap defined, const char **p, ExpansionBuffer *buf);
static void predefineMacros(StringMap *macros, Target *target);
Tokenization lex (const char *filename, Paths paths, Target *target) {
	typedef struct {
		u16 file;
		const char *pos;
	} Inclusion;

	qsort(names, sizeof(names)/sizeof(names[0]), sizeof(names[0]), keyword_cmp);

	Arena macro_arena = create_arena(2048);
	Tokenization t = {0};
	// Used to hold macro invocations for expansion
	Tokenization macro_pre_expansion = {0};

	SourceFile *initial_source = readAllAlloc(zString(""), zString(filename));
	if (initial_source == NULL) {
		fprintf(stderr, RED "error: " RESET "could not open file \"%s\"\n", filename);
		exit(1);
	}
	PUSH(t.files, initial_source);
	SourceFile source = *initial_source;

	StringMap sources = {0};
	StringMap macros = {0};
	StringMap macro_parameters = {0};
	LIST(Inclusion) includes_stack = {0};
	ExpansionBuffer prepreoc_evaluation_buf = {0};

	const char *pos = source.content.ptr;

	predefineMacros(&macros, target);

	while (*pos) {
		bool file_begin = pos == source.content.ptr;
		bool line_begin;
		Token tok;
		const char *begin;
		while (true) {
			line_begin = tryGobbleSpace(source, &pos) == Space_Linebreak || file_begin;
			begin = pos;
			tok = getToken(&pos);
			if (tok.kind != Tok_EOF || source.idx == 0)
				break;
			Inclusion inc = POP(includes_stack);
			pos = inc.pos;
			source = *t.files.ptr[inc.file];
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
				while (pos[0] != 0 && !isSpace(pos[0]) && pos[0] != '(') {
					if (!isAlnum(pos[0]))
						lexerror(source, pos, "macro identifier may only contain alphanumeric characters");
					pos++;
				}
				String name = {pos - start, start};
				void **entry = mapGetOrCreate(&macros, name);
				if (*entry != NULL)
					fprintf(stderr, "redefining %.*s\n", (int) name.len, name.ptr);

				Macro *mac = ALLOC(&macro_arena, Macro);
				*entry = mac;
				*mac = (Macro) {name, source.idx};

				u32 parameters = 0;
				if (pos[0] == '(') {
					pos++;
					const char *p = pos;
					while (true) {
						gobbleSpaceToNewline(&p);
						if (*p == '\n' || *p == 0)
							lexerror(source, pos, "expected parameter before end of line");

						Token t = getToken(&p);
						if (t.kind != Tok_Identifier && t.kind != Tok_TripleDot)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
						parameters++;
						gobbleSpaceToNewline(&p);
						if (*p == '\n' || *p == 0)
							lexerror(source, pos, "expected parameter before end of line");

						t = getToken(&p);
						if (t.kind == Tok_CloseParen)
							break;
						else if (t.kind != Tok_Comma)
							lexerror(source, pos, "the parameters of function-like macros must be valid identifiers");
					}
					mac->parameters.ptr = aalloc(&macro_arena, sizeof(Parameter) * parameters);
					mac->parameters.len = parameters;
					mac->is_function_like = true;

					for (u32 i = 0; i < parameters; i++) {
						gobbleSpaceToNewline(&pos);
						Token t = getToken(&pos);
						assert(t.kind == Tok_Identifier || t.kind == Tok_TripleDot);
						String name;
						if (t.kind == Tok_TripleDot) {
							if (i + 1 != parameters)
								lexerror(source, pos, "an ellipsis may only appear as the last parameter to a variadic macro");
							// TODO Version check for C99
							name = zString("__VA_ARGS__");
						} else {
							name = t.val.identifier;
						}

						mac->parameters.ptr[i].name = name;
						void **entry = mapGetOrCreate(&macro_parameters, name);
						if (*entry)
							lexerror(source, ((String *)*entry)->ptr, "macro parameters may not be duplicated");
						*entry = &mac->parameters.ptr[i];

						gobbleSpaceToNewline(&pos);
						t = getToken(&pos);
					}
				}

				LIST(MacroToken) macro_assembly = {0};
				while (true) {
					bool havespace = gobbleSpaceToNewline(&pos);
					if (*pos == '\n')
						break;

					MacroToken t = {getToken(&pos), pos - source.content.ptr, 0, havespace};
					if (t.tok.kind == Tok_EOF)
						lexerror(source, pos, "macro definition must end before end of source file");
					if (parameters && t.tok.kind == Tok_Identifier) {
						Parameter *param = mapGet(&macro_parameters, t.tok.val.identifier);
						if (param)
							t.parameter = param - mac->parameters.ptr + 1;
					}
					PUSH(macro_assembly, t);
				}
				mac->tokens.ptr = macro_assembly.ptr;
				mac->tokens.len = macro_assembly.len;

				if (parameters > 0)
					mapFree(&macro_parameters);
			} else if (eql("undef", directive)) {
				gobbleSpaceToNewline(&pos);
				if (*pos == '\n' || *pos == 0)
					lexerror(source, pos, "#undef expects one identifier");

				tok = getToken(&pos);

				gobbleSpaceToNewline(&pos);
				if (tok.kind != Tok_Identifier || *pos != '\n')
					lexerror(source, pos, "#undef expects one identifier");
// 				Macro *prev = mapRemove(&macros, tok.val.identifier);

// 				if (prev)
// 					free(prev->tokens.ptr); // TODO Allocate the tokens from the macro_arena too
				else
					fprintf(stderr, "%.*s was never defined\n", (int)tok.val.identifier.len, tok.val.identifier.ptr);
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

				SourceFile *new_source = NULL;
				void **already_loaded = mapGetOrCreate(&sources, includefilename);
				if (*already_loaded) {
					new_source = *already_loaded;
				} else {
					if (delimiter == '\"') {
						for (u32 i = 0; i < paths.user_include_dirs.len && new_source == NULL; i++) {
							new_source = readAllAlloc(paths.user_include_dirs.ptr[i], includefilename);
						}
					}
					for (u32 i = 0; i < paths.sys_include_dirs.len && new_source == NULL; i++) {
						new_source = readAllAlloc(paths.sys_include_dirs.ptr[i], includefilename);
					}
					if (new_source == NULL)
						lexerror(source, begin, "could not open include file \"%.*s\"", (int)includefilename.len, includefilename.ptr);
					new_source->idx = t.files.len;
					PUSH(t.files, new_source);
					*already_loaded = new_source;
				}
				pos++;
				PUSH(includes_stack, ((Inclusion) {source.idx, pos}));

				source = *new_source;
				pos = source.content.ptr;
			} else if (eql("pragma", directive)) {
				// TODO: ???
				while (*pos && tryGobbleSpace(source, &pos) != Space_Linebreak) pos++;;
			} else if (eql("if", directive)) {
				while (!evalPreprocExpression(source, macros, &pos, &prepreoc_evaluation_buf) &&
					skipToElseIfOrEnd(source, &pos) == If_ElseIf);

			} else if (eql("ifdef", directive) || eql("ifndef", directive)) {
				if (tryGobbleSpace(source, &pos) == Space_Linebreak)
					lexerror(source, pos, "unknown preprocessor directive");
				tok = getToken(&pos);
				if (tok.kind != Tok_Identifier)
					lexerror(source, pos, "#ifdef must be followed by an identifier");


				bool got = mapGet(&macros, tok.val.identifier);
				bool required = directive.ptr[2] != 'n';
				if (got == required) {
					if (tryGobbleSpace(source, &pos) != Space_Linebreak)
						lexerror(source, pos, "#ifdef may not be followed by more than one identifier");

					while (skipToElseIfOrEnd(source, &pos) == If_ElseIf &&
						!evalPreprocExpression(source, macros, &pos, &prepreoc_evaluation_buf));

				}
			} else if (eql("else", directive) || eql("elseif", directive)) {
				// TODO Check correct nesting
				skipToEndIf(source, &pos);
			} else if (eql("endif", directive)) {
			} else {
				lexerror(source, directive.ptr, "unknown preprocessor directive");
			}
			continue;
		}

		if (tok.kind == Tok_Identifier) {
			Macro *macro = mapGet(&macros, tok.val.identifier);
			if (macro) {
				macro_pre_expansion.files = t.files;
				macro_pre_expansion.count = 0;
				appendOneToken(&macro_pre_expansion, tok, (TokenPosition) {source_pos, source.idx});

				if (macro->is_function_like) {
					const char *macro_begin = pos;
					tryGobbleSpace(source, &macro_begin);
					tok = getToken(&macro_begin);

					if (tok.kind != Tok_OpenParen) {
						// Macro is not invoked as a function.
						// STYLE Copypasta due to inconvenient control
						// flow.
						appendOneToken(&t,
							macro_pre_expansion.tokens[0],
							macro_pre_expansion.positions[0]);
						continue;
					}
					pos = macro_begin;
					appendOneToken(&macro_pre_expansion, tok, (TokenPosition) {0});

					u32 depth = 1;
					while (depth > 0) {
						tok = getToken(&pos);
						appendOneToken(&macro_pre_expansion, tok, (TokenPosition) {begin - source.content.ptr, source.idx});

						if (tok.kind == Tok_EOF)
							lexerror(source, macro_begin-1, "missing close paren"); // FIXME Wording
						else if (tok.kind == Tok_OpenParen)
							depth++;
						else if (tok.kind == Tok_CloseParen)
							depth--;
					}
				}

				u32 base = t.count;
				expandInto(&macro_arena, &t, &macro_pre_expansion, &macros);
				macro_pre_expansion.count = 0;

				for (u32 i = base; i < t.count; i++) {
					t.positions[i].source_file_offset = source_pos;
					t.positions[i].source_file_ref = source.idx;
				}
				continue;
			}
		}
		appendOneToken(&t, tok, (TokenPosition) {source_pos, source.idx});

		if (tok.kind == Tok_EOF && source.idx == 0)
			break;
	}

// 	t.count = i;

	free(prepreoc_evaluation_buf.ptr);
	free(includes_stack.ptr);
	free(macro_pre_expansion.tokens);
	free(macro_pre_expansion.positions);
	for (u32 i = 0; i < macros.capacity; i++) {
		if (macros.content[i] && ((Macro*)macros.content[i])->tokens.len)
			free(((Macro*)macros.content[i])->tokens.ptr);
	}
	free_arena(&macro_arena);
	mapFree(&macros);
	mapFree(&sources);
	return t;
}



static void expandInto (Arena *arena, Tokenization *dest, Tokenization *source, const StringMap *macros) {
	for (u32 i = 0; i < source->count; i++) {
		Token t = source->tokens[i];
		Macro *mac;
		if (t.kind == Tok_Identifier
			&& (mac = mapGet(macros, t.val.identifier)) != NULL
			&& !mac->being_replaced)
		{
			mac->being_replaced = true;

			if (mac->is_function_like) {
				if (i + 1 >= source->count || source->tokens[i+1].kind != Tok_OpenParen) {
					// Macro is not invoked as a function.
					mac->being_replaced = false;
					appendOneToken(dest, source->tokens[i], source->positions[i]);
					continue;
				}
				SourceFile *invocation_file = source->files.ptr[mac->source_ref];
				const char *invocation_pos = invocation_file->content.ptr + source->positions[i].source_file_offset;
				i += 2;

				u32 argument_start = i;
				u32 current_param = 0;
				for (u32 depth = 1; depth > 0; i++) {
					if (i > source->count)
						lexerror(*invocation_file, invocation_pos,
								"missing closing parenthesis");


					if (source->tokens[i].kind == Tok_OpenParen)
						depth++;
					else if (source->tokens[i].kind == Tok_CloseParen)
						depth--;

					if (depth == 0 ||
						(source->tokens[i].kind == Tok_Comma &&
						 depth == 1))
					{

						if (current_param + 1 < mac->parameters.len || depth == 0) {
							mac->parameters.ptr[mac->parameters.len - 1].current_argument = (Tokenization) {
								dest->files,
								source->tokens + argument_start,
								source->positions + argument_start,
								i - argument_start
							};
							argument_start = i + 1;
							current_param++;
						} else if (!mac->is_vararg && depth != 0) {
							lexerror(*invocation_file, invocation_pos,
									"too many arguments supplied to this macro invocation");
						}
					}
				}

				// I believe this copy is actually necessary,
				// because argument substitution / stringification
				// happens entirely before re-scanning.
				// TODO THINK THAT THROUGH THOROUGHLY.
				Tokenization tmp = { dest->files };
				ensureCapacity(&tmp, mac->tokens.len);

				for (u32 k = 0; k < mac->tokens.len; k++) {
					MacroToken tok = mac->tokens.ptr[k];
					if (tok.parameter) {
						if (tok.tok.kind == Tok_PreprocDirective)
							lexerror(*source->files.ptr[mac->source_ref], 0, "TODO Implement stringification");
						// PERFOMANCE Should the argument list
						// rather be scanned once, into another
						// buffer?
						expandInto(arena, &tmp, &mac->parameters.ptr[tok.parameter - 1].current_argument, macros);
					} else {
						ensureCapacity(&tmp, 1);
						tmp.tokens[tmp.count] = tok.tok;
						tmp.positions[tmp.count] = (TokenPosition) {
							.macro_file_ref = mac->source_ref,
							.macro_file_offset = tok.source_pos,
						};
						tmp.count++;
					}
				}
				expandInto(arena, dest, &tmp, macros);
				free(tmp.tokens);
				free(tmp.positions);
			} else {
				// PERFORMANCE Eliminate this allocation & copy:
				// make a separate expandInto which takes a
				// LIST(MacroToken), or store macros as
				// Tokenizations.
				Tokenization tmp = { dest->files,
					aalloc(arena, sizeof(Token) * mac->tokens.len),
					aalloc(arena, sizeof(TokenPosition) * mac->tokens.len),
					mac->tokens.len,
					mac->tokens.len,
				};

				for (u32 i = 0; i < mac->tokens.len; i++) {
					tmp.tokens[i] = mac->tokens.ptr[i].tok;

					tmp.positions[i] = (TokenPosition) {
						.macro_file_ref = mac->source_ref,
						.macro_file_offset = mac->tokens.ptr[i].source_pos,
					};
				}
				expandInto(arena, dest, &tmp, macros);
			}

			mac->being_replaced = false;
			continue;
		}
		appendOneToken(dest, source->tokens[i], source->positions[i]);
	}
}

static void ensureCapacity (Tokenization *t, u32 required) {
	if (t->count + required > t->capacity) {
		if (t->capacity == 0) {
			t->capacity = required;
			t->tokens = calloc(sizeof(t->tokens[0]), t->capacity);
			t->positions = calloc(sizeof(t->positions[0]), t->capacity);
		} else {
			t->capacity = t->capacity * 3 / 2 + required;
			t->tokens = realloc(t->tokens, sizeof(t->tokens[0]) * t->capacity);
			t->positions = realloc(t->positions, sizeof(t->positions[0]) * t->capacity);
		}
	}
}

static void appendOneToken (Tokenization *t, Token tok, TokenPosition pos) {
	ensureCapacity(t, 1);
	t->tokens[t->count] = tok;
	t->positions[t->count] = pos;
	t->count++;
}



// PERFORMANCE This function may be critical for the parse.
static SpaceClass tryGobbleSpace (SourceFile source, const char **p) {
	const char *pos = *p;
	SpaceClass spacing = Space_Regular;
	while (true) {
		if (pos[0] == '\\' && pos[1] == '\n') {
			pos += 2;
		} else if (pos[0] == '/' && pos[1] == '/') {
			// TODO Check for C99
			while (pos[0] && pos[0] != '\n') pos++;
		} else if (pos[0] == '/' && pos[1] == '*') {
			const char *begin = pos;
			while (pos[0] && !(pos[0] == '*' && pos[1] == '/')) pos++;
			if (!pos[0])
				lexerror(source, begin, "comment must end before end of file");
			pos += 2;
		} else {
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

// PERFORMANCE This function is potentially a very hot path.
static void skipToEndIf (SourceFile source, const char **p) {
	const char *pos = *p;

	bool linebegin = false;
	u32 nesting = 1;
	while (nesting > 0) {
		if (*pos == '\n') {
			linebegin = true;
		} else if (*pos == '#' && linebegin) {
			pos++;
			while (isSpace(*pos)) pos++;
			const char *begin = pos;
			while (*pos && !isSpace(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive)) {
				nesting++;
			} else if (eql("endif", directive)) {
				nesting--;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n')
				pos++;
			else
				linebegin = false;
		}
		pos++;
		if (*pos == 0)
			lexerror(source, *p, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
	}
	*p = pos;
}

static IfClass skipToElseIfOrEnd (SourceFile source, const char **p) {
	const char *pos = *p;

	bool linebegin = false;
	while (true) {
		if (*pos == '\n') {
			linebegin = true;
		} else if (*pos == '#' && linebegin) {
			pos++;
			while (isSpace(*pos)) pos++;
			const char *begin = pos;
			while (*pos && !isSpace(*pos)) pos++;
			String directive = {pos - begin, begin};
			if (eql("if", directive) || eql("ifdef", directive)) {
				skipToEndIf(source, &pos);
			} else if (eql("elseif", directive)) {
				*p = pos;
				return If_ElseIf;
			} else if (eql("else", directive)) {
				// TODO Check that the line ends after the else directive.
				*p = pos;
				return If_Else;
			} else if (eql("endif", directive)) {
				*p = pos;
				return If_End;
			}
			pos--;
		} else if (!isSpace(*pos)) {
			if (pos[0] == '\\' && pos[1] == '\n')
				pos++;
			else
				linebegin = false;
		}
		pos++;
		if (*pos == 0)
			lexerror(source, *p, "%s#if%s or %s#ifdef%s needs to be closed by an %s#endif%s", BOLD, RESET, BOLD, RESET, BOLD, RESET);
	}
}

static bool gobbleSpaceToNewline (const char **p) {
	bool got = false;
	while (**p != '\n' && isSpace(**p)) {
		got = true;
		(*p)++;
	}
	return got;
}


// === Parsing constant expressions for #if ===

typedef struct {
	SourceFile source;
	const MacroToken *tok;
} ConstParse;

static int parseOr(ConstParse *parse);
static int parseAnd(ConstParse *parse);
static int parseEq(ConstParse *parse);
static int parseCmp(ConstParse *parse);
static int parseUnop(ConstParse *parse);
static int parseCore(ConstParse *parse);

static int parseOr(ConstParse *parse) {
	int x = parseAnd(parse);
	while (parse->tok->tok.kind == Tok_DoublePipe) {
		parse->tok++;
		x = x || parseAnd(parse);
	}
	return x;
}

static int parseAnd(ConstParse *parse) {
	int x = parseEq(parse);
	while (parse->tok->tok.kind == Tok_DoublePipe) {
		parse->tok++;
		x = x || parseEq(parse);
	}
	return x;
}

static int parseEq(ConstParse *parse) {
	int x = parseCmp(parse);
	while (true) {
		if (parse->tok->tok.kind == Tok_DoubleEquals) {
			parse->tok++;
			x = x == parseCmp(parse);
		} else if (parse->tok->tok.kind == Tok_BangEquals) {
			parse->tok++;
			x = x != parseCmp(parse);
		} else {
			break;
		}
	}
	return x;
}
static int parseCmp(ConstParse *parse) {
	int x = parseUnop(parse);

	while (true) {
		if (parse->tok->tok.kind == Tok_Less) {
			parse->tok++;
			x = x < parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_LessEquals) {
			parse->tok++;
			x = x <= parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_Greater) {
			parse->tok++;
			x = x > parseUnop(parse);
		} else if (parse->tok->tok.kind == Tok_GreaterEquals) {
			parse->tok++;
			x = x >= parseUnop(parse);
		} else {
			break;
		}
	}
	return x;
}

static int parseUnop(ConstParse *parse) {
	switch (parse->tok->tok.kind) {
	case Tok_Minus:
		parse->tok++;
		return -parseUnop(parse);
	case Tok_Tilde:
		parse->tok++;
		return ~parseUnop(parse);
	case Tok_Bang:
		parse->tok++;
		return !parseUnop(parse);
	default:
		return parseCore(parse);
	}
}
static int parseCore(ConstParse *parse) {
	if (parse->tok->tok.kind == Tok_Integer) {
		int x = parse->tok->tok.val.integer;
		parse->tok++;
		return x;
	} else if (parse->tok->tok.kind == Tok_OpenParen) {
		parse->tok++;
		int x = parseOr(parse);
		if (parse->tok->tok.kind != Tok_CloseParen)
			lexerror(parse->source, parse->source.content.ptr + parse->tok->source_pos, "exepected closing parenthesis");
		parse->tok++;
		return x;
	} else {
		lexerror(parse->source, parse->source.content.ptr + parse->tok->source_pos, "exepected an expression");
	}
}

static bool evalPreprocExpression(SourceFile source, StringMap defined, const char **p, ExpansionBuffer *buf) {
	const char *pos = *p;
	while (true) {
		// TODO Perfom macro replacement
		gobbleSpaceToNewline(&pos);
		if (*pos == '\n' || *pos == 0)
			break;
		Token tok = getToken(&pos);
		if (tok.kind == Tok_Identifier) {
			if (eql("defined", tok.val.identifier)) {
				tok = getToken(&pos);
				bool parenthesized = tok.kind == Tok_OpenParen;
				if (parenthesized)
					tok = getToken(&pos);
				if (tok.kind != Tok_Identifier)
					lexerror(source, pos, "the operator %sdefined%s expects an identifier as an argument", BOLD, RESET);

				bool found = mapGet(&defined, tok.val.identifier);
				tok = (Token) {Tok_Integer, {.integer = found, .int_type = Int_int}};

				if (parenthesized && getToken(&pos).kind != Tok_CloseParen)
					lexerror(source, pos, "missing closing parenthesis");
			} else {
				Macro *m = mapGet(&defined, tok.val.identifier);
				if (m) {
					if (m->parameters.len > 0)
						lexerror(source, pos, "TODO Implement function-like macros");
					for (u32 i = 0; i < m->tokens.len; i++)
						PUSH(*buf, m->tokens.ptr[i]);
					continue;
				} else {
					tok = (Token) {Tok_Integer, {.integer = 0, .int_type = Int_int}};
				}
			}
		}
		PUSH(*buf, ((MacroToken) {tok, pos - *p}));
	}
	*p = pos;
	PUSH(*buf, ((MacroToken) {{Tok_EOF}, pos - *p}));

	ConstParse parse = {source, buf->ptr};
	int res = parseOr(&parse);
	buf->len = 0;
	return res;
}


// === Helpers ===


// TODO Come up with a better system for nice highlighting.
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
	[Tok_Dot] = BOLD "." RESET,
	[Tok_TripleDot] = BOLD "..." RESET,

	[Tok_Bang] = BOLD "!" RESET,
	[Tok_BangEquals] = BOLD "!=" RESET,
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
	[Tok_LessEquals] = BOLD "<=" RESET,
	[Tok_DoubleLess] = BOLD "<<" RESET,
	[Tok_Greater] = BOLD ">" RESET,
	[Tok_GreaterEquals] = BOLD ">=" RESET,
	[Tok_DoubleGreater] = BOLD ">>" RESET,
	[Tok_Ampersand] = BOLD "&" RESET,
	[Tok_DoubleAmpersand] = BOLD "&&" RESET,
	[Tok_Pipe] = BOLD "|" RESET,
	[Tok_DoublePipe] = BOLD "||" RESET,
	[Tok_Hat] = BOLD "^" RESET,
	[Tok_Tilde] = BOLD "~" RESET,

	[Tok_EOF] = "end of file",
};

static char name[1024] = {0};

const char *tokenName (TokenKind kind) {
	if (token_names[kind]) {
		return token_names[kind];
	} else {
		if (kind >= Tok_Key_First && kind <= Tok_Key_Last) {
			const int count = sizeof(names) / sizeof(names[0]);
			for (int i = 0; i < count; i++) {
				if (names[i].key == kind)
					return names[i].name;
			}
		}

		strcpy(name, "unidentified token, ID (hacky, only the last emitted one is certain): ");
		sprintf(name + strlen(name), "%d", kind);
		return name;
	}
}

bool isSpace (char c) {
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

bool isAlpha (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit (char c) {
	return c >= '0' && c <= '9';
}

bool isAlnum (char c) {
	return isAlpha(c) || isDigit(c);
}


static void predefineMacros (StringMap *macros, Target *target) {
	(void) macros;
	(void) target;
	// TODO __STDC_ANALYZABLE__, __GNU__ etc.
}
