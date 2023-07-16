# Compiler Internals

Necc consists of a combined lexer/preprocessor
(_lex\_preproc.c_) generating a list of tokens, a parser (_parse.c_)
translating that to an intermediate representation (_ir\_gen.c_),
analyses and optimizations on the IR (_analysis.c_) and target-specific
backends translating the IR into assembly (_x86\_64.c_).

This document describes some of the implementation choices; it will
hopefully help a great deal with diving into the source, and possibly
even with making your own compiler. To read it, you should have a decent
grasp of C, but you will not need to know very much about compilers.

[TOC]

## Lexing and preprocessing

Lexing is simple. Optimizing the lexer should be straightforward and
very local, so I am postponing that for a long time. Digraphs and
trigraphs are kind of finicky, but so rarely used that they may easily
be moved to a slow path.

The only mildly interesting thing happening here is that the lexer puts
each identifier into a global hash table of `Symbol`s, so that the
preprocessor and the parser can very efficently compare them simply by
index.

I also came up with a macro replacement algorithm that is terribly neat
but doesn't currently work. Once it does, I'll describe it here.

## Parsing

The syntax of C has been carefully designed to enable single-pass
compilation; the parser works by simple recursive descent¹ and
generates IR, never needing to construct a syntax tree. This works well
enough, but the dozen-or-so precedence levels of C make the call stack
deep enough that a shunting-yard may be faster for expressions.

Most of the parser works on expressions and generates `Value`s. These
consist of a `Type`, an `IrRef` denoting the instruction that generated
the value, and a value `Category` describing whether the instruction
computes the actual value (rvalue) or the address the value is stored at
(lvalue). The address-of operator `&` marks an lvalue as rvalue, the
dereference operator `*` marks an rvalue as lvalue. Actual loads are
performed by “lvalue conversion” when an lvalue is used in a context
that requires an rvalue. Pretty standard stuff.

Types are handled by value, and inner types are somewhat haphazardly
allocated from an arena; the data representation is far from optimal. At
least C has nominal typing, so types can be compared cheaply without
needing deep comparison or uniquing—except for function types, but
uniquing them would probably not be worth it.

The complete semantics of definitions and declarations in C are formed
by multiple properties scattered throughout the specification. You
probably already have a good mental model of these, but I have not been
able to put the logic into code as succinctly as I would have liked. The
fact that the words ‘static’ and ‘extern’ are semantically
overloaded by the standard does not make things easier, so let me lay
out the basic concepts:

Each identifier is associated with one `Symbol`, which describes the
current meaning of the identifier as a _label_ name, as a _name tag_ of
a structure, union or enum (`NameTaggedType`), and as an
`OrdinaryIdentifier` (the standard calls these categories “name
spaces”, no relation to C++'s concept of namespace). Labels are global
to a function, name tags and ordinary identifiers in one scope can
shadow definitions in enclosing scopes. An ordinary identifier can refer
to a `typedef` name, an enum constant, a stack-allocated variable (a
`Value` referring to an `Ir_StackAlloc` instruction), or a function or
variable with static storage duration (a `StaticValue`).

A parsed program is described by a `Module` containing a number of
`StaticValue`s. These can be public (“external linkage”) or private
(“internal linkage”). They may either be undefined, tentatively
defined, or defined. A value may be defined at most once, but a
declaration at file scope with no storage specifier or storage specifier
`static` constitutes a _tentative definition_, which can occur multiple
times and is turned into a single complete definition if the identifier
is not defined elsewhere in the translation unit.

## The intermediate representation

The intermediate representation (_IR_) is a graph of Static Single
Assignment (_SSA_) instructions allocated from a contiguous list for
each function. The list starts off unordered, with control flow
described by a graph of `Block`s. Each block covers a number of
side-effecting instructions which need to be executed in program order.
An IR scheduling stage later assigns the complete set of instructions to
be executed in each block. Jumps and branches always happen from the end
of one block to the beginning of another. Values pass from one block to
another through so-called Phi (𝜑) instructions. This is a very
well-established model of control flow that seems generally sensible,
except for:
* computed gotos, which will probably have to be translated to a switch
* critical edges, which need to be explicitly found and removed for multiple operations.

A critical edge in the control flow graph is a jump from a block with
multiple possible outgoing paths to one with multiple incoming paths.
Reasonable register allocation for 𝜑-instructions, for example,
requires the absence of critical edges.

A preliminary constant-folding pass is automatically performed while
the IR is first generated. This means that constant expressions can
simply be read back from the folded IR; on the other hand, error
generation for a value that is not constant where required becomes a bit
annoying.

### Optimizations

Very high-value optimizations appear to be load/store elisions
(currently only intra-block) and global code motion/global value
numbering (entirely TODO).

Memory optimization mostly means backtracking from each mem op and
deciding whether it aliases any previous mem op. Alias analysis
currently only consists in distinguishing stack allocations from globals
and function parameters but may become arbitrarily sophisticated, which
will probably need some per-value pre-computation. When the validity of
the memory address is preserved, load operations can be moved backwards
across basic blocks, inserting phi nodes where necessary. I believe this
is enough to SSA-ify all possible stack-allocated variables, which
allows me to completely discard SSA-ification theory and spares me of a
whole lot of complexity in the IR generator.

Global Code Motion: Cliff Click, 1995 ACM 0-89791-697-2/95/0006

## x86-64 code generation

Current backend: the most naive option possible. The value of each
instruction gets a dedicated stack slot. Results in tons of `mov`s.

---

¹ If you find a hand-written parser inelegant, you are worrying about
the wrong problem. Parsing is easily the most straightforward part of
any compiler, and much of parser theory is based in academic fantasy
land.
