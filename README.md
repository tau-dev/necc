# _necc_

The Not-Enough-C Compiler, with fairly complete support for C89, C99,
and C11 as well as some C23, GCC and MSVC extensions, targeting x86-64
Windows and Linux.

I have a number of cool optimizations I want to implement, but the back
end will remain entirely naive until I have gotten the front end to a
reasonable level of completeness and correctness. It just got to the
point of self-hosting, [and can already run some random C projects I found](doc/compat.md).

### Building

Necc uses the [musl](https://musl.libc.org/) standard library, and
currently invokes the GNU assembler as well as musl-gcc for assembly and
linking.

To build the project, set MUSL\_DIR to the path containing your musl
installation's include, arch and obj directories, and run GNU `make`;
the binaries are put into bin/. To execute the tests, run `make test`.

---

Necc is covered by the [BSD 3-clause Clear license](LICENSE). Some files
in this repository are redistributed from other projects; the applicable
licenses are contained in the respective files or directories.
