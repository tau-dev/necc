# _necc_

The Not-Enough-C Compiler, with fairly complete support for C89, C99,
and C11 as well as some C23, GCC and MSVC extensions, targeting x86-64
Linux [TODO: and Windows].

I have a number of cool optimizations I want to implement, but the back
end will remain entirely naive until I have gotten the front end to a
reasonable level of completeness and correctness. It just got to the
point of self-hosting, [and can already run some random C projects I found](doc/compat.md).

### Building

Necc currently invokes GCC for assembly and linking simply because GCC
probably knows the sane parameters for your platform. Needing an entire
separate C compiler to run my C compiler is of course quite silly;
pretty soon, I'll get around to writing the necessary logic for these
parameters, and then Necc shouldn't be requiring anything other than
binutils and a standard library anymore.

To build the project, set GLIBC\_DIR to the path containing your glibc's
include directory (`/usr/lib/gcc/x86_64-pc-linux-gnu/13.2.1` on my
machine) and run GNU `make`; the binaries are put into bin/. To execute
the tests, run `make test`.

---

Necc is covered by the [BSD 3-Clause Clear license](LICENSE). Some files
in this repository are redistributed from other projects; the applicable
licenses are contained in the respective files or directories.
