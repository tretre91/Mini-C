# Mini-C

A C to WebAssembly compiler written in OCaml

## Build

In order to build the compiler you will need an OCaml compiler, dune, and the menhir and cmdliner libraries installed, you can then build the compiler with the command:
```
dune build
```

## Compiling a program

In order to compile a C program, you can use the command:
```
minic prog.c
```

By default, the output program is sent on standard output, you can use the `-o` option to send it to a file, the full list of options can be seen with the command
```
minic --help
```

The output format is WebAssembly's [textual format](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format), the generated file can then be compiled to the binary format using the wat2wasm tool from the [WebAssembly binary toolkit](https://github.com/WebAssembly/wabt#wabt-the-webassembly-binary-toolkit).

## Executing a program

A wasm program which was compiled by minic can be executed on the command line using [deno](https://deno.land) and the typescript runtime located in the [`runtime`](./runtime/) directory. For example, from the root directory:
```
dune exec ./bin/minic.exe -- prog.c -o prog.wat
wat2wasm prog.wat
deno run --allow-read --quiet runtime/runtime.mts prog.wasm
```
will compile prog.mnc, create the corresponding binary wasm file, and execute its code using the runtime.

These programs can also be executed in an internet browser using https://github.com/tretre91/mnc_runtime.

**Note:** A program which doesn't include stdio.h can be used as a standalone WebAssembly module, as it won't depend on any imported function

## Tests

Tests are located in the [`test`](./test/) directory, executing the tests will require that you have [deno](https://deno.land/), [wabt](https://github.com/WebAssembly/wabt#wabt-the-webassembly-binary-toolkit) (for `wat2wasm`), the [patdiff](https://opam.ocaml.org/packages/patdiff/) library and gcc installed.

You can then run the tests with:
```
dune runtest
```

## Features

Features which are not currently implemented include:
- The address operator (&)
- Multidimensional arrays (`int matrix[5][6];`)
- increment and decrement operators (`++` and `--`)
- assignments operators (`a += 2`, `b *= 3`, etc)
- assignments as expressions (`int a; if (a = 5) {...`)
- single line blocks (`if (true) ...;`, `while(false) ...;`)

A small subset of the C standard library is included (putchar, puts, malloc, free and some functions from string.h), the source files related to the standard library can be found in the [`include`](./include/) directory.

A basic preprocessor is also included, the available directives are `#define` (for simple macros only), `#undef`, `#include`, `#ifdef`, `#ifndef`, `#else` and `#endif`.
