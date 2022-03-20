# BFC - linux x64\_86 brainfuck compiler with non-blocking real-time io

This is a brainfuck compiler, requires the GNU `as` and `ld`, doesn't use std / libc, written in rust.

> This doesn't hard depend on the GNU ones per se, but I've only ever tried with them (and the compiled ASM is in AT&T/GAS syntax).

## Usage

compile with

```bash
cargo build
```

and run (or `cargo run`).

See `--help` for more.
