#![allow(dead_code)]
use std::{
    fmt::Display,
    fs,
    io::Write,
    process::{Command, Output},
    str::FromStr,
};

use indoc::writedoc;

use clap::{ArgEnum, Parser};
use tempfile::NamedTempFile;

struct Program {
    size: u64,
    cell_size: CellSize,
    code: Vec<Instruction>,
    mem_ovfl: MemoryOverflowBehaviour,
}

struct LabelProvider {
    counter: u64,
}

impl LabelProvider {
    fn new() -> Self {
        Self { counter: 0 }
    }

    fn get_label(&mut self) -> String {
        let res = format!("L{:04X}", self.counter);
        self.counter += 1;
        res
    }
}

#[derive(Clone, ArgEnum)]
enum MemoryOverflowBehaviour {
    Undefined,
    Wrap,
    Abort,
}

impl Default for MemoryOverflowBehaviour {
    fn default() -> Self {
        Self::Wrap
    }
}

#[derive(Clone, Copy, ArgEnum)]
enum CellSize {
    Byte = 1,
    Word = 2,
    DoubleWord = 4,
    QuadWord = 8,
}

impl CellSize {
    fn as_suffix(&self) -> &'static str {
        match self {
            Self::Byte => "b",
            Self::Word => "w",
            Self::DoubleWord => "l",
            Self::QuadWord => "q",
        }
    }
}

#[derive(Clone, Copy)]
enum Instruction {
    NextCell,
    PreviousCell,
    Increment,
    Decrement,
    Print,
    Input,
    LoopStart,
    LoopEnd,

    // Special, optimized, instructions
    NextMany(u64),
    PreviousMany(u64),
    DecrementMany(u64),
    IncrementMany(u64),
    Save,
    Restore,
    RestoreAdd,
    SetCell(u64),
}

impl TryFrom<char> for Instruction {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '<' => Self::PreviousCell,
            '>' => Self::NextCell,
            '+' => Self::Increment,
            '-' => Self::Decrement,
            '[' => Self::LoopStart,
            ']' => Self::LoopEnd,
            '.' => Self::Print,
            ',' => Self::Input,
            _ => return Err(()),
        })
    }
}

impl Instruction {
    fn access_cell(&self) -> bool {
        !matches!(
            self,
            Instruction::NextCell
                | Instruction::PreviousCell
                | Instruction::NextMany(_)
                | Instruction::PreviousMany(_)
        )
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writedoc!(
            f,
            "
            .data
                mem: .space {}, 0
                termios: .space 60, 0
                char: .byte 0
            .text
                error: .ascii \"Exception raised.\\n\"
            get_termios:
                movq %rdi, %r8
                movq %rsi, %r9
                movq $16, %rax
                movq $0, %rdi
                movq $0x5401, %rsi
                leaq termios(%rip), %rdx
                syscall
                movq %r8, %rdi
                movq %r9, %rsi
                ret
            set_termios:
                movq %rdi, %r8
                movq %rsi, %r9
                movq $16, %rax
                movq $0, %rdi
                movq $0x5402, %rsi
                leaq termios(%rip), %rdx
                syscall
                movq %r8, %rdi
                movq %r9, %rsi
                ret
            set_raw:
                call get_termios
                leaq termios(%rip), %r8
                andl $0xFFFFFFF5, 12(%r8)
                call set_termios
                ret
            unset_raw:
                call get_termios
                leaq termios(%rip), %r8
                orl $0x00000000A, 12(%r8)
                call set_termios
                ret
            fail:
                # print error
                movq $1, %rax
                movq $1, %rdi
                leaq error(%rip), %rsi
                movq $18, %rdx
                syscall
                call unset_raw
                # exit
                movq $60, %rax
                movq $1, %rdi
                syscall
            .globl _start
            .type _start, @function
            _start:
            call set_raw
            # non blocking io
            # get file status
            movq $72, %rax
            movq $0, %rdi
            movq $3, %rsi
            movq $0, %rdx
            syscall
            # set O_NONBLOCK
            movq %rax, %rdx
            orq $2048, %rdx
            # set file status
            movq $72, %rax
            movq $0, %rdi
            movq $4, %rsi
            syscall
            # rsi holds the address of the memory
            leaq mem(%rip), %rsi
            # rdi holds the current index
            movq $0, %rdi

        ",
            self.size * self.cell_size as u64
        )?;

        let mut lblprv = LabelProvider::new();

        let size_pow2 = self.size.is_power_of_two();
        // those only make sense if size is a power of 2
        let size_log2 = 63 - self.size.leading_zeros();
        let mask = (1 << size_log2) - 1;

        // whether or not the current index could be outside of the mem's length.
        let mut rdi_dirty = false;

        // stacks of loop labels
        let mut loop_start = Vec::new();
        let mut loop_end = Vec::new();

        for i in &self.code {
            if i.access_cell() {
                if rdi_dirty {
                    match self.mem_ovfl {
                        MemoryOverflowBehaviour::Wrap => {
                            if size_pow2 {
                                writedoc!(
                                    f,
                                    "
                                    andq ${mask}, %rdi
                                "
                                )?;
                            } else {
                                writedoc!(
                                    f,
                                    "
                                    cmpq ${size}, %rdi
                                    jb {end_label}
                                    movq $0, %rdx
                                    movq %rdi, %rax
                                    idivq {size}
                                    movq %rdx, %rdi
                                    {end_label}:
                                ",
                                    end_label = lblprv.get_label(),
                                    size = self.size
                                )?;
                            }
                        }
                        MemoryOverflowBehaviour::Abort => writedoc!(
                            f,
                            "
                                cmpq ${size}, %rdi
                                jae fail
                            ",
                            size = self.size
                        )?,
                        MemoryOverflowBehaviour::Undefined => {}
                    }
                    rdi_dirty = false;
                }
            } else {
                // instructions that do not read the current cell changes the cell pointer, and
                // therefore dirty rdi.
                rdi_dirty = true;
            }
            match i {
                Instruction::NextCell => writedoc!(
                    f,
                    "
                        incq %rdi
                    "
                )?,
                Instruction::NextMany(n) => writedoc!(
                    f,
                    "
                        addq ${n}, %rdi
                    "
                )?,
                Instruction::PreviousCell => writedoc!(
                    f,
                    "
                        decq %rdi
                    "
                )?,
                Instruction::PreviousMany(n) => writedoc!(
                    f,
                    "
                        subq ${n}, %rdi
                    "
                )?,
                Instruction::Increment => writedoc!(
                    f,
                    "
                        inc{} (%rsi, %rdi, {})
                    ",
                    self.cell_size.as_suffix(),
                    self.cell_size as u64
                )?,
                Instruction::IncrementMany(n) => writedoc!(
                    f,
                    "
                        add{} ${n}, (%rsi, %rdi, {})
                    ",
                    self.cell_size.as_suffix(),
                    self.cell_size as u64
                )?,
                Instruction::Decrement => writedoc!(
                    f,
                    "
                        dec{} (%rsi, %rdi, {})
                    ",
                    self.cell_size.as_suffix(),
                    self.cell_size as u64
                )?,
                Instruction::DecrementMany(n) => writedoc!(
                    f,
                    "
                        sub{} ${n}, (%rsi, %rdi, {})
                    ",
                    self.cell_size.as_suffix(),
                    self.cell_size as u64
                )?,
                Instruction::SetCell(v) => writedoc!(
                    f,
                    "
                        mov{s} ${v}, (%rsi, %rdi, {l})
                    ",
                    s = self.cell_size.as_suffix(),
                    l = self.cell_size as u64
                )?,
                Instruction::Save => writedoc!(
                    f,
                    "
                        mov{s} (%rsi, %rdi, {l}), %r15{s}
                    ",
                    s = self.cell_size.as_suffix(),
                    l = self.cell_size as u64
                )?,
                Instruction::Restore => writedoc!(
                    f,
                    "
                        mov{s} %r15{s}, (%rsi, %rdi, {l})
                    ",
                    s = self.cell_size.as_suffix(),
                    l = self.cell_size as u64
                )?,
                Instruction::RestoreAdd => writedoc!(
                    f,
                    "
                        add{s} %r15{s}, (%rsi, %rdi, {l})
                    ",
                    s = self.cell_size.as_suffix(),
                    l = self.cell_size as u64
                )?,
                Instruction::LoopStart => {
                    loop_start.push(lblprv.get_label());
                    loop_end.push(lblprv.get_label());

                    writedoc!(
                        f,
                        "
                        cmp{s} $0, (%rsi, %rdi, {size})
                        je {loop_end}
                        {loop_start}:
                    ",
                        size = self.cell_size as u64,
                        s = self.cell_size.as_suffix(),
                        loop_end = loop_end.last().unwrap(),
                        loop_start = loop_start.last().unwrap()
                    )?
                }
                Instruction::LoopEnd => writedoc!(
                    f,
                    "
                        cmp{s} $0, (%rsi, %rdi, {size})
                        jne {loop_start}
                        {loop_end}:
                    ",
                    size = self.cell_size as u64,
                    s = self.cell_size.as_suffix(),
                    loop_end = loop_end.pop().unwrap_or_else(|| "fail".to_owned()),
                    loop_start = loop_start.pop().unwrap_or_else(|| "fail".to_owned())
                )?,
                Instruction::Print => writedoc!(
                    f,
                    "
                        movq %rdi, %r8
                        movq %rsi, %r9
                        movq (%r9, %rdi, {size}), %r10
                        movb %r10b, char(%rip)
                        movq $1, %rax
                        movq $1, %rdi
                        leaq char(%rip), %rsi
                        movq $1, %rdx
                        syscall
                        movq %r8, %rdi
                        movq %r9, %rsi
                    ",
                    size = self.cell_size as u64
                )?,
                Instruction::Input => writedoc!(
                    f,
                    "
                        movq %rdi, %r8
                        movq %rsi, %r9
                        movq $0, %rax
                        movq $0, %rdi
                        leaq char(%rip), %rsi
                        movq $1, %rdx
                        syscall
                        sarq $63, %rax
                        notq %rax
                        movq %r8, %rdi
                        movq %r9, %rsi
                        leaq char(%rip), %r9
                        mov{s} (%r9), %r8{s}
                        andq %rax, %r8
                        mov{s} %r8{s}, (%rsi, %rdi, {size})
                    ",
                    size = self.cell_size as u64,
                    s = self.cell_size.as_suffix()
                )?,
            }
        }

        writedoc!(
            f,
            "
            call unset_raw
            movq $60, %rax
            movq $0, %rdi
            syscall
        "
        )
    }
}

mod optimize {
    use crate::{
        Instruction::{self, *},
        Program,
    };

    pub struct OptimizationRule(fn(&mut Vec<Instruction>) -> Option<Vec<Instruction>>);
    impl OptimizationRule {
        fn run(&self, v: &mut Vec<Instruction>) -> Option<Vec<Instruction>> {
            self.0(v)
        }
    }

    macro_rules! _opt_internal {
        ($p:pat => $e:expr, $i:expr, $ins:ident) => {
            if let Some($p) = $ins.get($i).cloned() {
                let res = $e.to_vec();
                $ins.drain(0..($i + 1));
                return Some(res);
            }
        };
        ($f:pat, $($p:pat),+ => $e:expr, $i:expr, $ins:ident) => {
            if let Some($f) = $ins.get($i).cloned() {
                _opt_internal!($($p),+ => $e, $i + 1, $ins);
            }
        };
    }

    macro_rules! opt_rule {
        ($($p:pat),+ => $e:expr) => {
            OptimizationRule(|ins| {
                _opt_internal!($($p),+ => $e, 0, ins);
                None
            })
        };
    }

    macro_rules! opt_rules {
        ($($($p:pat),+ => $e:expr),*$(,)?) => {{
            vec![$(opt_rule!($($p),+ => $e)),+]
        }};
    }

    impl Program {
        fn get_opt_rules() -> Vec<Vec<OptimizationRule>> {
            vec![
                // remove all singulars
                opt_rules! {
                    Increment => [IncrementMany(1)],
                    Decrement => [DecrementMany(1)],
                    NextCell => [NextMany(1)],
                    PreviousCell => [PreviousMany(1)],
                },
                // fold manys
                opt_rules! {
                    IncrementMany(a), IncrementMany(b) => [IncrementMany(a + b)],
                    IncrementMany(a), DecrementMany(b) => [if a > b { IncrementMany(a - b) } else { DecrementMany(b - a) }],
                    DecrementMany(a), DecrementMany(b) => [DecrementMany(a + b)],
                    DecrementMany(a), IncrementMany(b) => [if a > b { DecrementMany(a - b) } else { IncrementMany(b - a) }],
                    NextMany(a), NextMany(b) => [NextMany(a + b)],
                    NextMany(a), PreviousMany(b) => [if a > b { NextMany(a - b) } else { PreviousMany(b - a) }],
                    PreviousMany(a), PreviousMany(b) => [PreviousMany(a + b)],
                    PreviousMany(a), NextMany(b) => [if a > b { PreviousMany(a - b) } else { NextMany(b - a) }],
                },
                opt_rules! {
                    LoopStart, DecrementMany(1), LoopEnd => [SetCell(0)],
                    SetCell(n), IncrementMany(v) => [SetCell(n + v)],
                    LoopStart, DecrementMany(1), NextMany(a), IncrementMany(1), PreviousMany(b), LoopEnd =>
                        if a == b { [LoopStart, Save, SetCell(0), NextMany(a), RestoreAdd, PreviousMany(b), LoopEnd] } else { None? },
                    LoopStart, DecrementMany(1), PreviousMany(a), IncrementMany(1), NextMany(b), LoopEnd =>
                        if a == b { [LoopStart, Save, SetCell(0), PreviousMany(a), RestoreAdd, NextMany(b), LoopEnd] } else { None? },
                    LoopStart, NextMany(a), IncrementMany(1), PreviousMany(b), DecrementMany(1), LoopEnd =>
                        if a == b { [LoopStart, Save, SetCell(0), NextMany(a), RestoreAdd, PreviousMany(b), LoopEnd] } else { None? },
                    LoopStart, PreviousMany(a), IncrementMany(1), NextMany(b), DecrementMany(1), LoopEnd =>
                        if a == b { [LoopStart, Save, SetCell(0), PreviousMany(a), RestoreAdd, NextMany(b), LoopEnd] } else { None? },
                },
                // Turn back special manys into singular (and optimize away some)
                opt_rules! {
                    IncrementMany(0) => [],
                    DecrementMany(0) => [],
                    NextMany(0) => [],
                    PreviousMany(0) => [],
                    IncrementMany(1) => [Increment],
                    DecrementMany(1) => [Decrement],
                    NextMany(1) => [NextCell],
                    PreviousMany(1) => [PreviousCell],
                },
            ]
        }
        pub fn optimize(&mut self) {
            let rules = Self::get_opt_rules();

            for layer in &rules {
                let mut optimized = true;

                while optimized {
                    optimized = false;

                    let mut new = Vec::new();

                    while !self.code.is_empty() {
                        let mut found = false;

                        for rule in layer {
                            let ins = rule.run(&mut self.code);
                            if let Some(mut ins) = ins {
                                new.append(&mut ins);
                                optimized = true;
                                found = true;
                                break;
                            }
                        }
                        // if we went over all the rules, without any results
                        if !found {
                            new.push(self.code.remove(0));
                        }
                    }

                    self.code = new;
                }
            }
        }
    }
}

#[derive(Parser)]
struct Cli {
    /// The path to the brainfuck source
    #[clap(parse(from_os_str))]
    path: std::path::PathBuf,
    /// The path to the output
    #[clap(short, long, parse(from_os_str))]
    output: Option<std::path::PathBuf>,
    /// How should the program react to the cursor exiting the memory
    #[clap(short, long, arg_enum, default_value = "wrap")]
    memory_overflow: MemoryOverflowBehaviour,
    /// Size of the memory in cells (powers of 2 are faster)
    #[clap(short = 'z', long, default_value = "32768")]
    memory_size: u64,
    /// Size of the cells
    #[clap(short = 'e', long, default_value = "byte", arg_enum)]
    cell_size: CellSize,
    /// Optimize repeated instructions
    #[clap(short = 'O', long)]
    no_optimize: bool,
    /// Only compile, don't assemble or link
    #[clap(short = 'S')]
    only_compile: bool,
    /// Don't link
    #[clap(short = 'c')]
    no_link: bool,
}

fn hanndle_output(otp: Output) -> Result<(), String> {
    if !otp.status.success() {
        eprintln!(
            "{}",
            String::from_utf8(otp.stderr)
                .map_err(|_| "Couldn't convert stderr to string".to_string())?
        );
        Err("Command exited with non zero status".to_string())
    } else {
        Ok(())
    }
}

fn main() {
    let arg = Cli::parse();
    let code = fs::read_to_string(arg.path.clone()).expect("Couldn't read the source file");
    let mut prg = Program {
        size: arg.memory_size,
        mem_ovfl: arg.memory_overflow,
        cell_size: arg.cell_size,
        code: code.chars().filter_map(|c| c.try_into().ok()).collect(),
    };
    if !arg.no_optimize {
        prg.optimize();
    }
    let output = arg.output.unwrap_or_else(|| {
        if arg.only_compile {
            arg.path.with_extension("s")
        } else if arg.no_link {
            arg.path.with_extension("o")
        } else {
            std::path::PathBuf::from_str("a.out").unwrap()
        }
    });

    if arg.only_compile {
        fs::write(output, format!("{}", prg)).expect("Couldn't write");
    } else {
        let mut assembly = NamedTempFile::new().unwrap();
        writeln!(assembly, "{}", prg).expect("Couldn't write");

        if arg.no_link {
            hanndle_output(
                Command::new("as")
                    .arg(assembly.path())
                    .arg("-o")
                    .arg(output)
                    .output()
                    .map_err(|err| eprintln!("{err:?}"))
                    .expect("Error when assembling"),
            )
            .map_err(|err| eprintln!("{err}"))
            .expect("Error when assembling");
        } else {
            let object = NamedTempFile::new().unwrap();

            hanndle_output(
                Command::new("as")
                    .arg(assembly.path())
                    .arg("-o")
                    .arg(object.path())
                    .output()
                    .map_err(|err| eprintln!("{err:?}"))
                    .expect("Error when assembling"),
            )
            .map_err(|err| eprintln!("{err}"))
            .expect("Error when assembling");

            hanndle_output(
                Command::new("ld")
                    .arg("--nostd")
                    .arg(object.path())
                    .arg("-o")
                    .arg(output)
                    .output()
                    .map_err(|err| eprintln!("{err:?}"))
                    .expect("Error when linking"),
            )
            .map_err(|err| eprintln!("{err}"))
            .expect("Error when assembling");
        }
    }
}
