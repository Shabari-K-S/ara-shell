# Ara (Aura) Shell

**Ara** (also known as **Aura**) is a modern, high-performance, POSIX-compliant shell written in Rust. It aims to combine the speed and correctness of a traditional shell with the user-friendly features of modern interactive shells like Fish.

## Features

### 🚀 Core Architecture
- **Rust-Powered**: Built for safety and performance using Rust 2021 edition.
- **Custom Parser**: Hand-written recursive descent parser for fine-grained control and error reporting.
- **Robust Execution**: Direct `fork` and `exec` management via `nix` and `libc`.

### ✨ Interactive Experience (Phase 3 Complete)
- **Syntax Highlighting**: Real-time coloring of commands (Green), operators (Cyan), and strings.
- **Smart History**: Persisted command history (`~/.aura_history`) with Up/Down navigation and **Ctrl+R** reverse search.
- **Tab Completion**: Auto-completion for file and directory names.
- **Line Editing**: Full line editing capabilities powered by `rustyline`.

### 🛠️ Shell Capabilities
- **Pipelines**: Infinite pipelines supported (`ls -la | grep cargo | wc -l`).
- **Redirection**: Standard input/output redirection (`>`, `>>`, `<`).
- **Built-ins**: `cd`, `exit`, `export`, `unset`.
- **Quote Handling**: Correctly handles single (`'`) and double (`"`) quotes.
- **Variables**: Environment variable management (`export`, `unset`) and parameter expansion (`$VAR`, `${VAR}`, `$?`).
- **Tilde Expansion**: `~` expands to home directory (`~/Projects` → `/home/user/Projects`).
- **Glob Expansion**: Wildcard patterns (`*.rs`, `src/*`, `?.toml`) expand to matching files.
- **Flow Control**: `if`/`else` conditionals, `while`/`for` loops, and function definitions.
- **Job Control**: Background processes (`&`), `jobs`, `fg`, `bg` builtins, signal handling.

## Installation & Usage

### Prerequisites
- [Rust Toolchain](https://rustup.rs/) (cargo, rustc)

### Building
```bash
git clone https://github.com/yourusername/ara.git
cd ara
cargo build --release
```

### Running
To start the shell:
```bash
cargo run
```

You will see the prompt:
```bash
aura src $ 
```

### Examples
```bash
# List files
ls -la

# Pipe output
cat Cargo.toml | grep version

# Variables
export NAME="Ara"
echo "Hello $NAME"

# Control Flow
if true; then echo "It works!"; fi

# Loops
for i in 1 2 3; do echo $i; done

# Functions
greet() { echo "Hello $1"; }
greet World

# Background jobs
sleep 10 &
jobs
fg %1
```

## Roadmap

The development of Aura follows a structured 10-phase plan to deliver a complete, production-ready shell.

- [x] **Phase 1: Core Architecture & Parsing**
    - Lexer, Parser, AST generation.
    - Basic command parsing logic.

- [x] **Phase 2: Execution Engine**
    - Process creation (`fork`/`exec`), pipeline orchestration.
    - Basic I/O redirection (`>`, `>>`, `<`).

- [x] **Phase 3: Interactive Frontend**
    - Syntax highlighting (Green commands, Cyan operators).
    - Persistent history with navigation.
    - `rustyline` integration for rich line editing.

- [x] **Phase 4: Variable Expansion & Environment**
    - [x] Environment variable management (`export`, `unset`).
    - [x] Parameter expansion (`${VAR}`, `$?`).
    - [ ] Arithmetic expansion (`$((1 + 1))`).

- [x] **Phase 5: Flow Control & Logic**
    - [x] Conditional execution (`if`, `else`).
    - [x] Loops (`for`, `while`).
    - [x] Function definitions.

- [x] **Phase 6: Advanced I/O & Redirections**
    - Here-documents (`<<EOF`) and Here-strings (`<<<`).
    - File descriptor duplication (`2>&1`).
    - Process substitution (`<(cmd)`).

- [x] **Phase 7: Job Control & Signal Handling**
    - Background process management (`&`).
    - Job control built-ins (`jobs`, `fg`, `bg`).
    - Signal trapping (`trap`) and Ctrl-C/Z handling.

- [ ] **Phase 8: Configuration & Extensibility**
    - Startup configuration (`.ararc`).
    - Aliases and custom prompt definition.
    - Plugin system foundation.

- [ ] **Phase 9: OS Integration & Native Built-ins**
    - Native implementation of critical utilities (`mkdir`, `touch`, `cp`) for speed.
    - User/Group management helpers.
    - Extended globbing (`*`, `?`, `[]`, `**`).

- [ ] **Phase 10: Performance & Hardening**
    - JIT parsing for complex scripts.
    - Security auditing and restricted shell mode (`-r`).
    - Comprehensive POSIX compliance verification.

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.
