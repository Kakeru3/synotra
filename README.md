# Synotra

**Synotra** is a concurrent, actor-model based programming language with built-in support for distributed systems and strong consistency guarantees.

## Features

✨ **Actor-Based Concurrency** - Lightweight actors with message passing  
⚡ **True Multi-Core Parallelism** - Harness multiple CPU cores efficiently  
🔒 **Single Writer Semantics** - Strong consistency guarantees  
🎯 **IO Safety** - Explicit `io` keyword for side-effecting operations  
🚀 **High Performance** - Optimized runtime with Tokio async/await

## Quick Start

### Installation

```bash
# Clone the repository
git clone <repo-url>
cd synotra

# Build the compiler and VM (release mode for best performance)
cargo build --release
```

### Running Programs

Use the `run.sh` script for quick development (compiles and runs in one command):

```bash
# Run a .sy file directly
./run.sh example/map_test.sy
```

Or compile and run manually:

```bash
# 1. Compile .sy to .syi
./target/release/synotra example/your_program.sy > example/your_program.syi

# 2. Run with the VM
cd syvm
cargo run --release -- ../example/your_program.syi
```

## Language Syntax

Synotra uses Kotlin-like syntax with explicit actor definitions:

```kotlin
// Define a message type
class Start()

// Define an actor
actor Worker(name: String) {
    // IO function (can perform side effects)
    io fun print(msg: String) { }
    
    // Message handler
    io fun run(msg: Start) {
        print("Hello from Worker!")
        
        // Variables and control flow
        var i = 0
        while (i < 10) {
            print(i)
            i = i + 1
        }
    }
}
```

### Supported Features

- **Control Flow**: `if`/`else`, `while` loops, `for (i in 0..10)` ranges
- **Operators**: `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Types**: `Int`, `String`, user-defined message classes
- **Actors**: Multiple actors with concurrent execution
- **Message Passing**: Inter-actor communication (planned)

## Examples

### Parallel Fibonacci (Multi-Core)

Demonstrates true parallel execution across multiple CPU cores:

```bash
# Compile
cargo run --release --bin synotra -- example/fib_parallel_100x.sy

# Run
cargo run --release --bin syvm -- example/fib_parallel_100x.syi
```

**Performance:**

- Sequential (300 iterations): ~1.37s
- Parallel (3 workers × 100 iterations): ~1.01s
- **Speedup: 26% faster with multi-core execution!**

### Sequential Fibonacci

Single-threaded baseline for comparison:

```bash
cargo run --release --bin synotra -- example/fib_sequential_300x.sy
cargo run --release --bin syvm -- example/fib_sequential_300x.syi
```

## Project Structure

```
synotra/
├── synotra/           # Compiler
│   ├── src/
│   │   ├── lexer.rs      # Tokenizer (Logos)
│   │   ├── parser.rs     # Parser (Chumsky)
│   │   ├── ast.rs        # Abstract Syntax Tree
│   │   ├── sema.rs       # Semantic analyzer (type checking, IO safety)
│   │   ├── codegen.rs    # IR code generation
│   │   ├── ir.rs         # Intermediate representation (SSA)
│   │   └── main.rs       # CLI entry point
│   └── Cargo.toml
├── syvm/              # Virtual Machine
│   ├── src/
│   │   ├── bytecode.rs   # IR data structures
│   │   ├── actor.rs      # Actor runtime & mailbox
│   │   ├── runtime.rs    # Scheduler & execution engine
│   │   └── main.rs       # CLI entry point (multi-threaded runtime)
│   └── Cargo.toml
├── example/           # Example programs
│   ├── fib_parallel_100x.sy     # Parallel Fibonacci
│   └── fib_sequential_300x.sy   # Sequential Fibonacci
└── README.md
```

## Architecture

### Compilation Pipeline

```
.sy source → Lexer → Parser → AST → Semantic Analysis → IR (.syi) → VM
```

### Multi-Core Runtime

- **Tokio Multi-Threaded Runtime**: 8 worker threads
- **Spawn Blocking**: Actors run on OS threads for true parallelism
- **Message Passing**: Async channels between actors
- **Concurrent Execution**: Multiple actors run simultaneously

## Performance Tuning

For maximum performance:

1. **Use release builds**: `cargo build --release`
2. **Tune worker threads**: Modify `worker_threads` in `syvm/src/main.rs`
3. **Minimize I/O**: Reduce `print()` calls in CPU-intensive loops
4. **Batch work**: Give each actor substantial computation to offset message-passing overhead

## Development

### Running Tests

```bash
# Compile examples
cd synotra
cargo run --release --bin synotra -- ../example/fib_parallel_100x.sy

# Execute
cd ../syvm
cargo run --release --bin syvm -- ../example/fib_parallel_100x.syi
```

### Adding New Features

1. **Lexer**: Add tokens in `lexer.rs`
2. **Parser**: Extend grammar in `parser.rs`
3. **AST**: Define new node types in `ast.rs`
4. **Semantic Analysis**: Add validation in `sema.rs`
5. **Codegen**: Generate IR in `codegen.rs`
6. **VM**: Implement execution in `actor.rs`

## Editor Support

### VS Code Extension

A VS Code extension for Synotra is available, providing syntax highlighting and language support:

**[synotra-vscode](https://github.com/BlueGeckoJP/synotra-vscode)**

Features:

- Syntax highlighting for `.sy` files
- Language server protocol (LSP) support
- Code snippets and autocompletion

## Roadmap

### ✅ Completed

- [x] `send()` instruction for inter-actor messaging
- [x] `ask()` pattern for request/response (async with futures)
- [x] Function definitions and calls (local functions)
- [x] Collections (Single Writer: `List<T>`, `MutableMap<K,V>`, `MutableSet<T>`)
- [x] String interpolation with expression support
- [x] Actor-level field declarations (`var`/`val`)
- [x] IO safety checks (compile-time enforcement)
- [x] Control flow (`if`/`else`, `while`, `for..in`)
- [x] Import statements with generics

### 🚧 In Progress

- [x] Collections (List, Map, Set)
  - [x] Single Writer (SW) support
  - [x] Collection methods (add, get, size, etc.)
- [ ] Type Inference Improvements
- [ ] Mutable List support (set index)

### 📋 Planned

- [ ] CRDT state support for distributed consistency
- [ ] Pattern matching
- [ ] Struct/Class definitions beyond message types
- [ ] More collection types (immutable variants)
- [ ] Network distribution (remote actors)
- [ ] Persistence and snapshots
- [ ] Debugging tools and REPL

## License

MIT License

## Contributing

Contributions welcome! Please open issues or pull requests.
