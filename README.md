# Synotra

**Synotra** is a concurrent, actor-model based programming language with built-in support for distributed systems and strong consistency guarantees.

## Features

✨ **Actor-Based Concurrency** - Lightweight actors with message passing  
⚡ **True Multi-Core Parallelism** - Harness multiple CPU cores efficiently  
🔒 **Single Writer Semantics** - Strong consistency guarantees for collections  
🎯 **IO Safety** - Explicit `io` keyword for side-effecting operations  
📦 **Rich Collections** - List, Map, Set with full type inference  
🔄 **For-Each Loops** - Iterate over collections easily  
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
./run.sh example/hello_world.sy
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

### Hello World

```kotlin
// Simple "Hello World" actor
actor main(name: String) {
    var str = ", World!"
    io fun run() {
        println("Hello${str}")
    }
}
```

### Collections and For-Each Loops

```kotlin
actor main(env: Env) {
    io fun run() {
        // Create and iterate over a List
        var list = List.new()
        list.add(10)
        list.add(20)
        list.add(30)

        for (item in list) {
            println("Item: ${item}")
        }

        // Create and iterate over a Map
        var map = MutableMap.new()
        map.put("a", 1)
        map.put("b", 2)

        for (key in map.keys()) {
            println("Key: ${key}")
        }

        for (entry in map.entrySet()) {
            println("${entry.key()}: ${entry.value()}")
        }
    }
}
```

### Actor Communication

```kotlin
class Start()

actor main(name: String) {
    io fun run(msg: Start) {
        // Ask another actor to perform computation
        var result = ask(Worker1, calc_fib, 35)
        println("Result: ${result}")
    }
}

actor Worker1(name: String) {
    io fun calc_fib(n: Int) {
        var a = 0
        var b = 1
        var i = 0
        while (i < n) {
            var temp = a + b
            a = b
            b = temp
            i = i + 1
        }
        return a
    }
}
```

### Supported Features

#### Data Types

- **Primitives**: `Int`, `String`, `Bool`
- **Collections**: `List<T>`, `MutableMap<K,V>`, `MutableSet<T>`
- **User-Defined**: Message classes, actors

#### Control Flow

- **Conditionals**: `if`/`else`
- **Loops**: `while`, `for (i in start..end)`, `for (item in collection)`
- **Functions**: Pure functions and IO functions

#### Operators

- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **String Interpolation**: `"Hello ${name}!"`

#### Actor Features

- **Message Passing**: `ask(actor, handler, args)` for request/response
- **IO Safety**: Compile-time checks for side-effecting code
- **Local Functions**: Define helper functions within actors
- **State**: Actor-level variables (`var`/`val`)

#### Collection Methods

- **List**: `add()`, `get()`, `size()`, `isEmpty()`, `addAll()`, `clear()`
- **Map**: `put()`, `get()`, `containsKey()`, `keys()`, `values()`, `entrySet()`, `size()`, `clear()`
- **Set**: `add()`, `remove()`, `contains()`, `size()`, `values()`, `addAll()`, `clear()`
- **Entry**: `key()`, `value()` (for Map entries)

## Examples

### Available Examples

```bash
# Basic concepts
./run.sh example/hello_world.sy          # Simple hello world
./run.sh example/control_flow.sy         # If/else and loops
./run.sh example/function_simple.sy      # Functions demonstration
./run.sh example/function_recursive.sy   # Recursive functions

# Collections
./run.sh example/collection_foreach.sy   # For-each iteration
./run.sh example/map_entryset.sy        # Map.entrySet() usage
./run.sh example/set_iteration.sy       # Set iteration

# Actor communication
./run.sh example/test_async_fib.sy      # Ask pattern with fibonacci
./run.sh example/fib_parallel_recursive.sy  # Parallel computation

# IO Safety
./run.sh example/io_safety_test_pass.sy  # ✅ Valid IO usage
./run.sh example/io_safety_test_fail.sy  # ❌ Compile error demo
```

### Performance: Parallel Fibonacci

Demonstrates true parallel execution across multiple CPU cores:

```bash
./run.sh example/fib_parallel_recursive.sy
```

**Features:**

- 6 worker actors computing different Fibonacci numbers in parallel
- Demonstrates `ask()` pattern for collecting results
- Shows multi-core performance benefits

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
├── example/           # Example programs (.sy files)
├── run.sh            # Compile and run script
└── README.md
```

## Architecture

### Compilation Pipeline

```
.sy source → Lexer → Parser → AST → Semantic Analysis → IR (.syi) → VM
```

### Type System

- **Type Inference**: Automatic type inference for variables and collections
- **Generic Collections**: Full support for `List<T>`, `Map<K,V>`, `Set<T>`
- **IO Safety**: Compile-time enforcement of IO context requirements

### Multi-Core Runtime

- **Tokio Multi-Threaded Runtime**: Worker threads for concurrent execution
- **Spawn Blocking**: Actors run on OS threads for true parallelism
- **Message Passing**: Async channels between actors
- **Ask/Response**: Future-based request/response pattern

## Development

### Running Tests

```bash
# Basic functionality
./run.sh example/hello_world.sy
./run.sh example/collection_foreach.sy

# Actor communication
./run.sh example/test_async_fib.sy
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

- [x] Actor-based concurrency with message passing
- [x] `ask()` pattern for request/response (async with futures)
- [x] Function definitions and calls (local functions)
- [x] Collections (`List<T>`, `MutableMap<K,V>`, `MutableSet<T>`)
  - [x] Single Writer (SW) support
  - [x] Full set of collection methods
  - [x] Type inference for generic collections
- [x] For-each loops (`for (item in collection)`)
- [x] String interpolation with expression support
- [x] Actor-level field declarations (`var`/`val`)
- [x] IO safety checks (compile-time enforcement)
- [x] Control flow (`if`/`else`, `while`, `for..in`)
- [x] Range-based iteration (`for (i in 0..10)`)
- [x] Type-safe actor/handler references (no string literals)

### 🚧 In Progress

- [ ] Enhanced error messages
- [ ] Type inference improvements
- [ ] Pattern matching

### 📋 Planned

- [ ] CRDT state support for distributed consistency
- [ ] Struct/Class definitions beyond message types
- [ ] Immutable collection variants
- [ ] Network distribution (remote actors)
- [ ] Persistence and snapshots
- [ ] Debugging tools and REPL
- [ ] Standard library expansion

## Performance Tips

For maximum performance:

1. **Use release builds**: Always compile with `cargo build --release`
2. **Minimize I/O**: Reduce `println()` calls in tight loops
3. **Batch work**: Give each actor substantial computation
4. **Collections**: Use appropriate collection types for your use case

## License

MIT License

## Contributing

Contributions welcome! Please open issues or pull requests.
