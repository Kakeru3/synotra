# Synotra

**Synotra** is a concurrent, actor-based programming language with built-in support for message passing and strong type safety.

## Features

✨ **Actor-Based Concurrency** - Lightweight actors with message passing  
⚡ **True Multi-Core Parallelism** - Harness multiple CPU cores efficiently  
� **Single Writer Principle** - Each actor owns and modifies only its own state  
�📦 **Data Messages** - Type-safe message definitions with field access  
🎯 **Dynamic Actor Spawning** - Create actors at runtime  
💬 **Send & Ask** - Fire-and-forget and request-response messaging  
🛡️ **IO Safety** - Explicit `io` keyword for side-effecting operations  
🚀 **High Performance** - Optimized runtime with multi-threading

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

Use the `run.sh` script for quick development:

```bash
# Run a .sy file directly
./run.sh example/01_hello_world.sy
```

## Language Syntax

### Hello World

```synotra
actor main(name: String) {
    io fun run() {
        println("Hello, World!")
    }
}
```

### Variables and String Interpolation

```synotra
actor main(name: String) {
    io fun run() {
        var greeting = "Hello"
        var target = "World"
        println("${greeting}, ${target}!")
        
        var x = 10
        var y = 20
        println("Sum: ${x + y}")
    }
}
```

### Control Flow

```synotra
actor main(name: String) {
    io fun run() {
        // If-else
        var x = 10
        if (x > 5) {
            println("x is greater than 5")
        } else {
            println("x is less than or equal to 5")
        }
        
        // While loop
        var counter = 0
        while (counter < 3) {
            println("Counter: ${counter}")
            counter = counter + 1
        }
        
        // For loop
        for (i in 0..5) {
            println("For loop: ${i}")
        }
    }
}
```

### Data Messages

```synotra
data message User(val id: Int, val name: String)
data message Task(val id: Int, val description: String, val assignee: User)

actor main(name: String) {
    io fun run() {
        // Create messages
        var user = User(1, "Alice")
        println("User: ${user.name} (ID: ${user.id})")
        
        // Nested messages
        var task = Task(101, "Review code", user)
        println("Task: ${task.description}")
        println("Assigned to: ${task.assignee.name}")
        
        // Direct nesting
        var task2 = Task(102, "Write docs", User(2, "Bob"))
        println("Task assigned to ${task2.assignee.name}")
    }
}
```

### Functions

Pure functions are defined with `fun`. If they return a value, the return type is **mandatory**.

```synotra
// Pure function with return value
fun add(a: Int, b: Int): Int {
    return a + b
}

// Pure function without return value (return type omitted)
fun log(msg: String) {
    // ...
}
```

IO functions are defined with `io fun`. They perform side effects and **do not have a return type**.

```synotra
io fun printMessage(msg: String) {
    println(msg)
}
```

### Actor Spawning and Message Passing

```synotra
data message Task(val id: Int, val description: String)

// Worker actor processes tasks
actor Worker(workerId: Int) {
    io fun receive(task: Task) {
        println("Worker ${workerId} processing task ${task.id}: ${task.description}")
    }
}

// Main actor spawns workers and sends tasks
actor main(name: String) {
    io fun run() {
        // Spawn workers
        var worker1 = spawn(Worker, 1)
        var worker2 = spawn(Worker, 2)
        
        // Send messages (fire-and-forget)
        worker1.send(Task(101, "Process data batch 1"))
        worker2.send(Task(102, "Process data batch 2"))
        
        // Ask (synchronous request-response)
        worker1.ask(Task(103, "Urgent analysis"))
        
        println("All tasks dispatched!")
    }
}
```

## Language Features

### Data Types

- **Primitives**: `Bool`, `Int`, `String`
- **Messages**: User-defined via `data message`
- **Actors**: `ActorRef<MessageType>`

### Control Flow

- **Conditionals**: `if`/`else`
- **Loops**: `while`, `for (i in start..end)`
- **Functions**: `fun` (pure), `io fun` (side effects)

### Operators

- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **String Interpolation**: `"Hello ${name}!"`

### Actor Features

- **Data Messages**: `data message Name(val field: Type, ...)`
- **Field Access**: `msg.fieldName`, `msg.nested.field`
- **Spawning**: `var actor = spawn(ActorType, args...)`
- **Send**: `actor.send(message)` - Fire-and-forget
- **Ask**: `var result = actor.ask(message)` - Request-response
- **IO Safety**: Compile-time checks for side-effecting code

## Examples

All examples are in the `example/` directory:

```bash
./run.sh example/01_hello_world.sy    # Hello World
./run.sh example/02_control_flow.sy   # If/else, loops
./run.sh example/03_variables.sy      # Variables and interpolation
./run.sh example/04_messages.sy       # Data messages and field access
./run.sh example/05_actors.sy         # Actor spawning and messaging
./run.sh example/06_functions.sy      # Pure function definitions
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
│   │   ├── ir.rs         # Intermediate representation
│   │   └── main.rs       # CLI entry point
│   └── Cargo.toml
├── syvm/              # Virtual Machine
│   ├── src/
│   │   ├── bytecode.rs   # IR data structures
│   │   ├── actor.rs      # Actor runtime & execution
│   │   ├── runtime.rs    # Scheduler & multi-threading
│   │ │   └── main.rs       # CLI entry point
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

- **Type Inference**: Automatic type inference for variables
- **Message Types**: Structural typing for data messages
- **IO Safety**: Compile-time enforcement of IO context requirements

### Actor Model & Single Writer

- **Isolation**: Each actor has its own private state (fields)
- **Single Writer**: An actor can only modify its own state, never another actor's
- **Message Passing**: Actors communicate exclusively through immutable messages
- **No Shared Memory**: No shared mutable state between actors
- **Thread Safety**: Each actor runs independently, eliminating data races

### Multi-Core Runtime

- **Thread-per-Actor**: Each spawned actor runs on a dedicated OS thread
- **Message Passing**: MPSC channels between actors
- **Ask/Response**: Blocking request-response with reply channels

## Roadmap

### ✅ Phase 4 Complete

- [x] Data message definitions (`data message`)
- [x] Message construction and field access
- [x] Actor spawning (`spawn`)
- [x] Message passing (`send`, `ask`)
- [x] Nested message support
- [x] String interpolation with field access

### 📋 Phase 5 Planned

- [ ] Handler return values for `ask`
- [ ] Type-safe message routing
- [ ] Direct handler calls (`actor.handlerName(msg)`)
- [ ] Error handling for dead actors
- [ ] Async `ask` (non-blocking)

### 🚀 Future

- [ ] CRDT state support
- [ ] Pattern matching
- [ ] Collections (List, Map, Set)
- [ ] Network distribution (remote actors)
- [ ] Standard library expansion

## Performance Tips

1. **Use release builds**: `cargo build --release`
2. **Minimize I/O**: Reduce `println()` in tight loops
3. **Batch work**: Give each actor substantial computation
4. **Leverage parallelism**: Spawn multiple workers

## License

MIT License

## Contributing

Contributions welcome! Please open issues or pull requests.
