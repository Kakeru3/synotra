mod bytecode;
mod actor;
mod runtime;

use clap::Parser;
use std::fs;
use bytecode::IrProgram;
use runtime::Runtime;
use actor::Message;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input IR file (.syi)
    input: String,
}

fn main() {
    let args = Args::parse();
    let src = fs::read_to_string(&args.input).expect("Failed to read input file");

    let program: IrProgram = serde_json::from_str(&src).expect("Failed to parse IR");
    println!("Loaded Program with {} actors", program.actors.len());

    // Collect actor names BEFORE moving program
    let actor_names: Vec<String> = program.actors.iter().map(|a| a.name.clone()).collect();

    let mut runtime = Runtime::new(program);
    runtime.spawn_all();

    println!("Spawned {} actor(s)", runtime.actor_count());

    // Send "run" message ONLY to "main" actor
    if actor_names.contains(&"main".to_string()) {
        println!("Starting execution from 'main' actor...");
        
        let start = std::time::Instant::now();

        runtime.send("main", Message {
            name: "run".to_string(),
            args: vec![],
            reply_to: None,
        });

        // Wait for all actors to be idle (no pending messages)
        runtime.wait_for_idle();
        
        let elapsed = start.elapsed();
        println!("\n⏱️  Total execution time: {:?}", elapsed);
    } else {
        println!("Error: 'main' actor not found. Execution requires a 'main' actor.");
        return;
    }
    
    // Shutdown runtime (signals actors to stop)
    runtime.shutdown();
    runtime.wait_for_completion();
}
