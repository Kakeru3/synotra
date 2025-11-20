mod bytecode;
mod actor;
mod runtime;

use clap::Parser;
use std::fs;
use bytecode::{IrProgram, Value};
use runtime::Runtime;
use actor::Message;
use std::collections::HashMap;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input IR file (.syi)
    input: String,
}

#[tokio::main(flavor = "multi_thread", worker_threads = 8)]
async fn main() {
    let args = Args::parse();
    let src = fs::read_to_string(&args.input).expect("Failed to read input file");

    let program: IrProgram = serde_json::from_str(&src).expect("Failed to parse IR");
    println!("Loaded Program with {} actors", program.actors.len());

    let mut runtime = Runtime::new();

    // Spawn ALL actors from the program
    let actors_vec: Vec<_> = program.actors.into_iter().collect();
    
    // Collect actor names BEFORE moving actors_vec
    let actor_names: Vec<String> = actors_vec.iter().map(|a| a.name.clone()).collect();
    
    runtime.spawn_all(actors_vec, |name| {
        let mut state = HashMap::new();
        state.insert("name".to_string(), Value::ConstString(name.to_string()));
        state
    });

    println!("Spawned {} actor(s)", runtime.actor_count());

    // Send "run" message to ALL actors for parallel execution
    if !actor_names.is_empty() {
        println!("Starting parallel execution...");
        
        let start = std::time::Instant::now();
        
        for actor_name in &actor_names {
            runtime.send(actor_name, Message {
                name: "run".to_string(),
                args: vec![],
                reply_to: None,
            }).await;
        }

        // Wait for all actors to complete
        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        
        let elapsed = start.elapsed();
        println!("\n⏱️  Parallel execution time: {:?}", elapsed);
    } else {
        println!("No actors found in the program");
    }

    // Keep alive for a bit
    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
}
