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

    // Send "run" message to ALL actors for parallel execution
    if !actor_names.is_empty() {
        println!("Starting parallel execution...");
        
        for actor_name in &actor_names {
            runtime.send(actor_name, Message {
                name: "run".to_string(),
                args: vec![],
                reply_to: None,
            });
        }

        // Give actors a moment to process messages before shutdown
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Wait for actors to run
    // We sleep here to allow the computation to finish. 
    // In a real system, we would wait for a termination signal.
    std::thread::sleep(std::time::Duration::from_secs(15));

    let start = std::time::Instant::now();
    
    // Shutdown runtime (signals actors to stop)
    runtime.shutdown();
        runtime.wait_for_completion();
        
        let elapsed = start.elapsed();
        println!("\n⏱️  Pure execution time (shutdown + cleanup): {:?}", elapsed);
    } else {
        println!("No actors found in the program");
    }
}
