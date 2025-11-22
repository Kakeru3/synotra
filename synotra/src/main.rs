mod lexer;
mod ast;
mod parser;
mod sema;
mod ir;
mod codegen;

use clap::Parser as ClapParser;
use logos::Logos;
use chumsky::Parser;
use std::fs;
use crate::lexer::Token;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input file
    input: String,
}

fn main() {
    let args = Args::parse();

    let code = fs::read_to_string(&args.input).expect("Failed to read input file");

    let lexer = Token::lexer(&code);
    let tokens: Vec<_> = lexer.filter_map(|t| t.ok()).collect(); // TODO: Handle errors
    println!("Tokens: {:?}", tokens);

    let parser = parser::parser();
    let result = parser.parse(tokens);

    match result {
        Ok(program) => {
            println!("{:#?}", program);
            match sema::analyze(&program) {
                Ok(_) => {
                    println!("Semantic analysis passed");
                    use ir::{IrProgram, IrActor};
                    use codegen::Codegen;
                    use ast::{Definition, ActorMember};
                    let mut ir_program = IrProgram { actors: Vec::new() };

                    for def in &program.definitions {
                        if let Definition::Actor(actor_def) = def {
                            let mut handlers = Vec::new();
                            for member in &actor_def.members {
                                if let ActorMember::Method(func) = member {
                                    let mut codegen = Codegen::new(actor_def.name.clone());
                                    let handler = codegen.generate(func);
                                    handlers.push(handler);
                                }
                            }
                            ir_program.actors.push(IrActor {
                                name: actor_def.name.clone(),
                                handlers,
                            });
                        }
                    }
                    
                    let json = serde_json::to_string_pretty(&ir_program).unwrap();
                    println!("IR Output:\n{}", json);
                    
                    // Write to .syi file (support both .sy and .syo extensions)
                    let output_path = if args.input.ends_with(".sy") {
                        args.input.replace(".sy", ".syi")
                    } else {
                        args.input.replace(".syo", ".syi")
                    };
                    fs::write(output_path, json).expect("Failed to write IR file");
                },
                Err(e) => println!("Semantic error: {}", e),
            }
        },
        Err(errs) => {
            for err in errs {
                println!("Parse error: {:?}", err);
            }
        }
    }
}
