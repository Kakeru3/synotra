mod ast;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod sema;

use crate::lexer::Token;
use chumsky::Parser;
use clap::Parser as ClapParser;
use logos::Logos;
use std::fs;

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
                                                                 // println!("Tokens: {:?}", tokens);

    let parser = parser::parser();
    let result = parser.parse(tokens);

    match result {
        Ok(program) => {
            eprintln!("[DEBUG] Parsed AST: {:#?}", program);
            match sema::analyze(&program) {
                Ok(symbols) => {
                    // println!("Semantic analysis passed");
                    use ast::{ActorMember, Definition};
                    use codegen::Codegen;
                    use ir::{IrActor, IrProgram};
                    let mut ir_program = IrProgram { actors: Vec::new() };

                    for def in &program.definitions {
                        if let Definition::Actor(actor_def) = def {
                            // Collect fields
                            let mut fields = Vec::new();
                            for member in &actor_def.members {
                                if let ActorMember::Field(field) = member {
                                    fields.push(field.clone());
                                }
                            }

                            let mut handlers = Vec::new();
                            for member in &actor_def.members {
                                if let ActorMember::Method(func) = member {
                                    let codegen = Codegen::new(actor_def.name.clone(), &symbols);
                                    let handler = codegen.generate(func, &fields);
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
                    // println!("IR Output:\n{}", json);

                    // Write to .syi file (support both .sy and .syo extensions)
                    let output_path = if args.input.ends_with(".sy") {
                        args.input.replace(".sy", ".syi")
                    } else {
                        args.input.replace(".syo", ".syi")
                    };
                    fs::write(output_path, json).expect("Failed to write IR file");
                }
                Err(e) => eprintln!("Semantic error: {}", e),
            }
        }
        Err(errs) => {
            for err in errs {
                eprintln!("Parse error: {:?}", err);
            }
        }
    }
}
