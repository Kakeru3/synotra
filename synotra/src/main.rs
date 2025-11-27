mod ast;
mod codegen;
mod error;
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

    // Collect tokens with their positions (spans)
    let tokens_with_spans: Vec<(Token, std::ops::Range<usize>)> = lexer
        .spanned()
        .filter_map(|(tok, span)| tok.ok().map(|t| (t, span)))
        .collect();

    // For now, extract just tokens for the parser (Phase 3 will use spans)
    let tokens: Vec<_> = tokens_with_spans.iter().map(|(t, _)| t.clone()).collect();
    // println!("Tokens: {:?}", tokens);

    let parser = parser::parser();
    let result = parser.parse(tokens);

    match result {
        Ok(program) => {
            // println!("{:#?}", program);
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
                            let field_names: Vec<String> =
                                fields.iter().map(|f| f.name.clone()).collect();
                            ir_program.actors.push(IrActor {
                                name: actor_def.name.clone(),
                                fields: field_names,
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
                Err(errors) => {
                    // Create error formatter for better error messages
                    use error::*;
                    let formatter = ErrorFormatter::new(code.clone(), args.input.clone());

                    // Track seen errors to avoid duplicates
                    let mut seen_errors = std::collections::HashSet::new();

                    for mut err in errors {
                        // Deduplication based on title
                        if !seen_errors.insert(err.title.clone()) {
                            continue;
                        }

                        // Recover span information if missing
                        if err.span.is_none() {
                            if let Some(kind) = &err.kind {
                                match kind {
                                    ErrorKind::UndefinedVariable { name }
                                    | ErrorKind::DuplicateDefinition { name }
                                    | ErrorKind::ImmutableAssignment { name }
                                    | ErrorKind::UnknownFunction { name } => {
                                        if let Some(span) = formatter.find_identifier(&name, 0) {
                                            err.span = Some(span);
                                        }
                                    }
                                    ErrorKind::UninitializedVariable { name } => {
                                        // Find declaration first
                                        if let Some(decl_span) = formatter.find_identifier(&name, 0)
                                        {
                                            // Find usage after declaration
                                            if let Some(usage_span) =
                                                formatter.find_identifier(&name, decl_span.end)
                                            {
                                                err.span = Some(usage_span);
                                            } else {
                                                // Fallback to declaration
                                                err.span = Some(decl_span);
                                            }
                                        }
                                    }
                                    ErrorKind::UnknownField { field, .. } => {
                                        if let Some(span) = formatter.find_identifier(&field, 0) {
                                            err.span = Some(span);
                                        }
                                    }
                                    ErrorKind::UnknownMethod { method, .. } => {
                                        if let Some(span) = formatter.find_identifier(&method, 0) {
                                            err.span = Some(span);
                                        }
                                    }
                                    // For other errors, we might need more context or just print without span
                                    _ => {}
                                }
                            }
                        }

                        eprint!("{}", formatter.format_compile_error(&err));
                    }
                    std::process::exit(1);
                }
            }
        }
        Err(errs) => {
            for err in errs {
                eprintln!("Parse error: {:?}", err);
            }
        }
    }
}
