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
                Err(e) => {
                    // Create error formatter for better error messages
                    use error::*;
                    let formatter = ErrorFormatter::new(code.clone(), args.input.clone());

                    // Check if there are multiple errors (separated by newlines)
                    let error_messages: Vec<&str> = e.split('\n').collect();

                    // Track seen errors to avoid duplicates
                    let mut seen_errors = std::collections::HashSet::new();

                    for error_msg in error_messages {
                        // Skip if we've already seen this exact error
                        if !seen_errors.insert(error_msg) {
                            continue;
                        }

                        // Match different error patterns
                        let err = if error_msg.starts_with("Undefined variable:") {
                            // Extract variable name (e.g., "Undefined variable: foo" -> "foo")
                            let var_name = error_msg
                                .strip_prefix("Undefined variable: ")
                                .unwrap_or("")
                                .trim()
                                .to_string();

                            // Use ErrorKind for structured error with explanation and suggestion
                            let kind = ErrorKind::UndefinedVariable {
                                name: var_name.clone(),
                            };
                            let mut compile_err = CompileError::from_kind(kind);

                            // Try to find the variable in source code
                            if let Some(span) = formatter.find_identifier(&var_name, 0) {
                                compile_err = compile_err.with_span(span);
                            }

                            compile_err
                        } else if error_msg.starts_with("Duplicate definition:") {
                            // Extract variable name from "Duplicate definition: 'x' is already defined..."
                            let var_name = error_msg
                                .split('\'')
                                .nth(1)
                                .unwrap_or("unknown")
                                .to_string();

                            let kind = ErrorKind::DuplicateDefinition {
                                name: var_name.clone(),
                            };
                            let mut compile_err = CompileError::from_kind(kind);

                            // Try to find the duplicate in source code
                            if let Some(span) = formatter.find_identifier(&var_name, 0) {
                                compile_err = compile_err.with_span(span);
                            }

                            compile_err
                        } else if error_msg.starts_with("Undefined function") {
                            // Extract function name from "Undefined function 'name'"
                            let func_name = error_msg
                                .split('\'')
                                .nth(1)
                                .unwrap_or("unknown")
                                .to_string();

                            let kind = ErrorKind::UndefinedVariable { name: func_name };
                            CompileError::from_kind(kind)
                        } else if error_msg.starts_with("Uninitialized variable:") {
                            // Extract variable name from "Uninitialized variable: name"
                            let var_name = error_msg
                                .strip_prefix("Uninitialized variable: ")
                                .unwrap_or("")
                                .trim()
                                .to_string();

                            let kind = ErrorKind::UninitializedVariable {
                                name: var_name.clone(),
                            };
                            let mut compile_err = CompileError::from_kind(kind);

                            // Find the first occurrence (declaration) and then find the second occurrence (usage)
                            if let Some(first_span) = formatter.find_identifier(&var_name, 0) {
                                // Search for the next occurrence after the declaration
                                let next_offset = first_span.end;
                                if let Some(usage_span) =
                                    formatter.find_identifier(&var_name, next_offset)
                                {
                                    compile_err = compile_err.with_span(usage_span);
                                } else {
                                    // Fallback to declaration if usage not found
                                    compile_err = compile_err.with_span(first_span);
                                }
                            }

                            compile_err
                        } else if error_msg.contains("must have a return type") {
                            // Pure function missing return type
                            CompileError::from_kind(ErrorKind::MissingReturn)
                        } else {
                            // Generic error without position
                            CompileError::error(error_msg.to_string())
                        };

                        // Format and print the error
                        eprint!("{}", formatter.format_compile_error(&err));
                    }
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
