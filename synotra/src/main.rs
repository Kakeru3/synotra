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

    let eoi = code.len()..code.len();

    let parser = parser::parser();
    let result = parser.parse(chumsky::Stream::from_iter(
        eoi.clone(),
        tokens_with_spans
            .iter()
            .map(|(tok, span)| (tok.clone(), span.clone())),
    ));

    match result {
        Ok(program) => {
            // println!("{:#?}", program);
            match sema::analyze(&program) {
                Ok((symbols, warnings)) => {
                    // Display warnings if any
                    if !warnings.is_empty() {
                        use error::*;
                        let formatter = ErrorFormatter::new(code.clone(), args.input.clone());

                        eprintln!(); // blank line before warnings
                        for warning in &warnings {
                            eprintln!("{}", formatter.format_compile_error(warning));
                        }
                    }

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
                        // Deduplication based on title, explanation, and span
                        let dedup_key = format!(
                            "{}:{}:{:?}",
                            err.title,
                            err.explanation.clone().unwrap_or_default(),
                            err.span
                        );

                        if !seen_errors.insert(dedup_key) {
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
            use error::*;
            let formatter = ErrorFormatter::new(code.clone(), args.input.clone());

            eprintln!("\n{} parse error(s) found:\n", errs.len());

            for err in errs {
                // Convert Chumsky Simple error to CompileError
                let span = Span {
                    start: err.span().start,
                    end: err.span().end,
                };

                // Helper function to convert token to human-readable string
                let token_to_string = |tok: &Option<Token>| -> String {
                    match tok {
                        Some(Token::LParen) => "(".to_string(),
                        Some(Token::RParen) => ")".to_string(),
                        Some(Token::LBrace) => "{".to_string(),
                        Some(Token::RBrace) => "}".to_string(),
                        Some(Token::LBracket) => "[".to_string(),
                        Some(Token::RBracket) => "]".to_string(),
                        Some(Token::Comma) => ",".to_string(),
                        Some(Token::Colon) => ":".to_string(),
                        Some(Token::Equals) => "=".to_string(),
                        Some(Token::Val) => "variable declaration (`val`)".to_string(),
                        Some(Token::Var) => "variable declaration (`var`)".to_string(),
                        Some(Token::Fun) => "function definition (`fun`)".to_string(),
                        Some(Token::If) => "`if` statement".to_string(),
                        Some(Token::While) => "`while` loop".to_string(),
                        Some(Token::For) => "`for` loop".to_string(),
                        Some(Token::Return) => "`return` statement".to_string(),
                        Some(Token::Io) => "`io` keyword".to_string(),
                        Some(Token::Actor) => "actor definition".to_string(),
                        Some(Token::Data) => "data definition".to_string(),
                        Some(Token::Message) => "message definition".to_string(),
                        Some(Token::True) => "`true`".to_string(),
                        Some(Token::False) => "`false`".to_string(),
                        Some(Token::Ident(_)) => "identifier".to_string(),
                        Some(Token::Int(_)) => "number".to_string(),
                        Some(Token::String(_)) => "string".to_string(),
                        None => "end of file".to_string(),
                        _ => format!("{:?}", tok).to_lowercase(),
                    }
                };

                let found_str = token_to_string(&err.found().cloned());

                // Generate helpful error message based on context
                let (title, explanation, suggestion) = if err.expected().count() == 0 {
                    (
                        "Syntax error".to_string(),
                        format!("Unexpected {} here", found_str),
                        Some("Check for missing or extra punctuation".to_string()),
                    )
                } else {
                    // Try to generate a specific message based on what was expected
                    let expected: Vec<_> = err.expected().collect();

                    let message = if expected.iter().any(|t| matches!(t, Some(Token::LParen)))
                        && matches!(err.found(), Some(Token::LBrace))
                    {
                        (
                            "Missing opening parenthesis".to_string(),
                            format!("Expected `(` before `{}`", found_str),
                            Some("Add `(` before the `{` to fix this".to_string()),
                        )
                    } else if expected.iter().any(|t| matches!(t, Some(Token::RParen))) {
                        (
                            "Missing closing parenthesis".to_string(),
                            format!("Expected `)` but found {}", found_str),
                            Some("Add `)` to close the opening `(`".to_string()),
                        )
                    } else if expected.iter().any(|t| matches!(t, Some(Token::RBrace))) {
                        (
                            "Missing closing brace".to_string(),
                            format!("Expected `}}` but found {}", found_str),
                            Some("Add `}}` to close the opening `{{`".to_string()),
                        )
                    } else if expected.iter().any(|t| matches!(t, Some(Token::LBrace))) {
                        (
                            "Missing function body".to_string(),
                            format!("Expected `{{` but found {}", found_str),
                            Some("Add `{{ }}` to define the function body".to_string()),
                        )
                    } else if expected.iter().all(|t| {
                        matches!(
                            t,
                            Some(Token::True)
                                | Some(Token::False)
                                | Some(Token::Int(_))
                                | Some(Token::String(_))
                                | Some(Token::Ident(_))
                                | Some(Token::LParen)
                        )
                    }) {
                        (
                            "Missing expression".to_string(),
                            format!("Expected an expression, but found {}", found_str),
                            Some("Provide a value, variable, or expression here".to_string()),
                        )
                    } else {
                        // Fallback: list a few expected tokens
                        let expected_strs: Vec<_> = expected
                            .iter()
                            .take(3)
                            .map(|t| token_to_string(&t.clone()))
                            .collect();

                        let expected_msg = if expected_strs.len() == 1 {
                            expected_strs[0].clone()
                        } else {
                            format!("one of: {}", expected_strs.join(", "))
                        };

                        (
                            "Syntax error".to_string(),
                            format!("Expected {}, but found {}", expected_msg, found_str),
                            None,
                        )
                    };

                    message
                };

                let mut compile_error = CompileError {
                    level: ErrorLevel::Error,
                    title,
                    span: Some(span),
                    explanation: Some(explanation),
                    suggestion,
                    kind: None,
                };

                eprint!("{}", formatter.format_compile_error(&compile_error));
            }
            std::process::exit(1);
        }
    }
}
