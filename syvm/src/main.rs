mod vm;
mod examples;
mod compiler;
mod interpreter;

use std::fs;
use std::env;
use compiler::{lexer::Lexer, parser::Parser};
use interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // ファイル指定がある場合はそのファイルを実行
    if args.len() > 1 {
        let file_path = &args[1];
        run_syi_file(file_path);
        return;
    }
    
    // デフォルトはデモ実行
    println!("=== Synotra VM Demo ===\n");
    
    println!("\n{}\n", "=".repeat(60));
}

fn run_syi_file(file_path: &str) {
    println!("=== Running {} ===\n", file_path);
    
    // .syi ファイルを読み込み
    let source = fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read {}", file_path));
    
    // 字句解析
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize()
        .expect("Failed to tokenize");
    
    // デバッグ: トークン列を出力（簡略版）
    if std::env::var("DEBUG_TOKENS").is_ok() {
        eprintln!("DEBUG: Tokens:");
        for (i, token) in tokens.iter().enumerate() {
            eprintln!("  [{}]: {:?}", i, token);
        }
    }
    
    // 構文解析
    let mut parser = Parser::new(tokens);
    let program = parser.parse()
        .expect("Failed to parse");
    
    // デバッグ: パースされたプログラムを出力
    eprintln!("DEBUG: Parsed program:");
    for task in &program.tasks {
        eprintln!("  Task: {}", task.name);
        eprintln!("    Params: {:?}", task.params);
        eprintln!("    Body statements: {}", task.body.len());
        for (i, stmt) in task.body.iter().enumerate() {
            eprintln!("      [{}]: {:?}", i, stmt);
        }
    }
    
    // 実行
    let interpreter = Interpreter::new(program);
    interpreter.run()
        .expect("Failed to run program");
}