mod compiler;
mod examples;
mod interpreter;
mod vm;

use compiler::{lexer::Lexer, parser::Parser};
use interpreter::Interpreter;
use std::env;
use std::fs;
use std::time::Instant;
use rayon::ThreadPoolBuilder;

fn main() {
    let args: Vec<String> = env::args().collect();
    // --comp <file> [runs]: 比較実行モード（シングルスレッド vs 並列）
    if args.len() > 1 && args[1] == "--comp" {
        if args.len() < 3 {
            eprintln!("Usage: syvm --comp <file.syi> [runs]");
            return;
        }
        let file_path = &args[2];
        let runs: usize = if args.len() > 3 {
            args[3].parse().unwrap_or(3)
        } else {
            3
        };

    // ソースを読み込み、1回だけ解析して AST を作成します
        let source = fs::read_to_string(file_path).unwrap_or_else(|_| panic!("Failed to read {}", file_path));
        let mut lexer = compiler::lexer::Lexer::new(&source);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        let mut parser = compiler::parser::Parser::new(tokens);
        let program = parser.parse().expect("Failed to parse");

        let interpreter = interpreter::Interpreter::new(program.clone());

        println!("Comparing single-threaded (1) vs parallel (default) for {} runs on {}", runs, file_path);

    // シングルスレッド実行（rayon のプールを 1 スレッドにして実行）
        let mut single_times = Vec::new();
        for i in 0..runs {
            let pool = ThreadPoolBuilder::new().num_threads(1).build().expect("Failed to build rayon pool");
            let start = Instant::now();
            pool.install(|| {
                interpreter.run().expect("Program failed during single-threaded run");
            });
            let dur = start.elapsed();
            println!("single run {}: {:?}", i + 1, dur);
            single_times.push(dur.as_secs_f64());
        }

    // 並列実行（デフォルトの rayon プール）
        let mut parallel_times = Vec::new();
        for i in 0..runs {
            let pool = ThreadPoolBuilder::new().build().expect("Failed to build rayon pool");
            let start = Instant::now();
            pool.install(|| {
                interpreter.run().expect("Program failed during parallel run");
            });
            let dur = start.elapsed();
            println!("parallel run {}: {:?}", i + 1, dur);
            parallel_times.push(dur.as_secs_f64());
        }

        let avg = |v: &Vec<f64>| v.iter().sum::<f64>() / (v.len() as f64);
        let single_avg = avg(&single_times);
        let parallel_avg = avg(&parallel_times);
        println!("\naverage single-threaded: {:.6}s", single_avg);
        println!("average parallel: {:.6}s", parallel_avg);
        if parallel_avg > 0.0 {
            println!("speedup (single / parallel): {:.3}x", single_avg / parallel_avg);
        }
        return;
    }

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
    let source =
        fs::read_to_string(file_path).unwrap_or_else(|_| panic!("Failed to read {}", file_path));

    // 字句解析
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().expect("Failed to tokenize");

    // デバッグ: トークン列を出力（簡略版）
    if std::env::var("DEBUG_TOKENS").is_ok() {
        eprintln!("DEBUG: Tokens:");
        for (i, token) in tokens.iter().enumerate() {
            eprintln!("  [{}]: {:?}", i, token);
        }
    }

    // 構文解析
    let mut parser = Parser::new(tokens);
    let program = parser.parse().expect("Failed to parse");

    // デバッグ: パースされたプログラムを出力（必要なときは環境変数 DEBUG_PARSED を設定）
    if std::env::var("DEBUG_PARSED").is_ok() {
        eprintln!("DEBUG: Parsed program:");
        for task in &program.tasks {
            eprintln!("  Task: {}", task.name);
            eprintln!("    Params: {:?}", task.params);
            eprintln!("    Body statements: {}", task.body.len());
            for (i, stmt) in task.body.iter().enumerate() {
                eprintln!("      [{}]: {:?}", i, stmt);
            }
        }
    }

    // 実行
    let interpreter = Interpreter::new(program);
    interpreter.run().expect("Failed to run program");
}
