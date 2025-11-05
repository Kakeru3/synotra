mod ast;
mod codegen;
mod emitter;
mod lexer;
mod parser;
mod repl;
mod token;

use crate::parser::Parser;
use std::env;
use std::path::Path;

fn main() {
    // 起動時の引数に応じてREPLモードかファイル実行モードかを切り替え
    // 最初の引数をファイルパスとして扱う
    // それ以外はREPLモード
    // cargo run で動かす場合、REPLモード、cargo run -- x.synotra でファイル実行モード
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        let p = Path::new(&path);
    // デバッグ用の特別オプション: `--lex <file>` を指定すると字句解析結果を表示します
        if path == "--lex" {
            if let Some(fpath) = args.next() {
                let content = std::fs::read_to_string(&fpath).unwrap_or_default();
                let toks = Parser::lex_all(&content);
                println!("Tokens:\n{:?}", toks);
                return;
            }
        }
        if p.exists() {
            if let Err(e) = repl::run_file(p) {
                eprintln!("Error running file {}: {}", path, e);
            }
            return;
        } else {
            eprintln!("File not found: {}", path);
        }
    }

    if let Err(e) = repl::run_repl() {
        eprintln!("Error running REPL: {}", e);
    }
}
