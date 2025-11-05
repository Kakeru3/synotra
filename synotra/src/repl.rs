// REPLおよびファイル実行モードの実装
// cargo run で REPL を起動
// cargo run -- <file.sy> でファイル実行

use crate::codegen::CodeGen;
use crate::parser::Parser;
// emitter モジュールは後方互換のため残していますが、ランタイムは現在 Parser -> CodeGen の流れを推奨します
use crate::ast::TopLevel;
use anyhow::Result;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use std::fs;
use std::path::Path;

// REPLモード
pub fn run_repl() -> Result<()> {
    // IR ベースの CodeGen を生成します。これは .syi ライクなテキスト IR を出力します。
    let mut codegen = CodeGen::new("synotra");

    let mut rl: Editor<(), DefaultHistory> = Editor::new()?;
    println!("synotra REPL (type 'quit' or Ctrl-D)");
    loop {
        let readline = rl.readline("k> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                if line == "quit" || line == "exit" {
                    break;
                }
                // 入力行を処理する（ファイル処理と共通）
                // ユーザが `task` ブロックを書き始めた場合、対応する `}` が来るまで行を収集します。
                if line.starts_with("task") {
                    // ブロック全体を組み立てます（最初の行はそのまま含めます）
                    let mut block = String::new();
                    block.push_str(line);
                    // 波カッコの数を数えてブロックの終端を検出します
                    let mut brace_count: i32 = 0;
                    for ch in line.chars() {
                        if ch == '{' {
                            brace_count += 1
                        } else if ch == '}' {
                            brace_count -= 1
                        }
                    }

                    // もし開き波カッコが最初の行になければ、見つかるまで読み続けます
                    while brace_count > 0 {
                        match rl.readline(".. ") {
                            Ok(next) => {
                                let next_trim = next.trim_end();
                                block.push('\n');
                                block.push_str(next_trim);
                                for ch in next_trim.chars() {
                                    if ch == '{' {
                                        brace_count += 1
                                    } else if ch == '}' {
                                        brace_count -= 1
                                    }
                                }
                            }
                            Err(_) => break,
                        }
                    }
                    // 収集したタスクをパースして CodeGen を使って .syi を出力します
                    let mut parser = Parser::new(&block);
                    match parser.parse_source() {
                        Ok(items) => {
                            for item in &items {
                                if let TopLevel::Task(t) = item {
                                    codegen.register_task_name(&t.name);
                                }
                            }
                            for item in items {
                                if let TopLevel::Task(t) = item {
                                    let out_dir = std::path::Path::new(".");
                                    let path = out_dir.join(format!("{}.syi", t.name));
                                    match codegen.codegen_task_to_path(&t, &path) {
                                        Ok(p) => eprintln!("wrote .syi file: {}", p),
                                        Err(e) => println!("codegen error: {}", e),
                                    }
                                }
                            }
                        }
                        Err(e) => println!("parse error: {}", e),
                    }
                } else {
                    process_line(line, &mut codegen);
                }
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C (中断)
                continue;
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}

// 共通の行処理ロジック
fn process_line(line: &str, codegen: &mut CodeGen) {
    // コマンド判定 (def / extern / top-level)
    if line.starts_with("def") {
        let body = &line[3..];
        let mut p = Parser::new(body);
        match p.parse_definition() {
            Ok(fun) => match codegen.codegen_function(&fun) {
                Ok(_fv) => {
                    println!("Parsed function: {}", fun.proto.name);
                }
                Err(e) => println!("codegen error: {}", e),
            },
            Err(e) => println!("parse error: {}", e),
        }
    } else if line.starts_with("extern") {
        let body = &line[6..];
        let mut p = Parser::new(body);
        match p.parse_extern() {
            Ok(proto) => match codegen.codegen_proto(&proto) {
                Ok(_) => println!("Parsed extern: {}", proto.name),
                Err(e) => println!("codegen error: {}", e),
            },
            Err(e) => println!("parse error: {}", e),
        }
    } else {
        let mut p = Parser::new(line);
    // まず、入力が 'task' ブロックかを確認し、そうであればパースして CodeGen を使います
        if line.starts_with("task") {
            let mut parser = Parser::new(line);
            match parser.parse_source() {
                Ok(items) => {
                    for item in &items {
                        if let TopLevel::Task(t) = item {
                            codegen.register_task_name(&t.name);
                        }
                    }
                    for item in items {
                        if let TopLevel::Task(t) = item {
                            let out_dir = std::path::Path::new(".");
                            let path = out_dir.join(format!("{}.syi", t.name));
                            match codegen.codegen_task_to_path(&t, &path) {
                                Ok(p) => eprintln!("wrote .syi file: {}", p),
                                Err(e) => eprintln!("codegen error: {}", e),
                            }
                        }
                    }
                }
                Err(e) => println!("parse error: {}", e),
            }
            return;
        }

        match p.parse_top_level_expr() {
            Ok(fun) => {
                match codegen.codegen_anonymous(&fun) {
                    Ok((_fv, name)) => {
                        // ファイル実行時も IR を表示する
                        println!("Compiled anonymous expression. IR:\n{}", codegen.module());
                        // JIT は IR モードではサポートされない
                        match codegen.run_function_by_name(&name) {
                            Ok(v) => println!("=> {}", v),
                            Err(_e) => println!("(execution not supported in IR backend)"),
                        }
                    }
                    Err(e) => println!("codegen error: {}", e),
                }
            }
            Err(e) => println!("parse error: {}", e),
        }
    }
}

// ファイル実行モード
pub fn run_file(path: &Path) -> Result<()> {
    let content = fs::read_to_string(path)?;
    let mut codegen = CodeGen::new("synotra");
    // 新しいフロー: ファイル全体をパースしてトップレベル AST を得る
    let mut parser = Parser::new(&content);
    let items = parser.parse_source()?;
    for item in &items {
        if let TopLevel::Task(t) = item {
            codegen.register_task_name(&t.name);
        }
    }
    let mut rendered_tasks: Vec<String> = Vec::new();
    for item in items {
        match item {
            TopLevel::Extern(proto) => {
                codegen.codegen_proto(&proto)?;
                println!("Parsed extern: {}", proto.name);
            }
            TopLevel::Function(f) => {
                if f.proto.name == "__anon_expr" {
                    let (_name, _module) = codegen.codegen_anonymous(&f)?;
                    println!("Compiled anonymous expression. IR:\n{}", codegen.module());
                } else {
                    codegen.codegen_function(&f)?;
                    println!("Parsed function: {}", f.proto.name);
                }
            }
            TopLevel::Task(t) => match codegen.render_task(&t) {
                Ok(text) => rendered_tasks.push(text),
                Err(e) => println!("codegen error: {}", e),
            },
        }
    }
    if !rendered_tasks.is_empty() {
        let out_dir = path.parent().unwrap_or(Path::new("."));
        let file_name = path
            .file_stem()
            .map(|s| format!("{}.syi", s.to_string_lossy()))
            .unwrap_or_else(|| "output.syi".to_string());
        let out_path = out_dir.join(file_name);
        let joined = rendered_tasks.join("\n");
        fs::write(&out_path, joined)?;
        eprintln!("wrote .syi file: {}", out_path.display());
    }
    Ok(())
}
