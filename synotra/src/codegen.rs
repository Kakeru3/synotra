use crate::ast::Task;
use crate::ast::{Expr, Function, Prototype};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;

#[derive(Default)]
pub struct CodeGen {
    // モジュールレベルの要素をテキストスニペットとして収集します
    items: Vec<String>,
    // 登録済みプロトタイプ（名前 -> 引数リスト）
    protos: HashMap<String, Vec<String>>,
}

impl CodeGen {
    pub fn new(_module_name: &str) -> Self {
        Self {
            items: Vec::new(),
            protos: HashMap::new(),
        }
    }

    pub fn register_task_name<S: AsRef<str>>(&mut self, name: S) {
        let _ = name;
    }

    /// モジュールのテキスト表現を返します（収集したアイテムを連結したもの）
    pub fn module(&self) -> String {
        self.items.join("\n")
    }

    pub fn codegen_proto(&mut self, proto: &Prototype) -> Result<()> {
        // プロトタイプを記録します
        self.protos.insert(proto.name.clone(), proto.args.clone());
        Ok(())
    }

    pub fn codegen_function(&mut self, function: &Function) -> Result<String> {
    // 関数を IR ブロックとして出力します。.syi との互換性のために @function 形式で出します
        let mut lines: Vec<String> = Vec::new();
        lines.push(format!("@function {}", function.proto.name));
        if !function.proto.args.is_empty() {
            let args_repr = format!("[{}]", function.proto.args.join(", "));
            lines.push(format!("  args {}", args_repr));
        }
        lines.push("  body".to_string());
        self.emit_expr_into(&function.body, 2, &mut lines)?;
        let text = lines.join("\n") + "\n";
        self.items.push(text.clone());
        Ok(function.proto.name.clone())
    }

    pub fn codegen_anonymous(&mut self, func: &Function) -> Result<(String, String)> {
    // 以前の挙動に倣い、一意の名前となるよう調整します
        let mut name = func.proto.name.clone();
        if self.protos.contains_key(&name)
            || self
                .items
                .iter()
                .any(|it| it.contains(&format!("@function {}", name)))
        {
            let mut i = 0;
            loop {
                let cand = format!("{}{}", name, i);
                if !self
                    .items
                    .iter()
                    .any(|it| it.contains(&format!("@function {}", cand)))
                {
                    name = cand;
                    break;
                }
                i += 1;
            }
        }
        let proto = Prototype {
            name: name.clone(),
            args: func.proto.args.clone(),
        };
        let func2 = Function {
            proto,
            body: func.body.clone(),
        };
        let _ = self.codegen_function(&func2)?;
        // 名前とモジュールのテキストを返します
        Ok((name, self.module()))
    }

    pub fn render_task(&mut self, task: &Task) -> Result<String> {
        self.register_task_name(&task.name);

        let mut lines: Vec<String> = Vec::new();
        lines.push(format!("@task {}", task.name));
        if let Some(ref m) = task.mode {
            lines.push(format!("  mode {}", m));
        }
        if !task.params.is_empty() {
            lines.push("  params".to_string());
            for (n, t) in &task.params {
                if t.is_empty() {
                    lines.push(format!("    {}", n));
                } else {
                    lines.push(format!("    {}: {}", n, t));
                }
            }
        }
        lines.push("  body".to_string());
    // ブロック内の文は既存の emit_expr_into を使って出力します
        match &task.body {
            Expr::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.emit_expr_into(stmt, 2, &mut lines)?;
                }
            }
            other => {
                self.emit_expr_into(other, 2, &mut lines)?;
            }
        }

        let ar_text = lines.join("\n") + "\n";
        Ok(ar_text)
    }

    /// REPL から単一タスクをファイルに書き出すための便利ヘルパ
    pub fn codegen_task_to_path(&mut self, task: &Task, out_path: &Path) -> Result<String> {
        let ar_text = self.render_task(task)?;
        let mut f = File::create(out_path)?;
        f.write_all(ar_text.as_bytes())?;
        Ok(out_path.to_string_lossy().to_string())
    }

    fn emit_expr_into(&self, expr: &Expr, indent: usize, out: &mut Vec<String>) -> Result<()> {
        let pad = "  ".repeat(indent);
        match expr {
            Expr::Number(n) => {
                out.push(format!("{}literal {}", pad, n));
            }
            Expr::Variable(name) => {
                out.push(format!("{}var {}", pad, name));
            }
            Expr::Binary(op, lhs, rhs) => {
                // 演算子文字を syi 形式の文字列に変換
                let op_str = match op {
                    '*' => "*",
                    '/' => "/",
                    '%' => "%",
                    '+' => "+",
                    '-' => "-",
                    '<' => "<",
                    '>' => ">",
                    'l' => "<=",  // <= (LessEqual)
                    'g' => ">=",  // >= (GreaterEqual)
                    '=' => "==",  // == (EqualEqual)
                    'n' => "!=",  // != (NotEqual)
                    '&' => "&&",  // && (And)
                    '|' => "||",  // || (Or)
                    _ => "?",     // 未知の演算子
                };
                out.push(format!("{}binary \"{}\"", pad, op_str));
                self.emit_expr_into(lhs, indent + 1, out)?;
                self.emit_expr_into(rhs, indent + 1, out)?;
            }
            Expr::If(cond, then_e, else_e) => {
                out.push(format!("{}if", pad));
                self.emit_expr_into(cond, indent + 1, out)?;
                out.push(format!("{}then", pad));
                self.emit_expr_into(then_e, indent + 1, out)?;
                out.push(format!("{}else", pad));
                self.emit_expr_into(else_e, indent + 1, out)?;
            }
            Expr::Call { name, args } => {
                // 引数が単純な場合（変数、数値、文字列のみ）は1行で出力
                let all_simple = args.iter().all(|arg| matches!(arg, Expr::Variable(_) | Expr::Number(_) | Expr::String(_)));
                
                if all_simple {
                    out.push(self.render_call_line(&pad, name, args));
                } else {
                    // 複雑な引数がある場合は展開する
                    out.push(format!("{}call {}", pad, name));
                    if !args.is_empty() {
                        out.push(format!("{}  args", pad));
                        for arg in args {
                            self.emit_expr_into(arg, indent + 2, out)?;
                        }
                    }
                }
            }
            Expr::String(s) => {
                out.push(format!("{}literal \"{}\"", pad, s));
            }
            Expr::Let(name, val) => {
                if let Some(inline) = self.inline_expr(val) {
                    out.push(format!("{}let {} = {}", pad, name, inline));
                } else {
                    out.push(format!("{}let {} =", pad, name));
                    self.emit_expr_into(val, indent + 1, out)?;
                }
            }
            Expr::Val(name, val) => {
                if let Some(inline) = self.inline_expr(val) {
                    out.push(format!("{}val {} = {}", pad, name, inline));
                } else {
                    out.push(format!("{}val {} =", pad, name));
                    self.emit_expr_into(val, indent + 1, out)?;
                }
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
                // for ヘッダを出力します
                // start と end は式なので、単純な場合は見やすくインライン表示を試みます
                let mut hdr = format!("{}for {} in ", pad, var);
                // start と end が両方単純な式なら、見やすさのために inline で start..end として出力します
                match (&**start, &**end) {
                    (Expr::Number(n1), Expr::Variable(v2)) => {
                        hdr.push_str(&format!("{}..{}", n1, v2));
                    }
                    (Expr::Variable(v1), Expr::Variable(v2)) => {
                        hdr.push_str(&format!("{}..{}", v1, v2));
                    }
                    (Expr::Number(n1), Expr::Number(n2)) => {
                        hdr.push_str(&format!("{}..{}", n1, n2));
                    }
                    _ => {
                        hdr.push_str("range");
                    }
                }
                hdr.push_str(" do");
                out.push(hdr);
                // 本体の文を出力します
                for stmt in body.iter() {
                    self.emit_expr_into(stmt, indent + 1, out)?;
                }
            }
            Expr::While { condition, body } => {
                // while ヘッダを出力します
                let mut hdr = format!("{}while ", pad);
                // 条件式が単純ならインライン化して見やすく出力します
                if let Some(inline_cond) = self.inline_expr(condition) {
                    hdr.push_str(&inline_cond);
                } else {
                    hdr.push_str("condition");
                }
                hdr.push_str(" do");
                out.push(hdr);
                // インライン化されなかった場合は条件式を別行で出力します
                if self.inline_expr(condition).is_none() {
                    self.emit_expr_into(condition, indent + 1, out)?;
                }
                // 本体の文を出力します
                for stmt in body.iter() {
                    self.emit_expr_into(stmt, indent + 1, out)?;
                }
            }
            Expr::Block(stmts) => {
                for s in stmts.iter() {
                    self.emit_expr_into(s, indent, out)?;
                }
            }
            Expr::Return(e) => {
                if let Some(inline) = self.inline_expr(e) {
                    out.push(format!("{}return {}", pad, inline));
                } else {
                    out.push(format!("{}return", pad));
                    self.emit_expr_into(e, indent + 1, out)?;
                }
            }
            Expr::Function { proto, body } => {
                // codegen_function と同じフォーマットを再利用します
                out.push(format!("{}@function {}", pad, proto.name));
                let inner_pad = "  ".repeat(indent + 1);
                if !proto.args.is_empty() {
                    let args_repr = format!("[{}]", proto.args.join(", "));
                    out.push(format!("{}args {}", inner_pad, args_repr));
                }
                out.push(format!("{}body", inner_pad));
                self.emit_expr_into(body, indent + 2, out)?;
            }
        }
        Ok(())
    }

    fn inline_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Number(n) => Some(format!("literal {}", n)),
            Expr::String(s) => Some(format!("literal \"{}\"", s)),
            Expr::Variable(name) => Some(format!("var {}", name)),
            Expr::Call { name, args } => Some(self.render_call_line("", name, args)),
            _ => None,
        }
    }

    fn render_call_line(&self, pad: &str, name: &str, args: &[Expr]) -> String {
        let mut line = String::new();
        line.push_str(pad);
        line.push_str("call ");
        line.push_str(name);
        if !args.is_empty() {
            line.push_str(" args [");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    line.push_str(", ");
                }
                line.push_str(&self.render_call_arg(arg));
            }
            line.push(']');
        }
        line
    }

    fn render_call_arg(&self, expr: &Expr) -> String {
        match expr {
            Expr::Number(n) => n.to_string(),
            Expr::String(s) => format!("literal \"{}\"", s),
            Expr::Variable(name) => format!("var {}", name),
            _ => self
                .inline_expr(expr)
                .unwrap_or_else(|| format!("EXPR({:?})", expr)),
        }
    }

    /// 収集したモジュールテキストをファイルに書き出します
    #[allow(dead_code)]
    pub fn write_module_to(&self, path: &Path) -> Result<()> {
        let mut f = File::create(path)?;
        f.write_all(self.module().as_bytes())?;
        Ok(())
    }

    /// IR-only モードでは run_function_by_name はサポートされていません
    pub fn run_function_by_name(&mut self, _name: &str) -> Result<f64> {
        Err(anyhow!(
            "run_function_by_name は IR バックエンドではサポートされていません"
        ))
    }
}
