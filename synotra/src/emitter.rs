#![allow(dead_code)]

use anyhow::{Result, anyhow};
use std::fs::File;
use std::io::Write;
use std::path::Path;

/// 簡易 .syi エミッタ
/// 入力: task ブロックのソース全体（ヘッダと波括弧を含む）
/// 出力: (task_name, syi_text)
pub fn emit_ar_from_task_source(src: &str) -> Result<(String, String)> {
    // ヘッダを解析: task <name>(...) [mode]? {
    let header_start = src
        .find('{')
        .ok_or_else(|| anyhow!("no '{{' in task source"))?;
    let header = &src[..header_start];

    // ヘッダから名前・パラメータ・モードを取得
    let header = header.trim();
    // 例: task fizzbuzz(n: Int) distributed
    let mut parts = header.split_whitespace();
    let kw = parts.next().ok_or_else(|| anyhow!("empty header"))?;
    if kw != "task" {
        return Err(anyhow!("not a task header"));
    }
    // reparse to capture parentheses content robustly
    let name_start = header.find("task").unwrap() + 4;
    let after_task = header[name_start..].trim();
    let paren_idx = after_task
        .find('(')
        .ok_or_else(|| anyhow!("no ( after task name"))?;
    let name = after_task[..paren_idx].trim().to_string();
    let rest_after_paren = &after_task[paren_idx..];
    // extract params inside parentheses
    let params_start = rest_after_paren.find('(').unwrap();
    let params_end = rest_after_paren
        .find(')')
        .ok_or_else(|| anyhow!("no ) in task header"))?;
    let params_raw = &rest_after_paren[params_start + 1..params_end].trim();
    let mut params: Vec<(String, String)> = Vec::new();
    if !params_raw.is_empty() {
        for p in params_raw.split(',') {
            let p = p.trim();
            if p.is_empty() {
                continue;
            }
            if let Some(colon) = p.find(':') {
                let n = p[..colon].trim().to_string();
                let t = p[colon + 1..].trim().to_string();
                params.push((n, t));
            } else {
                params.push((p.to_string(), String::new()));
            }
        }
    }
    // mode is word after ')' if any
    let mut mode = String::new();
    if rest_after_paren.len() > params_end + 1 {
        let rest = rest_after_paren[params_end + 1..].trim();
        if !rest.is_empty() {
            if let Some(m) = rest.split_whitespace().next() {
                mode = m.to_string();
            }
        }
    }

    // ボディ: 中括弧内を取得（単純実装: 最初の '{' と最後の '}' の間）
    let first_brace = src.find('{').unwrap();
    let last_brace = src.rfind('}').ok_or_else(|| anyhow!("no closing }}"))?;
    let body = src[first_brace + 1..last_brace].trim();

    // Build AR text by parsing top-level body items: nested fun, for, and simple calls
    let mut lines: Vec<String> = Vec::new();
    lines.push(format!("@task \"{}\"", name));
    if !mode.is_empty() {
        lines.push(format!("  mode {}", mode));
    }
    // params
    if !params.is_empty() {
        lines.push("  params".to_string());
        for (n, t) in &params {
            if t.is_empty() {
                lines.push(format!("    {}", n));
            } else {
                lines.push(format!("    {}: {}", n, t));
            }
        }
    }
    lines.push("  body".to_string());

    // parse top-level statements within body by scanning and matching braces
    let mut i = 0usize;
    let s = body;
    let s_len = s.len();
    while i < s_len {
        // skip whitespace and newlines
        while i < s_len && s.as_bytes()[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= s_len {
            break;
        }
        // detect nested function: starts with 'fun'
        if s[i..].starts_with("fun ") {
            // read header until '{'
            if let Some(brace_pos_rel) = s[i..].find('{') {
                let header = s[i..i + brace_pos_rel].trim();
                // parse fun name and params
                // header example: fun fizzbuzzValue(i: Int): String
                let after_fun = header[3..].trim();
                if let Some(p_idx) = after_fun.find('(') {
                    let fun_name = after_fun[..p_idx].trim().to_string();
                    // params
                    if let Some(close_idx_rel) = after_fun.find(')') {
                        let params_raw = &after_fun[p_idx + 1..close_idx_rel];
                        let mut fparams: Vec<(String, String)> = Vec::new();
                        for p in params_raw.split(',') {
                            let p = p.trim();
                            if p.is_empty() {
                                continue;
                            }
                            if let Some(colon) = p.find(':') {
                                let n = p[..colon].trim().to_string();
                                let t = p[colon + 1..].trim().to_string();
                                fparams.push((n, t));
                            } else {
                                fparams.push((p.to_string(), String::new()));
                            }
                        }
                        // find function body via brace matching
                        let mut j = i + brace_pos_rel;
                        // find first '{'
                        while j < s_len && s.as_bytes()[j] != b'{' {
                            j += 1;
                        }
                        if j >= s_len {
                            break;
                        }
                        // j points at '{'
                        let mut brace_count = 1;
                        j += 1;
                        let body_start = j;
                        while j < s_len && brace_count > 0 {
                            let ch = s.as_bytes()[j] as char;
                            if ch == '{' {
                                brace_count += 1;
                            } else if ch == '}' {
                                brace_count -= 1;
                            }
                            j += 1;
                        }
                        let body_end = j - 1; // position of matching '}'
                        let fun_body = &s[body_start..body_end];
                        // emit function block
                        lines.push(format!("    @function \"{}\"", fun_name));
                        lines.push("      params".to_string());
                        for (n, t) in &fparams {
                            if t.is_empty() {
                                lines.push(format!("        {}", n));
                            } else {
                                lines.push(format!("        {}: {}", n, t));
                            }
                        }
                        lines.push("      body".to_string());
                        // transform function body (simple conditional/return formatting)
                        let mut fb = transform_function_body(fun_body);
                        for l in fb.drain(..) {
                            lines.push(format!("        {}", l));
                        }

                        i = j; // continue after function
                        continue;
                    }
                }
            }
        }

        // detect for loop: starts with 'for '
        if s[i..].starts_with("for ") {
            // parse until 'do' and then block
            if let Some(do_pos_rel) = s[i..].find("do") {
                let header = s[i..i + do_pos_rel].trim();
                // header example: for i in 1..n
                // parse var and range
                let rest = header[3..].trim();
                // expect form: <var> in <range>
                let mut parts = rest.splitn(3, ' ');
                let var = parts.next().unwrap_or("").to_string();
                let _in = parts.next();
                let range = parts.next().unwrap_or("").trim().to_string();

                // find block starting at first '{' after do
                if let Some(brace_pos) = s[i..].find('{') {
                    let mut j = i + brace_pos;
                    // find first '{'
                    while j < s_len && s.as_bytes()[j] != b'{' {
                        j += 1;
                    }
                    if j >= s_len {
                        break;
                    }
                    let mut brace_count = 1;
                    j += 1;
                    let body_start = j;
                    while j < s_len && brace_count > 0 {
                        let ch = s.as_bytes()[j] as char;
                        if ch == '{' {
                            brace_count += 1;
                        } else if ch == '}' {
                            brace_count -= 1;
                        }
                        j += 1;
                    }
                    let body_end = j - 1;
                    let for_body = &s[body_start..body_end];
                    lines.push(format!("    for {} in {} do", var, range));
                    // split statements inside for_body by semicolon or newlines
                    let stmts = split_statements(for_body);
                    for st in stmts {
                        let mut tlines = transform_statement(&st);
                        for tl in tlines.drain(..) {
                            lines.push(format!("      {}", tl));
                        }
                    }
                    i = j;
                    continue;
                }
            }
        }

        // fallback: try to parse a top-level call or other statement until semicolon or newline
        let rest = &s[i..];
        if let Some(endpos) = rest.find('\n') {
            let stmt = rest[..endpos].trim();
            if !stmt.is_empty() {
                let tlines = transform_statement(stmt);
                for tl in tlines {
                    lines.push(format!("    {}", tl));
                }
            }
            i += endpos + 1;
        } else {
            let stmt = rest.trim();
            if !stmt.is_empty() {
                let tlines = transform_statement(stmt);
                for tl in tlines {
                    lines.push(format!("    {}", tl));
                }
            }
            break;
        }
    }

    let ar_text = lines.join("\n") + "\n";
    Ok((name, ar_text))
}

// Split statements inside a block by semicolons or newlines, trimming braces
fn split_statements(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut depth = 0i32;
    for ch in s.chars() {
        if ch == '{' {
            depth += 1;
            cur.push(ch);
            continue;
        }
        if ch == '}' {
            depth -= 1;
            cur.push(ch);
            continue;
        }
        if (ch == ';' || ch == '\n') && depth == 0 {
            let t = cur.trim();
            if !t.is_empty() {
                out.push(t.to_string());
            }
            cur.clear();
        } else {
            cur.push(ch);
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur.trim().to_string());
    }
    out
}

// Transform simple statements: let bindings and direct calls
fn transform_statement(stmt: &str) -> Vec<String> {
    let s = stmt.trim();
    let mut res = Vec::new();
    if s.starts_with("let ") {
        // let name = expr
        if let Some(eq) = s.find('=') {
            let name = s[4..eq].trim();
            let expr = s[eq + 1..].trim().trim_end_matches(';').trim();
            // if expr is a call like fname(arg)
            if let Some(par) = expr.find('(') {
                let fname = expr[..par].trim();
                if let Some(close) = expr.rfind(')') {
                    let args_raw = &expr[par + 1..close];
                    let args: Vec<String> = args_raw
                        .split(',')
                        .map(|a| a.trim().to_string())
                        .filter(|a| !a.is_empty())
                        .collect();
                    let mut args_repr = String::from("[");
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 {
                            args_repr.push_str(", ");
                        }
                        // if identifier-like (variable), don't quote; otherwise quote
                        if is_identifier(a) {
                            args_repr.push_str(a);
                        } else {
                            args_repr.push('"');
                            args_repr.push_str(a);
                            args_repr.push('"');
                        }
                    }
                    args_repr.push(']');
                    res.push(format!(
                        "let {} = call \"{}\" args {}",
                        name, fname, args_repr
                    ));
                    return res;
                }
            }
            // fallback: keep assignment as-is
            res.push(format!("let {} = {}", name, expr));
            return res;
        }
    }
    // direct call like foo(x)
    if let Some(par) = s.find('(') {
        let fname = s[..par].trim();
        if let Some(close) = s.rfind(')') {
            let args_raw = &s[par + 1..close];
            let args: Vec<String> = args_raw
                .split(',')
                .map(|a| a.trim().to_string())
                .filter(|a| !a.is_empty())
                .collect();
            let mut args_repr = String::from("[");
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    args_repr.push_str(", ");
                }
                if is_identifier(a) {
                    args_repr.push_str(a);
                } else {
                    args_repr.push('\"');
                    args_repr.push_str(a);
                    args_repr.push('\"');
                }
            }
            args_repr.push(']');
            res.push(format!("call \"{}\" args {}", fname, args_repr));
            return res;
        }
    }
    // other: return ... or bare expression
    res.push(s.trim_end_matches(';').to_string());
    res
}

// Transform a function body focusing on if/else if/else with returns
fn transform_function_body(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut i = 0usize;
    let s_len = s.len();
    let src = s;
    while i < s_len {
        // skip whitespace
        while i < s_len && src.as_bytes()[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= s_len {
            break;
        }
        if src[i..].starts_with("if ") {
            // parse condition up to '{'
            if let Some(brace_rel) = src[i..].find('{') {
                let cond = src[i + 3..i + brace_rel].trim();
                // find block
                let mut j = i + brace_rel;
                while j < s_len && src.as_bytes()[j] != b'{' {
                    j += 1;
                }
                j += 1;
                let mut brace_count = 1;
                let body_start = j;
                while j < s_len && brace_count > 0 {
                    let ch = src.as_bytes()[j] as char;
                    if ch == '{' {
                        brace_count += 1;
                    } else if ch == '}' {
                        brace_count -= 1;
                    }
                    j += 1;
                }
                let body_end = j - 1;
                let inside = &src[body_start..body_end];
                // look for following else if / else chains
                let mut chain: Vec<(Option<String>, String)> = Vec::new();
                chain.push((Some(cond.to_string()), inside.trim().to_string()));
                // advance i to j and then check for else/else if
                i = j;
                loop {
                    while i < s_len && src.as_bytes()[i].is_ascii_whitespace() {
                        i += 1;
                    }
                    if src[i..].starts_with("else if") {
                        // parse else if cond
                        if let Some(brace2_rel) = src[i..].find('{') {
                            let cond2 = src[i + 7..i + brace2_rel].trim();
                            let mut k = i + brace2_rel;
                            while k < s_len && src.as_bytes()[k] != b'{' {
                                k += 1;
                            }
                            k += 1;
                            let mut bc = 1;
                            let body2_start = k;
                            while k < s_len && bc > 0 {
                                let ch = src.as_bytes()[k] as char;
                                if ch == '{' {
                                    bc += 1;
                                } else if ch == '}' {
                                    bc -= 1;
                                }
                                k += 1;
                            }
                            let body2_end = k - 1;
                            let inside2 = &src[body2_start..body2_end];
                            chain.push((Some(cond2.to_string()), inside2.trim().to_string()));
                            i = k;
                            continue;
                        } else {
                            break;
                        }
                    } else if src[i..].starts_with("else") {
                        // parse else block
                        if let Some(brace3_rel) = src[i..].find('{') {
                            let mut k = i + brace3_rel;
                            while k < s_len && src.as_bytes()[k] != b'{' {
                                k += 1;
                            }
                            k += 1;
                            let mut bc = 1;
                            let body3_start = k;
                            while k < s_len && bc > 0 {
                                let ch = src.as_bytes()[k] as char;
                                if ch == '{' {
                                    bc += 1;
                                } else if ch == '}' {
                                    bc -= 1;
                                }
                                k += 1;
                            }
                            let body3_end = k - 1;
                            let inside3 = &src[body3_start..body3_end];
                            chain.push((None, inside3.trim().to_string()));
                            i = k;
                            break;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                // emit chain with nested returns (indented)
                for (idx, (cond_opt, body_txt)) in chain.iter().enumerate() {
                    if idx == 0 {
                        if let Some(cond) = cond_opt {
                            out.push(format!("if {} then", cond));
                        }
                        let stmts = split_statements(body_txt);
                        for st in stmts {
                            if st.trim().starts_with("return ") {
                                out.push(format!("  {}", transform_return_stmt(st.trim())));
                            }
                        }
                    } else if cond_opt.is_some() {
                        out.push(format!("else if {} then", cond_opt.as_ref().unwrap()));
                        let stmts = split_statements(body_txt);
                        for st in stmts {
                            if st.trim().starts_with("return ") {
                                out.push(format!("  {}", transform_return_stmt(st.trim())));
                            }
                        }
                    } else {
                        out.push("else".to_string());
                        let stmts = split_statements(body_txt);
                        for st in stmts {
                            if st.trim().starts_with("return ") {
                                out.push(format!("  {}", transform_return_stmt(st.trim())));
                            }
                        }
                    }
                }
                continue;
            }
        }
        // fallback: consume line
        if let Some(nl) = src[i..].find('\n') {
            let line = src[i..i + nl].trim();
            if !line.is_empty() {
                out.push(line.to_string());
            }
            i += nl + 1;
        } else {
            let line = src[i..].trim();
            if !line.is_empty() {
                out.push(line.to_string());
            }
            break;
        }
    }
    out
}

fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    if let Some(first) = chars.next() {
        if !(first.is_alphabetic() || first == '_') {
            return false;
        }
    } else {
        return false;
    }
    for ch in chars {
        if !(ch.is_alphanumeric() || ch == '_') {
            return false;
        }
    }
    true
}

fn transform_return_stmt(s: &str) -> String {
    // s is like "return ..." (trimmed)
    let rest = s.strip_prefix("return ").unwrap_or("").trim();
    // detect pattern: <var>.toString()
    if let Some(dot_idx) = rest.find(".toString()") {
        let var = rest[..dot_idx].trim();
        if is_identifier(var) {
            return format!("return call \"toString\" args [{}]", var);
        }
    }
    // if rest is a quoted string, keep as-is
    if rest.starts_with('"') && rest.ends_with('"') {
        return format!("return {}", rest);
    }
    // otherwise, return as-is (variable or expression)
    format!("return {}", rest)
}

/// 便利: task ソースを受け取って指定ディレクトリに <taskname>.syi を書き出す
pub fn emit_and_write_ar(src: &str, out_dir: &Path) -> Result<String> {
    let (name, ar) = emit_ar_from_task_source(src)?;
    let filename = format!("{}.syi", name);
    let out_path = out_dir.join(&filename);
    let mut f = File::create(&out_path)?;
    f.write_all(ar.as_bytes())?;
    Ok(out_path.to_string_lossy().to_string())
}
