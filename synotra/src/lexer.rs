use crate::token::Token;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            self.advance_char();
            if ch == '\n' {
                break;
            }
        }
    }

    fn lex_identifier(&mut self) -> String {
        let mut end = self.pos;
        let s = self.input;
        while end < s.len() {
            let c = s[end..].chars().next().unwrap();
            if c.is_alphanumeric() || c == '_' {
                end += c.len_utf8();
            } else {
                break;
            }
        }
        let id = s[self.pos..end].to_string();
        self.pos = end;
        id
    }

    fn lex_string(&mut self) -> String {
    // 現在の文字は '"' と仮定して、その文字を読み飛ばします
        let _ = self.advance_char();
        let mut out = String::new();
        while let Some(ch) = self.peek_char() {
            if ch == '"' {
                self.advance_char();
                break;
            }
            // "\"" のような簡単なエスケープをサポートします
            if ch == '\\' {
                // バックスラッシュを消費します
                self.advance_char();
                if let Some(next) = self.advance_char() {
                    out.push(next);
                }
                continue;
            }
            out.push(ch);
            self.advance_char();
        }
        out
    }

    fn lex_number(&mut self) -> Result<f64, ()> {
        let mut end = self.pos;
        let s = self.input;
        let mut seen_dot = false;
        while end < s.len() {
            let c = s[end..].chars().next().unwrap();
            if c.is_ascii_digit() {
                end += c.len_utf8();
            } else if c == '.' && !seen_dot {
                // ドットの直後に別のドットがある場合、これは範囲演算子 ".." です。
                // そのため小数点の一部として消費してはいけません。
                let mut it = s[end..].chars();
                it.next();
                if let Some(nextch) = it.next() {
                    if nextch == '.' {
                        break;
                    }
                }
                seen_dot = true;
                end += c.len_utf8();
            } else {
                break;
            }
        }
        if end == self.pos {
            return Err(());
        }
        let num_str = &s[self.pos..end];
    // 解析が成功するまで `self.pos` を進めることを確定しないように試行的に解析します。
        match num_str.parse() {
            Ok(n) => {
                self.pos = end;
                Ok(n)
            }
            Err(_) => Err(()),
        }
    }

    // 先読みせず次のトークンを返します（位置を進めます）
    pub fn next(&mut self) -> Token {
        loop {
            self.consume_whitespace();
            if self.pos >= self.input.len() {
                return Token::Eof;
            }
            let mut chars = self.input[self.pos..].chars();
            let first = chars.next().unwrap();
            if first == '/' {
                if let Some(second) = chars.next() {
                    if second == '/' {
                        self.skip_line_comment();
                        continue;
                    }
                }
            }
            break;
        }
        let c = self.peek_char().unwrap();
        if c.is_alphabetic() {
            let id = self.lex_identifier();
            match id.as_str() {
                "def" => Token::Def,
                "extern" => Token::Extern,
                "task" => Token::Task,
                "distributed" => Token::Distributed,
                "cluster" => Token::Cluster,
                "var" => Token::Var,
                "fun" => Token::Fun,
                "return" => Token::Return,
                "if" => Token::If,
                "else" => Token::Else,
                "val" => Token::Val,
                "in" => Token::In,
                "do" => Token::Do,
                "let" => Token::Let,
                "for" => Token::For,
                "while" => Token::While,
                other => Token::Identifier(other.to_string()),
            }
        } else if c == '.' {
            // ここが範囲演算子 ".." であれば、数値解析より先に明示的に処理します。
            let mut it = self.input[self.pos..].chars();
            it.next();
            if let Some(nextch) = it.next() {
                if nextch == '.' {
                    // 2つのドットを消費します
                    self.advance_char();
                    self.advance_char();
                    return Token::Range;
                }
            }
            // それ以外は数値解析にフォールバックします（例: .5）
            match self.lex_number() {
                Ok(n) => Token::Number(n),
                // 数字でない場合はドットトークンとして扱う
                Err(_) => {
                    self.advance_char();
                    Token::Dot
                }
            }
        } else if c.is_ascii_digit() {
            match self.lex_number() {
                Ok(n) => Token::Number(n),
                Err(_) => {
                    self.advance_char();
                    Token::Char(c)
                }
            }
        } else {
            // 明示的に主要な単一文字トークンを列挙して処理します。
            // 見やすさのためにここで扱う文字をまとめておきます。
            match c {
                '(' => {
                    self.advance_char();
                    Token::Lparen
                }
                ')' => {
                    self.advance_char();
                    Token::Rparen
                }
                '{' => {
                    self.advance_char();
                    Token::Lbrace
                }
                '}' => {
                    self.advance_char();
                    Token::Rbrace
                }
                ',' => {
                    self.advance_char();
                    Token::Comma
                }
                ';' => {
                    self.advance_char();
                    Token::Semicolon
                }
                '"' => {
                    // 文字列リテラル
                    let s = self.lex_string();
                    Token::Str(s)
                }
                '.' => {
                    // 範囲演算子 '..'
                    // 2文字目を先読みして判定します
                    let mut it = self.input[self.pos..].chars();
                    it.next();
                    if let Some(nextch) = it.next() {
                        if nextch == '.' {
                            // 2つのドットを消費します
                            self.advance_char();
                            self.advance_char();
                            return Token::Range;
                        }
                    }
                    self.advance_char();
                    Token::Dot
                }
                '+' => {
                    self.advance_char();
                    Token::Plus
                }
                '-' => {
                    self.advance_char();
                    Token::Minus
                }
                '*' => {
                    self.advance_char();
                    Token::Star
                }
                '/' => {
                    self.advance_char();
                    Token::Slash
                }
                '%' => {
                    self.advance_char();
                    Token::Percent
                }
                '<' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '=' {
                            self.advance_char();
                            return Token::LessEqual;
                        }
                    }
                    Token::Less
                }
                '>' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '=' {
                            self.advance_char();
                            return Token::GreaterEqual;
                        }
                    }
                    Token::Greater
                }
                '=' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '=' {
                            self.advance_char();
                            return Token::EqualEqual;
                        }
                    }
                    Token::Char('=')
                }
                '!' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '=' {
                            self.advance_char();
                            return Token::NotEqual;
                        }
                    }
                    Token::Char('!')
                }
                '&' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '&' {
                            self.advance_char();
                            return Token::And;
                        }
                    }
                    Token::Char('&')
                }
                '|' => {
                    self.advance_char();
                    if let Some(nextc) = self.peek_char() {
                        if nextc == '|' {
                            self.advance_char();
                            return Token::Or;
                        }
                    }
                    Token::Char('|')
                }
                ':' => {
                    self.advance_char();
                    Token::Colon
                }
                '[' | ']' => {
                    self.advance_char();
                    Token::Char(c)
                }
                // それ以外の制御文字や未定義文字も1文字トークンとして返す
                _ => {
                    self.advance_char();
                    Token::Char(c)
                }
            }
        }
    }

    // 先読み（位置を変えずに次のトークンを取得）
    #[allow(dead_code)]
    pub fn peek(&self) -> Token {
        let mut clone = self.clone();
        clone.next()
    }
}
