use crate::compiler::token::Token;

/// 字句解析器
/// インデントベースの構文をトークン列に変換
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    indent_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            indent_stack: vec![0],
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut line_start = true;

        while !self.is_at_end() {
            // 行頭でインデントをチェック
            if line_start {
                let indent = self.count_indent();
                let current_indent = *self.indent_stack.last().unwrap();

                if indent > current_indent {
                    self.indent_stack.push(indent);
                    tokens.push(Token::Indent);
                } else if indent < current_indent {
                    while let Some(&stack_indent) = self.indent_stack.last() {
                        if stack_indent <= indent {
                            break;
                        }
                        self.indent_stack.pop();
                        tokens.push(Token::Dedent);
                    }
                }
                line_start = false;
            }

            self.skip_whitespace_inline();

            if self.is_at_end() {
                break;
            }

            let ch = self.current_char();

            match ch {
                '\n' => {
                    tokens.push(Token::Newline);
                    self.advance();
                    line_start = true;
                    self.line += 1;
                }
                '[' => {
                    tokens.push(Token::LBracket);
                    self.advance();
                }
                ']' => {
                    tokens.push(Token::RBracket);
                    self.advance();
                }
                ':' => {
                    tokens.push(Token::Colon);
                    self.advance();
                }
                ',' => {
                    tokens.push(Token::Comma);
                    self.advance();
                }
                '=' => {
                    tokens.push(Token::Operator("=".to_string()));
                    self.advance();
                }
                '"' => {
                    tokens.push(self.read_string()?);
                }
                '.' if self.peek() == Some('.') => {
                    tokens.push(Token::Range);
                    self.advance();
                    self.advance();
                }
                '@' => {
                    let word = self.read_identifier();
                    if let Some(token) = Token::from_keyword(&word) {
                        tokens.push(token);
                    } else {
                        return Err(format!("Unknown keyword: {}", word));
                    }
                }
                _ if ch.is_ascii_digit()
                    || (ch == '-' && self.peek().map_or(false, |c| c.is_ascii_digit())) =>
                {
                    tokens.push(self.read_number()?);
                }
                _ if ch.is_alphabetic() || ch == '_' => {
                    let word = self.read_identifier();
                    if let Some(token) = Token::from_keyword(&word) {
                        tokens.push(token);
                    } else {
                        tokens.push(Token::Identifier(word));
                    }
                }
                _ => {
                    return Err(format!(
                        "Unexpected character '{}' at line {}",
                        ch, self.line
                    ));
                }
            }
        }

        // 残りのインデントを閉じる
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(Token::Dedent);
        }

        tokens.push(Token::Eof);
        Ok(tokens)
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn peek(&self) -> Option<char> {
        if self.position + 1 < self.input.len() {
            Some(self.input[self.position + 1])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn skip_whitespace_inline(&mut self) {
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch == ' ' || ch == '\t' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn count_indent(&mut self) -> usize {
        let mut count = 0;
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch == ' ' {
                count += 1;
                self.advance();
            } else if ch == '\t' {
                count += 4; // タブは4スペース相当
                self.advance();
            } else {
                break;
            }
        }
        count
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        // @から始まる場合は@を含める
        if !self.is_at_end() && self.current_char() == '@' {
            result.push('@');
            self.advance();
        }

        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_alphanumeric() || ch == '_' {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        result
    }

    fn read_number(&mut self) -> Result<Token, String> {
        let mut result = String::new();

        if !self.is_at_end() && self.current_char() == '-' {
            result.push('-');
            self.advance();
        }

        while !self.is_at_end() && self.current_char().is_ascii_digit() {
            result.push(self.current_char());
            self.advance();
        }

        result
            .parse::<i64>()
            .map(Token::Number)
            .map_err(|_| format!("Invalid number: {}", result))
    }

    fn read_string(&mut self) -> Result<Token, String> {
    self.advance(); // 開始の '"' をスキップ
        let mut result = String::new();

        while !self.is_at_end() && self.current_char() != '"' {
            result.push(self.current_char());
            self.advance();
        }

        if self.is_at_end() {
            return Err("Unterminated string".to_string());
        }

    self.advance(); // 終了の '"' をスキップ
        Ok(Token::String(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokenize() {
        let input = "@task main\n  body\n    val n = literal 100";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Task);
        assert_eq!(tokens[1], Token::Identifier("main".to_string()));
    }
}
