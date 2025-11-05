use crate::ast::{Expr, Function, Prototype, Task, TopLevel};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{Result, anyhow};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let tokens = Self::lex_all(input);
        Self { tokens, pos: 0 }
    }

    pub fn lex_all(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next();
            let is_eof = matches!(tok, Token::Eof);
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        tokens
    }

    pub fn parse_source(&mut self) -> Result<Vec<TopLevel>> {
        let mut items = Vec::new();
        while !self.is_eof() {
            self.consume_semicolons();
            if self.is_eof() {
                break;
            }
            let item = self.parse_toplevel()?;
            items.push(item);
            self.consume_semicolons();
        }
        Ok(items)
    }

    pub fn parse_top_level_expr(&mut self) -> Result<Function> {
        let body = self.parse_expression()?;
        Ok(Function {
            proto: Prototype {
                name: "__anon_expr".to_string(),
                args: Vec::new(),
            },
            body,
        })
    }

    pub fn parse_definition(&mut self) -> Result<Function> {
        if matches!(self.current(), Token::Def) {
            self.advance();
        }
        let proto = self.parse_prototype()?;
        let body = self.parse_expression()?;
        Ok(Function { proto, body })
    }

    pub fn parse_extern(&mut self) -> Result<Prototype> {
        if matches!(self.current(), Token::Extern) {
            self.advance();
        }
        self.parse_prototype()
    }

    pub fn parse_prototype(&mut self) -> Result<Prototype> {
        let name = self.expect_identifier()?;
        self.expect_lparen()?;
        let mut args = Vec::new();
        if !matches!(self.current(), Token::Rparen) {
            loop {
                let arg_name = self.expect_identifier()?;
                    if self.consume_char(':') {
                        // 型注釈はとりあえず無視します（現状は単純な識別子のみサポート）
                        let _ = self.expect_identifier()?; // 型は単純識別子として扱います
                    }
                args.push(arg_name);
                if !self.consume_token(&Token::Comma) {
                    break;
                }
            }
        }
        self.expect_rparen()?;
        Ok(Prototype { name, args })
    }

    fn parse_toplevel(&mut self) -> Result<TopLevel> {
        match self.current() {
            Token::Task => {
                let task = self.parse_task_decl()?;
                Ok(TopLevel::Task(task))
            }
            Token::Extern => {
                let proto = self.parse_extern()?;
                Ok(TopLevel::Extern(proto))
            }
            Token::Def => {
                let fun = self.parse_definition()?;
                Ok(TopLevel::Function(fun))
            }
            Token::Eof => Err(anyhow!("unexpected EOF")),
            _ => {
                let fun = self.parse_top_level_expr()?;
                Ok(TopLevel::Function(fun))
            }
        }
    }

    fn parse_task_decl(&mut self) -> Result<Task> {
        if matches!(self.current(), Token::Task) {
            self.advance();
        }
        let name = self.expect_identifier()?;
        self.expect_lparen()?;
        let mut params = Vec::new();
        if !matches!(self.current(), Token::Rparen) {
            loop {
                let param_name = self.expect_identifier()?;
                let param_type = if self.consume_token(&Token::Colon) {
                    self.expect_identifier()? // 型注釈は単純な識別子として扱います
                } else {
                    String::new()
                };
                params.push((param_name, param_type));
                if !self.consume_token(&Token::Comma) {
                    break;
                }
            }
        }
        self.expect_rparen()?;
        let mode = match self.current() {
            Token::Distributed => {
                self.advance();
                Some("distributed".to_string())
            }
            Token::Cluster => {
                self.advance();
                Some("cluster".to_string())
            }
            Token::Identifier(name) if name == "distributed" || name == "cluster" => {
                let name = name.clone();
                self.advance();
                Some(name)
            }
            _ => None,
        };
        let body = self.parse_block()?;
        Ok(Task {
            name,
            params,
            mode,
            body,
        })
    }

    fn parse_block(&mut self) -> Result<Expr> {
        self.expect_lbrace()?;
        let mut items = Vec::new();
        while !matches!(self.current(), Token::Rbrace | Token::Eof) {
            self.consume_semicolons();
            if matches!(self.current(), Token::Rbrace | Token::Eof) {
                break;
            }
            let stmt = self.parse_statement()?;
            items.push(stmt);
            self.consume_semicolons();
        }
        self.expect_rbrace()?;
        Ok(Expr::Block(items))
    }

    fn parse_statement(&mut self) -> Result<Expr> {
        match self.current() {
            Token::Let => self.parse_let(),
            Token::Val => self.parse_val(),
            Token::Var => self.parse_var(),
            Token::For => self.parse_for(),
            Token::While => self.parse_while(),
            Token::Fun => self.parse_fun_expr(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_expression(),
            _ => self.parse_expression(),
        }
    }

    fn parse_let(&mut self) -> Result<Expr> {
        self.expect_token(&Token::Let)?;
        let name = self.expect_identifier()?;
        self.expect_char('=')?;
        let expr = self.parse_expression()?;
        Ok(Expr::Let(name, Box::new(expr)))
    }

    fn parse_val(&mut self) -> Result<Expr> {
        self.expect_token(&Token::Val)?;
        let name = self.expect_identifier()?;
        self.expect_char('=')?;
        let expr = self.parse_expression()?;
        Ok(Expr::Val(name, Box::new(expr)))
    }

    fn parse_var(&mut self) -> Result<Expr> {
        self.expect_token(&Token::Var)?;
        let name = self.expect_identifier()?;
        self.expect_char('=')?;
        let expr = self.parse_expression()?;
        // varはletと同じように扱う（再代入可能）
        Ok(Expr::Let(name, Box::new(expr)))
    }

    fn parse_return(&mut self) -> Result<Expr> {
        self.expect_token(&Token::Return)?;
        let expr = self.parse_expression()?;
        Ok(Expr::Return(Box::new(expr)))
    }

    fn parse_fun_expr(&mut self) -> Result<Expr> {
        self.expect_token(&Token::Fun)?;
        let name = self.expect_identifier()?;
        self.expect_lparen()?;
        let mut args = Vec::new();
        if !matches!(self.current(), Token::Rparen) {
            loop {
                let arg_name = self.expect_identifier()?;
                if self.consume_token(&Token::Colon) {
                    let _ = self.expect_identifier()?;
                }
                args.push(arg_name);
                if !self.consume_token(&Token::Comma) {
                    break;
                }
            }
        }
        self.expect_rparen()?;
        if self.consume_token(&Token::Colon) {
            if matches!(self.current(), Token::Identifier(_)) {
                self.advance();
            }
        }
        let body = self.parse_block()?;
        Ok(Expr::Function {
            proto: Prototype { name, args },
            body: Box::new(body),
        })
    }

    fn parse_for(&mut self) -> Result<Expr> {
        self.expect_token(&Token::For)?;
        let var = self.expect_identifier()?;
        self.expect_token(&Token::In)?;
        let start = self.parse_expression()?;
        self.expect_token(&Token::Range)?;
        let end = self.parse_expression()?;
        self.expect_token(&Token::Do)?;
        let body_expr = self.parse_block()?;
        let stmts = match body_expr {
            Expr::Block(stmts) => stmts,
            other => vec![other],
        };
        Ok(Expr::For {
            var,
            start: Box::new(start),
            end: Box::new(end),
            body: stmts,
        })
    }

    fn parse_while(&mut self) -> Result<Expr> {
        self.expect_token(&Token::While)?;
        let condition = self.parse_expression()?;
        self.expect_token(&Token::Do)?;
        let body_expr = self.parse_block()?;
        let stmts = match body_expr {
            Expr::Block(stmts) => stmts,
            other => vec![other],
        };
        Ok(Expr::While {
            condition: Box::new(condition),
            body: stmts,
        })
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        self.expect_token(&Token::If)?;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.consume_token(&Token::Else) {
            if matches!(self.current(), Token::If) {
                self.parse_if_expression()?
            } else {
                self.parse_block()?
            }
        } else {
            Expr::Block(Vec::new())
        };
        Ok(Expr::If(
            Box::new(condition),
            Box::new(then_branch),
            Box::new(else_branch),
        ))
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        if matches!(self.current(), Token::If) {
            return self.parse_if_expression();
        }
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, min_prec: u8) -> Result<Expr> {
        let mut lhs = self.parse_unary()?;
        loop {
            let (prec, op_char) = match self.binary_operator_info() {
                Some(info) => info,
                None => break,
            };
            if prec < min_prec {
                break;
            }
            self.advance();
            let rhs = self.parse_binary_expression(prec + 1)?;
            lhs = Expr::Binary(op_char, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn binary_operator_info(&self) -> Option<(u8, char)> {
        match self.current() {
            Token::Star => Some((60, '*')),
            Token::Slash => Some((60, '/')),
            Token::Percent => Some((60, '%')),
            Token::Plus => Some((50, '+')),
            Token::Minus => Some((50, '-')),
            Token::Less => Some((40, '<')),
            Token::Greater => Some((40, '>')),
            Token::LessEqual => Some((40, 'l')),     // '<='を表す文字
            Token::GreaterEqual => Some((40, 'g')),  // '>='を表す文字
            Token::EqualEqual => Some((30, '=')),
            Token::NotEqual => Some((30, 'n')),      // '!='を表す文字
            Token::And => Some((20, '&')),           // '&&'
            Token::Or => Some((10, '|')),            // '||'
            _ => None,
        }
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if self.consume_char('-') {
            let expr = self.parse_unary()?;
            Ok(Expr::Binary(
                '-',
                Box::new(Expr::Number(0.0)),
                Box::new(expr),
            ))
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.consume_token(&Token::Dot) {
                let method = self.expect_identifier()?;
                let args = if self.consume_token(&Token::Lparen) {
                    let args = self.parse_argument_list()?;
                    self.expect_rparen()?;
                    args
                } else {
                    Vec::new()
                };
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(expr);
                call_args.extend(args);
                expr = Expr::Call {
                    name: method,
                    args: call_args,
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match self.current() {
            Token::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Expr::Number(value))
            }
            Token::Str(s) => {
                let value = s.clone();
                self.advance();
                Ok(Expr::String(value))
            }
            Token::Identifier(_) => self.parse_identifier_expr(),
            Token::Lparen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_rparen()?;
                Ok(expr)
            }
            Token::Lbrace => self.parse_block(),
            other => Err(anyhow!("unexpected token in expression: {:?}", other)),
        }
    }

    fn parse_identifier_expr(&mut self) -> Result<Expr> {
        let name = if let Token::Identifier(name) = self.current() {
            name.clone()
        } else {
            return Err(anyhow!("expected identifier"));
        };
        self.advance();
        // 既存変数への代入: `name = expr`
        if self.consume_char('=') {
            let expr = self.parse_expression()?;
            return Ok(Expr::Let(name, Box::new(expr)));
        }

        if matches!(self.current(), Token::Lparen) {
            self.advance();
            let args = self.parse_argument_list()?;
            self.expect_rparen()?;
            Ok(Expr::Call { name, args })
        } else {
            Ok(Expr::Variable(name))
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.current(), Token::Rparen) {
            return Ok(args);
        }
        loop {
            let expr = self.parse_expression()?;
            args.push(expr);
            if !self.consume_token(&Token::Comma) {
                break;
            }
        }
        Ok(args)
    }

    fn consume_token(&mut self, token: &Token) -> bool {
        if self.current() == token {
            self.advance();
            return true;
        }
        false
    }

    fn expect_token(&mut self, token: &Token) -> Result<()> {
        if self.consume_token(token) {
            Ok(())
        } else {
            Err(anyhow!("unexpected token: {:?}", self.current()))
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        match self.current() {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            other => Err(anyhow!("expected identifier, found {:?}", other)),
        }
    }

    fn expect_lparen(&mut self) -> Result<()> {
        match self.current() {
            Token::Lparen => {
                self.advance();
                Ok(())
            }
            other => Err(anyhow!("expected '(', found {:?}", other)),
        }
    }

    fn expect_rparen(&mut self) -> Result<()> {
        match self.current() {
            Token::Rparen => {
                self.advance();
                Ok(())
            }
            other => Err(anyhow!("expected ')', found {:?}", other)),
        }
    }

    fn expect_lbrace(&mut self) -> Result<()> {
        match self.current() {
            Token::Lbrace => {
                self.advance();
                Ok(())
            }
            other => Err(anyhow!("expected '{{', found {:?}", other)),
        }
    }

    fn expect_rbrace(&mut self) -> Result<()> {
        match self.current() {
            Token::Rbrace => {
                self.advance();
                Ok(())
            }
            other => Err(anyhow!("expected '}}', found {:?}", other)),
        }
    }

    fn expect_char(&mut self, ch: char) -> Result<()> {
        match self.current() {
            Token::Char(c) if *c == ch => {
                self.advance();
                Ok(())
            }
            other => Err(anyhow!("expected '{}', found {:?}", ch, other)),
        }
    }

    fn consume_char(&mut self, ch: char) -> bool {
        if matches!(self.current(), Token::Char(c) if *c == ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_semicolons(&mut self) {
        while matches!(self.current(), Token::Semicolon) {
            self.advance();
        }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> &Token {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        self.tokens
            .get(self.pos.saturating_sub(1))
            .unwrap_or(&Token::Eof)
    }

    fn is_eof(&self) -> bool {
        matches!(self.current(), Token::Eof)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_collects_tokens() {
        let tokens = Parser::lex_all("task foo() {}");
        assert!(tokens.len() > 2);
        assert!(matches!(tokens.last(), Some(Token::Eof)));
    }
}

// このパーサはソーステキストを直接 `ast.rs` に定義された AST に変換します。
