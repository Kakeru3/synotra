use crate::compiler::token::Token;
use crate::compiler::ast::*;

/// 構文解析器
/// トークン列からASTを生成
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    indent_depth: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            indent_depth: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Program::new();

        while !self.is_at_end() {
            self.skip_newlines();
            // トップレベルに残っている不意の Dedent があれば消費して進める
            while self.check(&Token::Dedent) {
                self.advance();
                self.skip_newlines();
            }
            if self.is_at_end() {
                break;
            }

            if self.check(&Token::Task) {
                program.add_task(self.parse_task()?);
            } else if !program.tasks.is_empty() {
                // トップレベルで @task が来ない場合、現在のトークンが
                // 直前にパースしたタスクのボディに属する文である可能性がある。
                // その場合は最後に追加したタスクの body に追加で文をパースして
                // 結合する。（過渡的なパースずれを回復するための安全策）
                eprintln!("DEBUG: Top-level encountered non-Task token; appending statements to last task: {:?}", self.current());
                let stmts = self.parse_statements()?;
                if !stmts.is_empty() {
                    if let Some(last) = program.tasks.last_mut() {
                        last.body.extend(stmts);
                    }
                } else {
                    return Err(format!("Expected @task, got {:?}", self.current()));
                }
            } else {
                return Err(format!("Expected @task, got {:?}", self.current()));
            }
        }

        Ok(program)
    }

    fn parse_task(&mut self) -> Result<Task, String> {
        eprintln!("DEBUG: parse_task start at pos={} indent_depth={} current={:?}", self.position, self.indent_depth, self.current());
        self.expect(&Token::Task)?;
        
        let name = self.expect_identifier()?;
        self.skip_newlines();
        
        // タスクの本体はインデントされる
        if self.check(&Token::Indent) {
            self.advance();
        }

        // mode の解析
        let mut mode = TaskMode::Sequential;
        if self.check(&Token::Mode) {
            self.advance();
            if self.check(&Token::Distributed) {
                mode = TaskMode::Distributed;
                self.advance();
            }
            self.skip_newlines();
        }

        // params の解析
        let mut params = Vec::new();
        if self.check(&Token::Params) {
            self.advance();
            self.skip_newlines();
            
            // paramsの後のインデントをチェック（オプション）
            if self.check(&Token::Indent) {
                self.advance();
            }
            
            while !self.check(&Token::Dedent) && !self.is_at_end() {
                self.skip_newlines();
                if self.check(&Token::Dedent) {
                    break;
                }
                
                let param_name = self.expect_identifier()?;
                self.expect(&Token::Colon)?;
                let param_type = self.parse_type()?;
                params.push(Parameter {
                    name: param_name,
                    param_type,
                });
                self.skip_newlines();
            }
            
            // Dedentがある場合は消費
            if self.check(&Token::Dedent) {
                self.advance();
            }
            self.skip_newlines();
        }

        // body の解析
        self.expect(&Token::Body)?;
        self.skip_newlines();
        
        // bodyの後のインデントをチェック（オプション）
        let entry_depth = self.indent_depth;
        if self.check(&Token::Indent) {
            self.advance();
        }

        let body = self.parse_statements()?;

        // bodyブロックから元の深さに戻るまでのDedentを消費
        self.consume_dedents_to(entry_depth);

        eprintln!("DEBUG: parse_task end at pos={} indent_depth={} next={:?}", self.position, self.indent_depth, self.current());
        Ok(Task {
            name,
            mode,
            params,
            body,
        })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.current() {
            Token::IntType => {
                self.advance();
                Ok(Type::Int)
            }
            Token::StringType => {
                self.advance();
                Ok(Type::String)
            }
            Token::BoolType => {
                self.advance();
                Ok(Type::Bool)
            }
            _ => Err(format!("Expected type, got {:?}", self.current())),
        }
    }

    fn parse_statements(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();

        while !self.check(&Token::Dedent) && !self.check(&Token::Else) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) || self.check(&Token::Else) {
                break;
            }

            statements.push(self.parse_statement()?);
            self.skip_newlines();
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current() {
            Token::Val => self.parse_val(),
            Token::Let => self.parse_let(),
            Token::Function => self.parse_function_def(),
            Token::If => self.parse_if(),
            Token::For => self.parse_for(),
            Token::Call => self.parse_call_statement(),
            Token::Return => self.parse_return(),
            _ => Err(format!("Unexpected token in statement: {:?}", self.current())),
        }
    }

    fn parse_val(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Val)?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Operator("=".to_string()))?;
        let value = self.parse_expression()?;
        Ok(Statement::Val { name, value })
    }

    fn parse_let(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Let)?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Operator("=".to_string()))?;
        let value = self.parse_expression()?;
        Ok(Statement::Let { name, value })
    }

    fn parse_function_def(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Function)?;
        let name = self.expect_identifier()?;
        self.skip_newlines();
        
        eprintln!("DEBUG: Parsing function: {}", name);
        
        // @functionの本体はインデントされる
        if self.check(&Token::Indent) {
            self.advance();
        }

        // args の解析
        let mut args = Vec::new();
        if self.check(&Token::Args) {
            self.advance();
            self.skip_newlines();
            self.expect(&Token::LBracket)?;
            
            while !self.check(&Token::RBracket) {
                self.skip_newlines();
                if self.check(&Token::RBracket) {
                    break;
                }
                args.push(self.expect_identifier()?);
                if self.check(&Token::Comma) {
                    self.advance();
                }
                self.skip_newlines();
            }
            
            self.expect(&Token::RBracket)?;
            self.skip_newlines();
        }

        // body の解析
        self.expect(&Token::Body)?;
        self.skip_newlines();
        
        // bodyの後のインデントをチェック（オプション）
        let entry_depth = self.indent_depth;
        if self.check(&Token::Indent) {
            self.advance();
        }

        let body = self.parse_statements()?;

        eprintln!("DEBUG: Function {} body parsed, current token: {:?}", name, self.current());

        // @function定義全体から抜けるためのDedentを消費して元の深さに戻す
        // （関数内部で発生したインデント増減をここでまとめて戻す）
        self.consume_dedents_to(entry_depth);
        eprintln!("DEBUG: Function {} next token: {:?}", name, self.current());

        Ok(Statement::FunctionDef(Function { name, args, body }))
    }

    fn parse_if(&mut self) -> Result<Statement, String> {
        self.expect(&Token::If)?;
        self.skip_newlines();
        
        eprintln!("DEBUG: Parsing if statement");
        
        let condition = self.parse_expression()?;

        // 式の後に来る改行を先にスキップしてからDedentを消費する。
        // これは条件式が複数行でインデントされている場合に、
        // Newline->Dedent の順でトークンが来ることがあるため。
        self.skip_newlines();

        // 条件式後に続く可能性のある複数のDedentを消費
        while self.check(&Token::Dedent) {
            self.advance();
        }

        self.skip_newlines();

        self.expect(&Token::Then)?;
        self.skip_newlines();
        
        // thenの後のインデントをチェック（オプション）
        if self.check(&Token::Indent) {
            self.advance();
        }
        
        eprintln!("DEBUG: Parsing then branch");
        let then_branch = self.parse_statements()?;
        eprintln!("DEBUG: Then branch parsed, current token: {:?}", self.current());
        
        // then_branchブロックから出るDedentを1つだけ消費
        if self.check(&Token::Dedent) {
            self.advance();
            eprintln!("DEBUG: Consumed 1 Dedent after then branch");
        }
        self.skip_newlines();
        
        let mut else_branch = Vec::new();
        if self.check(&Token::Else) {
            eprintln!("DEBUG: Parsing else branch");
            self.advance();
            self.skip_newlines();
            
            // elseの後のインデントをチェック（オプション）
            if self.check(&Token::Indent) {
                self.advance();
            }
            
            else_branch = self.parse_statements()?;
            eprintln!("DEBUG: Else branch parsed, current token: {:?}", self.current());
            
            // else_branchブロックから出るDedentを1つだけ消費
            if self.check(&Token::Dedent) {
                self.advance();
                eprintln!("DEBUG: Consumed 1 Dedent after else branch, next token: {:?}", self.current());
            }
        }

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_for(&mut self) -> Result<Statement, String> {
        self.expect(&Token::For)?;
        let var = self.expect_identifier()?;
        self.expect(&Token::In)?;
        
        let start = self.parse_expression()?;
        
        // startの後のDedentをスキップ（1つだけ）
        if self.check(&Token::Dedent) {
            self.advance();
        }
        
        self.expect(&Token::Range)?;
        let end = self.parse_expression()?;
        
        // endの後のDedentをスキップ（1つだけ）
        if self.check(&Token::Dedent) {
            self.advance();
        }
        
        self.expect(&Token::Do)?;
        self.skip_newlines();
        
        // doの後のインデントをチェック（オプション）
        if self.check(&Token::Indent) {
            self.advance();
        }
        
        let body = self.parse_statements()?;
        
        // bodyブロックから出るDedentを1つだけ消費
        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(Statement::For {
            var,
            start,
            end,
            body,
        })
    }

    fn parse_call_statement(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Call)?;
        let name = self.expect_identifier()?;
        
        let mut args = Vec::new();
        
        // 改行とインデントをスキップ
        self.skip_newlines_and_indents();
        
        if self.check(&Token::Args) {
            self.advance();
            self.skip_newlines_and_indents();
            
            // 2つの形式をサポート:
            // 1. args [expr1, expr2] (従来の形式)
            // 2. args\n  expr1\n  expr2 (新しい形式)
            
            if self.check(&Token::LBracket) {
                // 従来の形式: args [...]
                self.advance();
                
                while !self.check(&Token::RBracket) {
                    args.push(self.parse_expression()?);
                    if self.check(&Token::Comma) {
                        self.advance();
                    }
                }
                
                self.expect(&Token::RBracket)?;
            } else {
                // 新しい形式: args の後にインデントされた式のリスト
                // インデントをスキップ
                let args_entry_depth = self.indent_depth;
                if self.check(&Token::Indent) {
                    self.advance();
                }

                // 各引数を読む（Dedentが来るまで）
                while !self.check(&Token::Dedent) && !self.is_at_end() {
                    self.skip_newlines();
                    if self.check(&Token::Dedent) {
                        break;
                    }
                    args.push(self.parse_expression()?);
                    self.skip_newlines();
                }

                // argsブロックから抜けて、元の深さに戻るまでのDedentを消費
                self.consume_dedents_to(args_entry_depth);
            }
        }

        Ok(Statement::Call { name, args })
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Return)?;
        // return の後に改行やインデントが続くケースをハンドル
        self.skip_newlines_and_indents();
        // 万一ここで Dedent が来てしまっている場合、1つ消費してから式を読む
        // （過去の Dedent 消費の影響で発生することがあるための保険）
        if self.check(&Token::Dedent) {
            self.advance();
            self.skip_newlines_and_indents();
        }
        let value = self.parse_expression()?;
        Ok(Statement::Return(value))
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        // 式の前の改行とインデントをスキップ
        self.skip_newlines_and_indents();
        
        // デバッグ: 式をパースし始めるときの現在トークン
        eprintln!("DEBUG: parse_expression current token: {:?}", self.current());

        match self.current() {
            Token::Literal => self.parse_literal_expr(),
            Token::Var => self.parse_var(),
            Token::Binary => self.parse_binary(),
            Token::Call => self.parse_call_expr(),
            Token::Identifier(_) => {
                // 識別子は変数参照として扱う
                let name = self.expect_identifier()?;
                Ok(Expression::Var(name))
            }
            Token::Number(n) => {
                let num = *n;
                self.advance();
                Ok(Expression::Literal(Literal::Int(num)))
            }
            _ => Err(format!("Unexpected token in expression: {:?}", self.current())),
        }
    }

    fn parse_literal_expr(&mut self) -> Result<Expression, String> {
        self.expect(&Token::Literal)?;
        
        match self.current() {
            Token::Number(n) => {
                let num = *n;
                self.advance();
                Ok(Expression::Literal(Literal::Int(num)))
            }
            Token::String(s) => {
                let str = s.clone();
                self.advance();
                Ok(Expression::Literal(Literal::String(str)))
            }
            _ => Err(format!("Expected literal value, got {:?}", self.current())),
        }
    }

    fn parse_var(&mut self) -> Result<Expression, String> {
        self.expect(&Token::Var)?;
        let name = self.expect_identifier()?;
        Ok(Expression::Var(name))
    }

    fn parse_binary(&mut self) -> Result<Expression, String> {
        self.expect(&Token::Binary)?;

    // entry_depth は、子ノードをパースする直前に記録する。
    // これにより、skip_newlines_and_indents によって新たに導入された
    // インデント分までを正しく扱える。

        let op = if let Token::String(s) = self.current() {
            let op_str = s.clone();
            self.advance();
            op_str
        } else {
            return Err(format!("Expected operator string, got {:?}", self.current()));
        };

        self.skip_newlines_and_indents();

        // 子ノードをパースする直前の深さを記録
        let entry_depth = self.indent_depth;

        let left = Box::new(self.parse_expression()?);

        // 左辺の解析で入れられたインデントがあれば戻す
        self.consume_dedents_to(entry_depth);
        self.skip_newlines_and_indents();

        let right = Box::new(self.parse_expression()?);

        // 右辺の解析後にも同様に、深さを復元する
        self.consume_dedents_to(entry_depth);

        self.skip_newlines_and_indents();

        Ok(Expression::Binary { op, left, right })
    }

    fn parse_call_expr(&mut self) -> Result<Expression, String> {
        self.expect(&Token::Call)?;
        let name = self.expect_identifier()?;
        
        let mut args = Vec::new();
        
        // 改行とインデントをスキップ
        self.skip_newlines_and_indents();
        
        if self.check(&Token::Args) {
            self.advance();
            self.skip_newlines_and_indents();
            
            // 2つの形式をサポート:
            // 1. args [expr1, expr2] (従来の形式)
            // 2. args\n  expr1\n  expr2 (新しい形式)
            
            if self.check(&Token::LBracket) {
                // 従来の形式: args [...]
                self.advance();
                
                while !self.check(&Token::RBracket) {
                    args.push(self.parse_expression()?);
                    if self.check(&Token::Comma) {
                        self.advance();
                    }
                }
                
                self.expect(&Token::RBracket)?;
            } else {
                // 新しい形式: args の後にインデントされた式のリスト
                // インデントをスキップ
                let args_entry_depth = self.indent_depth;
                if self.check(&Token::Indent) {
                    self.advance();
                }

                // 各引数を読む（Dedentが来るまで）
                while !self.check(&Token::Dedent) && !self.is_at_end() {
                    self.skip_newlines();
                    if self.check(&Token::Dedent) {
                        break;
                    }
                    args.push(self.parse_expression()?);
                    self.skip_newlines();
                }

                // argsブロックから抜けて、元の深さに戻るまでのDedentを消費
                self.consume_dedents_to(args_entry_depth);
            }
        }

        Ok(Expression::Call { name, args })
    }

    // ユーティリティメソッド

    fn current(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            // capture current token to update indent depth correctly
            let tok = self.current().clone();
            self.position += 1;
            match tok {
                Token::Indent => {
                    self.indent_depth += 1;
                    eprintln!("DEBUG: advance saw Indent -> indent_depth={}", self.indent_depth);
                }
                Token::Dedent => {
                    if self.indent_depth > 0 {
                        self.indent_depth -= 1;
                    }
                    eprintln!("DEBUG: advance saw Dedent -> indent_depth={}", self.indent_depth);
                }
                _ => {}
            }
        }
    }

    fn consume_dedents_to(&mut self, target: usize) {
        eprintln!("DEBUG: consume_dedents_to target={} current={}", target, self.indent_depth);
        let mut consumed = 0;
        while self.indent_depth > target && self.check(&Token::Dedent) {
            self.advance();
            consumed += 1;
        }
        eprintln!("DEBUG: consume_dedents_to consumed={} now={}", consumed, self.indent_depth);
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current(), Token::Eof)
    }

    fn check(&self, token: &Token) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(self.current()) == std::mem::discriminant(token)
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        if std::mem::discriminant(self.current()) == std::mem::discriminant(expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, got {:?} at position {}",
                expected,
                self.current(),
                self.position
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        if let Token::Identifier(name) = self.current() {
            let result = name.clone();
            self.advance();
            Ok(result)
        } else {
            Err(format!("Expected identifier, got {:?}", self.current()))
        }
    }

    fn skip_newlines(&mut self) {
        while self.check(&Token::Newline) {
            self.advance();
        }
    }
    
    fn skip_newlines_and_indents(&mut self) {
        while self.check(&Token::Newline) || self.check(&Token::Indent) {
            self.advance();
        }
    }
}
