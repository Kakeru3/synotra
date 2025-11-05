use crate::compiler::ast::*;
use std::collections::HashMap;
use std::sync::Arc;

/// インタプリタの実行時値
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
        }
    }
}

/// タスクの実行結果
pub type TaskResult = Value;

/// インタプリタ
/// ASTを実行してVMで並列処理を行う
pub struct Interpreter {
    program: Program,
    // グローバルな組み込み関数
    builtin_functions: HashMap<String, Arc<dyn Fn(Vec<Value>) -> Value + Send + Sync>>,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        let mut builtin_functions: HashMap<String, Arc<dyn Fn(Vec<Value>) -> Value + Send + Sync>> =
            HashMap::new();

        // 組み込み関数: print
        builtin_functions.insert(
            "print".to_string(),
            Arc::new(|args: Vec<Value>| {
                // route prints to stderr so they are visible together with
                // DEBUG logs (which use stderr). This avoids missing output
                // when the consumer only watches stderr.
                for arg in &args {
                    eprintln!("{}", arg);
                }
                Value::Unit
            }),
        );

        // 組み込み関数: toString
        builtin_functions.insert(
            "toString".to_string(),
            Arc::new(|args: Vec<Value>| {
                if let Some(arg) = args.first() {
                    Value::String(format!("{}", arg))
                } else {
                    Value::String(String::new())
                }
            }),
        );

        Interpreter {
            program,
            builtin_functions,
        }
    }

    /// プログラムを実行（並列実行モード）
    pub fn run(&self) -> Result<(), String> {
        // main タスクを探す
        let main_task = self
            .program
            .find_task("main")
            .ok_or_else(|| "No main task found".to_string())?;

        // main タスクを実行
        self.execute_task(main_task, vec![])?;

        Ok(())
    }

    /// タスクを実行
    fn execute_task(&self, task: &Task, args: Vec<Value>) -> Result<TaskResult, String> {
        // 環境を初期化
        let mut env = Environment::new();
        for (param, arg) in task.params.iter().zip(args.iter()) {
            env.set(param.name.clone(), arg.clone());
        }
        // タスク内の文を実行（task 単位での分散は execute_statements 側で制御）
        self.execute_statements(&task.body, &mut env)
    }

    /// 文のリストを実行
    fn execute_statements(
        &self,
        statements: &[Statement],
        env: &mut Environment,
    ) -> Result<Value, String> {
        let mut last_value = Value::Unit;
        let mut i = 0;
        while i < statements.len() {
            // バッチ：連続する distributed タスク呼び出しをまとめて並列実行する
            if let Statement::Call { name, args } = &statements[i] {
                if let Some(task) = self.program.find_task(name) {
                    if task.mode == TaskMode::Distributed {
                        // collect consecutive distributed task calls
                        let mut batch: Vec<(Task, Vec<Value>)> = Vec::new();
                        while i < statements.len() {
                            if let Statement::Call { name: nm, args: as_ex } = &statements[i] {
                                if let Some(t) = self.program.find_task(nm) {
                                    if t.mode == TaskMode::Distributed {
                                        // evaluate args now (in parent env)
                                        let mut arg_vals: Vec<Value> = Vec::new();
                                        for ex in as_ex {
                                            match self.eval_expression(ex, env) {
                                                Ok(v) => arg_vals.push(v),
                                                Err(ControlFlow::Return(rv)) => return Ok(rv),
                                                Err(ControlFlow::Error(msg)) => return Err(msg),
                                            }
                                        }
                                        batch.push((t.clone(), arg_vals));
                                        i += 1;
                                        continue;
                                    }
                                }
                            }
                            break;
                        }

                        use rayon::prelude::*;
                        let results: Vec<Value> = batch
                            .into_par_iter()
                            .map(|(task, args)| match self.execute_task(&task, args) {
                                Ok(v) => v,
                                Err(_) => Value::Unit,
                            })
                            .collect();

                        last_value = results.last().cloned().unwrap_or(Value::Unit);
                        continue; // i is already advanced by the inner loop
                    }
                }
            }

            // 通常の文は逐次実行
            match self.execute_statement(&statements[i], env) {
                Ok(val) => last_value = val,
                Err(ControlFlow::Return(val)) => return Ok(val),
                Err(ControlFlow::Error(msg)) => return Err(msg),
            }
            i += 1;
        }

        Ok(last_value)
    }

    /// 文を実行
    fn execute_statement(
        &self,
        stmt: &Statement,
        env: &mut Environment,
    ) -> Result<Value, ControlFlow> {
        match stmt {
            Statement::Val { name, value } => {
                let val = self.eval_expression(value, env)?;
                env.set(name.clone(), val.clone());
                Ok(val)
            }

            Statement::Let { name, value } => {
                let val = self.eval_expression(value, env)?;
                env.set(name.clone(), val.clone());
                Ok(val)
            }

            Statement::FunctionDef(func) => {
                // function definitions are stored in the environment; no debug print here
                env.set_function(func.name.clone(), func.clone());
                Ok(Value::Unit)
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.eval_expression(condition, env)?;

                if self.is_truthy(&cond_val) {
                    self.execute_statements(then_branch, env)
                        .map_err(ControlFlow::Error)
                } else {
                    self.execute_statements(else_branch, env)
                        .map_err(ControlFlow::Error)
                }
            }

            Statement::For { var, start, end, body } => {
                let start_val = self.eval_expression(start, env)?;
                let end_val = self.eval_expression(end, env)?;
                let start_num = if let Value::Int(n) = start_val { n } else {
                    return Err(ControlFlow::Error("For loop start must be an integer".to_string()));
                };
                let end_num = if let Value::Int(n) = end_val { n } else {
                    return Err(ControlFlow::Error("For loop end must be an integer".to_string()));
                };
                // distributedモードならforループを並列化
                if let Some(task) = self.program.find_task(&env.get("__current_task_name").unwrap_or(Value::String("".to_string())).to_string()) {
                    if task.mode == TaskMode::Distributed {
                        use rayon::prelude::*;
                        let results: Vec<_> = (start_num..=end_num).into_par_iter().map(|i| {
                            let mut env = env.clone();
                            env.set(var.clone(), Value::Int(i));
                            self.execute_statements(body, &mut env).unwrap_or(Value::Unit)
                        }).collect();
                        Ok(results.last().cloned().unwrap_or(Value::Unit))
                    } else {
                        let mut last_val = Value::Unit;
                        for i in start_num..=end_num {
                            env.set(var.clone(), Value::Int(i));
                            last_val = self.execute_statements(body, env).map_err(ControlFlow::Error)?;
                        }
                        Ok(last_val)
                    }
                } else {
                    // タスク情報がなければ逐次実行
                    let mut last_val = Value::Unit;
                    for i in start_num..=end_num {
                        env.set(var.clone(), Value::Int(i));
                        last_val = self.execute_statements(body, env).map_err(ControlFlow::Error)?;
                    }
                    Ok(last_val)
                }
            }

            Statement::While { condition, body } => {
                // Evaluate condition each iteration and run body while truthy
                let mut last_val = Value::Unit;
                loop {
                    let cond_val = self.eval_expression(condition, env)?;
                    if !self.is_truthy(&cond_val) {
                        break;
                    }
                    last_val = self
                        .execute_statements(body, env)
                        .map_err(ControlFlow::Error)?;
                }
                Ok(last_val)
            }

            Statement::Call { name, args } => {
                let arg_vals: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.eval_expression(arg, env))
                    .collect();
                let arg_vals = arg_vals?;

                self.call_function(name, arg_vals, env)
            }

            Statement::Return(expr) => {
                let val = self.eval_expression(expr, env)?;
                Err(ControlFlow::Return(val))
            }
        }
    }

    /// 式を評価
    fn eval_expression(&self, expr: &Expression, env: &Environment) -> Result<Value, ControlFlow> {
        match expr {
            Expression::Literal(lit) => Ok(self.eval_literal(lit)),

            Expression::Var(name) => env
                .get(name)
                .ok_or_else(|| ControlFlow::Error(format!("Undefined variable: {}", name))),

            Expression::Binary { op, left, right } => {
                let left_val = self.eval_expression(left, env)?;
                let right_val = self.eval_expression(right, env)?;
                self.eval_binary_op(op, left_val, right_val)
            }

            Expression::Call { name, args } => {
                let arg_vals: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.eval_expression(arg, env))
                    .collect();
                let arg_vals = arg_vals?;

                self.call_function(name, arg_vals, env)
            }
        }
    }

    /// リテラルを評価
    fn eval_literal(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Int(n) => Value::Int(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
        }
    }

    /// 二項演算を評価
    fn eval_binary_op(&self, op: &str, left: Value, right: Value) -> Result<Value, ControlFlow> {
        match (op, left, right) {
            // equality operators: support both "==" and legacy "="
            ("==", Value::Int(l), Value::Int(r)) | ("=", Value::Int(l), Value::Int(r)) => {
                Ok(Value::Bool(l == r))
            }
            ("==", Value::String(l), Value::String(r))
            | ("=", Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
            ("==", Value::Bool(l), Value::Bool(r)) | ("=", Value::Bool(l), Value::Bool(r)) => {
                Ok(Value::Bool(l == r))
            }
            // not-equal
            ("!=", Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l != r)),
            ("!=", Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
            ("!=", Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
            ("%", Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
            ("+", Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            // string concatenation
            ("+", Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            ("+", Value::String(l), Value::Int(r)) => Ok(Value::String(format!("{}{}", l, r))),
            ("+", Value::Int(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            ("-", Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            ("*", Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            ("/", Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
            ("<", Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
            (">", Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
            ("<=", Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
            (">=", Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
            // logical operators
            ("&&", Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
            ("||", Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
            _ => Err(ControlFlow::Error(format!("Unsupported operation: {}", op))),
        }
    }

    /// 関数を呼び出す
    fn call_function(
        &self,
        name: &str,
        args: Vec<Value>,
        env: &Environment,
    ) -> Result<Value, ControlFlow> {
        // 組み込み関数をチェック
        if let Some(builtin) = self.builtin_functions.get(name) {
            return Ok(builtin(args));
        }

        // ユーザー定義関数をチェック
        if let Some(func) = env.get_function(name) {
            let mut new_env = env.clone();

            // 引数をバインド
            for (param, arg) in func.args.iter().zip(args.iter()) {
                new_env.set(param.clone(), arg.clone());
            }

            return self
                .execute_statements(&func.body, &mut new_env)
                .map_err(ControlFlow::Error);
        }

        // タスクをチェック
        if let Some(task) = self.program.find_task(name) {
            return self.execute_task(task, args).map_err(ControlFlow::Error);
        }

        Err(ControlFlow::Error(format!("Undefined function: {}", name)))
    }

    /// 値が真かどうかを判定
    fn is_truthy(&self, val: &Value) -> bool {
        match val {
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::String(s) => !s.is_empty(),
            Value::Unit => false,
        }
    }
}

/// 実行環境（変数と関数のスコープ）
#[derive(Debug, Clone)]
struct Environment {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn set(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    fn set_function(&mut self, name: String, func: Function) {
        self.functions.insert(name, func);
    }

    fn get_function(&self, name: &str) -> Option<Function> {
        self.functions.get(name).cloned()
    }
}

/// 制御フロー（return や error）
enum ControlFlow {
    Return(Value),
    Error(String),
}

impl From<String> for ControlFlow {
    fn from(s: String) -> Self {
        ControlFlow::Error(s)
    }
}
