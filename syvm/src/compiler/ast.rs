/// AST（抽象構文木）のノード定義
/// 
/// 将来的にタスク内の処理も細粒度で並列化できるよう、
/// 拡張性を考慮した設計になっています。

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub tasks: Vec<Task>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Task {
    pub name: String,
    pub mode: TaskMode,
    pub params: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TaskMode {
    Distributed,
    Sequential,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // 変数宣言
    Val { name: String, value: Expression },
    Let { name: String, value: Expression },
    
    // 関数定義（タスク内のローカル関数）
    FunctionDef(Function),
    
    // 制御フロー
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Vec<Statement>,
    },
    
    For {
        var: String,
        start: Expression,
        end: Expression,
        body: Vec<Statement>,
    },
    
    // 関数呼び出し（副作用のある呼び出し）
    Call {
        name: String,
        args: Vec<Expression>,
    },
    
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // リテラル
    Literal(Literal),
    
    // 変数参照
    Var(String),
    
    // 二項演算
    Binary {
        op: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    
    // 関数呼び出し（値を返す）
    Call {
        name: String,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

impl Program {
    pub fn new() -> Self {
        Program { tasks: vec![] }
    }
    
    pub fn add_task(&mut self, task: Task) {
        self.tasks.push(task);
    }
    
    pub fn find_task(&self, name: &str) -> Option<&Task> {
        self.tasks.iter().find(|t| t.name == name)
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
