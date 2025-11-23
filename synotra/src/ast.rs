#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Import(ImportDef),
    Actor(ActorDef),
    Message(MessageDef),
    Function(FunctionDef),
    Module(ModuleDef),
}

#[derive(Debug, Clone)]
pub struct ImportDef {
    pub path: Vec<String>,        // e.g., ["std", "collections", "List"]
    pub type_params: Vec<String>, // e.g., ["T"] or ["K", "V"]
}

#[derive(Debug, Clone)]
pub struct ActorDef {
    pub name: String,
    pub params: Vec<Param>,
    pub members: Vec<ActorMember>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum ActorMember {
    Field(FieldDef),
    Method(FunctionDef),
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    pub ty: Type,
    pub init: Expr,
    pub is_mutable: bool,
    pub is_crdt: bool,
    pub is_io: bool,
}

#[derive(Debug, Clone)]
pub struct MessageDef {
    pub name: String,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub is_io: bool,
}

#[derive(Debug, Clone)]
pub struct ModuleDef {
    pub name: String,
    pub functions: Vec<FunctionDef>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    String,
    Bool,
    UserDefined(String),
    Generic(String, Vec<Type>), // e.g., List<Int>
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(String, Option<Type>, Expr),
    Var(String, Option<Type>, Expr),
    Assign(String, Expr),                      // variable reassignment
    AssignIndex(String, Box<Expr>, Box<Expr>), // name[index] = value
    Expr(Expr),
    Return(Option<Expr>),
    Send {
        target: Expr,
        message: Expr,
        args: Vec<Expr>,
    },
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
    For(String, Expr, Expr, Block), // iterator, start, end, body
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Call(Box<Expr>, String, Vec<Expr>), // target.method(args) or func(args)
    Index(Box<Expr>, Box<Expr>),        // target[index]
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
    Ask {
        target: Box<Expr>,
        message: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}
