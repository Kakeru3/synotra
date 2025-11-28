use crate::error::Span;

/// Wrapper type that adds position information to AST nodes
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Import(ImportDef),
    Actor(ActorDef),
    Message(MessageDef),
    DataMessage(DataMessageDef), // data message Name(val field: Type, ...)
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
pub struct DataMessageDef {
    pub name: String,
    pub fields: Vec<DataField>, // All fields must be immutable (val)
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: String,
    pub field_type: Type,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
    Unit, // For IO functions - no actual value
    UserDefined(String),
    Generic(String, Vec<Type>), // e.g., List<Int>
    ActorRef(Box<Type>),        // e.g., ActorRef<CalcFib>
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Let(String, Option<Type>, Expr),
    Var(String, Option<Type>, Option<Expr>), // var name: Type = init (init is optional)
    Assign(String, Expr),                    // variable reassignment
    AssignIndex(String, Box<Expr>, Box<Expr>), // name[index] = value
    Expr(Expr),
    Return(Option<Expr>),
    // Send is now an expression
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
    For(String, Expr, Expr, Block), // for (i in start..end)
    ForEach(String, Expr, Block),   // for (item in collection) // iterator, start, end, body
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Variable(String),
    Call(Box<Expr>, String, Vec<Expr>), // target.method(args) or func(args)
    Index(Box<Expr>, Box<Expr>),        // target[index]
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
    Ask {
        target: Box<Expr>,
        message: Box<Expr>,
    },
    Send {
        target: Box<Expr>,
        message: Box<Expr>,
    },
    FieldAccess(Box<Expr>, String), // target.field
    Construct {
        name: String,             // Data message name
        args: Vec<Expr>,          // Constructor arguments
        field_names: Vec<String>, // Field names in order
    },
    Spawn {
        actor_type: String, // Actor type to spawn
        args: Vec<Expr>,    // Constructor arguments for the actor
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
