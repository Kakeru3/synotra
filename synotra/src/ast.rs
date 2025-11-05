use std::fmt;

#[derive(Clone, Debug)]
pub enum Expr {
    Number(f64),
    String(String),
    Variable(String),
    Binary(char, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>),
    Val(String, Box<Expr>),
    For {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Vec<Expr>,
    },
    While {
        condition: Box<Expr>,
        body: Vec<Expr>,
    },
    Block(Vec<Expr>),
    Return(Box<Expr>),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Function {
        proto: Prototype,
        body: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct Task {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub mode: Option<String>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Function),
    Extern(Prototype),
    Task(Task),
}

#[derive(Clone, Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub proto: Prototype,
    pub body: Expr,
}

impl fmt::Display for Prototype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}({})", self.name, self.args.join(", "))
    }
}
