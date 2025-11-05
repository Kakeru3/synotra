#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,

    Def,
    Extern,
    Identifier(String),
    Number(f64),
    Char(char),

    Task,
    Distributed,
    Cluster,
    Var,
    Fun,
    If,
    Else,
    Return,
    Val,
    In,
    Do,
    Let,
    For,
    Str(String),
    Range,
    EqualEqual,

    Lbrace,
    Rbrace,
    Lparen,
    Rparen,
    Semicolon,
    Comma,
}
