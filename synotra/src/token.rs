#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,

    Def,
    Extern,
    Identifier(String),
    Number(f64),
    // 単一文字トークン（演算子、括弧、区切り文字など）
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
