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
    While,
    Str(String),
    Range,
    
    // 比較演算子
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    
    // 論理演算子
    And,
    Or,
    
    // 算術演算子
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    
    // その他
    Dot,
    Colon,

    Lbrace,
    Rbrace,
    Lparen,
    Rparen,
    Semicolon,
    Comma,
}
