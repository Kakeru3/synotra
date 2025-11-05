#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // キーワード
    Task,           // @task
    Function,       // @function
    Mode,           // mode
    Distributed,    // distributed
    Params,         // params
    Body,           // body
    Args,           // args
    Val,            // val
    Let,            // let
    If,             // if
    Then,           // then
    Else,           // else
    For,            // for
    In,             // in
    Do,             // do
    Return,         // return
    Call,           // call
    Var,            // var
    Literal,        // literal
    Binary,         // binary
    
    // リテラル
    Identifier(String),
    Number(i64),
    String(String),
    
    // 型
    IntType,        // Int
    StringType,     // String
    BoolType,       // Bool
    
    // 演算子（文字列として扱われる場合もある）
    Operator(String), // "=", "%", "+", "-", etc.
    
    // 区切り記号
    LBracket,       // [
    RBracket,       // ]
    Colon,          // :
    Comma,          // ,
    Range,          // ..
    
    // その他
    Newline,
    Indent,
    Dedent,
    Eof,
}

impl Token {
    pub fn from_keyword(s: &str) -> Option<Token> {
        match s {
            "@task" => Some(Token::Task),
            "@function" => Some(Token::Function),
            "mode" => Some(Token::Mode),
            "distributed" => Some(Token::Distributed),
            "params" => Some(Token::Params),
            "body" => Some(Token::Body),
            "args" => Some(Token::Args),
            "val" => Some(Token::Val),
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "then" => Some(Token::Then),
            "else" => Some(Token::Else),
            "for" => Some(Token::For),
            "in" => Some(Token::In),
            "do" => Some(Token::Do),
            "return" => Some(Token::Return),
            "call" => Some(Token::Call),
            "var" => Some(Token::Var),
            "literal" => Some(Token::Literal),
            "binary" => Some(Token::Binary),
            "Int" => Some(Token::IntType),
            "String" => Some(Token::StringType),
            "Bool" => Some(Token::BoolType),
            _ => None,
        }
    }
}
