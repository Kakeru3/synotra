use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrProgram {
    pub actors: Vec<IrActor>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrActor {
    pub name: String,
    pub handlers: Vec<IrHandler>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrHandler {
    pub name: String,
    pub params: Vec<(String, String)>, // Name, Type
    pub local_count: usize, // Number of local variables
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasicBlock {
    pub id: usize,
    pub instrs: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction {
    Assign(usize, Value), // Local index, Value
    BinOp {
        result: usize, // Local index
        op: String,
        lhs: Value,
        rhs: Value,
    },
    CallPure {
        result: usize, // Local index
        func: String,
        args: Vec<Value>,
    },
    CallIo {
        result: usize, // Local index
        func: String,
        args: Vec<Value>,
    },
    Send {
        target: Value,
        msg: Value,
        args: Vec<Value>,
    },
    Ask {
        result: usize, // Local index
        target: Value,
        msg: Value,
        args: Vec<Value>,
    },
    Exit,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Terminator {
    Return(Option<Value>),
    Jump(usize),
    JumpCond(Value, usize, usize),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    ConstInt(i64),
    ConstString(String),
    Local(usize), // Local variable index
}
