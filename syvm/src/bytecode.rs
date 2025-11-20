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
    pub params: Vec<(String, String)>,
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
    // SSA
    Assign(String, Value), // v1 = ...
    BinOp { result: String, op: String, lhs: Value, rhs: Value },
    
    // Actor Ops
    Send { target: Value, msg: Value, args: Vec<Value> },
    Ask { result: String, target: Value, msg: Value, args: Vec<Value> }, // v1 = ASK ...
    
    // State Ops
    SwLoad { result: String, var: String },
    SwStore { var: String, val: Value },
    
    // IO/Pure Calls
    CallPure { result: String, func: String, args: Vec<Value> },
    CallIo { result: String, func: String, args: Vec<Value> },
    
    // File Ops (Primitive IO)
    FileOp { op: String, args: Vec<Value> },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Terminator {
    Return(Option<Value>),
    Jump(usize),
    JumpCond(Value, usize, usize), // cond, true_block, false_block
}

use std::sync::{Arc, Mutex};
use std::sync::mpsc::Receiver;
use serde::{Serializer, Deserializer};

#[derive(Debug)]
pub enum FutureState {
    Pending(Receiver<Value>),
    Resolved(Box<Value>),
}

#[derive(Debug, Clone)]
pub struct RuntimeFuture(pub Arc<Mutex<FutureState>>);

impl Serialize for RuntimeFuture {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        serializer.serialize_none()
    }
}

impl<'de> Deserialize<'de> for RuntimeFuture {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de> {
        use serde::de::Error;
        Err(D::Error::custom("RuntimeFuture cannot be deserialized"))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    ConstInt(i64),
    ConstString(String),
    Var(String), // SSA variable or Local
    #[serde(skip)]
    Future(RuntimeFuture),
}
