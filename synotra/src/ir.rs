use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

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
    pub local_count: usize,            // Number of local variables
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
    CallMethod {
        result: usize, // Local index
        target: usize, // Local index of the object (collection)
        method: String,
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
    SwLoad {
        result: usize,
        collection: usize,
        index: usize,
    },
    SwStore {
        collection: usize,
        index: usize,
        value: usize,
    },
    CreateMessage {
        result: usize,
        type_name: String,
        field_values: Vec<(String, Value)>, // (field_name, value) pairs
    },
    GetField {
        result: usize,
        target: Value,
        field_name: String,
    },
    Spawn {
        result: usize,
        actor_type: String,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Value {
    ConstInt(i64),
    ConstString(String),
    ConstBool(bool),
    Local(usize), // Local variable index

    // Collections
    List(Vec<Value>),
    Map(HashMap<Value, Value>),
    Set(HashSet<Value>),

    // Structured Messages
    Message {
        type_name: String,
        fields: HashMap<String, Box<Value>>,
    },
    ActorRef(String), // Actor ID (UUID or instance ID)
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::ConstInt(i) => {
                0u8.hash(state);
                i.hash(state);
            }
            Value::ConstString(s) => {
                1u8.hash(state);
                s.hash(state);
            }
            Value::ConstBool(b) => {
                2u8.hash(state);
                b.hash(state);
            }
            Value::Local(l) => {
                3u8.hash(state);
                l.hash(state);
            }
            Value::List(l) => {
                5u8.hash(state);
                l.hash(state);
            }
            Value::Message { type_name, .. } => {
                8u8.hash(state);
                type_name.hash(state); // Only hash type_name to avoid deep recursion
            }
            Value::ActorRef(id) => {
                9u8.hash(state);
                id.hash(state);
            }
            Value::Map(_) => {
                panic!("Cannot hash Map");
            }
            Value::Set(_) => {
                panic!("Cannot hash Set");
            }
        }
    }
}
