use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrProgram {
    pub actors: Vec<IrActor>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrActor {
    pub name: String,
    pub fields: Vec<String>,
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
    SwLoad {
        result: usize,
        collection: usize, // Local index of collection
        index: usize,      // Local index of key/index
    },
    SwStore {
        collection: usize, // Local index of collection
        index: usize,      // Local index of key/index
        value: usize,      // Local index of value
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
    Send {
        target: Value,
        message: Value,
    },
    Ask {
        result: usize,
        target: Value,
        message: Value,
    },
    Exit,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Terminator {
    Return(Option<Value>),
    Jump(usize),
    JumpCond(Value, usize, usize), // cond, true_block, false_block
}

use serde::{Deserializer, Serializer};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum FutureState {
    Pending(Receiver<Value>),
    Resolved(Box<Value>),
}

#[derive(Debug, Clone)]
pub struct RuntimeFuture(pub Arc<Mutex<FutureState>>);

impl PartialEq for RuntimeFuture {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for RuntimeFuture {}

impl Hash for RuntimeFuture {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

impl Serialize for RuntimeFuture {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_none()
    }
}

impl<'de> Deserialize<'de> for RuntimeFuture {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Error;
        Err(D::Error::custom("RuntimeFuture cannot be deserialized"))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Value {
    ConstInt(i64),
    ConstString(String),
    ConstBool(bool),
    Local(usize), // Local variable index
    #[serde(skip)]
    Future(RuntimeFuture),

    // Collections
    List(Vec<Value>),
    Map(HashMap<Value, Value>),
    Set(HashSet<Value>),

    // Structured Messages
    Message {
        type_name: String,
        fields: HashMap<String, Box<Value>>,
    },
    ActorRef(String),              // Actor ID
    Entry(Box<Value>, Box<Value>), // Key, Value
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
            Value::Future(f) => {
                4u8.hash(state);
                f.hash(state);
            }
            Value::List(l) => {
                5u8.hash(state);
                l.hash(state);
            }
            Value::Message { type_name, .. } => {
                6u8.hash(state);
                type_name.hash(state);
            }
            Value::Map(_) => {
                panic!("Cannot hash Map");
            }
            Value::Set(_) => {
                panic!("Cannot hash Set");
            }
            Value::ActorRef(id) => {
                7u8.hash(state);
                id.hash(state);
            }
            Value::Entry(k, v) => {
                8u8.hash(state);
                k.hash(state);
                v.hash(state);
            }
        }
    }
}
