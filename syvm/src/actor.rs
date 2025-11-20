use crate::bytecode::*;
use std::collections::HashMap;
use tokio::sync::mpsc;

pub struct Actor {
    pub definition: IrActor,
    pub mailbox: mpsc::Receiver<Message>,
    pub state: HashMap<String, Value>, // Local variables (SW)
}

#[derive(Debug, Clone)]
pub struct Message {
    pub name: String,
    pub args: Vec<Value>,
    #[allow(dead_code)]
    pub reply_to: Option<mpsc::Sender<Value>>, // For ASK
}

impl Actor {
    pub fn new(definition: IrActor, mailbox: mpsc::Receiver<Message>) -> Self {
        Actor {
            definition,
            mailbox,
            state: HashMap::new(),
        }
    }

    pub async fn run(&mut self) {
        println!("Actor {} started", self.definition.name);
        while let Some(msg) = self.mailbox.recv().await {
            println!("Actor {} received message: {}", self.definition.name, msg.name);
            self.handle_message(msg).await;
        }
    }

    async fn handle_message(&mut self, msg: Message) {
        // Find handler
        let handler = self.definition.handlers.iter().find(|h| h.name == msg.name).cloned();
        
        if let Some(handler) = handler {
            self.execute_handler(&handler, msg.args).await;
        } else {
            println!("Handler {} not found in actor {}", msg.name, self.definition.name);
        }
    }

    async fn execute_handler(&mut self, handler: &IrHandler, args: Vec<Value>) {
        let mut locals: HashMap<String, Value> = HashMap::new();
        
        // HACK: Init args
        if handler.name == "receive" || handler.name == "print" {
             if let Some(arg) = args.get(0) {
                 locals.insert("msg".to_string(), arg.clone());
             }
        }

        let mut current_block_idx = 0;
        
        loop {
            if current_block_idx >= handler.blocks.len() {
                break;
            }
            let block = &handler.blocks[current_block_idx];
            
            for instr in &block.instrs {
                match instr {
                    Instruction::Assign(var, val) => {
                        let v = self.resolve_value(val, &locals);
                        locals.insert(var.clone(), v);
                    }
                    Instruction::BinOp { result, op, lhs, rhs } => {
                        let l = self.resolve_value(lhs, &locals);
                        let r = self.resolve_value(rhs, &locals);
                        let res = self.eval_bin_op(op, l, r);
                        locals.insert(result.clone(), res);
                    }
                    Instruction::CallPure { result, func, args } => {
                         // ... existing pure calls if any ...
                    }
                    Instruction::CallIo { result, func, args } => {
                        if func == "print" {
                            let val = self.resolve_value(&args[0], &locals);
                            if let Value::ConstString(s) = val {
                                println!(">> SYNOTRA IO: {}", s);
                            } else if let Value::ConstInt(i) = val {
                                println!(">> SYNOTRA IO: {}", i);
                            }
                            locals.insert(result.clone(), Value::ConstInt(0));
                        }
                    }
                    _ => {}
                }
            }
            
            match &block.terminator {
                Terminator::Return(_) => break,
                Terminator::Jump(target) => {
                    current_block_idx = *target;
                }
                Terminator::JumpCond(cond, true_target, false_target) => {
                    let c = self.resolve_value(cond, &locals);
                    let is_true = match c {
                        Value::ConstInt(i) => i != 0,
                        _ => false,
                    };
                    current_block_idx = if is_true { *true_target } else { *false_target };
                }
            }
        }
    }

    fn eval_bin_op(&self, op: &str, lhs: Value, rhs: Value) -> Value {
        match (op, lhs, rhs) {
            ("add", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l + r),
            ("add", Value::ConstString(l), Value::ConstString(r)) => Value::ConstString(format!("{}{}", l, r)),
            ("sub", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l - r),
            ("mul", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l * r),
            ("div", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l / r),
            ("eq", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l == r { 1 } else { 0 }),
            ("ne", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l != r { 1 } else { 0 }),
            ("lt", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l < r { 1 } else { 0 }),
            ("le", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l <= r { 1 } else { 0 }),
            ("gt", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l > r { 1 } else { 0 }),
            ("ge", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(if l >= r { 1 } else { 0 }),
            _ => Value::ConstInt(0), // Error or mismatch
        }
    }

    fn resolve_value(&self, val: &Value, locals: &HashMap<String, Value>) -> Value {
        match val {
            Value::ConstInt(_) | Value::ConstString(_) => val.clone(),
            Value::Var(name) => {
                if let Some(v) = locals.get(name) {
                    v.clone()
                } else if let Some(v) = self.state.get(name) {
                    v.clone()
                } else {
                    Value::ConstString(format!("Undefined({})", name))
                }
            }
        }
    }
}
