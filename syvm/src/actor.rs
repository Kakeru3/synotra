use crate::bytecode::*;
use std::collections::HashMap;
use std::sync::{mpsc, Arc, Mutex, RwLock};

pub struct Actor {
    pub definition: IrActor,
    pub mailbox: mpsc::Receiver<Message>,
    pub state: HashMap<String, Value>, // Local variables (SW)
    pub router: Arc<RwLock<HashMap<String, mpsc::Sender<Message>>>>,
    pub shutdown_signal: mpsc::Sender<()>,
}

#[derive(Debug, Clone)]
pub struct Message {
    pub name: String,
    pub args: Vec<Value>,
    #[allow(dead_code)]
    pub reply_to: Option<mpsc::Sender<Value>>, // For ASK
}

impl Actor {
    pub fn new(definition: IrActor, mailbox: mpsc::Receiver<Message>, router: Arc<RwLock<HashMap<String, mpsc::Sender<Message>>>>, shutdown_signal: mpsc::Sender<()>) -> Self {
        Actor {
            definition,
            mailbox,
            state: HashMap::new(),
            router,
            shutdown_signal,
        }
    }

    pub fn run(&mut self) {
        println!("Actor {} started", self.definition.name);
        while let Ok(msg) = self.mailbox.recv() {
            println!("Actor {} received message: {}", self.definition.name, msg.name);
            self.handle_message(msg);
        }
    }

    fn handle_message(&mut self, msg: Message) {
        // Find handler
        let handler = self.definition.handlers.iter().find(|h| h.name == msg.name).cloned();
        
        if let Some(handler) = handler {
            self.execute_handler(&handler, msg.args, msg.reply_to);
        } else {
            println!("Handler {} not found in actor {}", msg.name, self.definition.name);
            // If reply is expected, send default value
            if let Some(reply_tx) = msg.reply_to {
                let _ = reply_tx.send(Value::ConstInt(0));
            }
        }
    }

    fn execute_handler(&mut self, handler: &IrHandler, args: Vec<Value>, reply_to: Option<mpsc::Sender<Value>>) {
        // Initialize locals with default values (0)
        let mut locals: Vec<Value> = vec![Value::ConstInt(0); handler.local_count];
        
        // Bind arguments to parameters
        // Params are mapped to the first N locals
        for (i, _) in handler.params.iter().enumerate() {
            if let Some(arg_val) = args.get(i) {
                locals[i] = arg_val.clone();
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
                    Instruction::Assign(idx, val) => {
                        // Handle different assignment cases:
                        // 1. Local(x) where locals[x] is Future -> copy Future as-is (async)
                        // 2. Local(x) where locals[x] is value -> resolve to avoid aliasing
                        // 3. Future -> copy Future as-is (async)
                        // 4. Const -> resolve (no-op for constants)
                        let v = match val {
                            Value::Local(src_idx) => {
                                if let Some(src_val) = locals.get(*src_idx) {
                                    if matches!(src_val, Value::Future(_)) {
                                        src_val.clone()  // Preserve Future for async
                                    } else {
                                        self.resolve_value(val, &locals)  // Resolve to avoid aliasing
                                    }
                                } else {
                                    Value::ConstInt(0)
                                }
                            }
                            Value::Future(_) => val.clone(),  // Direct Future assignment
                            _ => self.resolve_value(val, &locals),  // Resolve constants/etc
                        };
                        locals[*idx] = v;
                    }
                    Instruction::BinOp { result, op, lhs, rhs } => {
                        let l = self.resolve_value(lhs, &locals);
                        let r = self.resolve_value(rhs, &locals);
                        let res = self.eval_bin_op(op, l, r);
                        locals[*result] = res;
                    }
                    Instruction::CallPure { result, func: _, args: _ } => {
                         // ... existing pure calls if any ...
                         // Still unimplemented, but result is now an index
                         locals[*result] = Value::ConstInt(0);
                    }
                    Instruction::CallIo { result, func, args } => {
                        if func == "print" {
                            let val = self.resolve_value(&args[0], &locals);
                            if let Value::ConstString(s) = val {
                                println!(">> SYNOTRA IO: {}", s);
                            } else if let Value::ConstInt(i) = val {
                                println!(">> SYNOTRA IO: {}", i);
                            }
                            locals[*result] = Value::ConstInt(0);
                        }
                    }
                    Instruction::Send { target, msg, args } => {
                        let target_val = self.resolve_value(target, &locals);
                        let msg_val = self.resolve_value(msg, &locals);
                        let arg_vals: Vec<Value> = args.iter().map(|a| self.resolve_value(a, &locals)).collect();
                        
                        if let (Value::ConstString(target_name), Value::ConstString(msg_name)) = (target_val, msg_val) {
                            let router = self.router.read().unwrap();
                            if let Some(tx) = router.get(&target_name) {
                                let msg = Message {
                                    name: msg_name, 
                                    args: arg_vals, 
                                    reply_to: None,
                                };
                                // Unbounded channel send is non-blocking, so we can do it synchronously
                                if let Err(e) = tx.send(msg) {
                                    eprintln!("Runtime Error: Failed to send message to {}: {}", target_name, e);
                                }
                            } else {
                                println!("Runtime: Actor {} not found", target_name);
                            }
                        }
                    }
                    Instruction::Ask { result, target, msg, args } => {
                        let target_val = self.resolve_value(target, &locals);
                        let msg_val = self.resolve_value(msg, &locals);
                        let arg_vals: Vec<Value> = args.iter().map(|a| self.resolve_value(a, &locals)).collect();
                        
                        if let (Value::ConstString(target_name), Value::ConstString(msg_name)) = (target_val, msg_val) {
                            let router = self.router.read().unwrap();
                            if let Some(tx) = router.get(&target_name) {
                                // Create one-shot channel for reply
                                let (reply_tx, reply_rx) = mpsc::channel();
                                
                                let msg = Message {
                                    name: msg_name.clone(), 
                                    args: arg_vals, 
                                    reply_to: Some(reply_tx),
                                };
                                
                                // Send message
                                if let Err(e) = tx.send(msg) {
                                    eprintln!("Runtime Error: Failed to send ask to {}: {}", target_name, e);
                                    locals[*result] = Value::ConstInt(0);
                                } else {
                                    // Async: Return Future immediately
                                    let future = Value::Future(RuntimeFuture(Arc::new(Mutex::new(FutureState::Pending(reply_rx)))));
                                    locals[*result] = future;
                                }
                            } else {
                                println!("Runtime: Actor {} not found", target_name);
                                locals[*result] = Value::ConstInt(0);
                            }
                        } else {
                            locals[*result] = Value::ConstInt(0);
                        }
                    }
                    Instruction::Exit => {
                        // Signal shutdown
                        let _ = self.shutdown_signal.send(());
                    }
                    _ => {}
                }
            }
            
            match &block.terminator {
                Terminator::Return(val_opt) => {
                    // If there's a reply_to channel, send the return value
                    if let Some(reply_tx) = reply_to {
                        let return_val = if let Some(val) = val_opt {
                            self.resolve_value(val, &locals)
                        } else {
                            Value::ConstInt(0) // Default return value
                        };
                        let _ = reply_tx.send(return_val);
                    }
                    break;
                }
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

    fn resolve_value(&self, val: &Value, locals: &Vec<Value>) -> Value {
        match val {
            Value::ConstInt(i) => Value::ConstInt(*i),
            Value::ConstString(s) => Value::ConstString(s.clone()),
            Value::Local(idx) => {
                if let Some(v) = locals.get(*idx) {
                    self.resolve_value(v, locals)  // Always recursively resolve
                } else {
                    Value::ConstInt(0) // Should not happen if compiled correctly
                }
            }
            Value::Future(rt_future) => {
                let mut state = rt_future.0.lock().unwrap();
                let val = match &*state {
                    FutureState::Resolved(v) => *v.clone(),
                    FutureState::Pending(rx) => {
                        match rx.recv() {
                            Ok(v) => v,
                            Err(_) => Value::ConstInt(0),
                        }
                    }
                };
                *state = FutureState::Resolved(Box::new(val.clone()));
                val
            }
        }
    }
}
