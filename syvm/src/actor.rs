use crate::bytecode::*;
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{mpsc, Arc, Condvar, Mutex, RwLock};

pub struct Actor {
    pub definition: IrActor,
    pub mailbox: mpsc::Receiver<Message>,
    pub state: HashMap<String, Value>, // Local variables (SW)
    pub router: Arc<RwLock<HashMap<String, mpsc::Sender<Message>>>>,
    pub shutdown_signal: mpsc::Sender<()>,
    pub handlers: HashMap<String, IrHandler>, // Local function handlers
    pub busy_count: Arc<AtomicUsize>,
    pub completion_cond: Arc<(Mutex<bool>, Condvar)>,
}

#[derive(Debug, Clone)]
pub struct Message {
    pub name: String,
    pub args: Vec<Value>,
    #[allow(dead_code)]
    pub reply_to: Option<mpsc::Sender<Value>>, // For ASK
}

impl Actor {
    pub fn new(
        definition: IrActor,
        rx: mpsc::Receiver<Message>,
        router: Arc<RwLock<HashMap<String, mpsc::Sender<Message>>>>,
        shutdown_signal: mpsc::Sender<()>,
        busy_count: Arc<AtomicUsize>,
        completion_cond: Arc<(Mutex<bool>, Condvar)>,
    ) -> Self {
        // Build function handlers map from definition
        let mut handlers = HashMap::new();
        for handler in &definition.handlers {
            handlers.insert(handler.name.clone(), handler.clone());
        }

        Actor {
            definition,
            mailbox: rx,
            state: HashMap::new(),
            router,
            shutdown_signal,
            handlers,
            busy_count,
            completion_cond,
        }
    }

    pub fn run(&mut self) {
        println!("Actor {} started", self.definition.name);
        while let Ok(msg) = self.mailbox.recv() {
            println!(
                "Actor {} received message: {}",
                self.definition.name, msg.name
            );
            self.handle_message(msg);
        }
    }

    fn handle_message(&mut self, msg: Message) {
        // Find handler
        let handler = self
            .definition
            .handlers
            .iter()
            .find(|h| h.name == msg.name)
            .cloned();

        if let Some(handler) = handler {
            // Process message
            self.execute_handler(&handler, msg.args, msg.reply_to);

            // Decrement busy count
            let prev = self.busy_count.fetch_sub(1, Ordering::SeqCst);
            if prev == 1 {
                // Count dropped to 0, signal completion
                let (lock, cvar) = &*self.completion_cond;
                let mut finished = lock.lock().unwrap();
                *finished = true;
                cvar.notify_all();
            }
        } else {
            println!(
                "Handler {} not found in actor {}",
                msg.name, self.definition.name
            );
            // If reply is expected, send default value
            if let Some(reply_tx) = msg.reply_to {
                let _ = reply_tx.send(Value::ConstInt(0));
            }
        }
    }

    fn execute_handler(
        &mut self,
        handler: &IrHandler,
        args: Vec<Value>,
        reply_to: Option<mpsc::Sender<Value>>,
    ) {
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
                                        src_val.clone() // Preserve Future for async
                                    } else {
                                        self.resolve_value(val, &locals) // Resolve to avoid aliasing
                                    }
                                } else {
                                    Value::ConstInt(0)
                                }
                            }
                            Value::Future(_) => val.clone(), // Direct Future assignment
                            _ => self.resolve_value(val, &locals), // Resolve constants/etc
                        };
                        locals[*idx] = v;
                    }
                    Instruction::BinOp {
                        result,
                        op,
                        lhs,
                        rhs,
                    } => {
                        let l = self.resolve_value(lhs, &locals);
                        let r = self.resolve_value(rhs, &locals);
                        let res = self.eval_bin_op(op, l, r);
                        locals[*result] = res;
                    }
                    Instruction::CallMethod {
                        result,
                        target,
                        method,
                        args,
                    } => {
                        // eprintln!("DEBUG: CallMethod {} on target {}", method, target);
                        // Evaluate arguments
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();
                        // Resolve target
                        let target_val = self.resolve_value(&Value::Local(*target), &locals);

                        let return_val = match target_val {
                            Value::List(mut list) => {
                                match method.as_str() {
                                    "add" => {
                                        if let Some(arg) = arg_vals.first() {
                                            list.push(arg.clone());
                                            locals[*target] = Value::List(list); // Update the local
                                            Value::ConstBool(true)
                                        } else {
                                            eprintln!(
                                                "Runtime Error: List.add requires an argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "get" => {
                                        if let Some(Value::ConstInt(idx)) = arg_vals.first() {
                                            if *idx >= 0 && (*idx as usize) < list.len() {
                                                list[*idx as usize].clone()
                                            } else {
                                                eprintln!(
                                                    "Runtime Error: List index out of bounds: {}",
                                                    idx
                                                );
                                                Value::ConstInt(0)
                                            }
                                        } else {
                                            eprintln!(
                                                "Runtime Error: List.get requires an integer index"
                                            );
                                            Value::ConstInt(0)
                                        }
                                    }
                                    "size" => Value::ConstInt(list.len() as i64),
                                    "isEmpty" => Value::ConstBool(list.is_empty()),
                                    "addAll" => {
                                        if let Some(Value::List(other)) = arg_vals.first() {
                                            list.extend(other.clone());
                                            locals[*target] = Value::List(list);
                                            Value::ConstBool(true)
                                        } else {
                                            eprintln!("Runtime Error: List.addAll requires a List argument");
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "clear" => {
                                        list.clear();
                                        locals[*target] = Value::List(list);
                                        Value::ConstBool(true)
                                    }
                                    _ => {
                                        eprintln!(
                                            "Runtime Error: Unknown List method '{}'",
                                            method
                                        );
                                        Value::ConstInt(0)
                                    }
                                }
                            }
                            Value::Map(mut map) => {
                                match method.as_str() {
                                    "put" => {
                                        if let (Some(key), Some(val)) =
                                            (arg_vals.first(), arg_vals.get(1))
                                        {
                                            map.insert(key.clone(), val.clone());
                                            locals[*target] = Value::Map(map); // Update the local
                                            Value::ConstBool(true)
                                        } else {
                                            eprintln!("Runtime Error: Map.put requires key and value arguments");
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "get" => {
                                        if let Some(key) = arg_vals.first() {
                                            if let Some(val) = map.get(key) {
                                                val.clone()
                                            } else {
                                                Value::ConstInt(0) // Return 0/Null if key not found
                                            }
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Map.get requires a key argument"
                                            );
                                            Value::ConstInt(0)
                                        }
                                    }
                                    "remove" => {
                                        if let Some(key) = arg_vals.first() {
                                            if let Some(val) = map.remove(key) {
                                                locals[*target] = Value::Map(map); // Update the local
                                                val
                                            } else {
                                                Value::ConstBool(false) // Return false if key not found? Or null?
                                            }
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Map.remove requires a key argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "size" => Value::ConstInt(map.len() as i64),
                                    "containsKey" => {
                                        if let Some(key) = arg_vals.first() {
                                            Value::ConstBool(map.contains_key(key))
                                        } else {
                                            eprintln!("Runtime Error: Map.containsKey requires a key argument");
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "keys" => {
                                        let mut keys = Vec::new();
                                        for k in map.keys() {
                                            keys.push(k.clone());
                                        }
                                        Value::List(keys)
                                    }
                                    "values" => {
                                        let mut vals = Vec::new();
                                        for v in map.values() {
                                            vals.push(v.clone());
                                        }
                                        Value::List(vals)
                                    }
                                    "putAll" | "addAll" => {
                                        if let Some(Value::Map(other)) = arg_vals.first() {
                                            for (k, v) in other {
                                                map.insert(k.clone(), v.clone());
                                            }
                                            locals[*target] = Value::Map(map);
                                            Value::ConstBool(true)
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Map.putAll requires a Map argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "clear" => {
                                        map.clear();
                                        locals[*target] = Value::Map(map);
                                        Value::ConstBool(true)
                                    }
                                    "entrySet" => {
                                        let mut entries = Vec::new();
                                        for (k, v) in &map {
                                            entries.push(Value::Entry(
                                                Box::new(k.clone()),
                                                Box::new(v.clone()),
                                            ));
                                        }
                                        Value::List(entries)
                                    }
                                    _ => {
                                        eprintln!("Runtime Error: Unknown Map method '{}'", method);
                                        Value::ConstInt(0)
                                    }
                                }
                            }
                            Value::Set(mut set) => {
                                match method.as_str() {
                                    "add" => {
                                        if let Some(val) = arg_vals.first() {
                                            let inserted = set.insert(val.clone());
                                            locals[*target] = Value::Set(set); // Update the local
                                            Value::ConstBool(inserted)
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Set.add requires an argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "remove" => {
                                        if let Some(val) = arg_vals.first() {
                                            let removed = set.remove(val);
                                            locals[*target] = Value::Set(set); // Update the local
                                            Value::ConstBool(removed)
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Set.remove requires an argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "contains" => {
                                        if let Some(val) = arg_vals.first() {
                                            Value::ConstBool(set.contains(val))
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Set.contains requires an argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "size" => Value::ConstInt(set.len() as i64),
                                    "values" => {
                                        let mut vals = Vec::new();
                                        for v in &set {
                                            vals.push(v.clone());
                                        }
                                        Value::List(vals)
                                    }
                                    "addAll" => {
                                        if let Some(Value::Set(other)) = arg_vals.first() {
                                            set.extend(other.clone());
                                            locals[*target] = Value::Set(set);
                                            Value::ConstBool(true)
                                        } else {
                                            eprintln!(
                                                "Runtime Error: Set.addAll requires a Set argument"
                                            );
                                            Value::ConstBool(false)
                                        }
                                    }
                                    "clear" => {
                                        set.clear();
                                        locals[*target] = Value::Set(set);
                                        Value::ConstBool(true)
                                    }
                                    _ => {
                                        eprintln!("Runtime Error: Unknown Set method '{}'", method);
                                        Value::ConstInt(0)
                                    }
                                }
                            }
                            Value::Entry(k, v) => match method.as_str() {
                                "key" => *k.clone(),
                                "value" => *v.clone(),
                                _ => {
                                    eprintln!("Runtime Error: Unknown Entry method '{}'", method);
                                    Value::ConstInt(0)
                                }
                            },
                            _ => {
                                eprintln!(
                                    "Runtime Error: Method call on non-collection type: {:?}",
                                    target_val
                                );
                                Value::ConstInt(0)
                            }
                        };
                        locals[*result] = return_val;
                    }
                    Instruction::CallPure { result, func, args } => {
                        // Evaluate arguments
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();

                        // Check for built-in collection constructors
                        if func == "List.new" {
                            locals[*result] = Value::List(Vec::new());
                        } else if func == "MutableMap.new" {
                            locals[*result] = Value::Map(HashMap::new());
                        } else if func == "MutableSet.new" {
                            locals[*result] = Value::Set(HashSet::new());
                        } else if let Some(handler) = self.handlers.get(func).cloned() {
                            // Execute function synchronously
                            let return_val = self.execute_function(&handler, arg_vals);
                            locals[*result] = return_val;
                        } else {
                            // Function not found
                            eprintln!("Runtime Error: Function '{}' not found", func);
                            locals[*result] = Value::ConstInt(0);
                        }
                    }
                    Instruction::CallIo { result, func, args } => {
                        if func == "print" || func == "println" {
                            let val = self.resolve_value(&args[0], &locals);
                            match val {
                                Value::ConstString(s) => {
                                    if func == "print" {
                                        print!("{}", s);
                                        let _ = std::io::stdout().flush();
                                    } else {
                                        println!("{}", s);
                                    }
                                }
                                Value::ConstInt(i) => {
                                    if func == "print" {
                                        print!("{}", i);
                                        let _ = std::io::stdout().flush();
                                    } else {
                                        println!("{}", i);
                                    }
                                }
                                Value::ConstBool(b) => {
                                    if func == "print" {
                                        print!("{}", b);
                                        let _ = std::io::stdout().flush();
                                    } else {
                                        println!("{}", b);
                                    }
                                }
                                _ => {}
                            }
                            locals[*result] = Value::ConstInt(0);
                        }
                    }

                    Instruction::Send { target, msg, args } => {
                        let target_val = self.resolve_value(target, &locals);
                        let msg_val = self.resolve_value(msg, &locals);
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();

                        if let (Value::ConstString(target_name), Value::ConstString(msg_name)) =
                            (target_val, msg_val)
                        {
                            let router = self.router.read().unwrap();
                            if let Some(tx) = router.get(&target_name) {
                                let msg = Message {
                                    name: msg_name,
                                    args: arg_vals,
                                    reply_to: None,
                                };

                                // Increment busy count
                                self.busy_count.fetch_add(1, Ordering::SeqCst);

                                // Unbounded channel send is non-blocking, so we can do it synchronously
                                if let Err(e) = tx.send(msg) {
                                    eprintln!(
                                        "Runtime Error: Failed to send message to {}: {}",
                                        target_name, e
                                    );
                                    // Decrement if send failed
                                    self.busy_count.fetch_sub(1, Ordering::SeqCst);
                                }
                            } else {
                                println!("Runtime: Actor {} not found", target_name);
                            }
                        }
                    }
                    Instruction::Ask {
                        result,
                        target,
                        msg,
                        args,
                    } => {
                        let target_val = self.resolve_value(target, &locals);
                        let msg_val = self.resolve_value(msg, &locals);
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();

                        if let (Value::ConstString(target_name), Value::ConstString(msg_name)) =
                            (target_val, msg_val)
                        {
                            let router = self.router.read().unwrap();
                            if let Some(tx) = router.get(&target_name) {
                                // Create one-shot channel for reply
                                let (reply_tx, reply_rx) = mpsc::channel();

                                let msg = Message {
                                    name: msg_name.clone(),
                                    args: arg_vals,
                                    reply_to: Some(reply_tx),
                                };

                                // Increment busy count
                                self.busy_count.fetch_add(1, Ordering::SeqCst);

                                // Send message
                                if let Err(e) = tx.send(msg) {
                                    eprintln!(
                                        "Runtime Error: Failed to send ask to {}: {}",
                                        target_name, e
                                    );
                                    self.busy_count.fetch_sub(1, Ordering::SeqCst);
                                    locals[*result] = Value::ConstInt(0);
                                } else {
                                    // Async: Return Future immediately
                                    let future = Value::Future(RuntimeFuture(Arc::new(
                                        Mutex::new(FutureState::Pending(reply_rx)),
                                    )));
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
                    Instruction::CreateMessage {
                        result,
                        type_name,
                        field_values,
                    } => {
                        // Resolve all field values
                        let mut fields = HashMap::new();
                        for (field_name, val) in field_values.iter() {
                            let resolved = self.resolve_value(val, &locals);
                            fields.insert(field_name.clone(), Box::new(resolved));
                        }

                        // Create the message value
                        let msg_val = Value::Message {
                            type_name: type_name.clone(),
                            fields,
                        };

                        locals[*result] = msg_val;
                    }
                    Instruction::GetField {
                        result,
                        target,
                        field_name,
                    } => {
                        let target_val = self.resolve_value(target, &locals);

                        let field_val = match &target_val {
                            Value::Message { fields, .. } => {
                                // Try to get the field by name
                                fields
                                    .get(field_name)
                                    .map(|boxed_val| *boxed_val.clone())
                                    .unwrap_or(Value::ConstInt(0))
                            }
                            _ => {
                                println!(
                                    "Warning: GetField on non-message value: {:?}",
                                    target_val
                                );
                                Value::ConstInt(0) // Target is not a message
                            }
                        };

                        locals[*result] = field_val;
                    }
                    Instruction::Spawn {
                        result,
                        actor_type,
                        args,
                    } => {
                        // TODO: Implement spawn - requires runtime integration
                        // For now, create a placeholder ActorRef
                        let actor_id = format!("{}_placeholder", actor_type);
                        locals[*result] = Value::ActorRef(actor_id);
                    }
                    Instruction::Exit => {
                        // Signal shutdown
                        let _ = self.shutdown_signal.send(());
                    }
                    Instruction::SwLoad {
                        result,
                        collection,
                        index,
                    } => {
                        // Resolve index first
                        let idx_val = self.resolve_value(&Value::Local(*index), &locals);
                        let col_val = &locals[*collection];

                        match col_val {
                            Value::List(list) => {
                                if let Value::ConstInt(i) = idx_val {
                                    if i >= 0 && (i as usize) < list.len() {
                                        locals[*result] = list[i as usize].clone();
                                    } else {
                                        eprintln!("Runtime Error: List index out of bounds: {}", i);
                                        locals[*result] = Value::ConstInt(0);
                                    }
                                } else {
                                    eprintln!("Runtime Error: List index must be Int");
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            Value::Map(map) => {
                                if let Some(val) = map.get(&idx_val) {
                                    locals[*result] = val.clone();
                                } else {
                                    // Return 0/Null if key not found
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            _ => {
                                eprintln!("Runtime Error: SwLoad on non-collection");
                                locals[*result] = Value::ConstInt(0);
                            }
                        }
                    }
                    Instruction::SwStore {
                        collection,
                        index,
                        value,
                    } => {
                        let idx_val = self.resolve_value(&Value::Local(*index), &locals);
                        let val_val = self.resolve_value(&Value::Local(*value), &locals);

                        match &mut locals[*collection] {
                            Value::List(list) => {
                                if let Value::ConstInt(idx) = idx_val {
                                    let idx = idx as usize;
                                    if idx < list.len() {
                                        list[idx] = val_val;
                                    } else {
                                        eprintln!(
                                            "Runtime Error: List index out of bounds: {} (len: {})",
                                            idx,
                                            list.len()
                                        );
                                    }
                                } else {
                                    eprintln!("Runtime Error: List index must be an integer");
                                }
                            }
                            Value::Map(map) => {
                                map.insert(idx_val, val_val);
                            }
                            _ => {
                                eprintln!("Runtime Error: SwStore on non-collection");
                            }
                        }
                    }
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
            ("add", Value::ConstString(l), Value::ConstString(r)) => {
                Value::ConstString(format!("{}{}", l, r))
            }
            ("add", Value::ConstString(l), Value::ConstInt(r)) => {
                Value::ConstString(format!("{}{}", l, r))
            }
            ("add", Value::ConstInt(l), Value::ConstString(r)) => {
                Value::ConstString(format!("{}{}", l, r))
            }
            ("add", Value::ConstString(l), Value::ConstBool(r)) => {
                Value::ConstString(format!("{}{}", l, r))
            }
            ("add", Value::ConstBool(l), Value::ConstString(r)) => {
                Value::ConstString(format!("{}{}", l, r))
            }
            ("sub", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l - r),
            ("mul", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l * r),
            ("div", Value::ConstInt(l), Value::ConstInt(r)) => Value::ConstInt(l / r),
            ("eq", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l == r { 1 } else { 0 })
            }
            ("ne", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l != r { 1 } else { 0 })
            }
            ("lt", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l < r { 1 } else { 0 })
            }
            ("le", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l <= r { 1 } else { 0 })
            }
            ("gt", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l > r { 1 } else { 0 })
            }
            ("ge", Value::ConstInt(l), Value::ConstInt(r)) => {
                Value::ConstInt(if l >= r { 1 } else { 0 })
            }
            _ => Value::ConstInt(0), // Error or mismatch
        }
    }

    fn resolve_value(&self, val: &Value, locals: &Vec<Value>) -> Value {
        match val {
            Value::ConstInt(i) => Value::ConstInt(*i),
            Value::ConstString(s) => Value::ConstString(s.clone()),
            Value::ConstBool(b) => Value::ConstBool(*b),
            Value::Local(idx) => {
                if let Some(v) = locals.get(*idx) {
                    self.resolve_value(v, locals) // Always recursively resolve
                } else {
                    Value::ConstInt(0) // Should not happen if compiled correctly
                }
            }
            Value::Future(rt_future) => {
                let mut state = rt_future.0.lock().unwrap();
                let val = match &*state {
                    FutureState::Resolved(v) => *v.clone(),
                    FutureState::Pending(rx) => match rx.recv() {
                        Ok(v) => v,
                        Err(_) => Value::ConstInt(0),
                    },
                };
                *state = FutureState::Resolved(Box::new(val.clone()));
                val
            }
            Value::List(list) => Value::List(list.clone()),
            Value::Map(map) => Value::Map(map.clone()),
            Value::Set(s) => Value::Set(s.clone()),
            Value::Message { type_name, fields } => Value::Message {
                type_name: type_name.clone(),
                fields: fields.clone(),
            },
            Value::ActorRef(id) => Value::ActorRef(id.clone()),
            Value::Entry(k, v) => {
                let resolved_k = self.resolve_value(k, locals);
                let resolved_v = self.resolve_value(v, locals);
                Value::Entry(Box::new(resolved_k), Box::new(resolved_v))
            }
        }
    }

    fn execute_function(&mut self, handler: &IrHandler, args: Vec<Value>) -> Value {
        // Execute a function synchronously and return its result
        // Similar to execute_handler but without reply_to channel

        // Initialize locals with default values
        let mut locals: Vec<Value> = vec![Value::ConstInt(0); handler.local_count];

        // Bind arguments to parameters (first N locals)
        for (i, _) in handler.params.iter().enumerate() {
            if let Some(arg_val) = args.get(i) {
                locals[i] = arg_val.clone();
            }
        }

        let mut current_block_idx = 0;
        let mut return_value = Value::ConstInt(0); // Default return value

        loop {
            if current_block_idx >= handler.blocks.len() {
                break;
            }
            let block = &handler.blocks[current_block_idx];

            for instr in &block.instrs {
                match instr {
                    Instruction::Assign(idx, val) => {
                        let v = match val {
                            Value::Local(src_idx) => {
                                if let Some(src_val) = locals.get(*src_idx) {
                                    if matches!(src_val, Value::Future(_)) {
                                        src_val.clone()
                                    } else {
                                        self.resolve_value(val, &locals)
                                    }
                                } else {
                                    Value::ConstInt(0)
                                }
                            }
                            Value::Future(_) => val.clone(),
                            _ => self.resolve_value(val, &locals),
                        };
                        locals[*idx] = v;
                    }
                    Instruction::BinOp {
                        result,
                        op,
                        lhs,
                        rhs,
                    } => {
                        let l = self.resolve_value(lhs, &locals);
                        let r = self.resolve_value(rhs, &locals);
                        let res = self.eval_bin_op(op, l, r);
                        locals[*result] = res;
                    }
                    Instruction::CallPure { result, func, args } => {
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();

                        if func == "List.new" {
                            locals[*result] = Value::List(Vec::new());
                        } else if func == "MutableMap.new" {
                            locals[*result] = Value::Map(HashMap::new());
                        } else if func == "MutableSet.new" {
                            locals[*result] = Value::Set(HashSet::new());
                        } else if let Some(handler) = self.handlers.get(func).cloned() {
                            let return_val = self.execute_function(&handler, arg_vals);
                            locals[*result] = return_val;
                        } else {
                            // Function not found
                            eprintln!("Runtime Error: Function '{}' not found", func);
                            locals[*result] = Value::ConstInt(0);
                        }
                    }
                    Instruction::SwLoad {
                        result,
                        collection,
                        index,
                    } => {
                        // Resolve index first
                        let idx_val = self.resolve_value(&Value::Local(*index), &locals);
                        let col_val = &locals[*collection];

                        match col_val {
                            Value::List(list) => {
                                if let Value::ConstInt(i) = idx_val {
                                    if i >= 0 && (i as usize) < list.len() {
                                        locals[*result] = list[i as usize].clone();
                                    } else {
                                        eprintln!("Runtime Error: List index out of bounds: {}", i);
                                        locals[*result] = Value::ConstInt(0);
                                    }
                                } else {
                                    eprintln!("Runtime Error: List index must be Int");
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            Value::Map(map) => {
                                if let Some(val) = map.get(&idx_val) {
                                    locals[*result] = val.clone();
                                } else {
                                    // Return 0/Null if key not found
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            _ => {
                                eprintln!("Runtime Error: SwLoad on non-collection");
                                locals[*result] = Value::ConstInt(0);
                            }
                        }
                    }
                    Instruction::SwStore {
                        collection,
                        index,
                        value,
                    } => {
                        let idx_val = self.resolve_value(&Value::Local(*index), &locals);
                        let val_val = self.resolve_value(&Value::Local(*value), &locals);

                        match &mut locals[*collection] {
                            Value::List(_) => {
                                eprintln!("Runtime Error: Cannot assign to immutable List");
                            }
                            Value::Map(map) => {
                                map.insert(idx_val, val_val);
                            }
                            _ => {
                                eprintln!("Runtime Error: SwStore on non-collection");
                            }
                        }
                    }
                    Instruction::CallMethod {
                        result,
                        target,
                        method,
                        args,
                    } => {
                        // Evaluate arguments
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.resolve_value(a, &locals))
                            .collect();

                        // Get mutable reference to target collection
                        let target_val = &mut locals[*target];

                        match target_val {
                            Value::List(list) => {
                                if method == "add" {
                                    if arg_vals.len() == 1 {
                                        list.push(arg_vals[0].clone());
                                        eprintln!(
                                            "DEBUG: List.add called. New size: {}",
                                            list.len()
                                        );
                                        locals[*result] = Value::ConstBool(true);
                                    } else {
                                        eprintln!("Runtime Error: List.add takes 1 argument");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "get" {
                                    if arg_vals.len() == 1 {
                                        if let Value::ConstInt(idx) = arg_vals[0] {
                                            if idx >= 0 && (idx as usize) < list.len() {
                                                locals[*result] = list[idx as usize].clone();
                                            } else {
                                                eprintln!(
                                                    "Runtime Error: List index out of bounds: {}",
                                                    idx
                                                );
                                                locals[*result] = Value::ConstInt(0);
                                            }
                                        } else {
                                            eprintln!("Runtime Error: List index must be Int");
                                            locals[*result] = Value::ConstInt(0);
                                        }
                                    } else {
                                        eprintln!("Runtime Error: List.get takes 1 argument");
                                        locals[*result] = Value::ConstInt(0);
                                    }
                                } else if method == "size" {
                                    locals[*result] = Value::ConstInt(list.len() as i64);
                                } else if method == "isEmpty" {
                                    locals[*result] = Value::ConstBool(list.is_empty());
                                } else {
                                    eprintln!("Runtime Error: Unknown List method '{}'", method);
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            Value::Map(map) => {
                                if method == "put" {
                                    if arg_vals.len() == 2 {
                                        map.insert(arg_vals[0].clone(), arg_vals[1].clone());
                                        locals[*result] = Value::ConstBool(true);
                                    } else {
                                        eprintln!("Runtime Error: Map.put takes 2 arguments");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "get" {
                                    if arg_vals.len() == 1 {
                                        if let Some(val) = map.get(&arg_vals[0]) {
                                            locals[*result] = val.clone();
                                        } else {
                                            locals[*result] = Value::ConstInt(0);
                                        }
                                    } else {
                                        eprintln!("Runtime Error: Map.get takes 1 argument");
                                        locals[*result] = Value::ConstInt(0);
                                    }
                                } else if method == "remove" {
                                    if arg_vals.len() == 1 {
                                        map.remove(&arg_vals[0]);
                                        locals[*result] = Value::ConstBool(true);
                                    } else {
                                        eprintln!("Runtime Error: Map.remove takes 1 argument");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "size" {
                                    locals[*result] = Value::ConstInt(map.len() as i64);
                                } else if method == "containsKey" {
                                    if arg_vals.len() == 1 {
                                        locals[*result] =
                                            Value::ConstBool(map.contains_key(&arg_vals[0]));
                                    } else {
                                        eprintln!(
                                            "Runtime Error: Map.containsKey takes 1 argument"
                                        );
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "keys" {
                                    let mut keys = Vec::new();
                                    for k in map.keys() {
                                        keys.push(k.clone());
                                    }
                                    locals[*result] = Value::List(keys);
                                } else {
                                    eprintln!("Runtime Error: Unknown Map method '{}'", method);
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            Value::Set(set) => {
                                if method == "add" {
                                    if arg_vals.len() == 1 {
                                        let inserted = set.insert(arg_vals[0].clone());
                                        locals[*result] = Value::ConstBool(inserted);
                                    } else {
                                        eprintln!("Runtime Error: Set.add takes 1 argument");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "remove" {
                                    if arg_vals.len() == 1 {
                                        let removed = set.remove(&arg_vals[0]);
                                        locals[*result] = Value::ConstBool(removed);
                                    } else {
                                        eprintln!("Runtime Error: Set.remove takes 1 argument");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "contains" {
                                    if arg_vals.len() == 1 {
                                        locals[*result] =
                                            Value::ConstBool(set.contains(&arg_vals[0]));
                                    } else {
                                        eprintln!("Runtime Error: Set.contains takes 1 argument");
                                        locals[*result] = Value::ConstBool(false);
                                    }
                                } else if method == "size" {
                                    locals[*result] = Value::ConstInt(set.len() as i64);
                                } else {
                                    eprintln!("Runtime Error: Unknown Set method '{}'", method);
                                    locals[*result] = Value::ConstInt(0);
                                }
                            }
                            _ => {
                                eprintln!("Runtime Error: Method call on non-collection value");
                                locals[*result] = Value::ConstInt(0);
                            }
                        }
                    }
                    Instruction::CallIo { result, func, args } => {
                        if func == "print" || func == "println" {
                            let val = self.resolve_value(&args[0], &locals);
                            match val {
                                Value::ConstString(s) => {
                                    if func == "print" {
                                        print!("{}", s);
                                        let _ = std::io::stdout().flush();
                                    } else {
                                        println!("{}", s);
                                    }
                                }
                                Value::ConstInt(i) => {
                                    if func == "print" {
                                        print!("{}", i);
                                        let _ = std::io::stdout().flush();
                                    } else {
                                        println!("{}", i);
                                    }
                                }
                                _ => {}
                            }
                            locals[*result] = Value::ConstInt(0);
                        }
                    }
                    Instruction::Exit => {
                        // Exit is not allowed in pure functions, but handle it anyway
                        return Value::ConstInt(0);
                    }
                    _ => {}
                }
            }

            match &block.terminator {
                Terminator::Return(val_opt) => {
                    return_value = if let Some(val) = val_opt {
                        self.resolve_value(val, &locals)
                    } else {
                        Value::ConstInt(0)
                    };
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

        return_value
    }
}
