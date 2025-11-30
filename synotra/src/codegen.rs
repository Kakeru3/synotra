use crate::ast::BinaryOp;
use crate::ast::*;
use crate::ir::Instruction;
use crate::ir::Terminator;
use crate::ir::*;
use crate::sema::{Symbol, SymbolTable};
use std::collections::HashMap;

pub struct Codegen<'a> {
    blocks: Vec<BasicBlock>,
    current_block_idx: usize,
    locals_map: HashMap<String, usize>,
    next_local_id: usize,
    current_actor: String,
    symbols: &'a SymbolTable,
}

impl<'a> Codegen<'a> {
    pub fn new(actor_name: String, symbols: &'a SymbolTable) -> Self {
        Codegen {
            blocks: Vec::new(),
            current_block_idx: 0,
            locals_map: HashMap::new(),
            next_local_id: 0,
            current_actor: actor_name,
            symbols,
        }
    }

    pub fn get_or_alloc_local(&mut self, name: &str) -> usize {
        if let Some(&id) = self.locals_map.get(name) {
            id
        } else {
            let id = self.next_local_id;
            self.locals_map.insert(name.to_string(), id);
            self.next_local_id += 1;
            id
        }
    }

    fn alloc_temp(&mut self) -> usize {
        let id = self.next_local_id;
        self.next_local_id += 1;
        id
    }

    fn value_to_local_idx(&mut self, val: Value) -> usize {
        match val {
            Value::Local(idx) => idx,
            _ => {
                let idx = self.alloc_temp();
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
                idx
            }
        }
    }

    fn new_block(&mut self) {
        let idx = self.blocks.len();
        self.blocks.push(BasicBlock {
            id: idx,
            instrs: Vec::new(),
            terminator: Terminator::Jump(999999), // Placeholder, should be overwritten
        });
        self.current_block_idx = idx;
    }

    fn new_block_index(&mut self) -> usize {
        let idx = self.blocks.len();
        self.blocks.push(BasicBlock {
            id: idx,
            instrs: Vec::new(),
            terminator: Terminator::Jump(999999), // Placeholder
        });
        idx
    }

    fn switch_to_block(&mut self, idx: usize) {
        self.current_block_idx = idx;
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.blocks[self.current_block_idx]
    }

    pub fn generate(mut self, func: &FunctionDef, actor_fields: &[FieldDef]) -> IrHandler {
        // Register params as locals first
        for p in &func.params {
            self.get_or_alloc_local(&p.name);
        }

        // Create entry block
        self.new_block();

        // Initialize actor fields at the start of each handler
        for field in actor_fields {
            let field_local = self.get_or_alloc_local(&field.name);
            let init_val = self.gen_expr(&field.init);
            self.current_block_mut()
                .instrs
                .push(Instruction::Assign(field_local, init_val));
        }

        for stmt in &func.body.stmts {
            self.gen_stmt(stmt);
        }

        // Ensure terminator
        if matches!(
            self.current_block_mut().terminator,
            Terminator::Jump(999999)
        ) {
            // If this is the 'run' handler of the 'main' actor, append an Exit instruction
            // This allows the user to omit explicit exit() calls
            if self.current_actor == "main" && func.name == "run" {
                self.current_block_mut().instrs.push(Instruction::Exit);
            }

            self.current_block_mut().terminator = Terminator::Return(None);
        }

        IrHandler {
            name: func.name.clone(),
            params: func
                .params
                .iter()
                .map(|p| {
                    let type_name = match &p.ty {
                        Type::UserDefined(name) => name.clone(),
                        Type::Int => "Int".to_string(),
                        Type::String => "String".to_string(),
                        Type::Bool => "Bool".to_string(),
                        Type::Unit => "Unit".to_string(),
                        _ => format!("{:?}", p.ty),
                    };
                    (p.name.clone(), type_name)
                })
                .collect(),
            local_count: self.next_local_id,
            blocks: self.blocks.clone(),
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let(name, _, expr) => {
                let val = self.gen_expr(expr);
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            StmtKind::Var(name, ty_opt, expr_opt) => {
                let val = if let Some(expr) = expr_opt {
                    // Has initializer - use it
                    self.gen_expr(expr)
                } else {
                    // No initializer - use default value based on type
                    // For now, default to 0 for Int, empty string for String, etc.
                    match ty_opt {
                        Some(Type::Int) => Value::ConstInt(0),
                        Some(Type::String) => Value::ConstString("".to_string()),
                        Some(Type::Bool) => Value::ConstBool(false),
                        _ => Value::ConstInt(0), // Fallback default
                    }
                };
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            StmtKind::Assign(name, expr) => {
                let val = self.gen_expr(expr);
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            StmtKind::AssignIndex(name, index, value) => {
                let collection_idx = self.get_or_alloc_local(name);
                let index_val = self.gen_expr(index);
                let value_val = self.gen_expr(value);

                let index_idx = self.value_to_local_idx(index_val);
                let value_idx = self.value_to_local_idx(value_val);

                self.current_block_mut().instrs.push(Instruction::SwStore {
                    collection: collection_idx,
                    index: index_idx,
                    value: value_idx,
                });
            }
            StmtKind::Expr(expr) => {
                self.gen_expr(expr);
            }
            StmtKind::Return(expr) => {
                let val = expr.as_ref().map(|e| self.gen_expr(e));
                self.current_block_mut().terminator = Terminator::Return(val);
            }
            StmtKind::If(cond, then_block, else_block) => {
                let cond_val = self.gen_expr(cond);

                let then_idx = self.new_block_index();
                let else_idx = self.new_block_index();
                let merge_idx = self.new_block_index();

                // Jump from current
                let false_target = if else_block.is_some() {
                    else_idx
                } else {
                    merge_idx
                };
                self.current_block_mut().terminator =
                    Terminator::JumpCond(cond_val, then_idx, false_target);

                // Then block
                self.switch_to_block(then_idx);
                for s in &then_block.stmts {
                    self.gen_stmt(s);
                }
                // Jump to merge if no explicit return
                if matches!(
                    self.current_block_mut().terminator,
                    Terminator::Jump(999999)
                ) {
                    self.current_block_mut().terminator = Terminator::Jump(merge_idx);
                }

                // Else block
                if let Some(else_block) = else_block {
                    self.switch_to_block(else_idx);
                    for s in &else_block.stmts {
                        self.gen_stmt(s);
                    }
                    // Jump to merge if no explicit return
                    if matches!(
                        self.current_block_mut().terminator,
                        Terminator::Jump(999999)
                    ) {
                        self.current_block_mut().terminator = Terminator::Jump(merge_idx);
                    }
                }

                // Merge block
                self.switch_to_block(merge_idx);
            }
            StmtKind::While(cond, body) => {
                let header_idx = self.new_block_index();
                let body_idx = self.new_block_index();
                let exit_idx = self.new_block_index();

                // Jump to header
                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Header: check condition
                self.switch_to_block(header_idx);
                let cond_val = self.gen_expr(cond);
                self.current_block_mut().terminator =
                    Terminator::JumpCond(cond_val, body_idx, exit_idx);

                // Body
                self.switch_to_block(body_idx);
                for s in &body.stmts {
                    self.gen_stmt(s);
                }
                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Exit
                self.switch_to_block(exit_idx);
            }
            StmtKind::For(iter, start, end, body) => {
                // Desugar to while loop:
                // var iter = start
                // while (iter < end) { ...; iter = iter + 1 }

                // Init
                let start_val = self.gen_expr(start);
                let iter_idx = self.get_or_alloc_local(iter);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(iter_idx, start_val));

                let header_idx = self.new_block_index();
                let body_idx = self.new_block_index();
                let exit_idx = self.new_block_index();

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Header: iter < end
                self.switch_to_block(header_idx);
                let end_val = self.gen_expr(end);
                // For simplicity re-eval end expression (might be side-effecty, but usually const/var).
                // Better: var _end = end;
                // Let's stick to simple re-eval or assume it's a var/const for now.

                let iter_val = Value::Local(iter_idx);
                let cond_res = self.alloc_temp();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: cond_res,
                    op: "lt".to_string(),
                    lhs: iter_val.clone(),
                    rhs: end_val,
                });
                self.current_block_mut().terminator =
                    Terminator::JumpCond(Value::Local(cond_res), body_idx, exit_idx);

                // Body
                self.switch_to_block(body_idx);
                for s in &body.stmts {
                    self.gen_stmt(s);
                }

                // Increment
                let inc_res = self.alloc_temp();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: inc_res,
                    op: "add".to_string(),
                    lhs: Value::Local(iter_idx),
                    rhs: Value::ConstInt(1),
                });
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(iter_idx, Value::Local(inc_res)));

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Exit
                self.switch_to_block(exit_idx);
            }
            StmtKind::ForEach(iter, collection, body) => {
                // 1. Evaluate collection
                let col_val = self.gen_expr(collection);
                let col_idx = self.value_to_local_idx(col_val);

                // 2. Init index = 0
                let idx_idx = self.alloc_temp();
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx_idx, Value::ConstInt(0)));

                // 3. Get size
                let len_res = self.alloc_temp();
                self.current_block_mut()
                    .instrs
                    .push(Instruction::CallMethod {
                        result: len_res,
                        target: col_idx,
                        method: "size".to_string(),
                        args: vec![],
                    });
                let len_val = Value::Local(len_res);

                // 4. Loop Header
                let header_idx = self.new_block_index();
                let body_idx = self.new_block_index();
                let exit_idx = self.new_block_index();

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Header: idx < len
                self.switch_to_block(header_idx);
                let cond_res = self.alloc_temp();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: cond_res,
                    op: "lt".to_string(),
                    lhs: Value::Local(idx_idx),
                    rhs: len_val.clone(),
                });
                self.current_block_mut().terminator =
                    Terminator::JumpCond(Value::Local(cond_res), body_idx, exit_idx);

                // Body
                self.switch_to_block(body_idx);

                // var iter = col.get(idx)
                let iter_local = self.get_or_alloc_local(iter);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::CallMethod {
                        result: iter_local,
                        target: col_idx,
                        method: "get".to_string(),
                        args: vec![Value::Local(idx_idx)],
                    });

                // Body stmts
                for s in &body.stmts {
                    self.gen_stmt(s);
                }

                // Increment index
                let inc_res = self.alloc_temp();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: inc_res,
                    op: "add".to_string(),
                    lhs: Value::Local(idx_idx),
                    rhs: Value::ConstInt(1),
                });
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx_idx, Value::Local(inc_res)));

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Exit
                self.switch_to_block(exit_idx);
            }
        }
    }
    pub fn gen_expr(&mut self, expr: &Expr) -> Value {
        match &expr.kind {
            ExprKind::Literal(lit) => match lit {
                Literal::Int(i) => Value::ConstInt(*i),
                Literal::String(s) => Value::ConstString(s.clone()),
                Literal::Bool(b) => Value::ConstInt(if *b { 1 } else { 0 }), // Bool as int
            },
            ExprKind::Variable(name) => {
                let idx = self.get_or_alloc_local(name);
                Value::Local(idx)
            }
            ExprKind::BinaryOp(lhs, op, rhs) => {
                let l = self.gen_expr(lhs);
                let r = self.gen_expr(rhs);
                let res = self.alloc_temp();

                let op_str = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Sub => "sub",
                    BinaryOp::Mul => "mul",
                    BinaryOp::Div => "div",
                    BinaryOp::Eq => "eq",
                    BinaryOp::Ne => "ne",
                    BinaryOp::Lt => "lt",
                    BinaryOp::Le => "le",
                    BinaryOp::Gt => "gt",
                    BinaryOp::Ge => "ge",
                };

                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: res,
                    op: op_str.to_string(),
                    lhs: l,
                    rhs: r,
                });
                Value::Local(res)
            }
            ExprKind::Index(target, index) => {
                let target_val = self.gen_expr(target);
                let index_val = self.gen_expr(index);

                let collection_idx = self.value_to_local_idx(target_val);
                let index_idx = self.value_to_local_idx(index_val);

                let res = self.alloc_temp();
                self.current_block_mut().instrs.push(Instruction::SwLoad {
                    result: res,
                    collection: collection_idx,
                    index: index_idx,
                });
                Value::Local(res)
            }
            ExprKind::Call(target, method, args) => {
                // Handle static calls for collections (e.g. List.new())
                if let ExprKind::Variable(name) = &target.kind {
                    if name == "List" || name == "MutableMap" || name == "MutableSet" {
                        let func_name = format!("{}.{}", name, method);
                        let res = self.alloc_temp();
                        self.current_block_mut().instrs.push(Instruction::CallPure {
                            result: res,
                            func: func_name,
                            args: vec![], // Constructors usually don't take args for now
                        });
                        return Value::Local(res);
                    }
                }

                // Check if IO functions first (print, println, exit)
                let func_name = method.clone();
                if func_name == "print" || func_name == "println" {
                    let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                    let res = self.alloc_temp();
                    self.current_block_mut().instrs.push(Instruction::CallIo {
                        result: res,
                        func: func_name,
                        args: arg_vals,
                    });
                    return Value::Local(res);
                } else if func_name == "exit" {
                    self.current_block_mut().instrs.push(Instruction::Exit);
                    return Value::ConstInt(0);
                }

                // Check if this is a local function call (target is a variable, not an object)
                if let ExprKind::Variable(_func_name) = &target.kind {
                    // This is a pure function call like add(5, 3)
                    let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                    let res = self.alloc_temp();

                    self.current_block_mut().instrs.push(Instruction::CallPure {
                        result: res,
                        func: method.clone(),
                        args: arg_vals,
                    });
                    return Value::Local(res);
                }

                // Check if this is a collection method (comes after local function check)
                let collection_methods = [
                    "add",
                    "get",
                    "size",
                    "isEmpty",
                    "put",
                    "remove",
                    "containsKey",
                    "contains",
                    "keys",
                    "values",
                    "entrySet",
                    "key",
                    "value",
                    "addAll",
                    "putAll",
                    "clear",
                ];

                if collection_methods.contains(&method.as_str()) {
                    // Collection method call - requires target to be a local variable (or temp)
                    let target_val = self.gen_expr(target);

                    if let Value::Local(target_idx) = target_val {
                        let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                        let res = self.alloc_temp();

                        self.current_block_mut()
                            .instrs
                            .push(Instruction::CallMethod {
                                result: res,
                                target: target_idx,
                                method: method.clone(),
                                args: arg_vals,
                            });
                        return Value::Local(res);
                    } else {
                        // If target is not a local (e.g. constant or future), we can't mutate it easily via CallMethod
                        // For now, assume target is always resolved to a Local by gen_expr
                        // If gen_expr returns a Const, we can't mutate it anyway.
                        println!("Warning: Collection method called on non-local value");
                    }
                }

                // Generate target value (for instance methods)
                let _ = self.gen_expr(target); // Evaluate target for side effects if any
                let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                let res = self.alloc_temp();

                // Default to CallPure
                self.current_block_mut().instrs.push(Instruction::CallPure {
                    result: res,
                    func: method.clone(),
                    args: arg_vals,
                });

                Value::Local(res)
            }
            ExprKind::Ask { target, message } => {
                let target_val = self.gen_expr(target);
                let msg_val = self.gen_expr(message);
                let result_idx = self.alloc_temp();

                self.current_block_mut().instrs.push(Instruction::Ask {
                    result: result_idx,
                    target: target_val,
                    message: msg_val,
                });

                Value::Local(result_idx)
            }
            ExprKind::Spawn { actor_type, args } => {
                // Generate argument values
                let arg_values: Vec<Value> = args.iter().map(|arg| self.gen_expr(arg)).collect();

                // Allocate local for the result ActorRef
                let result_idx = self.get_or_alloc_local(&format!("_ref_{}", actor_type));

                // Emit Spawn instruction
                self.current_block_mut().instrs.push(Instruction::Spawn {
                    result: result_idx,
                    actor_type: actor_type.clone(),
                    args: arg_values,
                });

                Value::Local(result_idx)
            }
            ExprKind::Send { target, message } => {
                let target_val = self.gen_expr(target);
                let msg_val = self.gen_expr(message);

                self.current_block_mut().instrs.push(Instruction::Send {
                    target: target_val,
                    message: msg_val,
                });

                Value::ConstInt(0)
            }
            ExprKind::FieldAccess(target, field_name) => {
                let target_val = self.gen_expr(target);
                let result_idx = self.get_or_alloc_local(&format!("_field_{}", field_name));

                self.current_block_mut().instrs.push(Instruction::GetField {
                    result: result_idx,
                    target: target_val,
                    field_name: field_name.clone(),
                });

                Value::Local(result_idx)
            }
            ExprKind::Construct {
                name,
                args,
                field_names: _,
            } => {
                // DataMessage removed in Phase 5 - this will cause runtime error
                // Generate error instruction or placeholder
                // For now, we'll just use generic field names.
                // The original DataMessage field lookup was here:
                // let field_names =
                //     if let Some(Symbol::DataMessage(fields)) = self.symbols.lookup(name) {
                //         fields
                //             .iter()
                //             .map(|(name, _)| name.clone())
                //             .collect::<Vec<_>>()
                //     } else {
                //         // Fallback: use field_0, field_1, etc. if not found
                //         (0..args.len()).map(|i| format!("field_{}", i)).collect()
                //     };

                // Fallback: use field_0, field_1, etc. if not found
                let field_names = (0..args.len())
                    .map(|i| format!("field_{}", i))
                    .collect::<Vec<_>>();

                // Generate (field_name, value) pairs
                let mut field_pairs = Vec::new();
                for (field_name, arg) in field_names.iter().zip(args.iter()) {
                    field_pairs.push((field_name.clone(), self.gen_expr(arg)));
                }

                // Allocate local for the result
                let result_idx = self.get_or_alloc_local(&format!("_msg_{}", name));

                // Emit CreateMessage instruction with field pairs
                self.current_block_mut()
                    .instrs
                    .push(Instruction::CreateMessage {
                        result: result_idx,
                        type_name: name.clone(),
                        field_values: field_pairs, // (field_name, value) pairs
                    });

                Value::Local(result_idx)
            }
        }
    }
}
