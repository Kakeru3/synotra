use crate::ast::BinaryOp;
use crate::ast::*;
use crate::ir::Instruction;
use crate::ir::Terminator;
use crate::ir::*;
use std::collections::HashMap;

pub struct Codegen {
    blocks: Vec<BasicBlock>,
    current_block_idx: usize,
    locals_map: HashMap<String, usize>,
    next_local_id: usize,
    current_actor: String,
}

impl Codegen {
    pub fn new(actor_name: String) -> Self {
        Codegen {
            blocks: Vec::new(),
            current_block_idx: 0,
            locals_map: HashMap::new(),
            next_local_id: 0,
            current_actor: actor_name,
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
                .map(|p| (p.name.clone(), format!("{:?}", p.ty)))
                .collect(),
            local_count: self.next_local_id,
            blocks: self.blocks.clone(),
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(name, _, expr) => {
                let val = self.gen_expr(expr);
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            Stmt::Var(name, _, expr) => {
                let val = self.gen_expr(expr);
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            Stmt::Assign(name, expr) => {
                let val = self.gen_expr(expr);
                let idx = self.get_or_alloc_local(name);
                self.current_block_mut()
                    .instrs
                    .push(Instruction::Assign(idx, val));
            }
            Stmt::AssignIndex(name, index, value) => {
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
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
            }
            Stmt::Return(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.gen_expr(e))
                } else {
                    None
                };
                self.current_block_mut().terminator = Terminator::Return(val);
                // Unreachable code after return, but we need to keep pushing to a valid block
                // Create a dead block to catch subsequent instructions
                self.new_block();
            }
            Stmt::Send {
                target,
                message,
                args,
            } => {
                let target_val = self.gen_expr(target);
                let msg_val = self.gen_expr(message);
                let arg_vals = args.iter().map(|arg| self.gen_expr(arg)).collect();
                self.current_block_mut().instrs.push(Instruction::Send {
                    target: target_val,
                    msg: msg_val,
                    args: arg_vals,
                });
            }
            Stmt::If(cond, then_block, else_block) => {
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
            Stmt::While(cond, body) => {
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
            Stmt::For(iter, start, end, body) => {
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
            _ => {}
        }
    }

    pub fn gen_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => Value::ConstInt(*i),
                Literal::String(s) => Value::ConstString(s.clone()),
                Literal::Bool(b) => Value::ConstInt(if *b { 1 } else { 0 }), // Bool as int
            },
            Expr::Variable(name) => {
                let idx = self.get_or_alloc_local(name);
                Value::Local(idx)
            }
            Expr::BinaryOp(lhs, op, rhs) => {
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
            Expr::Index(target, index) => {
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
            Expr::Call(target, method, args) => {
                // Handle static calls for collections (e.g. List.new())
                if let Expr::Variable(name) = &**target {
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

                // Check if this is a collection method
                let collection_methods = [
                    "add",
                    "get",
                    "size",
                    "isEmpty",
                    "put",
                    "remove",
                    "containsKey",
                    "contains",
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
                let _ = self.gen_expr(target); // Evaluate target for side effects if any, but we don't use it for pure calls
                let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                let res = self.alloc_temp();

                // Check if IO or Pure (simplified)
                let func_name = method.clone();
                if func_name == "print" || func_name == "println" {
                    self.current_block_mut().instrs.push(Instruction::CallIo {
                        result: res,
                        func: func_name,
                        args: arg_vals,
                    });
                } else if func_name == "exit" {
                    self.current_block_mut().instrs.push(Instruction::Exit);
                } else {
                    self.current_block_mut().instrs.push(Instruction::CallPure {
                        result: res,
                        func: func_name,
                        args: arg_vals,
                    });
                }

                Value::Local(res)
            }
            Expr::Ask {
                target,
                message,
                args,
            } => {
                let target_val = self.gen_expr(target);
                let msg_val = self.gen_expr(message);
                let arg_vals = args.iter().map(|arg| self.gen_expr(arg)).collect();
                let res = self.alloc_temp();

                self.current_block_mut().instrs.push(Instruction::Ask {
                    result: res,
                    target: target_val,
                    msg: msg_val,
                    args: arg_vals,
                });

                Value::Local(res)
            }
        }
    }
}
