use crate::ast::*;
use crate::ir::*;
use crate::ast::BinaryOp;
use crate::ir::Terminator;
use crate::ir::Instruction;

pub struct Codegen {
    blocks: Vec<BasicBlock>,
    current_block_idx: usize,
    var_counter: usize,
    current_actor: String,
}

impl Codegen {
    pub fn new(actor_name: String) -> Self {
        Codegen {
            blocks: Vec::new(),
            current_block_idx: 0,
            var_counter: 0,
            current_actor: actor_name,
        }
    }

    fn new_var(&mut self) -> String {
        self.var_counter += 1;
        format!("v{}", self.var_counter)
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

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.blocks[self.current_block_idx]
    }

    pub fn generate(mut self, func: &FunctionDef) -> IrHandler {
        // Create entry block
        self.new_block();
        
        for stmt in &func.body.stmts {
            self.gen_stmt(stmt);
        }
        
        // Ensure terminator
        if matches!(self.current_block_mut().terminator, Terminator::Jump(999999)) {
            // If this is the 'run' handler of the 'main' actor, append an Exit instruction
            // This allows the user to omit explicit exit() calls
            if self.current_actor == "main" && func.name == "run" {
                self.current_block_mut().instrs.push(Instruction::Exit);
            }
            
            self.current_block_mut().terminator = Terminator::Return(None);
        }

        IrHandler {
            name: func.name.clone(),
            params: func.params.iter().map(|p| (p.name.clone(), format!("{:?}", p.ty))).collect(),
            blocks: self.blocks.clone(),
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(name, _, expr) => {
                let val = self.gen_expr(expr);
                self.current_block_mut().instrs.push(Instruction::Assign(name.clone(), val));
            }
            Stmt::Var(name, _, expr) => {
                let val = self.gen_expr(expr);
                self.current_block_mut().instrs.push(Instruction::Assign(name.clone(), val));
            }
            Stmt::Assign(name, expr) => {
                let val = self.gen_expr(expr);
                self.current_block_mut().instrs.push(Instruction::Assign(name.clone(), val));
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
            Stmt::Send { target, message, args } => {
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
                let false_target = if else_block.is_some() { else_idx } else { merge_idx };
                self.current_block_mut().terminator = Terminator::JumpCond(cond_val, then_idx, false_target);

                // Then block
                self.switch_to_block(then_idx);
                for s in &then_block.stmts {
                    self.gen_stmt(s);
                }
                // Jump to merge if no explicit return
                if matches!(self.current_block_mut().terminator, Terminator::Jump(999999)) {
                    self.current_block_mut().terminator = Terminator::Jump(merge_idx);
                }

                // Else block
                if let Some(else_block) = else_block {
                    self.switch_to_block(else_idx);
                    for s in &else_block.stmts {
                        self.gen_stmt(s);
                    }
                    // Jump to merge if no explicit return
                    if matches!(self.current_block_mut().terminator, Terminator::Jump(999999)) {
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
                self.current_block_mut().terminator = Terminator::JumpCond(cond_val, body_idx, exit_idx);

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
                self.current_block_mut().instrs.push(Instruction::Assign(iter.clone(), start_val));

                let header_idx = self.new_block_index();
                let body_idx = self.new_block_index();
                let exit_idx = self.new_block_index();

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Header: iter < end
                self.switch_to_block(header_idx);
                let end_val = self.gen_expr(end); // Re-eval end? Or eval once? Assuming eval once for now implies we should store it.
                // For simplicity re-eval end expression (might be side-effecty, but usually const/var).
                // Better: var _end = end;
                // Let's stick to simple re-eval or assume it's a var/const for now.

                let iter_val = Value::Var(iter.clone());
                let cond_res = self.new_var();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: cond_res.clone(),
                    op: "lt".to_string(),
                    lhs: iter_val.clone(),
                    rhs: end_val
                });
                self.current_block_mut().terminator = Terminator::JumpCond(Value::Var(cond_res), body_idx, exit_idx);

                // Body
                self.switch_to_block(body_idx);
                for s in &body.stmts {
                    self.gen_stmt(s);
                }

                // Increment
                let inc_res = self.new_var();
                self.current_block_mut().instrs.push(Instruction::BinOp {
                    result: inc_res.clone(),
                    op: "add".to_string(),
                    lhs: Value::Var(iter.clone()),
                    rhs: Value::ConstInt(1),
                });
                self.current_block_mut().instrs.push(Instruction::Assign(iter.clone(), Value::Var(inc_res)));

                self.current_block_mut().terminator = Terminator::Jump(header_idx);

                // Exit
                self.switch_to_block(exit_idx);
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => Value::ConstInt(*i),
                Literal::String(s) => Value::ConstString(s.clone()),
                Literal::Bool(b) => Value::ConstInt(if *b { 1 } else { 0 }), // Bool as int
            },
            Expr::Variable(name) => Value::Var(name.clone()),
            Expr::BinaryOp(lhs, op, rhs) => {
                let l = self.gen_expr(lhs);
                let r = self.gen_expr(rhs);
                let res = self.new_var();

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
                    result: res.clone(),
                    op: op_str.to_string(),
                    lhs: l,
                    rhs: r,
                });
                Value::Var(res)
            }
            Expr::Call(target, method, args) => {
                let arg_vals: Vec<Value> = args.iter().map(|a| self.gen_expr(a)).collect();
                let res = self.new_var();
                
                // Check if IO or Pure (simplified)
                // In real impl, we check symbol table. Assuming Pure for now unless "print"
                let func_name = method.clone();
                if func_name == "print" {
                     self.current_block_mut().instrs.push(Instruction::CallIo {
                        result: res.clone(),
                        func: func_name,
                        args: arg_vals,
                    });
                } else if func_name == "exit" {
                    self.current_block_mut().instrs.push(Instruction::Exit);
                    // Exit doesn't return, but we need a value for the expression
                    // In a real compiler we'd handle this better (e.g. bottom type)
                } else {
                     self.current_block_mut().instrs.push(Instruction::CallPure {
                        result: res.clone(),
                        func: func_name,
                        args: arg_vals,
                    });
                }
               
                Value::Var(res)
            }
            Expr::Ask { target, message, args } => {
                let target_val = self.gen_expr(target);
                let msg_val = self.gen_expr(message);
                let arg_vals = args.iter().map(|arg| self.gen_expr(arg)).collect();
                let res = self.new_var();
                
                self.current_block_mut().instrs.push(Instruction::Ask {
                    result: res.clone(),
                    target: target_val,
                    msg: msg_val,
                    args: arg_vals,
                });
                
                Value::Var(res)
            }
        }
    }
}
