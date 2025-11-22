use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(Type, bool), // Type, is_mutable
    Function(Vec<Type>, Option<Type>, bool), // Params, Return, is_io
    Actor(String),
    Message(String),
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, symbol);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}

pub fn analyze(program: &Program) -> Result<(), String> {
    let mut symbols = SymbolTable::new();
    
    // 1. Register top-level definitions
    for def in &program.definitions {
        match def {
            Definition::Actor(actor) => {
                symbols.insert(actor.name.clone(), Symbol::Actor(actor.name.clone()));
            }
            Definition::Message(msg) => {
                symbols.insert(msg.name.clone(), Symbol::Message(msg.name.clone()));
            }
            Definition::Function(func) => {
                let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
                symbols.insert(func.name.clone(), Symbol::Function(param_types, func.return_type.clone(), func.is_io));
            }
            Definition::Import(_import) => {
                // TODO: Register imported types in symbol table
                // For now, just validate the syntax by accepting it
            }
            _ => {}
        }
    }

    // 2. Analyze bodies
    for def in &program.definitions {
        match def {
            Definition::Actor(actor) => analyze_actor(actor, &mut symbols)?,
            Definition::Function(func) => analyze_function(func, &mut symbols)?,
            Definition::Import(_import) => {
                // No body to analyze for imports
            }
            _ => {}
        }
    }

    Ok(())
}

fn analyze_actor(actor: &ActorDef, symbols: &mut SymbolTable) -> Result<(), String> {
    symbols.enter_scope();
    
    // Register params as variables
    for param in &actor.params {
        symbols.insert(param.name.clone(), Symbol::Variable(param.ty.clone(), false));
    }

    // Register members
    for member in &actor.members {
        match member {
            ActorMember::Field(field) => {
                symbols.insert(field.name.clone(), Symbol::Variable(field.ty.clone(), field.is_mutable));
            }
            ActorMember::Method(method) => {
                let param_types = method.params.iter().map(|p| p.ty.clone()).collect();
                symbols.insert(method.name.clone(), Symbol::Function(param_types, method.return_type.clone(), method.is_io));
            }
        }
    }

    // Analyze methods
    for member in &actor.members {
        if let ActorMember::Method(method) = member {
            analyze_function(method, symbols)?;
        }
    }

    symbols.exit_scope();
    Ok(())
}

fn analyze_function(func: &FunctionDef, symbols: &mut SymbolTable) -> Result<(), String> {
    // Check: IO functions cannot have return types
    if func.is_io && func.return_type.is_some() {
        return Err(format!("IO function '{}' cannot have a return type. IO functions should only perform side effects.", func.name));
    }

    symbols.enter_scope();

    for param in &func.params {
        symbols.insert(param.name.clone(), Symbol::Variable(param.ty.clone(), false));
    }

    for stmt in &func.body.stmts {
        analyze_stmt(stmt, symbols, func.is_io)?;
    }

    symbols.exit_scope();
    Ok(())
}

fn analyze_stmt(stmt: &Stmt, symbols: &mut SymbolTable, is_io_context: bool) -> Result<(), String> {
    match stmt {
        Stmt::Let(name, _ty, expr) => {
            let expr_ty = analyze_expr(expr, symbols, is_io_context)?;
            symbols.insert(name.clone(), Symbol::Variable(expr_ty, false));
        }
        Stmt::Var(name, _ty, expr) => {
            let expr_ty = analyze_expr(expr, symbols, is_io_context)?;
            symbols.insert(name.clone(), Symbol::Variable(expr_ty, true));
        }
        Stmt::Assign(name, expr) => {
            // Check variable exists and is mutable
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(_, is_mutable) => {
                        if !is_mutable {
                            return Err(format!("Cannot assign to immutable variable '{}'", name));
                        }
                    }
                    _ => return Err(format!("'{}' is not a variable and cannot be assigned to", name)),
                }
            } else {
                return Err(format!("Variable '{}' not declared", name));
            }
            analyze_expr(expr, symbols, is_io_context)?;
        }
        Stmt::AssignIndex(name, index, value) => {
            // Check variable exists and is mutable collection
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(ty, is_mutable) => {
                        if !is_mutable {
                            return Err(format!("Cannot assign to immutable collection '{}'", name));
                        }
                        // TODO: Check if ty is a Map or List that supports assignment
                        // For now, assume it is if it's a variable
                    }
                    _ => return Err(format!("'{}' is not a variable", name)),
                }
            } else {
                return Err(format!("Variable '{}' not declared", name));
            }
            analyze_expr(index, symbols, is_io_context)?;
            analyze_expr(value, symbols, is_io_context)?;
        }
        Stmt::Expr(expr) => {
            analyze_expr(expr, symbols, is_io_context)?;
        }
        Stmt::Return(expr) => {
            if let Some(e) = expr {
                analyze_expr(e, symbols, is_io_context)?;
            }
        }
        Stmt::Send { target, message, args } => {
            // Check: send() can only be used in IO context
            if !is_io_context {
                return Err("Cannot use 'send' in a pure function. Use 'io fun' instead.".to_string());
            }
            
            analyze_expr(target, symbols, is_io_context)?;
            analyze_expr(message, symbols, is_io_context)?;
            for arg in args {
                analyze_expr(arg, symbols, is_io_context)?;
            }
        }
        Stmt::If(cond, then_block, else_block) => {
            analyze_expr(cond, symbols, is_io_context)?;
            // TODO: Check cond is boolean
            
            symbols.enter_scope();
            for s in &then_block.stmts {
                analyze_stmt(s, symbols, is_io_context)?;
            }
            symbols.exit_scope();

            if let Some(else_block) = else_block {
                symbols.enter_scope();
                for s in &else_block.stmts {
                    analyze_stmt(s, symbols, is_io_context)?;
                }
                symbols.exit_scope();
            }
        }
        Stmt::While(cond, body) => {
            analyze_expr(cond, symbols, is_io_context)?;
            
            symbols.enter_scope();
            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context)?;
            }
            symbols.exit_scope();
        }
        Stmt::For(iter, start, end, body) => {
            analyze_expr(start, symbols, is_io_context)?;
            analyze_expr(end, symbols, is_io_context)?;
            
            symbols.enter_scope();
            symbols.insert(iter.clone(), Symbol::Variable(Type::Int, false)); // Iterator is immutable int
            
            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context)?;
            }
            symbols.exit_scope();
        }
    }
    Ok(())
}

fn analyze_expr(expr: &Expr, symbols: &mut SymbolTable, is_io_context: bool) -> Result<Type, String> {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Int(_) => Ok(Type::Int),
            Literal::String(_) => Ok(Type::String),
            Literal::Bool(_) => Ok(Type::Bool),
        },
        Expr::Variable(name) => {
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(ty, _) => Ok(ty.clone()),
                    _ => Err(format!("{} is not a variable", name)),
                }
            } else {
                Err(format!("Undefined variable: {}", name))
            }
        }
        Expr::BinaryOp(lhs, _, rhs) => {
            let lhs_ty = analyze_expr(lhs, symbols, is_io_context)?;
            let _rhs_ty = analyze_expr(rhs, symbols, is_io_context)?;
            Ok(lhs_ty)
        }
        Expr::Index(target, index) => {
            let target_ty = analyze_expr(target, symbols, is_io_context)?;
            analyze_expr(index, symbols, is_io_context)?;
            
            // TODO: Check if target_ty is a collection and return element type
            // For now, if it's a Generic, assume it returns the first type arg (e.g. List<T> -> T)
            // Or second if Map<K, V> -> V
            match target_ty {
                Type::Generic(name, args) => {
                    if name == "List" && args.len() == 1 {
                        Ok(args[0].clone())
                    } else if (name == "Map" || name == "MutableMap") && args.len() == 2 {
                        Ok(args[1].clone())
                    } else {
                        Ok(Type::Int) // Fallback
                    }
                }
                _ => Ok(Type::Int), // Fallback
            }
        }
        Expr::Call(_target, method, args) => {
            for arg in args {
                analyze_expr(arg, symbols, is_io_context)?;
            }
            
            // Check if calling IO function from non-IO context
            // 1. Resolve method
            // For now, simple lookup if target is self/implicit
            let method_name = method.clone();
            
            // Check for built-in IO functions
            if method_name == "print" || method_name == "println" {
                if !is_io_context {
                    return Err(format!("Cannot call IO function '{}' from non-IO context", method_name));
                }
                return Ok(Type::Int); // Return type doesn't matter much for now
            }

            // Hack: Try to find function in symbol table
            // In a real compiler, we need to resolve type of target, then look up method in that type.
            // Here we assume global functions or local methods for simplicity of this prototype.
            if let Some(Symbol::Function(_, _, is_io_target)) = symbols.lookup(&method_name) {
                if *is_io_target && !is_io_context {
                    return Err(format!("Cannot call IO function '{}' from non-IO context", method_name));
                }
            }

            Ok(Type::Int) 
        }
        Expr::Ask { target, message, args } => {
            // Check: ask() can only be used in IO context
            if !is_io_context {
                return Err("Cannot use 'ask' in a pure function. Use 'io fun' instead.".to_string());
            }
            
            analyze_expr(target, symbols, is_io_context)?;
            analyze_expr(message, symbols, is_io_context)?;
            for arg in args {
                analyze_expr(arg, symbols, is_io_context)?;
            }
            Ok(Type::Int) // ask returns a value
        }
    }
}
