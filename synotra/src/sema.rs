use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(Type, bool),                    // Type, is_mutable
    Function(Vec<Type>, Option<Type>, bool), // Params, Return, is_io
    Actor(String),
    Message(String),
    DataMessage(Vec<(String, Type)>), // Fields with types
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
    pub fn update(&mut self, name: &str, new_symbol: Symbol) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), new_symbol);
                return true;
            }
        }
        false
    }
}

fn check_type_compatibility(expected: &Type, actual: &Type) -> bool {
    match (expected, actual) {
        (Type::Unknown, _) => true,
        (_, Type::Unknown) => true,
        (Type::Int, Type::Int) => true,
        (Type::String, Type::String) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::UserDefined(n1), Type::UserDefined(n2)) => n1 == n2,
        (Type::Generic(n1, args1), Type::Generic(n2, args2)) => {
            if n1 != n2 || args1.len() != args2.len() {
                return false;
            }
            for (a1, a2) in args1.iter().zip(args2.iter()) {
                if !check_type_compatibility(a1, a2) {
                    return false;
                }
            }
            true
        }
        (Type::ActorRef(t1), Type::ActorRef(t2)) => {
            // ActorRef types are compatible if their message types are compatible
            check_type_compatibility(t1, t2)
        }
        _ => false,
    }
}

// Validate that ActorRef types reference valid data messages
fn validate_actorref_type(ty: &Type, symbols: &SymbolTable) -> Result<(), String> {
    match ty {
        Type::ActorRef(msg_type) => {
            // Check if the message type is a UserDefined type (data message name)
            match &**msg_type {
                Type::UserDefined(name) => {
                    // Verify it's registered as a DataMessage in symbol table
                    if let Some(Symbol::DataMessage(_)) = symbols.lookup(name) {
                        Ok(())
                    } else {
                        Err(format!(
                            "ActorRef message type '{}' is not a valid data message",
                            name
                        ))
                    }
                }
                _ => Err(format!(
                    "ActorRef message type must be a data message name, found: {:?}",
                    msg_type
                )),
            }
        }
        Type::Generic(_, args) => {
            // Recursively validate generic type arguments
            for arg in args {
                validate_actorref_type(arg, symbols)?;
            }
            Ok(())
        }
        _ => Ok(()), // Other types don't need ActorRef validation
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
            Definition::DataMessage(data_msg) => {
                // Collect fields with their types for registration
                let fields: Vec<(String, Type)> = data_msg
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.field_type.clone()))
                    .collect();

                symbols.insert(data_msg.name.clone(), Symbol::DataMessage(fields));
            }
            Definition::Function(func) => {
                let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
                symbols.insert(
                    func.name.clone(),
                    Symbol::Function(param_types, func.return_type.clone(), func.is_io),
                );
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

    // Register actor parameters and validate ActorRef types
    for param in &actor.params {
        symbols.insert(
            param.name.clone(),
            Symbol::Variable(param.ty.clone(), false),
        );

        // Validate ActorRef types in actor parameters
        validate_actorref_type(&param.ty, symbols)?;
    }

    // Register actor members (fields and methods)
    for member in &actor.members {
        match member {
            ActorMember::Field(field) => {
                symbols.insert(
                    field.name.clone(),
                    Symbol::Variable(field.ty.clone(), field.is_mutable),
                );
            }
            ActorMember::Method(method) => {
                let param_types = method.params.iter().map(|p| p.ty.clone()).collect();
                symbols.insert(
                    method.name.clone(),
                    Symbol::Function(param_types, method.return_type.clone(), method.is_io),
                );
            }
        }
    }

    // Analyze field initializations and methods
    for member in &actor.members {
        match member {
            ActorMember::Field(field) => {
                // Analyze the initialization expression
                analyze_expr(&field.init, symbols, false)?;
            }
            ActorMember::Method(method) => {
                analyze_function(method, symbols)?;
            }
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
        symbols.insert(
            param.name.clone(),
            Symbol::Variable(param.ty.clone(), false),
        );
    }

    for stmt in &func.body.stmts {
        analyze_stmt(stmt, symbols, func.is_io)?;
    }

    symbols.exit_scope();
    Ok(())
}

fn analyze_stmt(stmt: &Stmt, symbols: &mut SymbolTable, is_io_context: bool) -> Result<(), String> {
    match stmt {
        Stmt::Let(name, ty_opt, expr) => {
            let expr_ty = analyze_expr(expr, symbols, is_io_context)?;
            if let Some(expected_ty) = ty_opt {
                if !check_type_compatibility(expected_ty, &expr_ty) {
                    return Err(format!(
                        "Type mismatch for variable '{}': expected {:?}, found {:?}",
                        name, expected_ty, expr_ty
                    ));
                }
            }
            symbols.insert(name.clone(), Symbol::Variable(expr_ty, false));
        }
        Stmt::Var(name, ty_opt, init) => {
            let init_ty = analyze_expr(init, symbols, is_io_context)?;
            let var_ty = if let Some(ty) = ty_opt {
                if init_ty != *ty {
                    return Err(format!(
                        "Type mismatch in variable declaration: expected {:?}, got {:?}",
                        ty, init_ty
                    ));
                }
                ty.clone()
            } else {
                init_ty
            };
            // println!("DEBUG: Defining variable '{}' with type {:?}", name, var_ty);
            symbols.insert(name.clone(), Symbol::Variable(var_ty, true));
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
                    _ => {
                        return Err(format!(
                            "'{}' is not a variable and cannot be assigned to",
                            name
                        ))
                    }
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
                            return Err(format!(
                                "Cannot assign to immutable collection '{}'",
                                name
                            ));
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
        // Stmt::Send removed from AST
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
        Stmt::ForEach(iter, collection, body) => {
            let collection_ty = analyze_expr(collection, symbols, is_io_context)?;
            let item_ty = match collection_ty {
                Type::Generic(name, args) if name == "List" || name == "Set" => {
                    if args.len() >= 1 {
                        args[0].clone()
                    } else {
                        return Err("Collection must have a type argument".to_string());
                    }
                }
                Type::Generic(name, _) if name == "Map" => {
                    return Err(
                        "Cannot iterate over Map directly. Use map.keys() or map.values()"
                            .to_string(),
                    );
                }
                _ => return Err(format!("Cannot iterate over type {:?}", collection_ty)),
            };

            symbols.enter_scope();
            symbols.insert(iter.clone(), Symbol::Variable(item_ty, false));
            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context)?;
            }
            symbols.exit_scope();
        }
    }
    Ok(())
}

fn analyze_expr(
    expr: &Expr,
    symbols: &mut SymbolTable,
    is_io_context: bool,
) -> Result<Type, String> {
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
        Expr::Call(target, method, args) => {
            for arg in args {
                analyze_expr(arg, symbols, is_io_context)?;
            }

            // Check if it's a method call on a variable
            if let Expr::Variable(var_name) = target.as_ref() {
                if let Some(Symbol::Variable(ty, _)) = symbols.lookup(var_name) {
                    // eprintln!(
                    //     "DEBUG: Method call '{}' on variable '{}' with type {:?}",
                    //     method, var_name, ty
                    // );
                }
            }

            if let Expr::Variable(name) = target.as_ref() {
                if method == "new" {
                    if name == "List" {
                        return Ok(Type::Generic("List".to_string(), vec![Type::Unknown]));
                    } else if name == "MutableMap" {
                        return Ok(Type::Generic(
                            "Map".to_string(),
                            vec![Type::Unknown, Type::Unknown],
                        ));
                    } else if name == "MutableSet" {
                        return Ok(Type::Generic("Set".to_string(), vec![Type::Unknown]));
                    }
                }
            }

            // Special case: parser generates Call(Variable("self"), method_name, args) for global functions
            // like println(...). In this case, we don't actually have a 'self' variable.
            let is_global_function = if let Expr::Variable(name) = target.as_ref() {
                name == "self"
            } else {
                false
            };

            // Analyze target type (skip if it's a global function call)
            let target_ty = if is_global_function {
                Type::Unknown // Placeholder, won't be used
            } else {
                analyze_expr(target, symbols, is_io_context)?
            };

            // Collection method inference
            match &target_ty {
                Type::Generic(name, type_args) => {
                    if name == "List" {
                        match method.as_str() {
                            "get" => {
                                if type_args.len() >= 1 {
                                    return Ok(type_args[0].clone());
                                }
                            }
                            "add" => {
                                if type_args.len() >= 1 {
                                    if let Type::Unknown = type_args[0] {
                                        if let Some(arg) = args.get(0) {
                                            let arg_ty = analyze_expr(arg, symbols, is_io_context)?;
                                            if let Expr::Variable(var_name) = target.as_ref() {
                                                let new_ty = Type::Generic(
                                                    name.clone(),
                                                    vec![arg_ty.clone()],
                                                );
                                                symbols.update(
                                                    var_name,
                                                    Symbol::Variable(new_ty, true),
                                                ); // Assuming mutable
                                            }
                                        }
                                    }
                                }
                                return Ok(Type::Bool);
                            }
                            "size" => return Ok(Type::Int),
                            "isEmpty" => return Ok(Type::Bool),
                            "addAll" | "clear" => return Ok(Type::Bool),
                            _ => {}
                        }
                    } else if name == "Map" {
                        match method.as_str() {
                            "get" => {
                                if type_args.len() >= 2 {
                                    return Ok(type_args[1].clone());
                                }
                            }
                            "keys" => {
                                if type_args.len() >= 2 {
                                    return Ok(Type::Generic(
                                        "List".to_string(),
                                        vec![type_args[0].clone()],
                                    ));
                                }
                            }
                            "values" => {
                                if type_args.len() >= 2 {
                                    return Ok(Type::Generic(
                                        "List".to_string(),
                                        vec![type_args[1].clone()],
                                    ));
                                }
                            }
                            "entrySet" => {
                                if type_args.len() >= 2 {
                                    let entry_type = Type::Generic(
                                        "Entry".to_string(),
                                        vec![type_args[0].clone(), type_args[1].clone()],
                                    );
                                    return Ok(Type::Generic("List".to_string(), vec![entry_type]));
                                }
                            }
                            "put" => {
                                // Infer key and value types if currently Unknown
                                if type_args.len() >= 2 {
                                    let mut new_args = type_args.clone();
                                    let mut changed = false;

                                    if let Type::Unknown = type_args[0] {
                                        if let Some(arg) = args.get(0) {
                                            new_args[0] =
                                                analyze_expr(arg, symbols, is_io_context)?;
                                            changed = true;
                                        }
                                    }
                                    if let Type::Unknown = type_args[1] {
                                        if let Some(arg) = args.get(1) {
                                            new_args[1] =
                                                analyze_expr(arg, symbols, is_io_context)?;
                                            changed = true;
                                        }
                                    }

                                    if changed {
                                        if let Expr::Variable(var_name) = target.as_ref() {
                                            let new_ty = Type::Generic(name.clone(), new_args);
                                            symbols
                                                .update(var_name, Symbol::Variable(new_ty, true));
                                        }
                                    }
                                }
                                return Ok(Type::Bool);
                            }
                            "remove" | "containsKey" | "contains" => return Ok(Type::Bool),
                            "size" => return Ok(Type::Int),
                            "putAll" | "clear" => return Ok(Type::Bool),
                            _ => {}
                        }
                    } else if name == "Set" {
                        // println!("DEBUG: Analyzing Set method '{}'", method);
                        match method.as_str() {
                            "add" => {
                                // Infer element type if currently Unknown
                                if type_args.len() >= 1 {
                                    if let Type::Unknown = type_args[0] {
                                        if let Some(arg) = args.get(0) {
                                            let arg_ty = analyze_expr(arg, symbols, is_io_context)?;
                                            if let Expr::Variable(var_name) = target.as_ref() {
                                                let new_ty = Type::Generic(
                                                    name.clone(),
                                                    vec![arg_ty.clone()],
                                                );
                                                symbols.update(
                                                    var_name,
                                                    Symbol::Variable(new_ty, true),
                                                );
                                            }
                                        }
                                    }
                                }
                                return Ok(Type::Bool);
                            }
                            "remove" | "contains" | "addAll" | "clear" => return Ok(Type::Bool),
                            "size" => return Ok(Type::Int),
                            "values" => {
                                if type_args.len() >= 1 {
                                    return Ok(Type::Generic(
                                        "List".to_string(),
                                        vec![type_args[0].clone()],
                                    ));
                                }
                            }
                            _ => {}
                        }
                    } else if name == "Entry" {
                        match method.as_str() {
                            "key" => {
                                if type_args.len() >= 2 {
                                    return Ok(type_args[0].clone());
                                }
                            }
                            "value" => {
                                if type_args.len() >= 2 {
                                    return Ok(type_args[1].clone());
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }

            // Check if calling IO function from non-IO context
            // 1. Resolve method
            // For now, simple lookup if target is self/implicit
            let method_name = method.clone();

            // Check for built-in IO functions
            if method_name == "print" || method_name == "println" {
                if !is_io_context {
                    return Err(format!(
                        "Cannot call IO function '{}' from non-IO context",
                        method_name
                    ));
                }
                return Ok(Type::Int); // Return type doesn't matter much for now
            }

            // Hack: Try to find function in symbol table
            // In a real compiler, we need to resolve type of target, then look up method in that type.
            // Here we assume global functions or local methods for simplicity of this prototype.
            if let Some(Symbol::Function(_, _, is_io_target)) = symbols.lookup(&method_name) {
                if *is_io_target && !is_io_context {
                    return Err(format!(
                        "Cannot call IO function '{}' from non-IO context",
                        method_name
                    ));
                }
            }

            Ok(Type::Int)
        }
        Expr::Ask { target, message } => {
            // Check: ask() can only be used in IO context
            if !is_io_context {
                return Err(
                    "Cannot use 'ask' in a pure function. Use 'io fun' instead.".to_string()
                );
            }

            let target_ty = analyze_expr(target, symbols, is_io_context)?;
            let msg_ty = analyze_expr(message, symbols, is_io_context)?;

            if let Type::ActorRef(expected_msg_ty) = target_ty {
                if !check_type_compatibility(&msg_ty, &expected_msg_ty) {
                    return Err(format!(
                        "ActorRef expects message type {:?}, got {:?}",
                        expected_msg_ty, msg_ty
                    ));
                }
            } else {
                return Err(format!(
                    "Target of 'ask' must be ActorRef, got {:?}",
                    target_ty
                ));
            }

            Ok(Type::Int) // ask returns a value (placeholder)
        }
        Expr::Send { target, message } => {
            // Check: send() can only be used in IO context
            if !is_io_context {
                return Err(
                    "Cannot use 'send' in a pure function. Use 'io fun' instead.".to_string(),
                );
            }

            let target_ty = analyze_expr(target, symbols, is_io_context)?;
            let msg_ty = analyze_expr(message, symbols, is_io_context)?;

            if let Type::ActorRef(expected_msg_ty) = target_ty {
                if !check_type_compatibility(&msg_ty, &expected_msg_ty) {
                    return Err(format!(
                        "ActorRef expects message type {:?}, got {:?}",
                        expected_msg_ty, msg_ty
                    ));
                }
            } else {
                return Err(format!(
                    "Target of 'send' must be ActorRef, got {:?}",
                    target_ty
                ));
            }

            Ok(Type::Int) // send returns Unit/Int (0)
        }
        Expr::Construct { name, args } => {
            // Validate that 'name' is a registered data message
            match symbols.lookup(name) {
                Some(Symbol::DataMessage(fields)) => {
                    // Clone fields to avoid borrow checker issues
                    let fields = fields.clone();

                    // Validate argument count
                    if args.len() != fields.len() {
                        return Err(format!(
                            "Data message '{}' expects {} arguments, got {}",
                            name,
                            fields.len(),
                            args.len()
                        ));
                    }

                    // Analyze and type-check each argument
                    for (i, arg) in args.iter().enumerate() {
                        let arg_type = analyze_expr(arg, symbols, is_io_context)?;
                        let expected_type = &fields[i].1;

                        if !check_type_compatibility(&arg_type, expected_type) {
                            return Err(format!(
                                "Data message '{}' field '{}' expects type {:?}, got {:?}",
                                name, fields[i].0, expected_type, arg_type
                            ));
                        }
                    }

                    // Return the constructed message type
                    Ok(Type::UserDefined(name.clone()))
                }
                Some(_) => Err(format!("'{}' is not a data message", name)),
                None => Err(format!("Undefined data message '{}'", name)),
            }
        }
    }
}
