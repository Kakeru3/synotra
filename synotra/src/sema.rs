use crate::ast::*;
use crate::error::{CompileError, ErrorKind};
use std::collections::HashMap;

/// Collects errors during semantic analysis
#[derive(Debug, Clone)]
pub struct ErrorCollector {
    pub errors: Vec<CompileError>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        ErrorCollector { errors: Vec::new() }
    }

    pub fn push(&mut self, error: CompileError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn to_result(&self) -> Result<(), Vec<CompileError>> {
        if self.has_errors() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(Type, bool, bool), // Type, is_mutable, is_initialized
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

    /// Check if a name exists in the current (innermost) scope only
    pub fn lookup_in_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.scopes.last().and_then(|scope| scope.get(name))
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

pub fn analyze(program: &Program) -> Result<SymbolTable, Vec<CompileError>> {
    let mut symbols = SymbolTable::new();
    let mut error_collector = ErrorCollector::new();

    // 1. Register top-level definitions
    // First pass: Register all data messages
    for def in &program.definitions {
        if let Definition::DataMessage(data_def) = def {
            let fields: Vec<(String, Type)> = data_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.field_type.clone()))
                .collect();
            symbols.insert(data_def.name.clone(), Symbol::DataMessage(fields));
        }
    }

    // Second pass: Register actors and their types
    for def in &program.definitions {
        if let Definition::Actor(actor_def) = def {
            symbols.insert(
                actor_def.name.clone(),
                Symbol::Actor(actor_def.name.clone()),
            );
        }
    }

    // 1. Register top-level definitions (excluding Data and Actor which are handled above)
    for def in &program.definitions {
        match def {
            Definition::Actor(_) => { /* Handled in second pass */ }
            Definition::Message(msg) => {
                symbols.insert(msg.name.clone(), Symbol::Message(msg.name.clone()));
            }
            Definition::DataMessage(_) => { /* Handled in first pass */ }
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

    // 2. Analyze function bodies and actor definitions
    for def in &program.definitions {
        match def {
            Definition::Function(func) => {
                if let Err(errors) = analyze_function(func, &mut symbols) {
                    for err in errors {
                        error_collector.push(err);
                    }
                }
            }
            Definition::Actor(actor) => {
                // Validate fields
                for member in &actor.members {
                    if let ActorMember::Field(field) = member {
                        if let Err(e) = validate_actorref_type(&field.ty, &symbols) {
                            error_collector
                                .push(CompileError::from_kind(ErrorKind::Other { message: e }));
                        }
                    }
                }

                // Register actor members (fields and methods)
                for member in &actor.members {
                    match member {
                        ActorMember::Field(field) => {
                            symbols.insert(
                                field.name.clone(),
                                Symbol::Variable(field.ty.clone(), field.is_mutable, true), // fields are initialized
                            );
                        }
                        ActorMember::Method(method) => {
                            let param_types = method.params.iter().map(|p| p.ty.clone()).collect();
                            symbols.insert(
                                method.name.clone(),
                                Symbol::Function(
                                    param_types,
                                    method.return_type.clone(),
                                    method.is_io,
                                ),
                            );
                        }
                    }
                }

                // Analyze field initializations and methods
                for member in &actor.members {
                    match member {
                        ActorMember::Field(field) => {
                            // Analyze the initialization expression
                            if let Err(e) = analyze_expr(&field.init, &mut symbols, false) {
                                error_collector.push(e);
                            }
                        }
                        ActorMember::Method(method) => {
                            if let Err(errors) = analyze_function(method, &mut symbols) {
                                for err in errors {
                                    error_collector.push(err);
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if error_collector.has_errors() {
        error_collector.to_result().map(|_| symbols)
    } else {
        Ok(symbols)
    }
}

fn analyze_function(
    func: &FunctionDef,
    symbols: &mut SymbolTable,
) -> Result<(), Vec<CompileError>> {
    let mut error_collector = ErrorCollector::new();

    // Check: IO functions cannot have return types
    if func.is_io && func.return_type.is_some() {
        error_collector.push(CompileError::from_kind(ErrorKind::Other {
            message: format!("IO function '{}' cannot have a return type. IO functions should only perform side effects.", func.name),
        }));
    }

    // Check: Pure functions must have a return type
    if !func.is_io && func.return_type.is_none() {
        error_collector.push(CompileError::from_kind(ErrorKind::MissingReturn));
    }

    // Check: Pure functions cannot explicitly return Unit
    if !func.is_io {
        if let Some(Type::Unit) = func.return_type {
            error_collector.push(CompileError::from_kind(ErrorKind::Other {
                message: format!("Pure function '{}' cannot explicitly return Unit. If it returns nothing, omit the return type.", func.name),
            }));
        }
    }

    symbols.enter_scope();

    for param in &func.params {
        symbols.insert(
            param.name.clone(),
            Symbol::Variable(param.ty.clone(), false, true), // params are always initialized
        );
    }

    // Collect errors from all statements
    let mut error_collector = ErrorCollector::new();
    for stmt in &func.body.stmts {
        if let Err(e) = analyze_stmt(stmt, symbols, func.is_io) {
            error_collector.push(e);
        }
    }

    symbols.exit_scope();

    // Return all collected errors
    error_collector.to_result()
}

fn analyze_stmt(
    stmt: &Stmt,
    symbols: &mut SymbolTable,
    is_io_context: bool,
) -> Result<(), CompileError> {
    match stmt {
        Stmt::Let(name, ty_opt, expr) => {
            // Check for duplicate definition in current scope
            if symbols.lookup_in_current_scope(name).is_some() {
                return Err(CompileError::from_kind(ErrorKind::DuplicateDefinition {
                    name: name.clone(),
                }));
            }

            let expr_ty = analyze_expr(expr, symbols, is_io_context)?;
            if let Some(expected_ty) = ty_opt {
                if !check_type_compatibility(expected_ty, &expr_ty) {
                    return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                        expected: format!("{:?}", expected_ty),
                        found: format!("{:?}", expr_ty),
                    }));
                }
            }
            symbols.insert(name.clone(), Symbol::Variable(expr_ty, false, true));
            // let is always initialized
        }
        Stmt::Var(name, ty_opt, init_opt) => {
            // Check for duplicate definition in current scope
            if symbols.lookup_in_current_scope(name).is_some() {
                return Err(CompileError::from_kind(ErrorKind::DuplicateDefinition {
                    name: name.clone(),
                }));
            }

            let var_ty = match (ty_opt, init_opt) {
                // Case 1: Both type and initializer
                (Some(ty), Some(init)) => {
                    let init_ty = analyze_expr(init, symbols, is_io_context)?;
                    if !check_type_compatibility(ty, &init_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: format!("{:?}", ty),
                            found: format!("{:?}", init_ty),
                        }));
                    }
                    ty.clone()
                }
                // Case 2: Only initializer (type inference)
                (None, Some(init)) => analyze_expr(init, symbols, is_io_context)?,
                // Case 3: Only type annotation (no initializer) - this is valid
                (Some(ty), None) => ty.clone(),
                // Case 4: Neither type nor initializer - ERROR
                (None, None) => {
                    return Err(CompileError::from_kind(ErrorKind::Other {
                        message: format!(
                            "Variable '{}' must have either a type annotation or an initializer",
                            name
                        ),
                    }));
                }
            };

            // Var is initialized if init_opt.is_some()
            let is_initialized = init_opt.is_some();
            symbols.insert(name.clone(), Symbol::Variable(var_ty, true, is_initialized));
        }
        Stmt::Assign(name, expr) => {
            // Check variable exists and is mutable
            let (var_ty, is_mutable) = if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(var_ty, is_mutable, _) => {
                        if !*is_mutable {
                            return Err(CompileError::from_kind(ErrorKind::ImmutableAssignment {
                                name: name.clone(),
                            }));
                        }
                        (var_ty.clone(), *is_mutable)
                    }
                    _ => {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: format!(
                                "'{}' is not a variable and cannot be assigned to",
                                name
                            ),
                        }));
                    }
                }
            } else {
                return Err(CompileError::from_kind(ErrorKind::UndefinedVariable {
                    name: name.clone(),
                }));
            };

            // Analyze expression after releasing the borrow
            analyze_expr(expr, symbols, is_io_context)?;
            // After assignment, variable is initialized
            symbols.update(name, Symbol::Variable(var_ty, is_mutable, true));
        }
        Stmt::AssignIndex(name, index, value) => {
            // Check variable exists and is mutable collection
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(ty, is_mutable, is_initialized) => {
                        if !is_mutable {
                            return Err(CompileError::from_kind(ErrorKind::Other {
                                message: format!(
                                    "Cannot assign to immutable collection '{}'",
                                    name
                                ),
                            }));
                        }
                        // TODO: Check if ty is a Map or List that supports assignment
                        // For now, assume it is if it's a variable
                    }
                    _ => {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: format!("'{}' is not a variable", name),
                        }));
                    }
                }
            } else {
                return Err(CompileError::from_kind(ErrorKind::UndefinedVariable {
                    name: name.clone(),
                }));
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
            symbols.insert(iter.clone(), Symbol::Variable(Type::Int, false, true)); // Iterator is immutable int, initialized

            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context)?;
            }
            symbols.exit_scope();
        }
        Stmt::ForEach(iter, collection, body) => {
            let collection_ty = analyze_expr(collection, symbols, is_io_context)?;
            let item_ty = match collection_ty {
                Type::Generic(name, args) if name == "List" || name == "Set" => {
                    if !args.is_empty() {
                        args[0].clone()
                    } else {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: "Collection must have a type argument".to_string(),
                        }));
                    }
                }
                Type::Generic(name, _) if name == "Map" => {
                    return Err(CompileError::from_kind(ErrorKind::Other {
                        message: "Cannot iterate over Map directly. Use map.keys() or map.values()"
                            .to_string(),
                    }));
                }
                _ => {
                    return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                        expected: "Collection (List or Set)".to_string(),
                        found: format!("{:?}", collection_ty),
                    }));
                }
            };

            symbols.enter_scope();
            symbols.insert(iter.clone(), Symbol::Variable(item_ty, false, true)); // initialized
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
) -> Result<Type, CompileError> {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Int(_) => Ok(Type::Int),
            Literal::String(_) => Ok(Type::String),
            Literal::Bool(_) => Ok(Type::Bool),
        },
        Expr::Variable(name) => {
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(ty, _, is_initialized) => {
                        if !is_initialized {
                            return Err(CompileError::from_kind(
                                ErrorKind::UninitializedVariable { name: name.clone() },
                            ));
                        }
                        Ok(ty.clone())
                    }
                    _ => Err(CompileError::from_kind(ErrorKind::Other {
                        message: format!("'{}' is not a variable", name),
                    })),
                }
            } else {
                Err(CompileError::from_kind(ErrorKind::UndefinedVariable {
                    name: name.clone(),
                }))
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
                if let Some(Symbol::Variable(ty, _, _)) = symbols.lookup(var_name) {
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
                // For global functions, check if the function exists in symbol table
                // (println, print, and other built-ins will be checked during codegen)
                if !["println", "print"].contains(&method.as_str()) {
                    // Check if this global function is defined
                    if symbols.lookup(method).is_none() {
                        return Err(CompileError::from_kind(ErrorKind::UnknownFunction {
                            name: method.clone(),
                        }));
                    }
                }
                Type::Unknown // Placeholder, won't be used
            } else {
                analyze_expr(target, symbols, is_io_context)?
            };

            // Collection method inference
            if let Type::Generic(name, type_args) = &target_ty {
                if name == "List" {
                    match method.as_str() {
                        "get" => {
                            if !type_args.is_empty() {
                                return Ok(type_args[0].clone());
                            }
                        }
                        "add" => {
                            if !type_args.is_empty() {
                                if let Type::Unknown = type_args[0] {
                                    if let Some(arg) = args.first() {
                                        let arg_ty = analyze_expr(arg, symbols, is_io_context)?;
                                        if let Expr::Variable(var_name) = target.as_ref() {
                                            let new_ty =
                                                Type::Generic(name.clone(), vec![arg_ty.clone()]);
                                            symbols.update(
                                                var_name,
                                                Symbol::Variable(new_ty, true, true),
                                            );
                                            // Assuming mutable
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
                                    if let Some(arg) = args.first() {
                                        new_args[0] = analyze_expr(arg, symbols, is_io_context)?;
                                        changed = true;
                                    }
                                }
                                if let Type::Unknown = type_args[1] {
                                    if let Some(arg) = args.get(1) {
                                        new_args[1] = analyze_expr(arg, symbols, is_io_context)?;
                                        changed = true;
                                    }
                                }

                                if changed {
                                    if let Expr::Variable(var_name) = target.as_ref() {
                                        let new_ty = Type::Generic(name.clone(), new_args);
                                        symbols
                                            .update(var_name, Symbol::Variable(new_ty, true, true));
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
                            if !type_args.is_empty() {
                                if let Type::Unknown = type_args[0] {
                                    if let Some(arg) = args.first() {
                                        let arg_ty = analyze_expr(arg, symbols, is_io_context)?;
                                        if let Expr::Variable(var_name) = target.as_ref() {
                                            let new_ty =
                                                Type::Generic(name.clone(), vec![arg_ty.clone()]);
                                            symbols.update(
                                                var_name,
                                                Symbol::Variable(new_ty, true, true),
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
                            if !type_args.is_empty() {
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

            // Check if calling IO function from non-IO context
            // 1. Resolve method
            // For now, simple lookup if target is self/implicit
            let method_name = method.clone();

            // Check for built-in IO functions
            if method_name == "print" || method_name == "println" {
                if !is_io_context {
                    return Err(CompileError::from_kind(ErrorKind::IOError {
                        message: format!(
                            "Cannot call IO function '{}' from non-IO context",
                            method_name
                        ),
                    }));
                }
                return Ok(Type::Int); // Return type doesn't matter much for now
            }

            // Hack: Try to find function in symbol table
            // In a real compiler, we need to resolve type of target, then look up method in that type.
            // Here we assume global functions or local methods for simplicity of this prototype.
            if let Some(Symbol::Function(_, _, is_io_target)) = symbols.lookup(&method_name) {
                if *is_io_target && !is_io_context {
                    return Err(CompileError::from_kind(ErrorKind::IOError {
                        message: format!(
                            "Cannot call IO function '{}' from non-IO context",
                            method_name
                        ),
                    }));
                }
            }

            Ok(Type::Int)
        }
        Expr::Ask { target, message } => {
            // Check: ask() can only be used in IO context
            if !is_io_context {
                return Err(CompileError::from_kind(ErrorKind::IOError {
                    message: "Cannot use 'ask' in a pure function. Use 'io fun' instead."
                        .to_string(),
                }));
            }

            let target_ty = analyze_expr(target, symbols, is_io_context)?;
            let msg_ty = analyze_expr(message, symbols, is_io_context)?;

            if let Type::ActorRef(_) = target_ty {
                // Relaxed check: allow any message type for now
            } else {
                return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                    expected: "ActorRef".to_string(),
                    found: format!("{:?}", target_ty),
                }));
            }

            Ok(Type::Unknown) // ask returns a value (placeholder)
        }
        Expr::Send { target, message } => {
            // Check: send() can only be used in IO context
            if !is_io_context {
                return Err(CompileError::from_kind(ErrorKind::IOError {
                    message: "Cannot use 'send' in a pure function. Use 'io fun' instead."
                        .to_string(),
                }));
            }

            let target_ty = analyze_expr(target, symbols, is_io_context)?;
            let msg_ty = analyze_expr(message, symbols, is_io_context)?;

            if let Type::ActorRef(_) = target_ty {
                // Relaxed check: allow any message type for now
            } else {
                return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                    expected: "ActorRef".to_string(),
                    found: format!("{:?}", target_ty),
                }));
            }

            Ok(Type::Unit) // send returns Unit/Int (0)
        }
        Expr::Construct {
            name,
            args,
            field_names: _,
        } => {
            // Validate that 'name' is a registered data message
            match symbols.lookup(name) {
                Some(Symbol::DataMessage(fields)) => {
                    // Clone fields to avoid borrow checker issues
                    let fields = fields.clone();

                    // Validate argument count
                    if args.len() != fields.len() {
                        return Err(CompileError::from_kind(ErrorKind::ArgumentCountMismatch {
                            expected: fields.len(),
                            found: args.len(),
                        }));
                    }

                    // Analyze and type-check each argument
                    let mut errors = Vec::new();
                    for (i, arg) in args.iter().enumerate() {
                        match analyze_expr(arg, symbols, is_io_context) {
                            Ok(arg_type) => {
                                let expected_type = &fields[i].1;
                                if !check_type_compatibility(&arg_type, expected_type) {
                                    errors.push(CompileError::from_kind(ErrorKind::TypeMismatch {
                                        expected: format!("{:?}", expected_type),
                                        found: format!("{:?}", arg_type),
                                    }));
                                }
                            }
                            Err(e) => {
                                errors.push(e);
                            }
                        }
                    }

                    // If there were any errors, return them all
                    if !errors.is_empty() {
                        // For now, return the first error. A more robust system would collect all.
                        return Err(errors.remove(0));
                    }

                    // Return the constructed message type
                    Ok(Type::UserDefined(name.clone()))
                }
                Some(_) => Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("'{}' is not a data message", name),
                })),
                None => Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("Undefined data message '{}'", name),
                })),
            }
        }
        Expr::Spawn { actor_type, args } => {
            // Validate that actor_type is a registered actor
            match symbols.lookup(actor_type) {
                Some(Symbol::Actor(_)) => {
                    // Validate args
                    for arg in args {
                        analyze_expr(arg, symbols, is_io_context)?;
                    }
                    // Return ActorRef<actor_type>
                    Ok(Type::ActorRef(Box::new(Type::UserDefined(
                        actor_type.clone(),
                    ))))
                }
                _ => Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("Unknown actor type: {}", actor_type),
                })),
            }
        }
        Expr::FieldAccess(expr, field) => {
            let target_ty = analyze_expr(expr, symbols, is_io_context)?;
            match target_ty {
                Type::UserDefined(type_name) => {
                    // Look up the type definition to find fields
                    if let Some(Symbol::DataMessage(fields)) = symbols.lookup(&type_name) {
                        for (fname, fty) in fields {
                            if fname == field {
                                return Ok(fty.clone());
                            }
                        }
                        Err(CompileError::from_kind(ErrorKind::UnknownField {
                            field: field.clone(),
                            ty: type_name,
                        }))
                    } else {
                        Err(CompileError::from_kind(ErrorKind::Other {
                            message: format!("Type '{}' does not have fields", type_name),
                        }))
                    }
                }
                _ => Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("Cannot access field '{}' on type {:?}", field, target_ty),
                })),
            }
        }
    }
}
