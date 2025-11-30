use crate::ast::*;
use crate::error::{CompileError, ErrorKind};
use std::collections::HashMap;

/// Collects errors and warnings during semantic analysis
#[derive(Debug, Clone)]
pub struct ErrorCollector {
    pub errors: Vec<CompileError>,
    pub warnings: Vec<CompileError>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        ErrorCollector {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn push(&mut self, error: CompileError) {
        use crate::error::ErrorLevel;
        match error.level {
            ErrorLevel::Error => self.errors.push(error),
            ErrorLevel::Warning => self.warnings.push(error),
        }
    }

    pub fn push_error(&mut self, error: CompileError) {
        self.errors.push(error);
    }

    pub fn push_warning(&mut self, warning: CompileError) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    pub fn to_result(&self) -> Result<(), Vec<CompileError>> {
        if self.has_errors() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }

    pub fn all_messages(&self) -> Vec<CompileError> {
        let mut all = self.errors.clone();
        all.extend(self.warnings.clone());
        all
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
    // DataMessage removed in Phase 5 - functions define message structure
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

    pub fn all_symbols(&self) -> Vec<String> {
        let mut symbols = Vec::new();
        for scope in &self.scopes {
            for key in scope.keys() {
                symbols.push(key.clone());
            }
        }
        symbols
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

    pub fn get_initialization_snapshot(&self) -> Vec<Vec<(String, bool)>> {
        self.scopes
            .iter()
            .map(|scope| {
                scope
                    .iter()
                    .filter_map(|(name, sym)| {
                        if let Symbol::Variable(_, _, init) = sym {
                            Some((name.clone(), *init))
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .collect()
    }

    pub fn restore_initialization_snapshot(&mut self, snapshot: &Vec<Vec<(String, bool)>>) {
        for (i, scope_snapshot) in snapshot.iter().enumerate() {
            if i < self.scopes.len() {
                let scope = &mut self.scopes[i];
                for (name, init) in scope_snapshot {
                    if let Some(Symbol::Variable(_, _, ref mut current_init)) = scope.get_mut(name)
                    {
                        *current_init = *init;
                    }
                }
            }
        }
    }

    pub fn set_initialization(&mut self, init_snapshot: Vec<Vec<(String, bool)>>) {
        for (scope, snapshot) in self.scopes.iter_mut().zip(init_snapshot.iter()) {
            for (name, is_init) in snapshot {
                if let Some(Symbol::Variable(ty, is_mut, _)) = scope.get(name) {
                    scope.insert(
                        name.clone(),
                        Symbol::Variable(ty.clone(), *is_mut, *is_init),
                    );
                }
            }
        }
    }
}

/// Tracks usage of variables and functions for warning generation
#[derive(Debug, Clone)]
pub struct UsageTracker {
    /// Maps variable names to their definition spans
    variable_definitions: HashMap<String, crate::error::Span>,
    /// Set of variables that have been used
    variable_usages: std::collections::HashSet<String>,
    /// Maps function names to (span, is_io)
    function_definitions: HashMap<String, (crate::error::Span, bool)>,
    /// Set of functions that have been called
    function_calls: std::collections::HashSet<String>,
}

impl UsageTracker {
    pub fn new() -> Self {
        UsageTracker {
            variable_definitions: HashMap::new(),
            variable_usages: std::collections::HashSet::new(),
            function_definitions: HashMap::new(),
            function_calls: std::collections::HashSet::new(),
        }
    }

    /// Track a variable definition
    pub fn define_variable(&mut self, name: String, span: crate::error::Span) {
        // Skip underscore-prefixed variables (convention for intentionally unused)
        if !name.starts_with('_') {
            self.variable_definitions.insert(name, span);
        }
    }

    /// Track a variable usage
    pub fn use_variable(&mut self, name: &str) {
        self.variable_usages.insert(name.to_string());
    }

    /// Track a function definition
    pub fn define_function(&mut self, name: String, span: crate::error::Span, is_io: bool) {
        // Skip main and IO handlers
        if name != "main" && name != "run" {
            self.function_definitions.insert(name, (span, is_io));
        }
    }

    /// Track a function call
    pub fn call_function(&mut self, name: &str) {
        self.function_calls.insert(name.to_string());
    }

    /// Generate warnings for unused variables and functions
    pub fn generate_warnings(&self) -> Vec<CompileError> {
        let mut warnings = Vec::new();

        // Check for unused variables
        for (name, span) in &self.variable_definitions {
            if !self.variable_usages.contains(name) {
                warnings.push(
                    CompileError::warning(ErrorKind::UnusedVariable { name: name.clone() })
                        .with_span(*span),
                );
            }
        }

        // Check for unused pure functions (not IO functions)
        for (name, (span, is_io)) in &self.function_definitions {
            if !is_io && !self.function_calls.contains(name) {
                warnings.push(
                    CompileError::warning(ErrorKind::UnusedFunction { name: name.clone() })
                        .with_span(*span),
                );
            }
        }

        warnings
    }
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_len = s1.chars().count();
    let s2_len = s2.chars().count();
    let mut matrix = vec![vec![0; s2_len + 1]; s1_len + 1];

    for i in 0..=s1_len {
        matrix[i][0] = i;
    }
    for j in 0..=s2_len {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = std::cmp::min(
                std::cmp::min(matrix[i][j + 1] + 1, matrix[i + 1][j] + 1),
                matrix[i][j] + cost,
            );
        }
    }
    matrix[s1_len][s2_len]
}

fn find_suggestion(name: &str, symbols: &SymbolTable) -> Option<String> {
    let mut all_symbols = symbols.all_symbols();
    // Add built-in functions
    all_symbols.push("print".to_string());
    all_symbols.push("println".to_string());

    let mut best_match = None;
    let mut min_distance = usize::MAX;

    for symbol in all_symbols {
        let distance = levenshtein_distance(name, &symbol);
        if distance < min_distance {
            min_distance = distance;
            best_match = Some(symbol);
        }
    }

    // Threshold for suggestion (e.g., distance <= 3)
    if min_distance <= 3 && min_distance > 0 {
        best_match
    } else {
        None
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
                    // Verify it's registered as a DataMessage or Actor in symbol table
                    match symbols.lookup(name) {
                        // DataMessage removed in Phase 5
                        Some(Symbol::Actor(_)) => Ok(()),
                        _ => Err(format!(
                            "ActorRef message type '{}' is not a valid data message or actor",
                            name
                        )),
                    }
                }
                _ => Err(format!(
                    "ActorRef message type must be a data message or actor name, found: {:?}",
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

pub fn analyze(program: &Program) -> Result<(SymbolTable, Vec<CompileError>), Vec<CompileError>> {
    let mut symbols = SymbolTable::new();
    let mut error_collector = ErrorCollector::new();
    let mut tracker = UsageTracker::new();

    // 1. Register top-level definitions
    // First pass: Register all data messages
    for def in &program.definitions {
        // DataMessage handling removed in Phase 5
        if false { // Disabled DataMessage processing
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
            // Definition::DataMessage removed in Phase 5
            Definition::Function(func) => {
                let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
                symbols.insert(
                    func.name.clone(),
                    Symbol::Function(param_types, func.return_type.clone(), func.is_io),
                );

                // Track function definition
                // TODO: Add spans to FunctionDef in the future
                let dummy_span = crate::error::Span::new(0, 0);
                tracker.define_function(func.name.clone(), dummy_span, func.is_io);
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
                if let Err(errors) = analyze_function(func, &mut symbols, &mut tracker) {
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

                // Enter actor scope
                symbols.enter_scope();

                // Register actor parameters
                for param in &actor.params {
                    symbols.insert(
                        param.name.clone(),
                        Symbol::Variable(param.ty.clone(), false, true), // immutable, initialized
                    );
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
                            if let Err(e) =
                                analyze_expr(&field.init, &mut symbols, false, &mut tracker)
                            {
                                error_collector.push(e);
                            }
                        }
                        ActorMember::Method(method) => {
                            if let Err(errors) =
                                analyze_function(method, &mut symbols, &mut tracker)
                            {
                                for err in errors {
                                    error_collector.push(err);
                                }
                            }
                        }
                    }
                }

                // Exit actor scope
                symbols.exit_scope();
            }
            _ => {}
        }
    }

    // Generate warnings from usage tracker
    let warnings = tracker.generate_warnings();
    for warning in &warnings {
        error_collector.push(warning.clone());
    }

    if error_collector.has_errors() {
        error_collector
            .to_result()
            .map(|_| (symbols, error_collector.warnings.clone()))
    } else {
        Ok((symbols, error_collector.warnings))
    }
}

fn analyze_function(
    func: &FunctionDef,
    symbols: &mut SymbolTable,
    tracker: &mut UsageTracker,
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
        if let Err(e) = analyze_stmt(stmt, symbols, func.is_io, tracker) {
            error_collector.push(e);
        }
    }

    symbols.exit_scope();

    // Return all collected errors
    error_collector.to_result()
}

pub fn analyze_stmt(
    stmt: &Stmt,
    symbols: &mut SymbolTable,
    is_io_context: bool,
    tracker: &mut UsageTracker,
) -> Result<(), CompileError> {
    match &stmt.kind {
        StmtKind::Let(name, ty_opt, expr) => {
            // Check for duplicate definition in current scope
            if symbols.lookup_in_current_scope(name).is_some() {
                return Err(CompileError::from_kind(ErrorKind::DuplicateDefinition {
                    name: name.clone(),
                })
                .with_span(stmt.span));
            }

            let expr_ty = analyze_expr(expr, symbols, is_io_context, tracker)?;
            if let Some(expected_ty) = ty_opt {
                if !check_type_compatibility(expected_ty, &expr_ty) {
                    return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                        expected: format!("{:?}", expected_ty),
                        found: format!("{:?}", expr_ty),
                    })
                    .with_span(expr.span));
                }
            }
            symbols.insert(name.clone(), Symbol::Variable(expr_ty, false, true));
            // Track variable definition
            tracker.define_variable(name.clone(), stmt.span);
            // let is always initialized
        }
        StmtKind::Var(name, ty_opt, init_opt) => {
            // Check for duplicate definition in current scope
            if symbols.lookup_in_current_scope(name).is_some() {
                return Err(CompileError::from_kind(ErrorKind::DuplicateDefinition {
                    name: name.clone(),
                })
                .with_span(stmt.span));
            }

            let var_ty = match (ty_opt, init_opt) {
                // Case 1: Both type and initializer
                (Some(ty), Some(init)) => {
                    let init_ty = analyze_expr(init, symbols, is_io_context, tracker)?;
                    if !check_type_compatibility(ty, &init_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: format!("{:?}", ty),
                            found: format!("{:?}", init_ty),
                        })
                        .with_span(init.span));
                    }
                    ty.clone()
                }
                // Case 2: Only initializer (type inference)
                (None, Some(init)) => analyze_expr(init, symbols, is_io_context, tracker)?,
                // Case 3: Only type annotation (no initializer) - this is valid
                (Some(ty), None) => ty.clone(),
                // Case 4: Neither type nor initializer - ERROR
                (None, None) => {
                    return Err(CompileError::from_kind(ErrorKind::Other {
                        message: format!(
                            "Variable '{}' must have either a type annotation or an initializer",
                            name
                        ),
                    })
                    .with_span(stmt.span));
                }
            };

            // Var is initialized if init_opt.is_some()
            let is_initialized = init_opt.is_some();
            symbols.insert(name.clone(), Symbol::Variable(var_ty, true, is_initialized));
            // Track variable definition
            tracker.define_variable(name.clone(), stmt.span);
        }
        StmtKind::Assign(name, expr) => {
            // Check variable exists and is mutable
            let (var_ty, is_mutable) = if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(var_ty, is_mutable, _) => {
                        if !*is_mutable {
                            return Err(CompileError::from_kind(ErrorKind::ImmutableAssignment {
                                name: name.clone(),
                            })
                            .with_span(stmt.span));
                        }
                        (var_ty.clone(), *is_mutable)
                    }
                    _ => {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: format!(
                                "'{}' is not a variable and cannot be assigned to",
                                name
                            ),
                        })
                        .with_span(stmt.span));
                    }
                }
            } else {
                return Err(CompileError::from_kind(ErrorKind::UndefinedVariable {
                    name: name.clone(),
                })
                .with_span(stmt.span));
            };

            // Analyze expression after releasing the borrow
            analyze_expr(expr, symbols, is_io_context, tracker)?;
            // After assignment, variable is initialized
            symbols.update(name, Symbol::Variable(var_ty, is_mutable, true));
        }
        StmtKind::AssignIndex(name, index, value) => {
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
                            })
                            .with_span(stmt.span));
                        }
                        // TODO: Check if ty is a Map or List that supports assignment
                        // For now, assume it is if it's a variable
                    }
                    _ => {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: format!("'{}' is not a variable", name),
                        })
                        .with_span(stmt.span));
                    }
                }
            } else {
                return Err(CompileError::from_kind(ErrorKind::UndefinedVariable {
                    name: name.clone(),
                })
                .with_span(stmt.span));
            }
            analyze_expr(index, symbols, is_io_context, tracker)?;
            analyze_expr(value, symbols, is_io_context, tracker)?;
        }
        StmtKind::Expr(expr) => {
            analyze_expr(expr, symbols, is_io_context, tracker)?;
        }
        StmtKind::Return(expr) => {
            if let Some(e) = expr {
                analyze_expr(e, symbols, is_io_context, tracker)?;
            }
        }
        StmtKind::Reply(expr) => {
            // Phase 5: Reply can only be used in io fun
            if !is_io_context {
                return Err(CompileError::from_kind(ErrorKind::IOError {
                    message: "Cannot use 'reply' in a pure function. Use 'io fun' instead."
                        .to_string(),
                })
                .with_span(stmt.span));
            }
            analyze_expr(expr, symbols, is_io_context, tracker)?;
        }
        StmtKind::If(cond, then_block, else_block) => {
            analyze_expr(cond, symbols, is_io_context, tracker)?;
            // TODO: Check cond is boolean

            // Snapshot initialization state before branching
            let pre_state = symbols.get_initialization_snapshot();

            // Analyze 'then' block
            symbols.enter_scope();
            for s in &then_block.stmts {
                analyze_stmt(s, symbols, is_io_context, tracker)?;
            }
            symbols.exit_scope();

            // Capture state after 'then' block
            let then_state = symbols.get_initialization_snapshot();

            // Restore state to pre-branching for 'else' block
            symbols.restore_initialization_snapshot(&pre_state);

            // Analyze 'else' block
            let else_state = if let Some(else_block) = else_block {
                symbols.enter_scope();
                for s in &else_block.stmts {
                    analyze_stmt(s, symbols, is_io_context, tracker)?;
                }
                symbols.exit_scope();
                symbols.get_initialization_snapshot()
            } else {
                pre_state.clone() // If no else, state is same as pre-branching
            };

            // Merge states: Variable is initialized only if initialized in BOTH branches
            let mut merged_state = pre_state.clone();

            for (i, scope) in merged_state.iter_mut().enumerate() {
                for (name, init) in scope.iter_mut() {
                    // Find status in then_state
                    let then_init = if i < then_state.len() {
                        then_state[i]
                            .iter()
                            .find(|(n, _)| n == name)
                            .map(|(_, init)| *init)
                            .unwrap_or(*init)
                    } else {
                        *init
                    };

                    // Find status in else_state
                    let else_init = if i < else_state.len() {
                        else_state[i]
                            .iter()
                            .find(|(n, _)| n == name)
                            .map(|(_, init)| *init)
                            .unwrap_or(*init)
                    } else {
                        *init
                    };

                    // Intersection: initialized only if initialized in both
                    *init = then_init && else_init;
                }
            }

            symbols.restore_initialization_snapshot(&merged_state);
        }
        StmtKind::While(cond, body) => {
            analyze_expr(cond, symbols, is_io_context, tracker)?;

            symbols.enter_scope();
            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context, tracker)?;
            }
            symbols.exit_scope();
        }
        StmtKind::For(iter, start, end, body) => {
            analyze_expr(start, symbols, is_io_context, tracker)?;
            analyze_expr(end, symbols, is_io_context, tracker)?;

            symbols.enter_scope();
            symbols.insert(iter.clone(), Symbol::Variable(Type::Int, false, true)); // Iterator is immutable int, initialized

            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context, tracker)?;
            }
            symbols.exit_scope();
        }
        StmtKind::ForEach(iter, collection, body) => {
            let collection_ty = analyze_expr(collection, symbols, is_io_context, tracker)?;
            let item_ty = match collection_ty {
                Type::Generic(name, args) if name == "List" || name == "Set" => {
                    if !args.is_empty() {
                        args[0].clone()
                    } else {
                        return Err(CompileError::from_kind(ErrorKind::Other {
                            message: "Collection must have a type argument".to_string(),
                        })
                        .with_span(stmt.span));
                    }
                }
                Type::Generic(name, _) if name == "Map" => {
                    return Err(CompileError::from_kind(ErrorKind::Other {
                        message: "Cannot iterate over Map directly. Use map.keys() or map.values()"
                            .to_string(),
                    })
                    .with_span(stmt.span));
                }
                _ => {
                    return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                        expected: "Collection (List or Set)".to_string(),
                        found: format!("{:?}", collection_ty),
                    })
                    .with_span(stmt.span));
                }
            };

            symbols.enter_scope();
            symbols.insert(iter.clone(), Symbol::Variable(item_ty, false, true)); // initialized
            for s in &body.stmts {
                analyze_stmt(s, symbols, is_io_context, tracker)?;
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
    tracker: &mut UsageTracker,
) -> Result<Type, CompileError> {
    match &expr.kind {
        ExprKind::Literal(lit) => match lit {
            Literal::Int(_) => Ok(Type::Int),
            Literal::String(_) => Ok(Type::String),
            Literal::Bool(_) => Ok(Type::Bool),
        },
        ExprKind::Variable(name) => {
            if let Some(sym) = symbols.lookup(name) {
                match sym {
                    Symbol::Variable(ty, _, is_initialized) => {
                        if !is_initialized {
                            return Err(CompileError::from_kind(
                                ErrorKind::UninitializedVariable { name: name.clone() },
                            )
                            .with_span(expr.span));
                        }
                        // Track variable usage
                        tracker.use_variable(name);
                        Ok(ty.clone())
                    }
                    _ => Err(CompileError::from_kind(ErrorKind::Other {
                        message: format!("'{}' is not a variable", name),
                    })
                    .with_span(expr.span)),
                }
            } else {
                println!("DEBUG: UndefinedVariable: {}", name);
                let mut err =
                    CompileError::from_kind(ErrorKind::UndefinedVariable { name: name.clone() })
                        .with_span(expr.span);
                if let Some(suggestion) = find_suggestion(name, symbols) {
                    err = err.with_suggestion(format!("Did you mean '{}'?", suggestion));
                }
                Err(err)
            }
        }
        ExprKind::BinaryOp(lhs, op, rhs) => {
            let lhs_ty = analyze_expr(lhs, symbols, is_io_context, tracker)?;
            let rhs_ty = analyze_expr(rhs, symbols, is_io_context, tracker)?;

            match op {
                BinaryOp::Add => {
                    if check_type_compatibility(&Type::Int, &lhs_ty)
                        && check_type_compatibility(&Type::Int, &rhs_ty)
                    {
                        Ok(Type::Int)
                    } else if check_type_compatibility(&Type::String, &lhs_ty)
                        || check_type_compatibility(&Type::String, &rhs_ty)
                    {
                        // If either operand is a String, the result is a String (concatenation)
                        Ok(Type::String)
                    } else {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: "Int or String".to_string(),
                            found: format!("{:?} + {:?}", lhs_ty, rhs_ty),
                        })
                        .with_span(expr.span));
                    }
                }
                BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                    if !check_type_compatibility(&Type::Int, &lhs_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: "Int".to_string(),
                            found: format!("{:?}", lhs_ty),
                        })
                        .with_span(lhs.span));
                    }
                    if !check_type_compatibility(&Type::Int, &rhs_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: "Int".to_string(),
                            found: format!("{:?}", rhs_ty),
                        })
                        .with_span(rhs.span));
                    }
                    Ok(Type::Int)
                }
                BinaryOp::Eq | BinaryOp::Ne => {
                    if !check_type_compatibility(&lhs_ty, &rhs_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: format!("{:?}", lhs_ty),
                            found: format!("{:?}", rhs_ty),
                        })
                        .with_span(expr.span));
                    }
                    Ok(Type::Bool)
                }
                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                    if !check_type_compatibility(&Type::Int, &lhs_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: "Int".to_string(),
                            found: format!("{:?}", lhs_ty),
                        })
                        .with_span(lhs.span));
                    }
                    if !check_type_compatibility(&Type::Int, &rhs_ty) {
                        return Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                            expected: "Int".to_string(),
                            found: format!("{:?}", rhs_ty),
                        })
                        .with_span(rhs.span));
                    }
                    Ok(Type::Bool)
                }
            }
        }
        ExprKind::Index(target, index) => {
            let target_ty = analyze_expr(target, symbols, is_io_context, tracker)?;
            analyze_expr(index, symbols, is_io_context, tracker)?;

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
        ExprKind::Call(target, method, args) => {
            // Track function call
            tracker.call_function(method);

            for arg in args {
                analyze_expr(arg, symbols, is_io_context, tracker)?;
            }

            // Check if it's a method call on a variable
            if let ExprKind::Variable(var_name) = &target.kind {
                if let Some(Symbol::Variable(ty, _, _)) = symbols.lookup(var_name) {
                    // eprintln!(
                    //     "DEBUG: Method call '{}' on variable '{}' with type {:?}",
                    //     method, var_name, ty
                    // );
                }
            }

            if let ExprKind::Variable(name) = &target.kind {
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
            let is_global_function = if let ExprKind::Variable(name) = &target.kind {
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
                        let mut err = CompileError::from_kind(ErrorKind::UnknownFunction {
                            name: method.clone(),
                        })
                        .with_span(expr.span);
                        if let Some(suggestion) = find_suggestion(method, symbols) {
                            err = err.with_suggestion(format!("Did you mean '{}'?", suggestion));
                        }
                        return Err(err);
                    }
                }
                Type::Unknown // Placeholder, won't be used
            } else {
                analyze_expr(target, symbols, is_io_context, tracker)?
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
                                        let arg_ty =
                                            analyze_expr(arg, symbols, is_io_context, tracker)?;
                                        if let ExprKind::Variable(var_name) = &target.kind {
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
                                        new_args[0] =
                                            analyze_expr(arg, symbols, is_io_context, tracker)?;
                                        changed = true;
                                    }
                                }
                                if let Type::Unknown = type_args[1] {
                                    if let Some(arg) = args.get(1) {
                                        new_args[1] =
                                            analyze_expr(arg, symbols, is_io_context, tracker)?;
                                        changed = true;
                                    }
                                }

                                if changed {
                                    if let ExprKind::Variable(var_name) = &target.kind {
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
                                        let arg_ty =
                                            analyze_expr(arg, symbols, is_io_context, tracker)?;
                                        if let ExprKind::Variable(var_name) = &target.kind {
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
                    })
                    .with_span(expr.span));
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
                    })
                    .with_span(expr.span));
                }
            }

            Ok(Type::Int)
        }
        ExprKind::Ask {
            target,
            method,
            args,
        } => {
            // Check: ask() can only be used in IO context
            if !is_io_context {
                return Err(CompileError::from_kind(ErrorKind::IOError {
                    message: "Cannot use 'ask' in a pure function. Use 'io fun' instead."
                        .to_string(),
                })
                .with_span(expr.span));
            }

            let target_ty = analyze_expr(target, symbols, is_io_context, tracker)?;

            // Analyze all args
            for arg in args {
                analyze_expr(arg, symbols, is_io_context, tracker)?;
            }

            if let Type::ActorRef(_) = target_ty {
                // TODO: Check method exists and return type matches
                Ok(Type::Unknown) // Placeholder
            } else {
                Err(CompileError::from_kind(ErrorKind::TypeMismatch {
                    expected: "ActorRef".to_string(),
                    found: format!("{:?}", target_ty),
                })
                .with_span(target.span))
            }
        }
        ExprKind::Send {
            target,
            method,
            args,
        } => {
            // Check: send() can only be used in IO context
            if !is_io_context {
                return Err(CompileError::from_kind(ErrorKind::IOError {
                    message: "Cannot use 'send' in a pure function. Use 'io fun' instead."
                        .to_string(),
                })
                .with_span(expr.span));
            }

            let target_ty = analyze_expr(target, symbols, is_io_context, tracker)?;

            // Analyze all args
            for arg in args {
                analyze_expr(arg, symbols, is_io_context, tracker)?;
            }

            if let Type::ActorRef(_) = target_ty {
                // send returns Unit
            } else {
                return Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("'send' requires an ActorRef target, found {:?}", target_ty),
                })
                .with_span(target.span));
            }

            Ok(Type::Unit) // send returns Unit/Int (0)
        }
        // ExprKind::Construct removed in Phase 5 - data messages no longer exist
        ExprKind::Construct { .. } => {
            // Return error - construct syntax no longer supported
            Err(CompileError::from_kind(ErrorKind::Other {
                message: "Data message construction is not supported in Phase 5. Use function calls instead.".to_string()
            }).with_span(expr.span))
        }
        ExprKind::Spawn { actor_type, args } => {
            // Validate that actor_type is a registered actor
            match symbols.lookup(actor_type) {
                Some(Symbol::Actor(_)) => {
                    // Validate args
                    for arg in args {
                        analyze_expr(arg, symbols, is_io_context, tracker)?;
                    }
                    // Return ActorRef<actor_type>
                    Ok(Type::ActorRef(Box::new(Type::UserDefined(
                        actor_type.clone(),
                    ))))
                }
                _ => {
                    let mut err = CompileError::from_kind(ErrorKind::Other {
                        message: format!("Unknown actor type: {}", actor_type),
                    })
                    .with_span(expr.span);
                    if let Some(suggestion) = find_suggestion(actor_type, symbols) {
                        err = err.with_suggestion(format!("Did you mean '{}'?", suggestion));
                    }
                    Err(err)
                }
            }
        }
        ExprKind::FieldAccess(expr_inner, field) => {
            let target_ty = analyze_expr(expr_inner, symbols, is_io_context, tracker)?;
            match target_ty {
                Type::UserDefined(type_name) => {
                    // DataMessage removed in Phase 5 - field access not supported
                    Err(CompileError::from_kind(ErrorKind::Other {
                        message: format!(
                            "Type '{}' does not have fields (data messages removed in Phase 5)",
                            type_name
                        ),
                    })
                    .with_span(expr.span))
                }
                _ => Err(CompileError::from_kind(ErrorKind::Other {
                    message: format!("Cannot access field '{}' on type {:?}", field, target_ty),
                })
                .with_span(expr.span)),
            }
        }
    }
}
