use crate::ast::*;
use crate::lexer::Token;
use chumsky::prelude::*;

#[derive(Clone, Debug)]
enum CallSuffix {
    Method(String, Option<Vec<Expr>>), // .name(args) or .name
    Call(Vec<Expr>),                   // (args)
    Index(Expr),                       // [index]
}

// Helper function to parse expressions from token sequence (for string interpolation)
fn parse_expr_from_tokens(tokens: &[Token]) -> Result<Expr, String> {
    if tokens.is_empty() {
        return Err("Empty expression".to_string());
    }

    let mut pos = 0;
    parse_comparison(tokens, &mut pos)
}

fn parse_comparison(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_additive(tokens, pos)?;

    while *pos < tokens.len() {
        let op = match &tokens[*pos] {
            Token::EqEq => BinaryOp::Eq,
            Token::NotEq => BinaryOp::Ne,
            Token::Lt => BinaryOp::Lt,
            Token::LtEq => BinaryOp::Le,
            Token::Gt => BinaryOp::Gt,
            Token::GtEq => BinaryOp::Ge,
            _ => break,
        };
        *pos += 1;
        let right = parse_additive(tokens, pos)?;
        left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_additive(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_multiplicative(tokens, pos)?;

    while *pos < tokens.len() {
        let op = match &tokens[*pos] {
            Token::Plus => BinaryOp::Add,
            Token::Minus => BinaryOp::Sub,
            _ => break,
        };
        *pos += 1;
        let right = parse_multiplicative(tokens, pos)?;
        left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_multiplicative(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_postfix(tokens, pos)?;

    while *pos < tokens.len() {
        let op = match &tokens[*pos] {
            Token::Star => BinaryOp::Mul,
            Token::Slash => BinaryOp::Div,
            _ => break,
        };
        *pos += 1;
        let right = parse_postfix(tokens, pos)?;
        left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_postfix(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut expr = parse_atom(tokens, pos)?;

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::LBracket => {
                *pos += 1; // Skip '['
                let index = parse_comparison(tokens, pos)?;
                if *pos < tokens.len() && tokens[*pos] == Token::RBracket {
                    *pos += 1; // Skip ']'
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                } else {
                    return Err("Expected ']'".to_string());
                }
            }
            Token::Dot => {
                *pos += 1; // Skip '.'
                if *pos < tokens.len() {
                    if let Token::Ident(method_name) = &tokens[*pos] {
                        *pos += 1; // Skip method/field name

                        // Check for arguments (method call)
                        if *pos < tokens.len() && tokens[*pos] == Token::LParen {
                            *pos += 1; // Skip '('
                            let mut args = Vec::new();
                            if *pos < tokens.len() && tokens[*pos] != Token::RParen {
                                loop {
                                    args.push(parse_comparison(tokens, pos)?);
                                    if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                        *pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                            }

                            if *pos < tokens.len() && tokens[*pos] == Token::RParen {
                                *pos += 1; // Skip ')'
                                expr = Expr::Call(Box::new(expr), method_name.clone(), args);
                            } else {
                                return Err("Expected ')'".to_string());
                            }
                        } else {
                            // Field access (no parentheses)
                            expr = Expr::FieldAccess(Box::new(expr), method_name.clone());
                        }
                    } else {
                        return Err("Expected field or method name after '.'".to_string());
                    }
                } else {
                    return Err("Unexpected end after '.'".to_string());
                }
            }
            _ => break,
        }
    }
    Ok(expr)
}

fn parse_atom(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    if *pos >= tokens.len() {
        return Err("Unexpected end of expression".to_string());
    }

    match &tokens[*pos] {
        Token::Int(i) => {
            *pos += 1;
            Ok(Expr::Literal(Literal::Int(*i)))
        }
        Token::String(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Literal(Literal::String(s)))
        }
        Token::Ident(name) => {
            let name = name.clone();
            *pos += 1;

            // Check for spawn(ActorType, args)
            if name == "spawn" && *pos < tokens.len() && tokens[*pos] == Token::LParen {
                *pos += 1; // Skip '('

                // Parse actor type (first argument)
                if *pos < tokens.len() {
                    if let Token::Ident(actor_type) = &tokens[*pos] {
                        let actor_type = actor_type.clone();
                        *pos += 1;

                        // Parse arguments
                        let mut args = Vec::new();
                        if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                            *pos += 1; // Skip comma after actor type

                            // Parse argument list
                            loop {
                                args.push(parse_comparison(tokens, pos)?);

                                if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                    *pos += 1;
                                } else {
                                    break;
                                }
                            }
                        }

                        if *pos < tokens.len() && tokens[*pos] == Token::RParen {
                            *pos += 1; // Skip ')'
                            return Ok(Expr::Spawn { actor_type, args });
                        } else {
                            return Err("Expected ')' in spawn expression".to_string());
                        }
                    } else {
                        return Err("Expected actor type after spawn(".to_string());
                    }
                } else {
                    return Err("Unexpected end in spawn expression".to_string());
                }
            }

            Ok(Expr::Variable(name))
        }
        Token::LParen => {
            *pos += 1; // Skip '('
            let expr = parse_comparison(tokens, pos)?;
            if *pos < tokens.len() && tokens[*pos] == Token::RParen {
                *pos += 1; // Skip ')'
                Ok(expr)
            } else {
                Err("Expected ')'".to_string())
            }
        }
        _ => Err(format!("Unexpected token: {:?}", tokens[*pos])),
    }
}

pub fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let ident = select! { Token::Ident(ident) => ident };
    let int_lit = select! { Token::Int(i) => Literal::Int(i) };
    let str_lit = select! { Token::String(s) => s };

    let type_parser = recursive(|type_parser| {
        ident
            .then(
                type_parser
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::Lt), just(Token::Gt))
                    .or_not(),
            )
            .map(|(name, args)| match name.as_str() {
                "Int" => Type::Int,
                "String" => Type::String,
                "Bool" => Type::Bool,
                "ActorRef" => {
                    // ActorRef<MessageType>
                    if let Some(mut args) = args {
                        if args.len() == 1 {
                            Type::ActorRef(Box::new(args.remove(0)))
                        } else {
                            Type::Unknown // Error: ActorRef requires exactly one type argument
                        }
                    } else {
                        Type::Unknown // Error: ActorRef requires a type argument
                    }
                }
                _ => {
                    if let Some(args) = args {
                        Type::Generic(name, args)
                    } else {
                        Type::UserDefined(name)
                    }
                }
            })
    });

    let expr = recursive(|expr| {
        /* Old ask syntax removed
        let ask_expr = just(Token::Ask) ...
        */

        let atom = choice((
            // ask_expr, // Removed
            int_lit.map(Expr::Literal),
            str_lit.map(|s| {
                use crate::lexer::Token as LexToken;
                use logos::Logos;

                // Parse interpolated string: "Hello ${name}!" or "Result: ${a + b}"
                let mut parts = Vec::new();
                let mut last_end = 0;
                let chars: Vec<(usize, char)> = s.char_indices().collect();
                let len = s.len();

                let mut i = 0;
                while i < chars.len() {
                    let (idx, c) = chars[i];
                    if c == '$' && i + 1 < chars.len() && chars[i + 1].1 == '{' {
                        // Found start of interpolation
                        if idx > last_end {
                            parts
                                .push(Expr::Literal(Literal::String(s[last_end..idx].to_string())));
                        }

                        // Find end of interpolation
                        let start_var = i + 2;
                        let mut end_var = start_var;
                        let mut brace_depth = 1;
                        while end_var < chars.len() && brace_depth > 0 {
                            if chars[end_var].1 == '{' {
                                brace_depth += 1;
                            } else if chars[end_var].1 == '}' {
                                brace_depth -= 1;
                            }
                            if brace_depth > 0 {
                                end_var += 1;
                            }
                        }

                        if end_var < chars.len() {
                            let expr_start = chars[start_var].0;
                            let expr_end = chars[end_var].0;
                            let expr_str =
                                s[expr_start..expr_end].to_string().replace("\\\"", "\"");

                            // Tokenize the expression
                            let mut lexer = LexToken::lexer(&expr_str);
                            let tokens: Vec<LexToken> = lexer.filter_map(|t| t.ok()).collect();

                            // Parse the expression from tokens
                            if let Ok(parsed_expr) = parse_expr_from_tokens(&tokens) {
                                parts.push(parsed_expr);
                            } else {
                                // Fallback: treat as variable name
                                parts.push(Expr::Variable(expr_str));
                            }

                            last_end = chars[end_var].0 + 1; // Skip '}'
                            i = end_var;
                        }
                    }
                    i += 1;
                }

                if last_end < len {
                    parts.push(Expr::Literal(Literal::String(s[last_end..len].to_string())));
                }

                if parts.is_empty() {
                    Expr::Literal(Literal::String("".to_string()))
                } else {
                    let mut expr = parts[0].clone();
                    for part in parts.into_iter().skip(1) {
                        expr = Expr::BinaryOp(Box::new(expr), BinaryOp::Add, Box::new(part));
                    }
                    expr
                }
            }),
            // spawn(ActorType, args) special form
            just(Token::Ident("spawn".to_string()))
                .ignore_then(
                    ident
                        .then(expr.clone().separated_by(just(Token::Comma)).or_not())
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(actor_type, args)| Expr::Spawn {
                    actor_type,
                    args: args.unwrap_or_default(),
                }),
            ident.map(Expr::Variable),
            expr.clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        let call = atom
            .clone()
            .then(
                choice((
                    // Method call or field access: .ident(args) or .ident
                    just(Token::Dot)
                        .ignore_then(ident)
                        .then(
                            expr.clone()
                                .separated_by(just(Token::Comma))
                                .delimited_by(just(Token::LParen), just(Token::RParen))
                                .or_not(),
                        )
                        .map(|(method, args)| CallSuffix::Method(method, args)),
                    // Function call: (args)
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map(CallSuffix::Call),
                    // Index access: [index]
                    expr.clone()
                        .delimited_by(just(Token::LBracket), just(Token::RBracket))
                        .map(CallSuffix::Index),
                ))
                .repeated(),
            )
            .foldl(|target, suffix| match suffix {
                CallSuffix::Index(index) => Expr::Index(Box::new(target), Box::new(index)),
                CallSuffix::Call(args) => match target {
                    Expr::Variable(name) => {
                        if let Some(first_char) = name.chars().next() {
                            if first_char.is_uppercase() {
                                // Uppercase: treat as message constructor
                                Expr::Construct {
                                    name: name.clone(),
                                    args,
                                    field_names: vec![], // Parser doesn't have field names, sema will fill this
                                }
                            } else {
                                Expr::Call(Box::new(Expr::Variable("self".to_string())), name, args)
                            }
                        } else {
                            Expr::Call(Box::new(Expr::Variable(name)), "invoke".to_string(), args)
                        }
                    }
                    _ => Expr::Call(Box::new(target), "invoke".to_string(), args),
                },
                CallSuffix::Method(m, args) => {
                    if let Some(args) = args {
                        if m == "send" && args.len() == 1 {
                            Expr::Send {
                                target: Box::new(target),
                                message: Box::new(args.into_iter().next().unwrap()),
                            }
                        } else if m == "ask" && args.len() == 1 {
                            Expr::Ask {
                                target: Box::new(target),
                                message: Box::new(args.into_iter().next().unwrap()),
                            }
                        } else {
                            Expr::Call(Box::new(target), m, args)
                        }
                    } else {
                        Expr::FieldAccess(Box::new(target), m)
                    }
                }
            });

        let op = |c| just(c);

        let product = call
            .clone()
            .then(
                op(Token::Star)
                    .to(BinaryOp::Mul)
                    .or(op(Token::Slash).to(BinaryOp::Div))
                    .then(call.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op(Token::Plus)
                    .to(BinaryOp::Add)
                    .or(op(Token::Minus).to(BinaryOp::Sub))
                    .then(product.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        sum.clone()
            .then(
                choice((
                    op(Token::EqEq).to(BinaryOp::Eq),
                    op(Token::NotEq).to(BinaryOp::Ne),
                    op(Token::LtEq).to(BinaryOp::Le),
                    op(Token::Lt).to(BinaryOp::Lt),
                    op(Token::GtEq).to(BinaryOp::Ge),
                    op(Token::Gt).to(BinaryOp::Gt),
                ))
                .then(sum.clone())
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)))
    });

    let stmt = recursive(|stmt| {
        let block = stmt
            .clone()
            .repeated()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|stmts| Block { stmts });

        let let_stmt = just(Token::Val)
            .ignore_then(ident)
            .then(just(Token::Colon).ignore_then(type_parser.clone()).or_not())
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map(|((name, ty), init)| Stmt::Let(name, ty, init));

        let var_stmt = just(Token::Var)
            .ignore_then(ident)
            .then(just(Token::Colon).ignore_then(type_parser.clone()).or_not())
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map(|((name, ty), init)| Stmt::Var(name, ty, init));

        let return_stmt = just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .map(Stmt::Return);

        // Old send syntax removed. Send is now an expression.
        /*
        let send_stmt = just(Token::Send)
            .ignore_then(
                ident
                    .clone() // Actor name (identifier)
                    .then_ignore(just(Token::Comma))
                    .then(ident.clone()) // Handler name (identifier)
                    .then(
                        just(Token::Comma)
                            .ignore_then(expr.clone().separated_by(just(Token::Comma)))
                            .or_not()
                            .map(|args| args.unwrap_or_default()),
                    )
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|((target, message), args)| Stmt::Send {
                target,
                message,
                args,
            });
        */

        let if_stmt = just(Token::If)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map(|((cond, then_block), else_block)| Stmt::If(cond, then_block, else_block));

        let while_stmt = just(Token::While)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then(block.clone())
            .map(|(cond, body)| Stmt::While(cond, body));

        let for_stmt = just(Token::For)
            .ignore_then(just(Token::LParen))
            .ignore_then(ident)
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then(choice((
                // Range loop: .. end )
                just(Token::Range)
                    .ignore_then(expr.clone())
                    .then_ignore(just(Token::RParen))
                    .map(|end| Some(end)),
                // Collection loop: )
                just(Token::RParen).map(|_| None),
            )))
            .then(block.clone())
            .map(|(((iter, start_or_collection), end_opt), body)| {
                if let Some(end) = end_opt {
                    Stmt::For(iter, start_or_collection, end, body)
                } else {
                    Stmt::ForEach(iter, start_or_collection, body)
                }
            });

        let assign_stmt = ident
            .clone()
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map(|(name, value)| Stmt::Assign(name, value));

        let assign_index_stmt = ident
            .clone()
            .then(
                expr.clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket)),
            )
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map(|((name, index), value)| {
                Stmt::AssignIndex(name, Box::new(index), Box::new(value))
            });

        choice((
            let_stmt,
            var_stmt,
            return_stmt,
            if_stmt,
            while_stmt,
            for_stmt,
            assign_index_stmt, // Try index assignment before variable assignment
            assign_stmt,
            expr.clone().map(Stmt::Expr),
        ))
        // .then_ignore(just(Token::Semicolon).or_not()) // No semicolon token
    });

    let param = ident
        .then_ignore(just(Token::Colon))
        .then(type_parser.clone())
        .map(|(name, ty)| Param { name, ty });

    let function_def = just(Token::Io)
        .or_not()
        .map(|io| io.is_some())
        .then_ignore(just(Token::Fun))
        .then(ident)
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(type_parser.clone())
                .map(|(name, ty)| Param { name, ty })
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(just(Token::Colon).ignore_then(type_parser.clone()).or_not())
        .then(
            stmt.clone()
                .repeated()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(|stmts| Block { stmts }),
        )
        .map(
            |((((is_io, name), params), return_type), body)| FunctionDef {
                name,
                params,
                return_type,
                body,
                is_io,
            },
        );

    let message_def = just(Token::Class)
        .ignore_then(ident.clone())
        .then(
            param
                .clone()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|(name, params)| MessageDef { name, params });

    // Parse: data message Name(val field1: Type1, val field2: Type2, ...)
    let data_field = just(Token::Val)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Colon))
        .then(type_parser.clone())
        .map(|(name, field_type)| DataField { name, field_type });

    let data_message_def = just(Token::Data)
        .ignore_then(just(Token::Message))
        .ignore_then(ident.clone())
        .then(
            data_field
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|(name, fields)| DataMessageDef { name, fields });

    let field_def = choice((just(Token::Var), just(Token::Val)))
        .then(ident.clone())
        .then(just(Token::Colon).ignore_then(type_parser.clone()).or_not())
        .then_ignore(just(Token::Equals))
        .then(expr.clone())
        .map(|(((var_or_val, name), ty), init)| {
            FieldDef {
                name,
                ty: ty.unwrap_or(Type::Int), // Default to Int if no type specified
                init,
                is_mutable: var_or_val == Token::Var,
                is_crdt: false,
                is_io: false,
            }
        });

    let actor_def = just(Token::Actor)
        .ignore_then(ident)
        .then(
            param
                .clone()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(
            choice((
                field_def.map(ActorMember::Field),
                function_def.clone().map(ActorMember::Method),
            ))
            .repeated()
            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|((name, params), members)| ActorDef {
            name,
            params,
            members,
        });

    // import std.collections.List<T>
    // Allow keywords like "io" in import paths
    let import_path_segment = select! {
        Token::Ident(name) => name,
        Token::Io => "io".to_string(),
        Token::Fun => "fun".to_string(),
        Token::Class => "class".to_string(),
        Token::Actor => "actor".to_string(),
    };

    let import_def = just(Token::Import)
        .ignore_then(
            import_path_segment
                .separated_by(just(Token::Dot))
                .at_least(1),
        )
        .then(
            ident
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::Lt), just(Token::Gt))
                .or_not(),
        )
        .map(|(path, type_params)| ImportDef {
            path,
            type_params: type_params.unwrap_or_default(),
        });

    let definition = choice((
        import_def.map(Definition::Import),
        data_message_def.map(Definition::DataMessage), // Must come before message_def
        actor_def.map(Definition::Actor),
        message_def.map(Definition::Message),
        function_def.map(Definition::Function),
    ));

    definition
        .repeated()
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}
