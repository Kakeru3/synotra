use crate::ast::*;
use crate::lexer::Token;
use chumsky::prelude::*;

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
                        *pos += 1; // Skip method name

                        // Check for arguments
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
                            return Err("Expected '(' after method name".to_string());
                        }
                    } else {
                        return Err("Expected method name after '.'".to_string());
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
        let ask_expr = just(Token::Ask)
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
            .map(|((target, message), args)| Expr::Ask {
                target,
                message,
                args,
            });

        let atom = choice((
            ask_expr,
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
            ident.map(Expr::Variable),
            expr.clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        let call = atom
            .clone()
            .then(
                choice((
                    // Method call: .method(args)
                    just(Token::Dot)
                        .ignore_then(ident)
                        .then(
                            expr.clone()
                                .separated_by(just(Token::Comma))
                                .delimited_by(just(Token::LParen), just(Token::RParen))
                                .or_not(),
                        )
                        .map(|(method, args)| (Some(method), args, false)), // (method, args, is_index)
                    // Function call: (args)
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map(|args| (Some("invoke".to_string()), Some(args), false)), // Treat as invoke or handle in map
                    // Index access: [index]
                    expr.clone()
                        .delimited_by(just(Token::LBracket), just(Token::RBracket))
                        .map(|index| (None, Some(vec![index]), true)), // (None, [index], is_index)
                ))
                .or_not(),
            )
            .map(|(target, call_suffix)| {
                if let Some((method, args, is_index)) = call_suffix {
                    if is_index {
                        // Index access: target[index]
                        let index = args.unwrap().into_iter().next().unwrap();
                        Expr::Index(Box::new(target), Box::new(index))
                    } else if let Some(args) = args {
                        // Method or Function call
                        if let Some(m) = method {
                            if m == "invoke" {
                                // Function call: target(args)
                                // If target is a variable, it's a function call.
                                // If target is an expression, it's an invoke?
                                // For println(args), target is Variable("println").
                                // We want Expr::Call(Variable("self"), "println", args) ??
                                // No, Expr::Call structure is (target, method, args).
                                // If I have `println(...)`, it's a global function or method on self.
                                // In original parser:
                                // if let Expr::Variable(name) = target {
                                //      Expr::Call(Box::new(Expr::Variable("self".to_string())), name, args)
                                // }

                                if let Expr::Variable(name) = target {
                                    // Check if it's a known global or just treat as method on self/global
                                    // For now, map to Call(self, name, args) is what original did?
                                    // Wait, original did:
                                    // if let Some(m) = method { Call(target, m, args) }
                                    // else { if Variable(name) = target { Call(self, name, args) } else { Call(target, "invoke", args) } }

                                    // Here m is "invoke" (my placeholder).
                                    // So I should check if I really have a method name from Dot.
                                    // But I merged them.

                                    // Let's distinguish Dot call from Paren call.
                                    // I can use an enum in map instead of tuple.

                                    Expr::Call(
                                        Box::new(Expr::Variable("self".to_string())),
                                        name,
                                        args,
                                    )
                                } else {
                                    Expr::Call(Box::new(target), "invoke".to_string(), args)
                                }
                            } else {
                                // Method call: target.method(args)
                                Expr::Call(Box::new(target), m, args)
                            }
                        } else {
                            // Should not happen
                            target
                        }
                    } else {
                        // Property access (field) - not handled yet
                        target
                    }
                } else {
                    target
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

        let compare = sum
            .clone()
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
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        compare
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

        let expr_stmt = expr.clone().map(Stmt::Expr);

        let_stmt
            .or(var_stmt)
            .or(return_stmt)
            .or(send_stmt)
            .or(if_stmt)
            .or(while_stmt)
            .or(for_stmt)
            .or(assign_index_stmt) // Check index assignment before simple assignment if ambiguous, but here they start different
            .or(assign_stmt)
            .or(expr_stmt)
            .then_ignore(just(Token::Dot).or_not()) // Optional semicolon/dot? Synotra doesn't specify, assuming optional or none
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
        .ignore_then(ident)
        .then(
            param
                .clone()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|(name, params)| MessageDef { name, params });

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
        actor_def.map(Definition::Actor),
        message_def.map(Definition::Message),
        function_def.map(Definition::Function),
    ));

    definition
        .repeated()
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}
