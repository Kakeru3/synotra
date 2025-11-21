use chumsky::prelude::*;
use crate::ast::*;
use crate::lexer::Token;

pub fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let ident = select! { Token::Ident(ident) => ident };
    let int_lit = select! { Token::Int(i) => Literal::Int(i) };
    let str_lit = select! { Token::String(s) => s };

    let type_parser = ident.map(|s| match s.as_str() {
        "Int" => Type::Int,
        "String" => Type::String,
        "Bool" => Type::Bool,
        _ => Type::UserDefined(s),
    });

    let expr = recursive(|expr| {
        let ask_expr = just(Token::Ask)
            .ignore_then(
                expr.clone()
                    .then_ignore(just(Token::Comma))
                    .then(expr.clone())
                    .then(
                        just(Token::Comma)
                            .ignore_then(expr.clone().separated_by(just(Token::Comma)))
                            .or_not()
                            .map(|args| args.unwrap_or_default())
                    )
                    .delimited_by(just(Token::LParen), just(Token::RParen))
            )
            .map(|((target, message), args)| Expr::Ask { 
                target: Box::new(target), 
                message: Box::new(message), 
                args 
            });

        let atom = choice((
            ask_expr,
            int_lit.map(Expr::Literal),
            str_lit.map(|s| {
                // Parse interpolated string: "Hello ${name}!"
                let mut parts = Vec::new();
                let mut last_end = 0;
                let mut chars: Vec<(usize, char)> = s.char_indices().collect();
                let len = s.len();
                
                let mut i = 0;
                while i < chars.len() {
                    let (idx, c) = chars[i];
                    if c == '$' && i + 1 < chars.len() && chars[i+1].1 == '{' {
                        // Found start of interpolation
                        if idx > last_end {
                            parts.push(Expr::Literal(Literal::String(s[last_end..idx].to_string())));
                        }
                        
                        // Find end of interpolation
                        let start_var = i + 2;
                        let mut end_var = start_var;
                        while end_var < chars.len() && chars[end_var].1 != '}' {
                            end_var += 1;
                        }
                        
                        if end_var < chars.len() {
                            let var_name_start = chars[start_var].0;
                            let var_name_end = chars[end_var].0;
                            let var_name = s[var_name_start..var_name_end].to_string();
                            parts.push(Expr::Variable(var_name));
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
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        let call = atom.clone()
            .then(just(Token::Dot).ignore_then(ident).or_not())
            .then(expr.clone().separated_by(just(Token::Comma)).delimited_by(just(Token::LParen), just(Token::RParen)).or_not())
            .map(|((target, method), args)| {
                if let Some(args) = args {
                    if let Some(m) = method {
                        Expr::Call(Box::new(target), m, args)
                    } else {
                        if let Expr::Variable(name) = target {
                            Expr::Call(Box::new(Expr::Variable("self".to_string())), name, args)
                        } else {
                            Expr::Call(Box::new(target), "invoke".to_string(), args)
                        }
                    }
                } else {
                    target
                }
            });

        let op = |c| just(c);

        let product = call.clone()
            .then(op(Token::Star).to(BinaryOp::Mul).or(op(Token::Slash).to(BinaryOp::Div))
                .then(call.clone())
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        let sum = product.clone()
            .then(op(Token::Plus).to(BinaryOp::Add).or(op(Token::Minus).to(BinaryOp::Sub))
                .then(product.clone())
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        let compare = sum.clone()
            .then(choice((
                op(Token::EqEq).to(BinaryOp::Eq),
                op(Token::NotEq).to(BinaryOp::Ne),
                op(Token::LtEq).to(BinaryOp::Le),
                op(Token::Lt).to(BinaryOp::Lt),
                op(Token::GtEq).to(BinaryOp::Ge),
                op(Token::Gt).to(BinaryOp::Gt),
            ))
            .then(sum.clone())
            .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)));

        compare
    });

    let stmt = recursive(|stmt| {
        let block = stmt.clone().repeated()
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
                expr.clone()
                    .then_ignore(just(Token::Comma))
                    .then(expr.clone())
                    .then(
                        just(Token::Comma)
                            .ignore_then(expr.clone().separated_by(just(Token::Comma)))
                            .or_not()
                            .map(|args| args.unwrap_or_default())
                    )
                    .delimited_by(just(Token::LParen), just(Token::RParen))
            )
            .map(|((target, message), args)| Stmt::Send { target, message, args });

        let if_stmt = just(Token::If)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map(|((cond, then_block), else_block)| Stmt::If(cond, then_block, else_block));

        let while_stmt = just(Token::While)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .map(|(cond, body)| Stmt::While(cond, body));

        let for_stmt = just(Token::For)
            .ignore_then(just(Token::LParen))
            .ignore_then(ident)
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then_ignore(just(Token::Range))
            .then(expr.clone())
            .then_ignore(just(Token::RParen))
            .then(block.clone())
            .map(|(((iter, start), end), body)| Stmt::For(iter, start, end, body));

        let assign_stmt = ident.clone()
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map(|(name, value)| Stmt::Assign(name, value));

        let expr_stmt = expr.clone().map(Stmt::Expr);

        let_stmt
            .or(var_stmt)
            .or(return_stmt)
            .or(send_stmt)
            .or(if_stmt)
            .or(while_stmt)
            .or(for_stmt)
            .or(assign_stmt)
            .or(expr_stmt)
        .then_ignore(just(Token::Dot).or_not()) // Optional semicolon/dot? Synotra doesn't specify, assuming optional or none
    });

    let param = ident
        .then_ignore(just(Token::Colon))
        .then(type_parser.clone())
        .map(|(name, ty)| Param { name, ty });

    let function_def = just(Token::Io).or_not().map(|io| io.is_some())
        .then_ignore(just(Token::Fun))
        .then(ident)
        .then(ident.then_ignore(just(Token::Colon)).then(type_parser.clone())
            .map(|(name, ty)| Param { name, ty })
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::LParen), just(Token::RParen)))
        .then(just(Token::Colon).ignore_then(type_parser.clone()).or_not())
        .then(stmt.clone().repeated().delimited_by(just(Token::LBrace), just(Token::RBrace)).map(|stmts| Block { stmts }))
        .map(|((((is_io, name), params), return_type), body)| FunctionDef {
            name,
            params,
            return_type,
            body,
            is_io,
        });

    let message_def = just(Token::Class)
        .ignore_then(ident)
        .then(param.clone().separated_by(just(Token::Comma)).delimited_by(just(Token::LParen), just(Token::RParen)))
        .map(|(name, params)| MessageDef { name, params });

    let actor_def = just(Token::Actor)
        .ignore_then(ident)
        .then(param.clone().separated_by(just(Token::Comma)).delimited_by(just(Token::LParen), just(Token::RParen)))
        .then(
            choice((
                function_def.clone().map(ActorMember::Method),
                // Field defs
            ))
            .repeated()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
        )
        .map(|((name, params), members)| ActorDef {
            name,
            params,
            members,
        });

    let definition = choice((
        actor_def.map(Definition::Actor),
        message_def.map(Definition::Message),
        function_def.map(Definition::Function),
    ));

    definition.repeated()
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}
