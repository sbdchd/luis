use crate::ast::*;
use crate::iter::{ForwardBackwardIterator, PeekableIterator, TokenIter};
use crate::lex::{Lexer, Token, TokenKind};
use std;

type Result<T> = std::result::Result<T, ()>;

fn to_kind(tk: &Token) -> TokenKind {
    tk.kind.clone()
}

/// parse a given blob of lua text
pub fn parse(text: &str) -> Result<Block> {
    let tokens: Vec<_> = Lexer::new(text)
        .filter(|t| match t {
            Token {
                kind: TokenKind::Comment(_),
                ..
            } => false,
            _ => true,
        })
        .collect();
    let mut tokens = TokenIter::new(&tokens);
    parse_block(&mut tokens)
}

/// label ::= ‘::’ Name ‘::’
fn parse_label(tokens: &mut TokenIter<Token>) -> Result<Name> {
    tokens.assert_next(&TokenKind::DBColon)?;
    if let Some(Token {
        kind: TokenKind::Ident(ident),
        ..
    }) = tokens.next()
    {
        tokens.assert_next(&TokenKind::DBColon)?;

        Ok(Name(ident.to_string()))
    } else {
        Err(())
    }
}
/// funcname ::= Name {‘.’ Name} [‘:’ Name]
fn parse_funcname(tokens: &mut TokenIter<Token>) -> Result<FuncName> {
    let first_name = tokens.next();
    let first_name = match first_name {
        Some(Token {
            kind: TokenKind::Ident(first_name),
            ..
        }) => first_name,
        _ => return Err(()),
    };
    let mut path = vec![Name(first_name.clone())];
    // if next token is period then loop
    while tokens.peek().map(to_kind) == Some(TokenKind::Period) {
        tokens.next();
        match tokens.next() {
            Some(Token {
                kind: TokenKind::Ident(ref content),
                ..
            }) => {
                path.push(Name(content.clone()));
            }
            _ => return Err(()),
        }
    }
    let mut method: Option<Name> = None;
    if let Some(Token {
        kind: TokenKind::Colon,
        ..
    }) = tokens.peek()
    {
        tokens.next();
        match tokens.next() {
            Some(Token {
                kind: TokenKind::Ident(ref name),
                ..
            }) => {
                method = Some(Name(name.clone()));
            }
            _ => return Err(()),
        }
    }
    Ok(FuncName { path, method })
}

fn parse_simple_exp(tokens: &mut TokenIter<Token>) -> Result<Expr> {
    match tokens.cur().map(to_kind) {
        Some(TokenKind::Nil) => Ok(Expr::Nil),
        Some(TokenKind::True) => Ok(Expr::Bool(true)),
        Some(TokenKind::False) => Ok(Expr::Bool(false)),
        Some(TokenKind::String(s)) => Ok(Expr::Str(s.to_string())),
        Some(TokenKind::Number(n)) => Ok(Expr::Num(n)),
        Some(TokenKind::Dots) => Ok(Expr::Dots),
        Some(TokenKind::Function) => {
            tokens.prev();
            parse_functiondef(tokens).map(Expr::FuncDef)
        }
        Some(TokenKind::LCurly) => {
            tokens.prev();
            parse_table_constructor(tokens).map(Expr::Table)
        }
        _ => Err(()),
    }
}

fn bin_priority(op: &Option<TokenKind>) -> i32 {
    match op {
        Some(TokenKind::Pow) => 12,
        Some(TokenKind::Mul)
        | Some(TokenKind::Div)
        | Some(TokenKind::IntDiv)
        | Some(TokenKind::Mod) => 10,
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => 9,
        Some(TokenKind::Concat) => 8,
        Some(TokenKind::SHL) | Some(TokenKind::SHR) => 7,
        Some(TokenKind::BitAnd) => 6,
        Some(TokenKind::BitXor) => 5,
        Some(TokenKind::BitOr) => 4,
        Some(TokenKind::LT) | Some(TokenKind::GT) | Some(TokenKind::LTE) | Some(TokenKind::GTE)
        | Some(TokenKind::EQ) | Some(TokenKind::NEQ) => 3,
        Some(TokenKind::Or) | Some(TokenKind::And) => 1,
        _ => 0,
    }
}

const UNARY_PRIORITY: i32 = 12;

fn parse_unexp(tokens: &mut TokenIter<Token>) -> Result<Expr> {
    match tokens.next().map(to_kind) {
        tk @ Some(TokenKind::Minus)
        | tk @ Some(TokenKind::Not)
        | tk @ Some(TokenKind::Hash)
        | tk @ Some(TokenKind::BitXor) => {
            let op = match tk {
                Some(TokenKind::Minus) => Unop::Minus,
                Some(TokenKind::Not) => Unop::Not,
                Some(TokenKind::Hash) => Unop::Len,
                Some(TokenKind::BitXor) => Unop::BitNot,
                _ => return Err(()),
            };

            let exp = parse_sub_expr(tokens, UNARY_PRIORITY)?;
            Ok(Expr::UnExp(UnExp {
                op,
                exp: Box::new(exp),
            }))
        }
        _ => Err(()),
    }
}

/// exp ::= nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
fn parse_expr(tokens: &mut TokenIter<Token>) -> Result<Expr> {
    parse_sub_expr(tokens, 0)
}

fn is_bin_op(token: &Option<TokenKind>) -> bool {
    match token {
        Some(TokenKind::Plus)
        | Some(TokenKind::Minus)
        | Some(TokenKind::Mul)
        | Some(TokenKind::Div)
        | Some(TokenKind::IntDiv)
        | Some(TokenKind::Pow)
        | Some(TokenKind::Mod)
        | Some(TokenKind::BitAnd)
        | Some(TokenKind::BitXor)
        | Some(TokenKind::BitOr)
        | Some(TokenKind::SHR)
        | Some(TokenKind::SHL)
        | Some(TokenKind::Concat)
        | Some(TokenKind::LT)
        | Some(TokenKind::LTE)
        | Some(TokenKind::GT)
        | Some(TokenKind::GTE)
        | Some(TokenKind::EQ)
        | Some(TokenKind::NEQ)
        | Some(TokenKind::And)
        | Some(TokenKind::Or) => true,
        _ => false,
    }
}

// subexpr ::= (simpleexp | unop subexpr ) { binop subexpr }
// see: https://github.com/lua/lua/blob/2c32bff60987d38a60a58d4f0123f3783da60a63/lparser.c#L1120-L1156
fn parse_sub_expr(tokens: &mut TokenIter<Token>, min_priority: i32) -> Result<Expr> {
    let mut expression = parse_unexp(tokens)
        .or_else(|_| parse_simple_exp(tokens))
        .or_else(|_| {
            tokens.prev();
            parse_prefix_exp(tokens).map(|x| Expr::PrefixExp(Box::new(x)))
        })?;

    while is_bin_op(&tokens.peek().map(to_kind))
        && bin_priority(&tokens.peek().map(to_kind)) > min_priority
    {
        tokens.next();

        let op = match tokens.cur().map(to_kind) {
            Some(TokenKind::Plus) => BinOp::Plus,
            Some(TokenKind::Minus) => BinOp::Minus,
            Some(TokenKind::Mul) => BinOp::Mul,
            Some(TokenKind::Div) => BinOp::Div,
            Some(TokenKind::IntDiv) => BinOp::IntDiv,
            Some(TokenKind::Pow) => BinOp::Pow,
            Some(TokenKind::Mod) => BinOp::Mod,
            Some(TokenKind::BitAnd) => BinOp::BitAnd,
            Some(TokenKind::BitXor) => BinOp::BitXor,
            Some(TokenKind::BitOr) => BinOp::BitOr,
            Some(TokenKind::SHR) => BinOp::BitShr,
            Some(TokenKind::SHL) => BinOp::BitShl,
            Some(TokenKind::Concat) => BinOp::Concat,
            Some(TokenKind::LT) => BinOp::LT,
            Some(TokenKind::LTE) => BinOp::LTE,
            Some(TokenKind::GT) => BinOp::GT,
            Some(TokenKind::GTE) => BinOp::GTE,
            Some(TokenKind::EQ) => BinOp::EQ,
            Some(TokenKind::NEQ) => BinOp::NEQ,
            Some(TokenKind::And) => BinOp::And,
            Some(TokenKind::Or) => BinOp::Or,
            _ => break,
        };

        let prority = bin_priority(&tokens.peek().map(to_kind));

        let rhs = match parse_sub_expr(tokens, prority) {
            Err(_) => break,
            Ok(rhs) => rhs,
        };

        expression = Expr::BinExp(BinExp {
            op,
            lhs: Box::new(expression),
            rhs: Box::new(rhs),
        })
    }

    Ok(expression)
}

/// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
fn parse_field(tokens: &mut TokenIter<Token>) -> Result<Field> {
    match tokens.next().map(to_kind) {
        // Name '=' exp
        Some(TokenKind::Ident(name)) => {
            tokens.assert_next(&TokenKind::Assign)?;

            let expr = match parse_expr(tokens) {
                Ok(expr) => expr,
                Err(_) => {
                    return Err(());
                }
            };

            Ok(Field::NameAssign(Name(name.clone()), expr))
        }
        // '[' exp ']' '=' exp
        Some(TokenKind::LBracket) => {
            let lexpr = parse_expr(tokens)?;

            tokens.assert_next(&TokenKind::RBracket)?;

            tokens.assert_next(&TokenKind::Assign)?;

            let rexpr = parse_expr(tokens)?;

            Ok(Field::ExprAssign(lexpr, rexpr))
        }
        // exp
        _ => match parse_expr(tokens) {
            Ok(e) => Ok(Field::PosAssign(e)),
            _ => Err(()),
        },
    }
}

/// tableconstructor ::= ‘{’ [fieldlist] ‘}’
fn parse_table_constructor(tokens: &mut TokenIter<Token>) -> Result<TableConstructor> {
    match tokens.next().map(to_kind) {
        Some(TokenKind::LCurly) => {
            if let Some(TokenKind::RCurly) = tokens.peek().map(to_kind) {
                tokens.next();
                return Ok(TableConstructor(vec![]));
            };
            let fieldlist = parse_fieldlist(tokens)?;
            match tokens.next().map(to_kind) {
                Some(TokenKind::RCurly) => Ok(TableConstructor(fieldlist)),
                _ => Err(()),
            }
        }
        _ => Err(()),
    }
}

/// varlist ::= var {‘,’ var}
fn parse_varlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Var>> {
    let mut varlist = vec![];

    if let Ok(var) = parse_var(tokens) {
        varlist.push(var);
    }

    while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
        tokens.next();
        match parse_var(tokens) {
            Ok(v) => {
                varlist.push(v);
            }
            Err(_) => {
                tokens.prev();
                break;
            }
        }
    }

    Ok(varlist)
}

/// explist ::= exp {‘,’ exp}
fn parse_exprlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Expr>> {
    let mut exprs = vec![];

    if let Ok(expr) = parse_expr(tokens) {
        exprs.push(expr);
    }

    while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
        tokens.next();
        match parse_expr(tokens) {
            Ok(expr) => {
                exprs.push(expr);
            }
            Err(_) => {
                tokens.prev();
                break;
            }
        }
    }

    Ok(exprs)
}

/// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
fn parse_args(tokens: &mut TokenIter<Token>) -> Result<Args> {
    match tokens.next().map(to_kind) {
        Some(TokenKind::String(s)) => Ok(Args::String(s.to_string())),
        Some(TokenKind::LParen) => {
            if let Some(TokenKind::RParen) = tokens.peek().map(to_kind) {
                tokens.next();
                return Ok(Args::ExprList(vec![]));
            }
            parse_exprlist(tokens).map(Args::ExprList)
        }
        Some(TokenKind::LCurly) => {
            // parse_table_constructor() expects a first token of LCurly
            tokens.prev();
            parse_table_constructor(tokens).map(Args::TableConstructor)
        }
        _ => Err(()),
    }
}

/// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
/// e.g.:
/// foo
/// bar[0]
/// bar.bizz
fn parse_var(tokens: &mut TokenIter<Token>) -> Result<Var> {
    match tokens.peek().map(to_kind) {
        Some(TokenKind::Ident(ident)) => {
            tokens.next();
            match tokens.peek().map(to_kind) {
                Some(TokenKind::LBracket) => {
                    tokens.next();

                    let expr = parse_expr(tokens)?;

                    tokens.assert_next(&TokenKind::RBracket)?;

                    Ok(Var::IndexExpr(IndexExpr {
                        expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                        arg: expr,
                    }))
                }
                Some(TokenKind::Period) => {
                    tokens.next();

                    if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
                        Ok(Var::PropertyAccess(PropertyAccess {
                            expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                            name: Name(name.to_string()),
                        }))
                    } else {
                        Err(())
                    }
                }
                _ => Ok(Var::Name(Name(ident.to_string()))),
            }
        }
        Some(_) => {
            let prefixexp = parse_prefix_exp(tokens)?;

            match tokens.peek().map(to_kind) {
                Some(TokenKind::LBracket) => {
                    tokens.next();

                    let expr = parse_expr(tokens)?;

                    tokens.assert_next(&TokenKind::RBracket)?;

                    Ok(Var::IndexExpr(IndexExpr {
                        expr: Box::new(prefixexp),
                        arg: expr,
                    }))
                }
                Some(TokenKind::Period) => {
                    tokens.next();

                    if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
                        Ok(Var::PropertyAccess(PropertyAccess {
                            expr: Box::new(prefixexp),
                            name: Name(name.to_string()),
                        }))
                    } else {
                        Err(())
                    }
                }
                _ => Err(()),
            }
        }
        None => Err(()),
    }
}

/// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
// functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
fn parse_prefix_exp(tokens: &mut TokenIter<Token>) -> Result<PrefixExpr> {
    match tokens.peek().map(to_kind) {
        Some(TokenKind::LParen) => {
            tokens.next();
            let expr = parse_expr(tokens).map(PrefixExpr::Expr);
            tokens.assert_next(&TokenKind::RParen)?;
            expr
        }
        Some(TokenKind::Ident(ident)) => {
            tokens.next();
            match tokens.peek().map(to_kind) {
                Some(TokenKind::LBracket) => {
                    tokens.next();

                    let expr = parse_expr(tokens)?;

                    tokens.assert_next(&TokenKind::RBracket)?;

                    Ok(PrefixExpr::Var(Var::IndexExpr(IndexExpr {
                        expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                        arg: expr,
                    })))
                }
                Some(TokenKind::Period) => {
                    tokens.next();

                    if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
                        Ok(PrefixExpr::Var(Var::PropertyAccess(PropertyAccess {
                            expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                            name: Name(name.to_string()),
                        })))
                    } else {
                        Err(())
                    }
                }
                Some(TokenKind::LParen) => Ok(PrefixExpr::FunctionCall(FunctionCall {
                    expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                    args: parse_args(tokens)?,
                })),
                _ => Ok(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
            }
        }
        _ => Err(()),
    }
}

/// stat ::=  ‘;’ |
///         varlist ‘=’ explist |
///         functioncall |
///         label |
///         break |
///         goto Name |
///         do block end |
///         while exp do block end |
///         repeat block until exp |
///         if exp then block {elseif exp then block} [else block] end |
///         for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
///         for namelist in explist do block end |
///         function funcname funcbody |
///         local function Name funcbody |
///         local namelist [‘=’ explist]
fn parse_stat(tokens: &mut TokenIter<Token>) -> Result<Stat> {
    match tokens.peek().map(to_kind) {
        Some(TokenKind::SemiColon) => {
            tokens.next();
            Ok(Stat::SemiColon)
        }
        Some(TokenKind::DBColon) => parse_label(tokens).map(Stat::Label),
        Some(TokenKind::Break) => {
            tokens.next();
            Ok(Stat::Break)
        }
        Some(TokenKind::Goto) => {
            tokens.next();
            if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
                Ok(Stat::Goto(Name(name.to_string())))
            } else {
                Err(())
            }
        }
        Some(TokenKind::Do) => parse_do_block(tokens).map(Stat::DoBlock),
        Some(TokenKind::While) => parse_while_block(tokens).map(Stat::WhileBlock),
        Some(TokenKind::Repeat) => parse_repeat_block(tokens).map(Stat::RepeatBlock),
        Some(TokenKind::If) => parse_if_block(tokens).map(|f| Stat::IfBlock(Box::new(f))),
        Some(TokenKind::For) => {
            tokens.next();
            tokens.next();
            if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
                tokens.prev();
                tokens.prev();
                parse_for_range(tokens).map(|f| Stat::ForRange(Box::new(f)))
            } else {
                tokens.prev();
                tokens.prev();
                parse_for_in(tokens).map(Stat::ForIn)
            }
        }
        Some(TokenKind::Function) => parse_function_def(tokens).map(Stat::FunctionDef),
        Some(TokenKind::Local) => {
            tokens.next();
            if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
                tokens.prev();
                parse_local_function_def(tokens).map(Stat::LocalFunctionDef)
            } else {
                tokens.prev();
                parse_local_assignment(tokens).map(Stat::LocalAssignment)
            }
        }
        Some(TokenKind::Ident(ident)) => {
            tokens.next();
            if let Some(TokenKind::LParen) = tokens.peek().map(to_kind) {
                Ok(Stat::FunctionCall(FunctionCall {
                    expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
                    args: parse_args(tokens)?,
                }))
            } else {
                tokens.prev();
                parse_assignment(tokens).map(Stat::Assignment)
            }
        }
        _ => Err(()),
    }
}

/// varlist ‘=’ explist
fn parse_assignment(tokens: &mut TokenIter<Token>) -> Result<Assignment> {
    let varlist = parse_varlist(tokens)?;

    if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let exprlist = parse_exprlist(tokens)?;

    Ok(Assignment { varlist, exprlist })
}

/// local namelist [‘=’ explist]
fn parse_local_assignment(tokens: &mut TokenIter<Token>) -> Result<LocalAssignment> {
    if let Some(TokenKind::Local) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let namelist = parse_namelist(tokens)?;

    let exprlist = if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
        tokens.next();
        Some(parse_exprlist(tokens)?)
    } else {
        None
    };

    Ok(LocalAssignment { namelist, exprlist })
}

/// local function Name funcbody
fn parse_local_function_def(tokens: &mut TokenIter<Token>) -> Result<LocalFunctionDef> {
    if let Some(TokenKind::Local) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }
    if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let name = match tokens.next().map(to_kind) {
        Some(TokenKind::Ident(name)) => Name(name.to_string()),
        _ => return Err(()),
    };

    let body = parse_funcbody(tokens)?;

    Ok(LocalFunctionDef { name, body })
}

/// function funcname funcbody
fn parse_function_def(tokens: &mut TokenIter<Token>) -> Result<FunctionDef> {
    if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let name = parse_funcname(tokens)?;

    let body = parse_funcbody(tokens)?;

    Ok(FunctionDef { name, body })
}

/// for namelist in explist do block end
fn parse_for_in(tokens: &mut TokenIter<Token>) -> Result<ForIn> {
    if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let namelist = parse_namelist(tokens)?;

    if let Some(TokenKind::In) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let exprlist = parse_exprlist(tokens)?;

    let block = parse_do_block(tokens)?;

    Ok(ForIn {
        namelist,
        exprlist,
        block,
    })
}

/// for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
fn parse_for_range(tokens: &mut TokenIter<Token>) -> Result<ForRange> {
    if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let name = match tokens.next().map(to_kind) {
        Some(TokenKind::Ident(name)) => Name(name.to_string()),
        _ => return Err(()),
    };

    if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let exp_start = parse_expr(tokens)?;

    if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let exp_end = parse_expr(tokens)?;

    let exp_step = if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
        tokens.next();
        Some(parse_expr(tokens)?)
    } else {
        None
    };

    let block = parse_do_block(tokens)?;

    Ok(ForRange {
        name,
        exprs: (exp_start, exp_end, exp_step),
        block,
    })
}

/// elseif exp then block
fn parse_elseif(tokens: &mut TokenIter<Token>) -> Result<ElseIf> {
    if let Some(TokenKind::ElseIf) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let expr = parse_expr(tokens)?;

    if let Some(TokenKind::Then) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let block = parse_block(tokens)?;

    Ok(ElseIf { block, expr })
}

/// else block
fn parse_else_block(tokens: &mut TokenIter<Token>) -> Result<Option<Block>> {
    if let Some(TokenKind::Else) = tokens.peek().map(to_kind) {
        tokens.next();
        Ok(Some(parse_block(tokens)?))
    } else {
        Ok(None)
    }
}

/// if exp then block {elseif exp then block} [else block] end
fn parse_if_block(tokens: &mut TokenIter<Token>) -> Result<IfBlock> {
    if let Some(TokenKind::If) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let expr = parse_expr(tokens)?;

    if let Some(TokenKind::Then) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let block = parse_block(tokens)?;

    let mut elseif = vec![];

    while let Ok(elif) = parse_elseif(tokens) {
        elseif.push(elif);
    }

    let else_blk = parse_else_block(tokens)?;

    Ok(IfBlock {
        expr,
        block,
        elseif,
        else_blk,
    })
}

/// repeat block until exp
fn parse_repeat_block(tokens: &mut TokenIter<Token>) -> Result<RepeatBlock> {
    if let Some(TokenKind::Repeat) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let block = parse_block(tokens)?;

    if let Some(TokenKind::Until) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let expr = parse_expr(tokens)?;

    Ok(RepeatBlock { block, expr })
}

/// while exp do block end
fn parse_while_block(tokens: &mut TokenIter<Token>) -> Result<WhileBlock> {
    if let Some(TokenKind::While) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let expr = parse_expr(tokens)?;

    let block = parse_do_block(tokens)?;

    Ok(WhileBlock { block, expr })
}

/// do block end
fn parse_do_block(tokens: &mut TokenIter<Token>) -> Result<Block> {
    if let Some(TokenKind::Do) = tokens.peek().map(to_kind) {
        tokens.next();
    } else {
        return Err(());
    }

    let blk = parse_block(tokens)?;

    if let Some(TokenKind::End) = tokens.next().map(to_kind) {
        Ok(blk)
    } else {
        Err(())
    }
}

/// block ::= {stat} [retstat]
fn parse_block(tokens: &mut TokenIter<Token>) -> Result<Block> {
    let mut stats = vec![];
    while let Ok(stat) = parse_stat(tokens) {
        stats.push(stat);
    }
    let retstat = parse_retstat(tokens).unwrap_or_default();
    Ok(Block { stats, retstat })
}

/// retstat ::= return [explist] [‘;’]
fn parse_retstat(tokens: &mut TokenIter<Token>) -> Result<Option<Vec<Expr>>> {
    match tokens.peek().map(to_kind) {
        Some(TokenKind::Return) => {
            tokens.next();
        }
        _ => return Ok(None),
    }

    let exprlist = parse_exprlist(tokens)?;

    if let Some(TokenKind::SemiColon) = tokens.peek().map(to_kind) {
        tokens.next();
    }
    Ok(Some(exprlist))
}

/// funcbody ::= ‘(’ [parlist] ‘)’ block end
fn parse_funcbody(tokens: &mut TokenIter<Token>) -> Result<FuncBody> {
    tokens.assert_next(&TokenKind::LParen)?;
    let params = parse_parlist(tokens)?;
    tokens.assert_next(&TokenKind::RParen)?;

    Ok(FuncBody {
        params,
        body: parse_block(tokens)?,
    })
}

/// functiondef ::= function funcbody
/// stat ::= function funcname funcbody
/// stat ::= local function Name funcbody
fn parse_functiondef(tokens: &mut TokenIter<Token>) -> Result<FunctionDef> {
    if let Some(TokenKind::Function) = tokens.next().map(to_kind) {
        let name = parse_funcname(tokens)?;
        let body = parse_funcbody(tokens)?;

        Ok(FunctionDef { name, body })
    } else {
        Err(())
    }
}

/// namelist ::= Name {‘,’ Name}
fn parse_namelist(tokens: &mut TokenIter<Token>) -> Result<Vec<Name>> {
    let first_name = match tokens.next().map(to_kind).ok_or_else(|| ())? {
        TokenKind::Ident(name) => name,
        _ => return Err(()),
    };

    let mut names = vec![Name(first_name.clone())];

    while tokens.peek().map(to_kind) == Some(TokenKind::Comma) {
        tokens.next();
        match tokens.next().map(to_kind) {
            Some(TokenKind::Ident(name)) => {
                names.push(Name(name.clone()));
            }
            Some(TokenKind::Dots) => {
                // put back Dots and the previous Comma tokens
                tokens.prev();
                tokens.prev();
                break;
            }
            _ => return Err(()),
        }
    }

    Ok(names)
}

/// parlist ::= namelist [‘,’ ‘...’] | ‘...’
fn parse_parlist(tokens: &mut TokenIter<Token>) -> Result<Params> {
    match tokens.peek().map(to_kind) {
        // | '...'
        Some(TokenKind::Dots) => {
            tokens.next();
            Ok(Params {
                names: vec![],
                variadic: true,
            })
        }
        // namelist
        Some(TokenKind::Ident(_)) => {
            let names = parse_namelist(tokens).unwrap_or_default();

            // [',' '...']
            let variadic = match tokens.peek().map(to_kind) {
                Some(TokenKind::Comma) => {
                    tokens.next();
                    match tokens.next().map(to_kind) {
                        Some(TokenKind::Dots) => true,
                        _ => false,
                    }
                }
                _ => false,
            };

            Ok(Params { names, variadic })
        }
        _ => Ok(Params {
            names: vec![],
            variadic: false,
        }),
    }
}

/// fieldlist ::= field {fieldsep field} [fieldsep]
/// fieldsep ::= ‘,’ | ‘;’
fn parse_fieldlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Field>> {
    let mut fields = vec![];

    while let Ok(f) = parse_field(tokens) {
        fields.push(f);

        match tokens.peek().map(to_kind) {
            Some(TokenKind::Comma) | Some(TokenKind::SemiColon) => {
                tokens.next();
                continue;
            }
            _ => break,
        }
    }
    Ok(fields)
}

#[cfg(test)]
mod test_parse {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_expr() {
        let p = r#"..."#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Dots));

        let p = r#"nil"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Nil));

        let p = r#"false"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(false)));

        let p = r#"true"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(true)));

        let p = r#"10"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Num(10f64)));
    }

    #[test]
    fn test_parse_unexp() {
        for (s, op) in vec![
            ("-", Unop::Minus),
            ("not ", Unop::Not),
            ("#", Unop::Len),
            ("~", Unop::BitNot),
        ] {
            let p = format!("{}foo", s);
            let tokens: Vec<_> = Lexer::new(&p).collect();
            let mut tokens = TokenIter::new(&tokens);
            assert_eq!(
                parse_expr(&mut tokens),
                Ok(Expr::UnExp(UnExp {
                    op,
                    exp: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                        String::from("foo")
                    ))))))
                }))
            );
        }
    }

    #[test]
    fn test_parse_binexp() {
        for (s, op) in vec![
            ("+", BinOp::Plus),
            ("-", BinOp::Minus),
            ("*", BinOp::Mul),
            ("/", BinOp::Div),
            ("//", BinOp::IntDiv),
            ("^", BinOp::Pow),
            ("%", BinOp::Mod),
            ("&", BinOp::BitAnd),
            ("~", BinOp::BitXor),
            ("|", BinOp::BitOr),
            (">>", BinOp::BitShr),
            ("<<", BinOp::BitShl),
            ("..", BinOp::Concat),
            ("<", BinOp::LT),
            ("<=", BinOp::LTE),
            (">", BinOp::GT),
            (">=", BinOp::GTE),
            ("==", BinOp::EQ),
            ("~=", BinOp::NEQ),
            ("and", BinOp::And),
            ("or", BinOp::Or),
        ] {
            let p = format!("foo {} bar", s);
            let tokens: Vec<_> = Lexer::new(&p).collect();
            let mut tokens = TokenIter::new(&tokens);
            assert_eq!(
                parse_expr(&mut tokens),
                Ok(Expr::BinExp(BinExp {
                    op,
                    lhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                        String::from("foo")
                    )))))),
                    rhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                        String::from("bar")
                    ))))))
                }))
            );
        }
    }

    #[test]
    fn test_multi_part_binexpr() {
        let tokens: Vec<_> = Lexer::new("4 + 3 - 2").collect();
        let mut tokens = TokenIter::new(&tokens);
        // TODO(sbdchd): pretty sure this should be (4 + 3) - 2
        // instead of 4 + (3 - 2)
        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::BinExp(BinExp {
                op: BinOp::Plus,
                lhs: Box::new(Expr::Num(4.0)),
                rhs: Box::new(Expr::BinExp(BinExp {
                    op: BinOp::Minus,
                    lhs: Box::new(Expr::Num(3.0)),
                    rhs: Box::new(Expr::Num(2.0)),
                })),
            }))
        );
    }

    #[test]
    fn test_simple_bin() {
        let tokens: Vec<_> = Lexer::new("foo - bar").collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::BinExp(BinExp {
                op: BinOp::Minus,
                lhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                    String::from("foo")
                )))))),
                rhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                    String::from("bar")
                ))))))
            }))
        );
    }

    #[test]
    fn test_parse_prefix_exp_parens() {
        let p = r#"foo"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                String::from("foo")
            ))))))
        );

        let p = r#"(foo)"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_prefix_exp(&mut tokens),
            Ok(PrefixExpr::Expr(Expr::PrefixExp(Box::new(
                PrefixExpr::Var(Var::Name(Name(String::from("foo"))))
            ))))
        );
    }

    #[test]
    fn test_parse_var() {
        let p = r#"foo"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_var(&mut tokens),
            Ok(Var::Name(Name(String::from("foo"))))
        );

        let p = r#"(foo)[1]"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_var(&mut tokens),
            Ok(Var::IndexExpr(IndexExpr {
                expr: Box::new(PrefixExpr::Expr(Expr::PrefixExp(Box::new(
                    PrefixExpr::Var(Var::Name(Name(String::from("foo")))),
                )))),
                arg: Expr::Num(1f64),
            }))
        );
        let p = r#"foo[1]"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_var(&mut tokens),
            Ok(Var::IndexExpr(IndexExpr {
                expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))),),
                arg: Expr::Num(1f64),
            }))
        );

        let p = r#"foo.bar"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_var(&mut tokens),
            Ok(Var::PropertyAccess(PropertyAccess {
                expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
                name: Name(String::from("bar"))
            }))
        );
    }

    #[test]
    fn test_parse_args() {
        let p = r#"()"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(parse_args(&mut tokens), Ok(Args::ExprList(vec![])));

        let p = r#"(foo, nil, false, 10)"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_args(&mut tokens),
            Ok(Args::ExprList(vec![
                Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                    "foo"
                )))))),
                Expr::Nil,
                Expr::Bool(false),
                Expr::Num(10f64),
            ]))
        );
    }

    #[test]
    fn test_parse_exprlist() {
        let p = r#"nil, false, true, "str""#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_exprlist(&mut tokens),
            Ok(vec![
                Expr::Nil,
                Expr::Bool(false),
                Expr::Bool(true),
                Expr::Str(String::from("str"))
            ])
        );

        let p = r#"nil, false,"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_exprlist(&mut tokens),
            Ok(vec![Expr::Nil, Expr::Bool(false)])
        );

        assert_eq!(tokens.next().map(to_kind), Some(TokenKind::Comma));
    }

    #[test]
    fn test_parse_varlist() {
        let p = r#"foo, bar, bizz"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_varlist(&mut tokens),
            Ok(vec![
                Var::Name(Name(String::from("foo"))),
                Var::Name(Name(String::from("bar"))),
                Var::Name(Name(String::from("bizz"))),
            ])
        );
    }

    #[test]
    fn test_parse_expr_func() {
        let p = r#"function foo(...) return end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::FuncDef(FunctionDef {
                name: FuncName {
                    path: vec![Name(String::from("foo"))],
                    method: None,
                },
                body: FuncBody {
                    params: Params {
                        names: vec![],
                        variadic: true
                    },
                    body: Block {
                        stats: vec![],
                        retstat: Some(vec![]),
                    }
                }
            }))
        );
    }

    #[test]
    fn test_parse_retstat() {
        let p = r#"return nil, false;"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_retstat(&mut tokens),
            Ok(Some(vec![Expr::Nil, Expr::Bool(false),]))
        );

        let p = r#"return 10, "foo", true, bar"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_retstat(&mut tokens),
            Ok(Some(vec![
                Expr::Num(10f64),
                Expr::Str(String::from("foo")),
                Expr::Bool(true),
                Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                    "bar"
                ))))))
            ]))
        );

        let p = r#"return"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(parse_retstat(&mut tokens), Ok(Some(vec![])));

        let p = r#"return;"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(parse_retstat(&mut tokens), Ok(Some(vec![])));
    }

    #[test]
    fn test_parse_block() {
        let p = r#"return 10, "foo", true, bar"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_block(&mut tokens),
            Ok(Block {
                stats: vec![],
                retstat: Some(vec![
                    Expr::Num(10f64),
                    Expr::Str(String::from("foo")),
                    Expr::Bool(true),
                    Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                        "bar"
                    ))))))
                ])
            })
        );

        let p = r#"print(foo)"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_block(&mut tokens),
            Ok(Block {
                stats: vec![Stat::FunctionCall(FunctionCall {
                    expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("print"))))),
                    args: Args::ExprList(vec![Expr::PrefixExp(Box::new(PrefixExpr::Var(
                        Var::Name(Name(String::from("foo")))
                    )))])
                })],
                retstat: None
            })
        );
    }

    #[test]
    fn test_parse_functiondef() {
        let p = r#"function foo(...) return end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_functiondef(&mut tokens),
            Ok(FunctionDef {
                name: FuncName {
                    path: vec![Name(String::from("foo"))],
                    method: None,
                },
                body: FuncBody {
                    params: Params {
                        names: vec![],
                        variadic: true
                    },
                    body: Block {
                        stats: vec![],
                        retstat: Some(vec![]),
                    }
                }
            })
        );
    }

    #[test]
    fn test_parse_table_constructor() {
        let p = r#"{foo = 'foo', bar = false, bizz = 1}"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_table_constructor(&mut tokens),
            Ok(TableConstructor(vec![
                Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
                Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
                Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
            ]))
        );

        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::Table(TableConstructor(vec![
                Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
                Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
                Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
            ])))
        );

        let p = r#"{}"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_table_constructor(&mut tokens),
            Ok(TableConstructor(vec![]))
        );
    }

    #[test]
    fn test_parse_with_comment() {
        // for now we just filter out comments before we parse
        let p = r#"--[[ example comment ]]--
        ;"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_block(&mut tokens),
            Ok(Block {
                stats: vec![],
                retstat: None
            })
        );
    }

    #[test]
    fn test_parse_prefix_exp() {
        let p = "false)";
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(false)),);
        assert_eq!(tokens.next().map(to_kind), Some(TokenKind::RParen));

        let p = r#"(false)"#;
        let tokens: Vec<_> = Lexer::new(p).collect();

        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_prefix_exp(&mut tokens),
            Ok(PrefixExpr::Expr(Expr::Bool(false)))
        );

        let p = r#"foo(false, true, nil)"#;
        let tokens: Vec<_> = Lexer::new(p).collect();

        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_prefix_exp(&mut tokens),
            Ok(PrefixExpr::FunctionCall(FunctionCall {
                expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
                args: Args::ExprList(vec![Expr::Bool(false), Expr::Bool(true), Expr::Nil])
            }))
        )
    }

    #[test]
    fn test_parse_parlist() {
        let p = r#"..."#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_parlist(&mut tokens),
            Ok(Params {
                names: vec![],
                variadic: true
            })
        );

        let p = r#"Name,Another_Name"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_parlist(&mut tokens),
            Ok(Params {
                names: vec![
                    Name(String::from("Name")),
                    Name(String::from("Another_Name"))
                ],
                variadic: false
            })
        );

        let p = r#"Name,Another_Name,..."#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_parlist(&mut tokens),
            Ok(Params {
                names: vec![
                    Name(String::from("Name")),
                    Name(String::from("Another_Name"))
                ],
                variadic: true
            })
        );
    }

    #[test]
    fn test_parse_namelist() {
        let p = r#"Name"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_namelist(&mut tokens),
            Ok(vec![Name(String::from("Name"))])
        );

        let p = r#"Name,Another_Name"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_namelist(&mut tokens),
            Ok(vec![
                Name(String::from("Name")),
                Name(String::from("Another_Name"))
            ])
        );
    }

    #[test]
    fn test_parse_stat() {
        let p = r#";"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_stat(&mut tokens), Ok(Stat::SemiColon));

        let p = r#"::foo::"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::Label(Name(String::from("foo"))))
        );

        let p = r#"break"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(parse_stat(&mut tokens), Ok(Stat::Break));

        let p = r#"goto foo"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::Goto(Name(String::from("foo"))))
        );
        let p = r#"do end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::DoBlock(Block {
                stats: vec![],
                retstat: None
            }))
        );

        let p = r#"while true do end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::WhileBlock(WhileBlock {
                expr: Expr::Bool(true),
                block: Block {
                    stats: vec![],
                    retstat: None
                }
            }))
        );

        let p = r#"repeat until false"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::RepeatBlock(RepeatBlock {
                expr: Expr::Bool(false),
                block: Block {
                    stats: vec![],
                    retstat: None
                }
            }))
        );

        let p = r#"if foo then goto foo elseif bar then else end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::IfBlock(Box::new(IfBlock {
                expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                    "foo"
                )))))),
                block: Block {
                    stats: vec![Stat::Goto(Name(String::from("foo")))],
                    retstat: None
                },
                elseif: vec![ElseIf {
                    expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                        String::from("bar")
                    ))))),
                    block: Block {
                        stats: vec![],
                        retstat: None,
                    }
                }],
                else_blk: Some(Block {
                    stats: vec![],
                    retstat: None,
                })
            })))
        );

        let p = r#"for foo = 1, 2, bar do end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::ForRange(Box::new(ForRange {
                name: Name(String::from("foo")),
                exprs: (
                    Expr::Num(1f64),
                    Expr::Num(2f64),
                    Some(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                        String::from("bar")
                    ))))))
                ),
                block: Block {
                    stats: vec![],
                    retstat: None
                },
            })))
        );

        let p = r#"for foo, bar in true, nil do end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::ForIn(ForIn {
                namelist: vec![Name(String::from("foo")), Name(String::from("bar"))],
                exprlist: vec![Expr::Bool(true), Expr::Nil],
                block: Block {
                    stats: vec![],
                    retstat: None
                },
            }))
        );

        let p = r#"function foo(a, ...) return end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::FunctionDef(FunctionDef {
                name: FuncName {
                    path: vec![Name(String::from("foo"))],
                    method: None,
                },
                body: FuncBody {
                    params: Params {
                        names: vec![Name(String::from("a"))],
                        variadic: true
                    },
                    body: Block {
                        stats: vec![],
                        retstat: Some(vec![]),
                    }
                }
            }))
        );

        let p = r#"local function bar() return end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::LocalFunctionDef(LocalFunctionDef {
                name: Name(String::from("bar")),
                body: FuncBody {
                    params: Params {
                        names: vec![],
                        variadic: false
                    },
                    body: Block {
                        stats: vec![],
                        retstat: Some(vec![]),
                    }
                }
            }))
        );

        let p = r#"local foo, bar, buzz = nil, 10, "word""#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::LocalAssignment(LocalAssignment {
                namelist: vec![
                    Name(String::from("foo")),
                    Name(String::from("bar")),
                    Name(String::from("buzz"))
                ],
                exprlist: Some(vec![
                    Expr::Nil,
                    Expr::Num(10f64),
                    Expr::Str(String::from("word"))
                ]),
            }))
        );

        let p = r#"foo, bar = true, nil"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::Assignment(Assignment {
                varlist: vec![
                    Var::Name(Name(String::from("foo"))),
                    Var::Name(Name(String::from("bar")))
                ],
                exprlist: vec![Expr::Bool(true), Expr::Nil],
            }))
        );

        let p = r#"foo("bar")"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_stat(&mut tokens),
            Ok(Stat::FunctionCall(FunctionCall {
                expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
                args: Args::ExprList(vec![Expr::Str(String::from("bar"))])
            }))
        );
    }
    #[test]
    fn test_parse_funcbody() {
        let p = r#"() return end"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_funcbody(&mut tokens),
            Ok(FuncBody {
                params: Params {
                    names: vec![],
                    variadic: false
                },
                body: Block {
                    stats: vec![],
                    retstat: Some(vec![]),
                }
            })
        );
    }

    #[test]
    fn test_parse_funcname() {
        let p = r#"abc"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_funcname(&mut tokens),
            Ok(FuncName {
                path: vec![Name("abc".to_string())],
                method: None,
            })
        );

        let p = r#"abc.bar1"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_funcname(&mut tokens),
            Ok(FuncName {
                path: vec![Name("abc".to_string()), Name("bar1".to_string())],
                method: None,
            })
        );

        let p = r#"abc.bar1:mno"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(
            parse_funcname(&mut tokens),
            Ok(FuncName {
                path: vec![Name("abc".to_string()), Name("bar1".to_string())],
                method: Some(Name("mno".to_string())),
            })
        );
    }

    #[test]
    fn test_parse_label() {
        let p = r#"::jump_point::"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(parse_label(&mut tokens), Ok(Name("jump_point".to_string())));

        let p = r#"::jump_point"#;
        let tokens: Vec<_> = Lexer::new(p).collect();
        let mut tokens = TokenIter::new(&tokens);

        assert_eq!(parse_label(&mut tokens), Err(()));
    }

}
