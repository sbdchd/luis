//! A Lua parser
//!
//!  ```
//! use luis::ast::*;
//! use luis::parse::parse;
//! # fn main() {
//!
//! let program = "print('hello world')";
//!
//! assert_eq!(
//!     parse(&program),
//!     Ok(Block {
//!         stats: vec![Stat::FunctionCall(FunctionCall {
//!             expr: Box::new(PrefixExpr::Var(Var::Name(
//!                 Name(String::from("print"))
//!             ))),
//!             args: Args::ExprList(vec![
//!                 Expr::Str(String::from("hello world"))
//!             ])
//!         })],
//!         retstat: None,
//!     })
//! );
//! # }
//!  ```
pub mod ast;
pub mod iter;
pub mod lex;
pub mod parse;
