#[macro_use]
extern crate lalrpop_util;

pub mod check;
pub mod error;
pub mod syntax;

lalrpop_mod!(pub parser);
