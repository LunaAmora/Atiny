#[macro_use] extern crate lalrpop_util;

pub mod syntax;
pub mod error;
pub mod check;

lalrpop_mod!(pub parser);