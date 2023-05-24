#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::use_self)]
#![feature(try_trait_v2)]

#[macro_use]
extern crate lalrpop_util;

pub mod check;
pub mod error;
pub mod location;
pub mod syntax;

lalrpop_mod!(#[allow(warnings)] pub parser);
