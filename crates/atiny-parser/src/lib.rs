//! This module parses a text source code into a [atiny_tree::abstract] tree. Following the spec
//! that is described in the README.md and using a LALR(1) parser generator called LALRPOP. It does
//! not include error recovery strategies nor incremental parsing.

#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::use_self)]

pub mod error;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(
    #[allow(warnings)]
    /// The parsing module
    parser
);

pub use parser::*;
