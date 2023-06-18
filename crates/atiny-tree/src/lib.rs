//! This module joins all of the trees that are necessary to compile atiny as a compiler. It does
//! not have dependency with anything of the compiler only with atiny-location that provides
//! constructions to localize things in the source code.
//!
//! The trees available in this module are:
//!
//! - Abstract: It is a purely syntactic tree that does not have semantic information, it comes from
//!   the parser.
//! - Elaborated: It is a tree that contains semantic information that comes from the type checking
//!   process.
//!

pub mod r#abstract;
pub mod elaborated;
