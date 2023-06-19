//! The Glorious Atiny Compiler.
//!
//! It compiles the atiny language to LLVM. (Not Yet!)
//!
//!

use std::{path::PathBuf, process::exit};

use atiny_checker::context::Ctx;
use atiny_checker::infer::Infer;
use atiny_parser::{error::from_lalrpop, ProgramParser};
use clap::Parser;

#[derive(Parser)]
enum CargoCli {
    /// Type checks a file and returns if it is valid or not.
    TypeCheck { file: PathBuf },
}

fn main() {
    let CargoCli::TypeCheck { file } = CargoCli::parse();

    let code = std::fs::read_to_string(file).expect("cannot read file!");

    let mut ctx = Ctx::default();

    ProgramParser::new()
        .parse(&code)
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| parsed.infer(&mut ctx))
        .map(|_| ctx.take_errors())
        .and_then(|errs| errs.map_or_else(|| Ok(()), Err))
        .unwrap_or_else(|errs| {
            for err in errs {
                eprint!("{}", err.with_code(&code));
            }

            exit(1)
        });
}
