//! The Glorious Atiny Compiler.
//!
//! It compiles the atiny language to LLVM. (Not Yet!)
//!
//!

use std::path::PathBuf;

use atiny_checker::infer::Infer;
use atiny_checker::program::Program;
use atiny_lowering::visitors::{ClosureMoveChecker, PartialAppRemover};
use atiny_lowering::walkable::Walkable;
use clap::Parser;

#[derive(Parser)]
enum CargoCli {
    /// Type checks a file and returns if it is valid or not.
    TypeCheck { file: PathBuf },
}

fn main() {
    let CargoCli::TypeCheck { file } = CargoCli::parse();

    Program::new(file).map_or_else(
        |err| println!("IO error: {}", err),
        |mut program| {
            let _ = program
                .get_entry(|ctx, parsed: Option<Vec<_>>| parsed.infer(ctx))
                .infer(program.clone());

            program.walk(&mut PartialAppRemover);
            program.walk(&mut ClosureMoveChecker::new(program.clone()));
            program.print_errors();
        },
    );
}
