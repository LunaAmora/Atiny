//! The Glorious Atiny Compiler.
//!
//! It compiles the atiny language to LLVM. (Not Yet!)
//!
//!

use std::path::PathBuf;

use atiny_checker::infer::Infer;
use atiny_checker::program::Program;
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
        |program| {
            let ctx = Program::get_entry_ctx(program, |ctx, parsed: Vec<_>| {
                parsed.infer(ctx);
            });

            let program = ctx.program();
            program.borrow_mut().print_errors();
        },
    );
}
