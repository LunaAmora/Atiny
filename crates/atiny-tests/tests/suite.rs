#![feature(custom_test_frameworks)]
#![test_runner(atiny_tests::test_runner)]

use atiny_checker::infer::Infer;
use atiny_checker::program::Program;
use atiny_tree::r#abstract::Expr;
use itertools::Itertools;

#[macro_use]
extern crate atiny_tests;

mk_test!("/suite/expr/", |file_name| {
    let program = Program::new(file_name).expect("IO error");

    let mut parsed_result = String::new();
    let ctx = program.get_entry_ctx(|ctx, parsed: Expr| {
        let (t, _) = parsed.infer(ctx.clone());
        parsed_result = t.to_string();
    });

    let program = Program::return_ctx(ctx);
    let mut prog = program.borrow_mut();
    prog.take_errors()
        .map_or_else(|| parsed_result, |errs| errs.into_iter().join(""))
});

mk_test!("/suite/parsing/", |file_name| {
    use itertools::Itertools;
    let program = Program::new(file_name).expect("IO error");

    let mut parsed_result = String::new();
    let ctx = program.get_entry_ctx(|_, parsed: Vec<_>| {
        parsed_result = parsed.iter().map(|top| top.to_string()).join("\n");
    });

    let program = Program::return_ctx(ctx);
    let mut prog = program.borrow_mut();
    prog.take_errors()
        .map_or_else(|| parsed_result, |errs| errs.into_iter().join(""))
});

mk_test!("/suite/", |file_name| {
    let program = Program::new(file_name).expect("IO error");

    let ctx = program.get_entry_ctx(|ctx, parsed: Vec<_>| {
        parsed.infer(ctx);
    });

    let program = Program::return_ctx(ctx);
    let mut prog = program.borrow_mut();
    prog.take_errors()
        .map_or_else(String::new, |err| err.into_iter().join(""))
});
