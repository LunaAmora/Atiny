#![feature(custom_test_frameworks)]
#![test_runner(atiny_tests::test_runner)]

use atiny_checker::context::Ctx;
use atiny_checker::infer::Infer;
use atiny_parser::{error::from_lalrpop, ExprParser, ProgramParser};

#[macro_use]
extern crate atiny_tests;

mk_test! { "/suite/expr/", |code, file_name| {
    let ctx = Ctx::default();
    ExprParser::new()
        .parse(&code)
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| (parsed.infer(ctx.clone()), ctx.take_errors()))
        .and_then(|((typ, _), errs)| errs.map_or_else(|| Ok(typ.to_string()), Err))
        .unwrap_or_else(|errs| {
            errs.into_iter().map(|x| x.with_code(&code, &file_name).to_string()).collect()
        })
} }

mk_test! { "/suite/parsing/", |code, file_name| {
    use itertools::Itertools;

    ProgramParser::new()
        .parse(&code)
        .map_err(from_lalrpop)
        .map(|x| x.iter().map(|x| x.to_string()).join("\n"))
        .unwrap_or_else(|err| err.with_code(&code, &file_name).to_string())
} }

mk_test! { "/suite/", |code, file_name| {
    let mut ctx = Ctx::default();

    ProgramParser::new()
        .parse(&code)
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| parsed.infer(&mut ctx))
        .map(|_| ctx.take_errors())
        .and_then(|errs| errs.map_or_else(|| Ok(String::new()), Err))
        .unwrap_or_else(|errs| {
            errs.into_iter().map(|x| x.with_code(&code, &file_name).to_string()).collect()
        })
} }
