#![feature(custom_test_frameworks)]
#![test_runner(atiny_tests::test_runner)]

use atiny_checker::Infer;
use atiny_checker::{context::Ctx, types::MonoType};
use atiny_parser::{error::from_lalrpop, ExprParser, ProgramParser};

#[macro_use]
extern crate atiny_tests;

pub fn default_context() -> Ctx {
    Ctx::default()
        .extend(
            "add".to_string(),
            MonoType::arrow(
                MonoType::var("Int".to_string()),
                MonoType::arrow(
                    MonoType::var("Int".to_string()),
                    MonoType::var("Int".to_string()),
                ),
            )
            .to_poly(),
        )
        .extend(
            "to_string".to_string(),
            MonoType::arrow(
                MonoType::var("Int".to_string()),
                MonoType::var("String".to_string()),
            )
            .to_poly(),
        )
}

mk_test! { "/suite/", |code| {
    let ctx = default_context();
    ExprParser::new()
        .parse(&code)
        .map_err(|x| vec![from_lalrpop(x)])
        .and_then(|parsed| {
            let typ = parsed.infer(ctx.clone());
            let borrowed = ctx.errors.borrow();
            if borrowed.is_empty() {
                Ok(typ.to_string())
            } else {
                Err(borrowed.clone())
            }
        })
        .unwrap_or_else(|errs| {
            errs.iter().map(|x| x.clone().with_code(&code).to_string()).collect()
        })
} }

mk_test! { "/suite/parsing/", |code| {
    use itertools::Itertools;

    ProgramParser::new()
        .parse(&code)
        .map_err(from_lalrpop)
        .map(|x| x.iter().map(|x| x.to_string()).join("\n"))
        .unwrap_or_else(|err| err.with_code(&code).to_string())
} }
