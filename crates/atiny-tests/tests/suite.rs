#![feature(custom_test_frameworks)]
#![test_runner(atiny_tests::test_runner)]

use atiny_checker::infer::Infer;
use atiny_checker::{context::Ctx, types::MonoType};
use atiny_parser::{error::from_lalrpop, ExprParser, ProgramParser};

#[macro_use]
extern crate atiny_tests;

pub fn default_context() -> Ctx {
    Ctx::default()
        .extend(
            "add".to_string(),
            MonoType::arrow(
                MonoType::typ("Int".to_string()),
                MonoType::arrow(
                    MonoType::typ("Int".to_string()),
                    MonoType::typ("Int".to_string()),
                ),
            )
            .to_poly(),
        )
        .extend(
            "to_string".to_string(),
            MonoType::arrow(
                MonoType::typ("Int".to_string()),
                MonoType::typ("String".to_string()),
            )
            .to_poly(),
        )
}

mk_test! { "/suite/", |code| {
    let ctx = default_context();
    ExprParser::new()
        .parse(&code)
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| (parsed.infer(ctx.clone()), ctx.get_errors()))
        .and_then(|(typ, errors)| errors.map_or_else(|| Ok(typ.to_string()), |err| Err(err.clone())))
        .unwrap_or_else(|errs| {
            errs.into_iter().map(|x| x.with_code(&code).to_string()).collect()
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
