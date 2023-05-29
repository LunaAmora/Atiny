#![feature(custom_test_frameworks)]
#![test_runner(atiny_tests::test_runner)]

use atiny_checker::Infer;
use atiny_checker::{context::Ctx, types::MonoType};
use atiny_parser::{error::from_lalrpop, ExprParser};

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

mk_test! { concat!(env!("CARGO_MANIFEST_DIR"), "/suite/"), |code| {
    ExprParser::new()
        .parse(&code)
        .map_err(from_lalrpop)
        .and_then(|parsed| parsed.infer( default_context()))
        .map(|x| x.to_string())
        .unwrap_or_else(|err| err.with_code(&code).to_string())
} }
