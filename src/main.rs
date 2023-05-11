use std::process::exit;

use atiny::{
    check::{context::Ctx, infer, types::MonoType},
    error::Error,
    parser::SyntaxParser,
};

fn main() {
    let code = "
        let id = |x| x;
        to_string (to_string 2)
    ";

    let ctx = Ctx::new(code);
    let ctx = ctx.extend(
        "add".to_string(),
        MonoType::arrow(
            MonoType::var("Int".to_string()),
            MonoType::arrow(
                MonoType::var("Int".to_string()),
                MonoType::var("Int".to_string()),
            ),
        )
        .to_poly(),
    );
    let ctx = ctx.extend(
        "to_string".to_string(),
        MonoType::arrow(
            MonoType::var("Int".to_string()),
            MonoType::var("String".to_string()),
        )
        .to_poly(),
    );

    let result_type = SyntaxParser::new()
        .parse(code)
        .map_err(|x| Error::from_lalrpop(x, code))
        .and_then(|parsed| infer(ctx.clone(), parsed))
        .unwrap_or_else(|err| {
            eprintln!("{}", err);
            exit(1)
        });

    println!("{}", result_type);
}
