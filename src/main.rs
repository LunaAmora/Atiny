use atiny::{
    check::{context::Ctx, types::MonoType, Infer},
    error::Error,
    parser::ExprParser,
};
use std::process::exit;

fn main() {
    let code = "
        let b = |x| x;
        (b : forall x. x -> x)
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

    let result_type = ExprParser::new()
        .parse(code)
        .map_err(|x| Error::from_lalrpop(x, code))
        .and_then(|parsed| parsed.infer(ctx))
        .unwrap_or_else(|err| {
            eprintln!("{}", err);
            exit(1)
        });

    println!("{}", result_type);
}
