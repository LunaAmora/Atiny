use std::process::exit;

use atiny_checker::{context::Ctx, types::MonoType, Infer};
use atiny_parser::{error::from_lalrpop, ExprParser};

fn main() {
    let code = "
        let a = to_string true;
        let b = to_string false;
        b
    ";

    let ctx = Ctx::default();
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
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| (parsed.infer(ctx.clone()), ctx.get_errors()))
        .and_then(|(typ, errors)| errors.map_or_else(|| Ok(typ), |err| Err(err.clone())))
        .unwrap_or_else(|errs| {
            for err in errs {
                eprintln!("{}", err.with_code(code));
            }

            exit(1)
        });

    println!("Result: {}", result_type);
}
