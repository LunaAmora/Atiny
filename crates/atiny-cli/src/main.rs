use std::process::exit;

use atiny_checker::{context::Ctx, types::MonoType, Infer};
use atiny_parser::{error::from_lalrpop, ExprParser};

fn main() {
    let code = "
        |a| match a {
            b => |x| |y| b,
            c => {
              let d = |x| c;
              let e = |y| d;
              e
            }
        }
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
        .map_err(from_lalrpop)
        .and_then(|parsed| parsed.infer(ctx))
        .unwrap_or_else(|err| {
            eprintln!("{}", err.with_code(code));
            exit(1)
        });

    println!("{}", result_type);
}
