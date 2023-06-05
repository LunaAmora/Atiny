use std::process::exit;

use atiny_checker::{context::Ctx, types::MonoType};
use atiny_parser::{error::from_lalrpop, ProgramParser};

fn main() {
    let code = "
        type Result t e =
            | Ok t
            | Err e
        
        fn ata : Int {
            match Ok (Ok 2) {
                Ok (Ok x) => x
            }
        }
    ";

    let mut ctx = Ctx::default();

    ctx = ctx.extend_type("Int".to_string());
    ctx = ctx.extend_type("Bool".to_string());

    ctx = ctx.extend(
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

    ctx = ctx.extend(
        "to_string".to_string(),
        MonoType::arrow(
            MonoType::var("Int".to_string()),
            MonoType::var("String".to_string()),
        )
        .to_poly(),
    );

    let result_type = ProgramParser::new()
        .parse(code)
        .map_err(|x| vec![from_lalrpop(x)])
        .and_then(|x| {
            ctx.add_top_level_types(x);
            ctx.get_errors()
                .map_or_else(|| Ok(format!("{}", ctx.signatures)), |err| Err(err.clone()))
        })
        .unwrap_or_else(|errs| {
            for err in errs {
                eprintln!("{}", err.with_code(code));
            }

            exit(1)
        });

    println!("{}", result_type);
}
