use std::process::exit;

use atiny_checker::{context::Ctx, types::MonoType};
use atiny_parser::{error::from_lalrpop, ProgramParser};

fn main() {
    let code = "
        type Result t e =
            | Ok t
            | Err e
        
        type List a = 
            | Cons a (List a)
            | Nil
        
        fn ata (a: T) (b: T) : T {
            |x| {
                let c = 2;
                c
            }
        }
    ";

    let mut ctx = Ctx::default();
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
