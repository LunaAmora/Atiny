use std::process::exit;

use atiny_checker::{
    context::Ctx,
    infer::Infer,
    types::{MonoType, TypeSignature, TypeValue},
};
use atiny_parser::{error::from_lalrpop, ProgramParser};

fn main() {
    let code = "
        type List t = | Cons t (List t) | Nil
        type Unit = | Unit
        type Bool = | true | false

        fn ata (x: List Int) : Int {
            match Nil {
                Cons true (Cons _ true) => 2,
                Nil                     => 4,
            }
        }
    ";

    let mut ctx = Ctx::default();

    ctx.signatures.types.insert(
        "Int".to_string(),
        TypeSignature {
            name: "Int".to_string(),
            params: vec![],
            value: TypeValue::Opaque,
        },
    );

    ctx = ctx.extend(
        "add".to_string(),
        MonoType::arrow(
            MonoType::typ("Int".to_string()),
            MonoType::arrow(
                MonoType::typ("Int".to_string()),
                MonoType::typ("Int".to_string()),
            ),
        )
        .to_poly(),
    );

    ctx = ctx.extend(
        "to_string".to_string(),
        MonoType::arrow(
            MonoType::typ("Int".to_string()),
            MonoType::typ("String".to_string()),
        )
        .to_poly(),
    );

    ProgramParser::new()
        .parse(code)
        .map_err(|x| vec![from_lalrpop(x)])
        .map(|parsed| parsed.infer(&mut ctx).take_errors())
        .and_then(|errs| errs.map_or_else(|| Ok(()), Err))
        .unwrap_or_else(|errs| {
            for err in errs {
                eprintln!("{}", err.with_code(code));
            }

            exit(1)
        });
}
