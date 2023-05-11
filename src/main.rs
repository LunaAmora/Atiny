use std::process::exit;

use atiny::{
    check::{Ctx, MonoType, TypeScheme},
    error::Error,
    parser::SyntaxParser,
};

fn main() {
    let code = "
        let id = |x| x;
        id 2
    ";

    let parsed = SyntaxParser::new().parse(code).unwrap_or_else(|err| {
        eprintln!("{}", Error::from_lalrpop(err, code));
        exit(1)
    });

    let mut names = 0;
    let mut ctx = Ctx::new(&mut names);

    let p = TypeScheme {
        names: vec!["a".to_string(), "b".to_string()],
        mono: MonoType::arrow(
            MonoType::var("a".to_string()),
            MonoType::arrow(
                MonoType::var("b".to_string()),
                MonoType::var("a".to_string()),
            ),
        ),
    };

    let p2 = p.instantiate(&mut ctx);

    if let MonoType::Arrow(n, _) = &&*p2 {
        if let MonoType::Hole(h) = &&**n {
            h.fill(MonoType::var("UwU".to_string()))
        }
    }

    println!("{}", p);
    println!("{}", p2);
    println!("{}", p2.generalize(&mut ctx));
}
