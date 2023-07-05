use atiny_error::Error;
use atiny_location::ByteRange;

use lalrpop_util::{lexer::Token, ParseError};

pub fn from_lalrpop(err: ParseError<usize, Token, &str>) -> Error {
    use ParseError::*;
    match err {
        InvalidToken { location } => error("Invalid token", location),
        UnrecognizedEof { location, .. } => error("unrecognized eof", location),
        UnrecognizedToken { token, .. } => error("unrecognized token", token.0),
        ExtraToken { token } => error("extra token", token.0),
        User { .. } => panic!("oh no :>"),
    }
}

fn error(error: &str, singleton: usize) -> Error {
    Error::new(
        atiny_error::Message::Single(error.to_owned()),
        ByteRange::singleton(singleton),
    )
}
