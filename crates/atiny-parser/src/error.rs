use atiny_error::Error;
use atiny_location::{ByteRange, NodeId};

use lalrpop_util::{lexer::Token, ParseError};

pub fn from_lalrpop(err: ParseError<usize, Token, &str>, id: NodeId) -> Error {
    use ParseError::*;
    match err {
        InvalidToken { location } => error("Invalid token", location, id),
        UnrecognizedEof { location, .. } => error("unrecognized eof", location, id),
        UnrecognizedToken { token, .. } => error("unrecognized token", token.0, id),
        ExtraToken { token } => error("extra token", token.0, id),
        User { .. } => panic!("oh no :>"),
    }
}

fn error(error: &str, singleton: usize, id: NodeId) -> Error {
    Error::new(
        atiny_error::Message::Single(error.to_owned()),
        ByteRange::singleton(singleton, id),
    )
}
