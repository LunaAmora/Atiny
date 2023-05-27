use atiny_error::Error;
use atiny_location::ByteRange;

use lalrpop_util::{lexer::Token, ParseError};

pub fn from_lalrpop(err: ParseError<usize, Token, &str>) -> Error {
    use ParseError::*;
    match err {
        InvalidToken { location } => Error::new("Invalid token", ByteRange::singleton(location)),
        UnrecognizedEof { location, .. } => {
            Error::new("unrecognized eof", ByteRange::singleton(location))
        }
        UnrecognizedToken { token, .. } => {
            Error::new("unrecognized token", ByteRange::singleton(token.0))
        }
        ExtraToken { token } => Error::new("extra token", ByteRange::singleton(token.0)),
        User { .. } => panic!("oh no :>"),
    }
}
