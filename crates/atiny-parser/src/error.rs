use atiny_error::Error;
use atiny_location::Range;

use lalrpop_util::{lexer::Token, ParseError};

pub fn from_lalrpop<'a>(err: ParseError<usize, Token, &str>, code: &'a str) -> Error<'a> {
    use ParseError::*;
    match err {
        InvalidToken { location } => {
            Error::new("Invalid token", Range::singleton(location, code), code)
        }
        UnrecognizedEof { location, .. } => {
            Error::new("unrecognized eof", Range::singleton(location, code), code)
        }
        UnrecognizedToken { token, .. } => {
            Error::new("unrecognized token", Range::singleton(token.0, code), code)
        }
        ExtraToken { token } => Error::new("extra token", Range::singleton(token.0, code), code),
        User { .. } => panic!("oh no :>"),
    }
}
