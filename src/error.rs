use core::fmt;
use std::fmt::Display;

use lalrpop_util::{ParseError, lexer::Token};

use crate::syntax::Range;

pub struct Error<'a> {
    message: String,
    location: Range,
    code: &'a str
}

impl<'a> Error<'a> {
    pub fn new(message: impl Into<String>, location: Range, code: &'a str) -> Self {
        Self {
            message: message.into(),
            location,
            code
        }
    }

    pub fn from_lalrpop(err: ParseError<usize, Token, &str>, code: &'a str) -> Error<'a> {
        use ParseError::*;
        match err {
            InvalidToken { location } => {
                Error::new("Invalid token", Range::singleton(location, code), code)
            }
            UnrecognizedEOF { location, .. } => {
                Error::new("unrecognized eof", Range::singleton(location, code), code)
            }
            UnrecognizedToken { token, .. } => {
                Error::new("unrecognized token", Range::singleton(token.0, code), code)
            }
            ExtraToken { token } => Error::new("extra token", Range::singleton(token.0, code), code),
            User { .. } => panic!("oh no :>"),
        }
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\n[error]: {message} at {location}\n", message = self.message, location = self.location)?;

        let lines = self.code.lines().collect::<Vec<_>>()[self.location.0.line..=self.location.1.line].to_vec();

        for (i, line) in lines.iter().enumerate() {
            write!(f, "{:>3} | {}\n", self.location.0.line + i, line)?;
        }

        Ok(())
    }
}