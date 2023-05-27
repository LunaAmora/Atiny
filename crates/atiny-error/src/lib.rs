use core::fmt;
use std::fmt::Display;

use atiny_location::ByteRange;

pub struct Error {
    message: String,
    location: ByteRange,
}

impl Error {
    pub fn new(message: impl Into<String>, location: ByteRange) -> Self {
        Self {
            message: message.into(),
            location,
        }
    }

    pub fn with_code(self, code: &str) -> ErrorWithCode {
        ErrorWithCode { err: self, code }
    }
}

pub struct ErrorWithCode<'a> {
    err: Error,
    code: &'a str,
}

impl<'a> Display for ErrorWithCode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let location = self.err.location.locate(self.code);

        writeln!(
            f,
            "\n[error]: {message} at {location}\n",
            message = self.err.message
        )?;

        let code = self.code.lines().collect::<Vec<_>>();
        let lines = code[location.0.line..=location.1.line].to_vec();

        for (i, line) in lines.iter().enumerate() {
            writeln!(f, "{:>3} | {}", location.0.line + i, line)?;
        }

        Ok(())
    }
}
