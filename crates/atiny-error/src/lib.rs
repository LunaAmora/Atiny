use core::fmt;
use std::fmt::Display;

use atiny_location::Range;

pub struct Error<'a> {
    message: String,
    location: Range,
    code: &'a str,
}

impl<'a> Error<'a> {
    pub fn new(message: impl Into<String>, location: Range, code: &'a str) -> Self {
        Self {
            message: message.into(),
            location,
            code,
        }
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "\n[error]: {message} at {location}\n",
            message = self.message,
            location = self.location
        )?;

        let lines = self.code.lines().collect::<Vec<_>>()
            [self.location.0.line..=self.location.1.line]
            .to_vec();

        for (i, line) in lines.iter().enumerate() {
            writeln!(f, "{:>3} | {}", self.location.0.line + i, line)?;
        }

        Ok(())
    }
}
