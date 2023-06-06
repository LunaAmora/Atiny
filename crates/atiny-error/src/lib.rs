use core::fmt;
use std::fmt::Display;

use atiny_location::ByteRange;

pub enum ErrorKind {
    Static(String),
    Dynamic(Box<dyn Fn() -> String>),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Static(s) => write!(f, "{}", s),
            Self::Dynamic(d) => write!(f, "{}", d()),
        }
    }
}

pub struct Error {
    message: ErrorKind,
    location: ByteRange,
}

impl Error {
    pub fn new(message: impl Into<String>, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Static(message.into()),
            location,
        }
    }

    pub fn new_dyn(message: impl Display + 'static, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Dynamic(Box::new(move || message.to_string())),
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

        let inline = location.0.line == location.1.line;
        let idx = location.1.line - location.0.line;

        for (i, line) in lines.iter().enumerate() {
            writeln!(f, "{:>3} | {}", location.0.line + i, line)?;

            let indent = location.0.column;
            let size = location.1.column - location.0.column;

            if inline && i == idx {
                writeln!(f, "{:>3} | {: >indent$}{:^>size$}", "", "", "")?;
            }
        }

        Ok(())
    }
}
