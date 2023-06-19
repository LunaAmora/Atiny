use core::fmt;
use std::fmt::Display;

use atiny_location::ByteRange;

pub enum ErrorKind {
    Static(String),
    Sugestion(String),
    Dynamic(Box<dyn Fn() -> String>),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Static(s) | Self::Sugestion(s) => write!(f, "{}", s),
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

    pub fn new_sugestion(message: impl Into<String>, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Sugestion(message.into()),
            location,
        }
    }

    pub fn new_dyn(message: impl Display + 'static, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Dynamic(Box::new(move || message.to_string())),
            location,
        }
    }

    pub fn with_code<'a>(self, code: &'a str, file_name: &'a str) -> ErrorWithCode<'a> {
        ErrorWithCode {
            err: self,
            code,
            file_name,
        }
    }
}

pub struct ErrorWithCode<'a> {
    err: Error,
    code: &'a str,
    file_name: &'a str,
}

impl<'a> Display for ErrorWithCode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let location = self.err.location.locate(self.code);

        let is_sugestion = matches!(self.err.message, ErrorKind::Sugestion(_));
        let pad = 3;

        if !is_sugestion {
            writeln!(
                f,
                "\n[error]: {message}\n{:>pad$} ┌─> {file_name}:{line}:{column}\n{:>pad$} │",
                "",
                "",
                message = self.err.message,
                file_name = self.file_name,
                line = location.0.line + 1,
                column = location.0.column + 1
            )?;
        }
        let code = self.code.lines().collect::<Vec<_>>();
        let lines = code[location.0.line..=location.1.line].to_vec();

        let inline = location.0.line == location.1.line;
        let idx = location.1.line - location.0.line;

        for (i, line) in lines.iter().enumerate() {
            let line_n = location.0.line + i + 1;
            writeln!(f, "{:>pad$} │{}", line_n, line)?;

            let indent = location.0.column;
            let size = location.1.column - location.0.column;

            if !is_sugestion && inline && i == idx {
                writeln!(f, "{:>pad$} │{: >indent$}{:^>size$}", "", "", "")?;
            }

            if is_sugestion {
                let message = self.err.message.to_string();
                let size = message.len();
                writeln!(f, "{:>pad$} │{:>indent$}{}", line_n + 1, "", message)?;
                writeln!(f, "{:>pad$} │{:>indent$}{:+>size$}", "", "", "")?;
            }
        }

        Ok(())
    }
}
