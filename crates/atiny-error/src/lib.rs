use core::fmt;
use std::fmt::Display;

use atiny_location::{ByteRange, Point, Range};

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
        let Self {
            err: Error { message, location },
            code,
            file_name,
        } = self;

        let Range(start @ Point { line, column }, end) = location.locate(code);
        const PAD: usize = 3;

        let print_lines = |f: &mut fmt::Formatter<'_>| -> fmt::Result {
            for (line, line_number) in code.lines().skip(line).zip(line..=end.line) {
                writeln!(f, "{:>PAD$} │ {}", line_number + 1, line)?;
            }
            Ok(())
        };

        match message {
            ErrorKind::Sugestion(sugestion) => {
                print_lines(f)?;

                let sugestion = sugestion.to_string();
                let size = sugestion.len();
                writeln!(f, "{:>PAD$} │ {:>column$}{sugestion}", end.line + 2, "")?;
                writeln!(f, "{:>PAD$} │ {:>column$}{:+>size$}", "", "", "")?;
            }

            error => {
                writeln!(f, "\n[error]: {error}\n")?;
                writeln!(f, "{:>PAD$} ┌─> {file_name}:{start}", "")?;
                writeln!(f, "{:>PAD$} │", "",)?;

                print_lines(f)?;

                if line == end.line {
                    let size = end.column - column;
                    writeln!(f, "{:>PAD$} │ {:>column$}{:^>size$}", "", "", "")?;
                }
            }
        };

        writeln!(f, "{:>PAD$} │", "")
    }
}
