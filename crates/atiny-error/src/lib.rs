use core::fmt::{Formatter, Result};
use std::fmt::Display;

use atiny_location::{ByteRange, Point, Range};

pub enum ErrorKind {
    Static(Message),
    Sugestion(Message, SugestionKind),
    Dynamic(Box<dyn Fn() -> String>),
}

pub enum Message {
    Single(String),
    Multi(Vec<String>),
}

pub enum SugestionKind {
    Insert,
    Replace,
}

pub struct Error {
    message: ErrorKind,
    location: ByteRange,
}

impl Error {
    pub fn new(message: Message, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Static(message),
            location,
        }
    }

    pub fn new_sugestion(message: Message, kind: SugestionKind, location: ByteRange) -> Self {
        Self {
            message: ErrorKind::Sugestion(message, kind),
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

const PAD: usize = 3;

fn write_lines(f: &mut Formatter<'_>, code: &str, line_start: usize, line_end: usize) -> Result {
    for (line, line_number) in code.lines().skip(line_start).zip(line_start..=line_end) {
        writeln!(f, "{:>PAD$} │ {}", line_number + 1, line)?;
    }
    Ok(())
}

fn write_err_header<'a>(f: &mut Formatter<'_>, a: impl IntoIterator<Item = &'a String>) -> Result {
    for (i, s) in a.into_iter().enumerate() {
        if i == 0 {
            writeln!(f, "\n[error]: {s}\n")?;
        } else {
            writeln!(f, "         {s}\n")?;
        }
    }
    Ok(())
}

impl<'a> Display for ErrorWithCode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let Self {
            err: Error { message, location },
            code,
            file_name,
        } = self;

        let Range(start @ Point { line, column }, end) = location.locate(code);

        match message {
            ErrorKind::Sugestion(sugestion, kind) => {
                if matches!(kind, SugestionKind::Insert) {
                    write_lines(f, code, line, end.line)?;
                }

                let end_line = match kind {
                    SugestionKind::Insert => end.line + 2,
                    SugestionKind::Replace => end.line + 1,
                };

                match sugestion {
                    Message::Single(s) => {
                        let size = s.len();
                        writeln!(f, "{:>PAD$} │ {:>column$}{s}", end_line, "")?;
                        writeln!(f, "{:>PAD$} │ {:>column$}{:+>size$}", "", "", "")?;
                    }
                    Message::Multi(ms) => {
                        for (i, m) in ms.iter().enumerate() {
                            writeln!(f, "{:>PAD$}+│ {:>column$}{m}", end_line + i, "")?;
                        }
                    }
                }
            }

            error => {
                match error {
                    ErrorKind::Static(Message::Single(s)) => write_err_header(f, Some(s))?,
                    ErrorKind::Static(Message::Multi(m)) => write_err_header(f, m)?,
                    ErrorKind::Dynamic(d) => write_err_header(f, Some(&d()))?,
                    ErrorKind::Sugestion(..) => unreachable!(),
                };

                writeln!(f, "{:>PAD$} ┌─> {file_name}:{start}", "")?;
                writeln!(f, "{:>PAD$} │", "",)?;

                write_lines(f, code, line, end.line)?;

                if line == end.line {
                    let size = end.column - column;
                    writeln!(f, "{:>PAD$} │ {:>column$}{:^>size$}", "", "", "")?;
                }
            }
        };

        writeln!(f, "{:>PAD$} │", "")
    }
}
