//! This module describes locations in the source code. There are two types of absolute positions
//! using indexes in the raw string that are [Byte] and [ByteRange]. There are also two types of
//! positions that are [Point] and [Range] that have line and column numbers instead as a better way
//! to generate error messages and warn users.

use core::fmt;
use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct NodeId(pub usize);

/// Byte position in a source file.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Byte(pub usize);

impl Byte {
    /// Discovers a [Point] that is a line and column structure using the index inside the source
    /// code. Probably it's an expensive operation so I guess we will change it in the future.
    pub fn locate(&self, code: &str) -> Point {
        let mut acc = 0;
        for (line, code_line) in code.lines().enumerate() {
            if acc + code_line.len() + 1 > self.0 {
                return Point {
                    line,
                    column: self.0 - acc,
                };
            }
            acc += code_line.len() + 1;
        }
        Point::default()
    }
}

/// Two byte positions inside a source file.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct ByteRange(pub Byte, pub Byte, pub NodeId);

impl ByteRange {
    pub fn locate(&self, code: &str) -> Range {
        Range(self.0.locate(code), self.1.locate(code))
    }

    pub fn singleton(byte: usize, id: NodeId) -> Self {
        Self(Byte(byte), Byte(byte), id)
    }
}

/// Line and column position inside a source file.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct Point {
    pub line: usize,
    pub column: usize,
}

/// Two line and column positions ([Point]s) inside a source file.
#[derive(Debug, PartialEq, Eq)]
pub struct Range(pub Point, pub Point);

impl Range {
    /// Creates a new range using two byte positions.
    pub fn new(start: usize, end: usize, code: &str) -> Self {
        Self(Byte(start).locate(code), Byte(end).locate(code))
    }

    /// Generates a [Range] using a single byte array so it's equivalent to a single character
    /// pointer in the code.
    pub fn singleton(byte: usize, code: &str) -> Self {
        let point = Byte(byte).locate(code);
        Self(point, point)
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 != self.1 {
            write!(f, "{}~{}", self.0, self.1)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

#[derive(Debug)]
pub struct Located<T> {
    pub location: ByteRange,
    pub data: T,
}

impl<T: Default> Default for Located<T> {
    fn default() -> Self {
        Self {
            location: Default::default(),
            data: Default::default(),
        }
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Self {
        Self {
            location: self.location,
            data: self.data.clone(),
        }
    }
}

impl<T> Located<T> {
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Located<R> {
        Located {
            location: self.location,
            data: f(self.data),
        }
    }
}

pub trait WithLoc: Sized {
    fn loc(self, location: ByteRange) -> Located<Self> {
        Located {
            location,
            data: self,
        }
    }
}

impl<T: Sized> WithLoc for T {}
