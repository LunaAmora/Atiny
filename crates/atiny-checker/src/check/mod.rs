//! Exposes a trait called [Check] that checks if a type T can check against a monotype. It's used
//! mainly for conversions and optimizations.

use crate::types::Type;

pub mod expr;

/// This trait exposes a function called [Check::check] that tries to check an expression against a
/// known type only producing some side effects. An example of this type rule is
///
/// ```md
///       G, x: 'a |- e <== b'
/// -------------------------------
///     G |- |x| e <== a' -> b'
/// ```
///
pub trait Check<'a> {
    type Context;
    type Result;

    /// Checks an expression against a known type.
    fn check(self, ctx: Self::Context, typ: Type) -> Self::Result;
}
