//! Exposes an interface to infer the type of expressions, patterns, types and some constructions.
//! The main construction of this module is the [Infer] trait.

pub mod expr;
pub mod pat;
pub mod top_level;
pub mod typ;

/// This trait exposes a function called [Infer::infer] that tries to discover a type for an
/// expression. A type rule that express this is:
///
/// ```md
///    'a = new_hole     G, x: 'a |- e => b'
/// ----------------------------------------
///         G |- |x| e => a' -> b'
/// ```
///
pub trait Infer {
    type Context<'a>;
    type Return;

    /// Infers the type of an expression.
    fn infer(self, ctx: Self::Context<'_>) -> Self::Return;
}
