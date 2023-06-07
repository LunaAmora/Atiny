//! Does exhaustiveness checking on a [Problem]. The exhaustiveness checking is an algorithm that
//! checks if a pattern match problem is covering all possible cases. This is done by refining a
//! problem into a set of sub problems and checking if the sub problems covers specific cases.

use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
};

use atiny_location::ByteRange;
use atiny_tree::r#abstract::*;
use itertools::Itertools;

use crate::{context::Ctx, types::*};

pub enum CaseTree {
    Node(Vec<(String, CaseTree)>),
    Leaf(usize),
}

impl CaseTree {
    pub fn render_indented(&self, f: &mut Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            Self::Node(vec) => {
                for (name, tree) in vec {
                    writeln!(f, "{:indent$}{}:", "", name, indent = indent)?;
                    tree.render_indented(f, indent + 2)?;
                }
            }
            Self::Leaf(index) => writeln!(f, "{:indent$}{}", "", index, indent = indent)?,
        }
        Ok(())
    }
}

impl Display for CaseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render_indented(f, 0)
    }
}

/// It is used to return possible constructors of a type. Some types have infinite constructors like
/// Int so we cannot return a list of all the constructors. In that case we return Infinite.
pub enum Finitude<T> {
    Infinite,
    Finite(T),
}

/// A witness is a proof that a pattern match problem is exhaustive. It contains a list of patterns
/// if it's not exhaustive proving it's non exhaustiveness by contradiction. If it's exhaustive then
/// it will return a case tree that contains all the possible cases of the problem.
pub enum Witness {
    Ok(CaseTree),
    NonExhaustive(Row),
}

/// Possible constructors used for specialization.
pub enum Constructor<T, U> {
    Defined(T),
    Tuple(Vec<U>),
    Int,
    Bool,
    Unit,
}

impl Witness {
    /// Creates a new witness that contains a new pattern based on the thing that got non exhausted.
    pub fn expand(self, decl: &ConstructorSignature) -> Self {
        if let Self::NonExhaustive(row) = self {
            let (vec, row) = row.split_vec(decl.args.len());
            let row = row.preppend(Pattern {
                location: ByteRange::singleton(0),
                data: PatternKind::Constructor(decl.name.clone(), vec),
            });
            Self::NonExhaustive(row)
        } else {
            self
        }
    }

    /// Checks if a witness is non exhaustive.
    pub fn is_non_exhaustive(&self) -> bool {
        matches!(self, Self::NonExhaustive(_))
    }

    /// Adds a pattern to a witness.
    pub fn add_pattern(self, pat: Pattern) -> Self {
        if let Self::NonExhaustive(row) = self {
            Self::NonExhaustive(row.preppend(pat))
        } else {
            self
        }
    }
}

/// A row is a clause of a pattern match. It contains a list of patterns that are being matched.
#[derive(Clone)]
pub struct Row(Option<usize>, Vec<Pattern>);

pub struct Column<'a>(Vec<&'a Pattern>);

impl Row {
    /// Checks if a row is empty.
    fn is_empty(&self) -> bool {
        self.1.is_empty()
    }

    /// Adds a sequence of patterns to the beginning of a row and removes the first pattern of the
    /// row. It's used to "inline" a sequence of patterns into a row.
    fn inline(mut self, vec: Vec<Pattern>) -> Self {
        self.1 = vec.into_iter().chain(self.1.into_iter().skip(1)).collect();
        self
    }

    /// Removes the first pattern of a row.
    fn pop_front(mut self) -> Self {
        self.1 = self.1.into_iter().skip(1).collect();
        self
    }

    /// Prepends a pattern to a row.
    fn preppend(mut self, pat: Pattern) -> Self {
        self.1 = vec![pat].into_iter().chain(self.1.into_iter()).collect();
        self
    }

    /// Splits a row into two rows. The first row contains the first `place` patterns and the second
    /// row contains the rest of the patterns.
    fn split_vec(mut self, place: usize) -> (Vec<Pattern>, Self) {
        let vec = self.1.split_off(self.1.len() - place);
        (vec, self)
    }

    /// Converts a row into a pattern. This is done by asserting that the row only contains one
    /// pattern and then returning that pattern.
    pub fn into_pattern(self) -> Pattern {
        assert_eq!(self.1.len(), 1);
        self.1.into_iter().next().unwrap()
    }

    pub fn first_column(&self) -> Column<'_> {
        Column(self.1.iter().collect())
    }

    /// Creates a "default_row" (removes everything from the beginning that does not matches "var")
    /// and then returns the rest of the row.
    pub fn default_row(self, ctx: &mut Ctx) -> Vec<Self> {
        match self.1[0].data.clone() {
            PatternKind::Atom(AtomKind::Identifier(n)) if ctx.lookup_cons(&n).is_none() => {
                vec![self.pop_front()]
            }
            PatternKind::Atom(AtomKind::Group(pat)) => {
                self.pop_front().preppend(*pat).default_row(ctx)
            }
            _ => vec![],
        }
    }

    pub fn specialize_tuple(self, ctx: &mut Ctx, size: usize) -> Vec<Self> {
        match self.1[0].data.clone() {
            PatternKind::Atom(AtomKind::Identifier(n)) if ctx.lookup_cons(&n).is_none() => {
                vec![self.inline(vec![wildcard(); size])]
            }
            PatternKind::Atom(AtomKind::Group(pat)) => {
                self.pop_front().preppend(*pat).specialize_tuple(ctx, size)
            }
            PatternKind::Atom(AtomKind::Tuple(pats)) => vec![self.inline(pats)],
            _ => vec![],
        }
    }

    pub fn is_all_wildcards(&self, ctx: &mut Ctx) -> bool {
        self.1.iter().all(|pat| {
            matches!(
            &pat.data,
            PatternKind::Atom(AtomKind::Identifier(n)) if ctx.lookup_cons(n).is_none()
            )
        })
    }

    /// Specializes a row for a certain constructor. It does this by checking if the first pattern
    /// matches the constructor and then flattening the rest of the patterns into the row.
    pub fn specialize(self, ctx: &mut Ctx, constructor: &ConstructorSignature) -> Vec<Self> {
        match self.1[0].data.clone() {
            // c(r1, ...., ra)  | r1...ra pi2...pin
            PatternKind::Constructor(name, args) if name == constructor.name => {
                vec![self.inline(args)]
            }

            PatternKind::Atom(AtomKind::Identifier(name)) if ctx.lookup_cons(&name).is_some() => {
                if name == constructor.name {
                    // c | pi2...pin
                    vec![self.pop_front()]
                } else {
                    // c' | No Row
                    vec![]
                }
            }
            // c'(r1, ...., ra) | No Row
            PatternKind::Constructor(_, _) => {
                vec![]
            }
            // _ | _...._ pi2...pin
            PatternKind::Atom(AtomKind::Identifier(_)) => {
                vec![self.inline(vec![wildcard(); constructor.args.len()])]
            }

            PatternKind::Atom(AtomKind::Group(pat)) => {
                self.pop_front().preppend(*pat).specialize(ctx, constructor)
            }

            _ => todo!(),
        }
    }

    fn first(&self) -> &Pattern {
        self.1.first().unwrap()
    }
}

/// This structure represents if a set of constructors are complete or not.
pub enum Completeness {
    Complete(Vec<String>),
    Incomplete(Finitude<Vec<String>>),
}

#[derive(Clone)]
pub struct Matrix(Vec<Row>);

impl Matrix {
    /// It checks if the matrix is empty (there are no rows inside of it) then it is not exhaustive.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// It transforms a column into a matrix. This is done by transforming each pattern into a row.
    pub fn from_column(column: Vec<Pattern>) -> Self {
        Self(
            column
                .into_iter()
                .enumerate()
                .map(|(place, pat)| Row(Some(place), vec![pat]))
                .collect(),
        )
    }

    /// Checks if the first pattern is a wildcard or a variable so it can determine the set complete.
    pub fn is_wildcard(&self, ctx: &mut Ctx) -> bool {
        self.0.iter().all(|row| {
            matches!(
            &row.1[0].data,
            PatternKind::Atom(AtomKind::Identifier(n)) if ctx.lookup_cons(n).is_none()
            )
        })
    }

    /// Gets the used constructor of a pattern. This is done by checking if the pattern is a
    /// constructor on the context.
    fn get_used_constructor(ctx: &mut Ctx, pat: &PatternKind) -> Option<String> {
        match pat {
            PatternKind::Atom(AtomKind::Identifier(id)) if ctx.lookup_cons(id).is_some() => {
                Some(id.clone())
            }
            PatternKind::Atom(AtomKind::Group(pat)) => Self::get_used_constructor(ctx, &pat.data),
            PatternKind::Constructor(name, _) => Some(name.clone()),
            _ => None,
        }
    }

    /// Gets the used constructors of a matrix. This is done by getting the used constructors of
    /// each of the rows.
    fn get_used_constructors(&self, ctx: &mut Ctx) -> Vec<String> {
        self.0
            .iter()
            .flat_map(|row| Self::get_used_constructor(ctx, &row.first().data))
            .collect()
    }

    /// It creates a default matrix by creating a default row for each row in the matrix. This is
    /// done by removing everything from the beginning that does not match a variable or a wildcard.
    pub fn default_matrix(self, ctx: &mut Ctx) -> Self {
        Self(
            self.0
                .into_iter()
                .flat_map(|row| row.default_row(ctx))
                .collect(),
        )
    }

    pub fn is_complete_type_sig(&self, ctx: &mut Ctx, type_sig: &TypeSignature) -> Completeness {
        let result = self.get_used_constructors(ctx);
        let names = type_sig.get_constructors();

        names.map_or(Completeness::Incomplete(Finitude::Infinite), |names| {
            let used_constructors = result
                .into_iter()
                .filter(|name| names.contains(name.as_str()))
                .collect::<HashSet<_>>();

            if used_constructors.len() == names.len() || self.is_wildcard(ctx) {
                Completeness::Complete(used_constructors.into_iter().collect_vec())
            } else {
                Completeness::Incomplete(Finitude::Finite(
                    names.difference(&used_constructors).cloned().collect(),
                ))
            }
        })
    }

    /// If removes all the rows that are after a row that is completely made out of wildcards or
    /// variables. It is just an optimization.
    pub fn filter_first_match(&mut self, ctx: &mut Ctx) {
        let index = self.0.iter().find_position(|row| row.is_all_wildcards(ctx));
        if let Some((index, _)) = index {
            self.0.truncate(index + 1);
        }
    }

    /// Creates a new matrix where all the rows are specialized by a constructor. This is done by
    /// checking if each row matches a constructor in the first pattern and then flattening all
    /// the patterns into the row or removing it if it does not matches.
    pub fn specialize(self, ctx: &mut Ctx, constructor: &ConstructorSignature) -> Self {
        Self(
            self.0
                .into_iter()
                .flat_map(|row| row.specialize(ctx, constructor))
                .collect(),
        )
    }
}

/// A problem is something that need to solve in order to check if a pattern match is exhaustive.
/// it contains the type of the problem, the case that is being checked and the rows that are the
/// patterns that are being matched.
#[derive(Clone)]
pub struct Problem {
    typ: Vec<Type>,
    case: Row,
    matrix: Matrix,
}

impl Display for Row {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.1.is_empty() {
            write!(f, "No Row")
        } else {
            write!(f, "| {}", self.1[0])?;
            for pat in self.1.iter().skip(1) {
                write!(f, " {}", pat)?;
            }
            Ok(())
        }
    }
}

impl Display for Problem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Problem:")?;
        for typ in self.typ.iter() {
            write!(f, " {}", typ)?;
        }
        writeln!(f)?;
        writeln!(f, "Case: \n  {}", self.case)?;
        writeln!(f, "Rows:")?;
        for row in self.matrix.0.iter() {
            writeln!(f, "  {}", row)?;
        }
        Ok(())
    }
}

impl Problem {
    /// Creates a new problem. It takes the scrutineer type of the problem, the case that is being
    /// checked and the patterns that are being matched.
    pub fn new(typ: Type, useful: Vec<Pattern>, columns: Vec<Pattern>) -> Self {
        Self {
            typ: vec![typ],
            case: Row(None, useful),
            matrix: Matrix::from_column(columns),
        }
    }

    /// Checks if the matrix is exhaustive by looking at all the rows and checking if any of them
    /// is empty (it matches ).
    fn get_exhaustive_row(&self) -> Option<&Row> {
        self.matrix.0.iter().find(|row| row.is_empty())
    }

    /// Checks if the problem is empty. A problem is empty if the matrix is empty.
    pub fn is_empty(&self) -> bool {
        self.matrix.is_empty()
    }

    /// Specializes into a new problem. This is done by instantiating the type of the problem with
    /// the type arguments and then flattening the rest of the type arguments into the type of the
    /// problem and specializing each of the rows with a constructor.
    fn specialize(
        self,
        ctx: &mut Ctx,
        type_args: &[Type],
        constructor: &ConstructorSignature,
        case: Vec<Pattern>,
    ) -> Self {
        let generalized = constructor.typ.instantiate_with(type_args);

        Self {
            typ: generalized
                .iter()
                .chain(self.typ.iter().skip(1).cloned())
                .collect(),
            case: self.case.inline(case),
            matrix: self.matrix.specialize(&mut ctx.clone(), constructor),
        }
    }

    /// Splits a problem into multiple by looking at the type of the problem and the name of the
    /// type and then looking at all the constructors of the type and getting a result of each of
    /// them.
    fn split_on_defined_cons(self, ctx: &mut Ctx, name: &str, type_args: &[Type]) -> Witness {
        let type_sig = ctx
            .lookup_type(name)
            .unwrap_or_else(|| panic!("cannot find type '{name}'"));

        if let TypeValue::Sum(sum) = type_sig.value.clone() {
            let mut nodes = Vec::new();

            for constructor in sum {
                let problem = self.clone().specialize(
                    ctx,
                    type_args,
                    &constructor,
                    vec![wildcard(); constructor.args.len()],
                );

                let witness = problem.exhaustiveness(ctx);

                if witness.is_non_exhaustive() {
                    return witness.expand(&constructor);
                } else if let Witness::Ok(tree) = witness {
                    nodes.push((constructor.name.clone(), tree));
                }
            }

            Witness::Ok(CaseTree::Node(nodes))
        } else {
            todo!("handle other cases")
        }
    }

    /// Returns the first type of the problem.
    fn current_type(&self) -> Type {
        self.typ.first().cloned().unwrap()
    }

    /// Returns the type signature of the first type of the problem.
    fn current_type_signature<'a, 'b: 'a>(&self, ctx: &mut Ctx) -> Option<TypeSignature> {
        let ty = self.current_type();
        match &*ty.flatten() {
            MonoType::Application(n, _) => ctx.lookup_type(n).cloned(),
            MonoType::Var(n) => ctx.lookup_type(n).cloned(),
            _ => None,
        }
    }

    /// Creates a new problem where the first type is removed and the case is updated to match the
    /// first type. It's used when we need to specialize the tree to an identifier
    fn default_matrix(self, ctx: &mut Ctx) -> Self {
        Self {
            typ: self.typ[1..].to_vec(),
            case: self.case.pop_front(),
            matrix: self.matrix.default_matrix(ctx),
        }
    }

    /// Generates a constructor pattern for the given constructor name.
    fn synthesize_constructor(&self, ctx: &mut Ctx, name: &str) -> Pattern {
        let cons_sig = ctx.lookup_cons(name).unwrap();

        let args = vec![wildcard(); cons_sig.args.len()];

        Pattern {
            location: ByteRange::default(),
            data: PatternKind::Constructor(name.to_string(), args),
        }
    }

    /// Checks if all the rows are wildcard.
    fn is_all_wildcard(&self, ctx: &mut Ctx) -> bool {
        self.matrix.is_wildcard(ctx)
    }
    /// Checks if the problem is exhaustive for a wildcard. This is done by checking if the matrix
    /// is complete for the current type (it means that it contains all of the constructors as
    /// wildcards)
    fn exhaustiveness_wildcard(self, ctx: &mut Ctx) -> Witness {
        // TODO: Check if this case is actually true, I mean, if there's not type signature that we
        // can use then we can just return the default matrix and add an incompleteness and that is
        // probably the best thing to do.
        let Some(cur_ty) = self.current_type_signature(ctx) else {
            return self.specialize_wildcard(ctx)
        };

        let result = self.matrix.is_complete_type_sig(ctx, &cur_ty);

        // If the first column is wildcard, then we can just return the [Problem::default_matrix]
        // that specializes the tree for identifiers removing all constructor rows.
        if self.is_all_wildcard(ctx) {
            self.specialize_wildcard(ctx)
        } else {
            // If it's not all wildcard, then we need to check if the matrix is complete for the
            // current type. If it is, then we need to split the problem into multiple problems.
            match result {
                // If it's complete then we split for all of the possible constructors. It's done
                // to ensure that all of the possible matrices are checked.
                Completeness::Complete(_) => self.exhaustiveness_wildcard_split(ctx),
                // If it's incomplete and infinite, then we can just return the default_matrix with
                // a wildcard that represents all of the missing constructors.
                Completeness::Incomplete(Finitude::Infinite) => {
                    let witness = self.default_matrix(ctx).exhaustiveness(ctx);
                    witness.add_pattern(wildcard())
                }
                // If it's incomplete, then we need to synthesize a constructor pattern and add it
                // in order to generate a case that contains the missing constructor.
                Completeness::Incomplete(Finitude::Finite(constructors)) => {
                    let first = self.synthesize_constructor(ctx, constructors.first().unwrap());
                    // Removes the first column from the matrix because we now it's incomplete.
                    let witness = self.default_matrix(ctx).exhaustiveness(ctx);
                    witness.add_pattern(first)
                }
            }
        }
    }

    fn specialize_wildcard(self, ctx: &mut Ctx) -> Witness {
        let witness = self.default_matrix(ctx).exhaustiveness(ctx);
        witness.add_pattern(wildcard())
    }

    fn exhaustiveness_wildcard_split(self, ctx: &mut Ctx) -> Witness {
        match &*self.current_type().flatten() {
            MonoType::Application(name, type_args) => {
                self.split_on_defined_cons(ctx, name, type_args)
            }
            ty => todo!("cannot process right now {ty:?}"),
        }
    }

    /// Specializes using a constructor and a set of patterns of this constructor.
    fn exhaustiveness_cons(self, ctx: &mut Ctx, n: &str, p: Vec<Pattern>) -> Witness {
        match &*self.current_type().flatten() {
            MonoType::Application(_, type_args) => {
                let cons_sig = ctx.lookup_cons(n).unwrap();

                let specialized = self.specialize(ctx, type_args, &cons_sig, p);

                specialized.exhaustiveness(ctx)
            }
            ty => todo!("cannot process right now {ty:?}"),
        }
    }

    /// Checks if the problem is exhaustive on a case. This is the "entrypoint" forall of the
    /// exhaustiveness checking and it works by getting a [Problem] and checking some things about
    /// it's structure.
    ///
    /// 1. If the problem is empty, then it is non-exhaustive because a problem was
    /// specialized into a form that is not exhaustive. e.g
    ///
    /// ```md
    /// match some_value {}
    /// ```
    ///
    /// This code above is non exhaustive if the type of `some_value` is `Option<T>`. Because it
    /// lacks two cases, `Some` and `None`. So `is_empty` means that there are no cases that match
    /// this specific problem.
    ///
    /// 2. The problem is exhaustive if there are empty rows, It means that we specialized the tree
    /// so much that this row was not removed and it matches the problem case. e.g
    ///
    /// ```md
    /// match some_value {
    ///    Some(_) => {}
    ///    None => {}
    /// }
    /// ```
    ///
    /// That the corresponding problem is:
    ///
    /// ```    /// Type: T
    /// Case: _
    /// Rows:
    ///     | _
    ///
    /// Type: Option<T>
    /// Case: _
    /// Rows:
    ///     | (Some _)
    ///     | None
    /// ```
    ///
    /// Will be specialized into two [Problem]s, one for `Some` and other for `None`. The `Some`
    /// problem will have an empty row, because we specialized both rows for `Some` and then the
    /// first row is inlined resulting a problem like
    ///
    /// ```js
    /// Type: T
    /// Case: _
    /// Rows:
    ///     | _
    ///
    /// // That will get specialized into
    ///
    /// Type:
    /// Case:
    /// Rows:
    ///     |
    ///
    /// ```
    ///
    /// This is the exhaustive case, because we specialized the problem into a form that contains at
    /// least one empty row.
    ///
    /// 3. The incomplete matrix case that is the most common case. It means that we have a problem
    /// that is not empty and not exhaustive. e.g
    ///
    /// ```md
    /// Type: Option<T>
    /// Case: _
    /// Rows:
    ///     | (Some _)
    /// ```
    ///
    /// And we need to check if it's complete or not. To do that we need to analyze the case and how
    /// it's going to force the type to be specialized. In this case, the case is `_` that means
    /// that we need to check if the type is complete for the `_` case using the function
    /// [Problem::exhaustiveness_wildcard].
    pub fn exhaustiveness(mut self, ctx: &mut Ctx) -> Witness {
        // It's non exhaustive if the current problem is empty (it means that there are no rows that
        // matches the case)
        if self.is_empty() {
            Witness::NonExhaustive(self.case)
        } else if let Some(row) = self.get_exhaustive_row().and_then(|x| x.0) {
            Witness::Ok(CaseTree::Leaf(row))
        } else {
            // The first pattern of the case will guide the specialization of the whole problem.
            match self.case.first().clone().data {
                PatternKind::Atom(AtomKind::Identifier(n)) if ctx.lookup_cons(&n).is_none() => {
                    self.exhaustiveness_wildcard(ctx)
                }
                PatternKind::Atom(atom) => match atom {
                    AtomKind::Identifier(name) => self.exhaustiveness_cons(ctx, &name, vec![]),
                    AtomKind::Number(_) => todo!(),
                    AtomKind::Tuple(_) => todo!("should use specialize_tuple"),
                    AtomKind::Group(x) => {
                        self.case.1[0] = *x;
                        self.exhaustiveness(ctx)
                    }
                },
                PatternKind::Constructor(name, args) => self.exhaustiveness_cons(ctx, &name, args),
            }
        }
    }
}
