//! Types and functions that describes the hindley-milner type system extended with higher kinded
//! types and type constraints.
//!
use std::{
    cell::{RefCell, RefMut},
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{self, Display},
    hash::{Hash, Hasher},
    ptr::addr_of,
    rc::Rc,
};

use itertools::Itertools;

use super::context::Ctx;

pub type Type = Rc<MonoType>;

/// A type scheme is a prenex polymorphic construction that is used to express value dependency on
/// types. E.g.
///
/// ```haskell
/// forall a. a -> a
/// ```
#[derive(Debug)]
pub struct TypeScheme {
    pub names: Vec<String>,
    pub mono: Type,
}

impl TypeScheme {
    pub fn instantiate(&self, ctx: Ctx) -> Type {
        let mut types = Vec::new();

        for _ in &self.names {
            types.push(MonoType::new_hole(ctx.new_name(), ctx.level));
        }

        self.instantiate_with(&types)
    }

    pub fn instantiate_with(&self, types: &[Type]) -> Type {
        let substitutions = self
            .names
            .iter()
            .cloned()
            .zip(types.iter().cloned())
            .collect::<HashMap<String, Type>>();

        self.mono.substitute(&substitutions)
    }
}

impl Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "forall ")?;

        for (i, name) in self.names.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", name)?;
        }

        write!(f, ". {}", self.mono)
    }
}

/// An empty or filled structure that is used for types that are about to find a value and is shared
/// between multiple places.
#[derive(Debug, Clone)]
pub enum Hole {
    Empty(usize),
    Filled(Type),
}

impl Display for Hole {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty(n) => write!(f, "Hole<{n}>"),
            Self::Filled(a) => write!(f, "Filled[{}]", a),
        }
    }
}

/// A hole with a name.
#[derive(Debug, Clone)]
pub struct RefItem {
    pub name: String,
    pub data: Hole,
}

impl RefItem {
    pub fn new(name: String, level: usize) -> Self {
        Self {
            name,
            data: Hole::Empty(level),
        }
    }
}

/// A shared mutable reference to a hole.
#[derive(Debug, Clone)]
pub struct Ref(Rc<RefCell<RefItem>>);

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        self.identifier() == other.identifier()
    }
}

impl Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let item = self.0.borrow();
        write!(f, "ref {{{}, {}}}", item.name, item.data)
    }
}

impl Eq for Ref {}

impl Ord for Ref {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.identifier().cmp(&other.identifier())
    }
}

impl PartialOrd for Ref {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.identifier().partial_cmp(&other.identifier())
    }
}

impl Hash for Ref {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.identifier().hash(state);
    }
}

impl Ref {
    pub fn identifier(&self) -> u64 {
        addr_of!(*self.0.as_ref().borrow()) as u64
    }

    pub fn new(name: String, level: usize) -> Self {
        Self(Rc::new(RefCell::new(RefItem {
            name,
            data: Hole::Empty(level),
        })))
    }

    pub fn fill(&self, typ: Type) {
        self.0.as_ref().borrow_mut().data = Hole::Filled(typ);
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.0.borrow().data, Hole::Empty(_))
    }

    pub fn is_filled(&self) -> bool {
        matches!(self.0.borrow().data, Hole::Filled(_))
    }

    pub fn get(&self) -> Hole {
        self.0.borrow().data.clone()
    }

    pub fn get_item_mut(&self) -> RefMut<RefItem> {
        self.0.as_ref().borrow_mut()
    }
}

/// A type that is not generalized but can contain arrow types, tuples and holes as part of it.
#[derive(Debug, Clone)]
pub enum MonoType {
    Var(String),
    Tuple(Vec<Type>),
    Arrow(Type, Type),
    Hole(Ref),
    Application(String, Vec<Type>),
    Error,
}

impl MonoType {
    pub fn iter(self: Type) -> Iter {
        Iter { typ: self }
    }

    pub fn flatten(self: Type) -> Type {
        match &*self {
            Self::Hole(hole) => match hole.get() {
                Hole::Filled(f) => f.flatten(),
                Hole::Empty(_) => self,
            },
            _ => self,
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(name) => write!(f, "{}", name),
            Self::Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Self::Arrow(from, to) => write!(f, "({} -> {})", from, to),
            Self::Hole(item) => match item.get() {
                Hole::Filled(typ) => write!(f, "{}", typ),
                Hole::Empty(0) => write!(f, "^{}", item.0.borrow().name),
                Hole::Empty(lvl) => write!(f, "^{lvl}~{}", item.0.borrow().name),
            },
            Self::Application(name, args) if args.is_empty() => write!(f, "{}", name),
            Self::Application(name, args) => write!(f, "({} {})", name, args.iter().join(" ")),
            Self::Error => write!(f, "ERROR"),
        }
    }
}

impl MonoType {
    pub fn substitute(&self, substs: &HashMap<String, Rc<Self>>) -> Rc<Self> {
        match self {
            Self::Var(name) => substs
                .get(name)
                .cloned()
                .unwrap_or_else(|| Rc::new(Self::Var(name.clone()))),

            Self::Tuple(vec) => Rc::new(Self::Tuple(
                vec.iter().map(|mono| mono.substitute(substs)).collect(),
            )),

            Self::Arrow(from, to) => {
                Rc::new(Self::Arrow(from.substitute(substs), to.substitute(substs)))
            }

            Self::Hole(item) => match item.get() {
                Hole::Filled(typ) => typ.substitute(substs),
                Hole::Empty(_) => Rc::new(Self::Hole(item.clone())),
            },

            Self::Application(string, args) => Rc::new(Self::Application(
                string.clone(),
                args.iter().map(|a| a.substitute(substs)).collect(),
            )),

            Self::Error => Rc::new(Self::Error),
        }
    }
}

impl MonoType {
    pub fn to_poly(&self) -> Rc<TypeScheme> {
        Rc::new(TypeScheme {
            names: vec![],
            mono: Rc::new(self.clone()),
        })
    }

    pub fn var(name: String) -> Rc<Self> {
        Rc::new(Self::Var(name))
    }

    pub fn typ(name: String) -> Rc<Self> {
        Rc::new(Self::Application(name, vec![]))
    }

    pub fn arrow(from: Rc<Self>, to: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Arrow(from, to))
    }

    pub fn new_hole(name: String, level: usize) -> Rc<Self> {
        Rc::new(Self::Hole(Ref::new(name, level)))
    }

    fn generalize_type(&self, ctx: Ctx, holes: &mut BTreeMap<Ref, String>) -> Rc<Self> {
        match self {
            Self::Var(_) => Rc::new(self.clone()),

            Self::Tuple(vec) => Rc::new(Self::Tuple(
                vec.iter()
                    .map(|mono| mono.generalize_type(ctx.clone(), holes))
                    .collect(),
            )),

            Self::Arrow(from, to) => Rc::new(Self::Arrow(
                from.generalize_type(ctx.clone(), holes),
                to.generalize_type(ctx, holes),
            )),

            Self::Hole(item) => match item.get() {
                Hole::Filled(typ) => typ.generalize_type(ctx, holes),
                Hole::Empty(lvl) if lvl > ctx.level => {
                    let name = holes.entry(item.clone()).or_insert_with(|| ctx.new_name());
                    Self::var(name.clone())
                }
                Hole::Empty(_) => Rc::new(Self::Hole(item.clone())),
            },

            Self::Application(fun, args) => Rc::new(Self::Application(
                fun.clone(),
                args.iter()
                    .map(|a| a.generalize_type(ctx.clone(), holes))
                    .collect(),
            )),

            Self::Error => Rc::new(Self::Error),
        }
    }

    pub fn generalize(&self, ctx: Ctx) -> Rc<TypeScheme> {
        let mut names = Default::default();

        let mono = self.generalize_type(ctx, &mut names);

        Rc::new(TypeScheme {
            names: names.into_values().collect(),
            mono,
        })
    }
}

/// Is the signature of a constructor of a type, as an example, the signature of the constructor of
/// the `Ok` constructor is `forall a b. a -> Result a b`.
#[derive(Clone, Debug)]
pub struct ConstructorSignature {
    pub name: String,
    pub args: Vec<Type>,
    pub typ: Rc<TypeScheme>,
}

impl Display for ConstructorSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} {}", self.name, self.typ)
    }
}

impl ConstructorSignature {
    pub fn new(name: String, names: Vec<String>, mono: Type, args: Vec<Type>) -> Self {
        Self {
            name,
            typ: TypeScheme { names, mono }.into(),
            args,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret: Type,
    pub entire_type: Rc<TypeScheme>,
    pub type_variables: Vec<String>,
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .args
            .iter()
            .map(|(n, t)| format!("({} : {})", n, t))
            .join(" ");

        write!(f, "(fn {} {} : {})", self.name, params, self.ret)
    }
}

/// Is the signature of a function, as an example, the signature of the `map` function or the
/// signature of a constructor like `Ok`.
#[derive(Clone, Debug)]
pub enum DeclSignature {
    Function(FunctionSignature),
    Constructor(Rc<ConstructorSignature>),
}

impl Display for DeclSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function(fs) => write!(f, "{}", fs),
            Self::Constructor(cs) => write!(f, "{}", cs),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeValue {
    Sum(Vec<Rc<ConstructorSignature>>),
    Opaque,
}

/// Type signature of a type e.g.
///
/// ```haskell
/// type Result a b = Ok a | Err b
/// ```
///
#[derive(Clone, Debug)]
pub struct TypeSignature {
    pub name: String,
    pub params: Vec<String>,
    pub value: TypeValue,
}

impl TypeSignature {
    pub fn get_constructors(&self) -> Option<HashSet<String>> {
        match &self.value {
            TypeValue::Sum(constructors) => Some(
                constructors
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<HashSet<_>>(),
            ),
            TypeValue::Opaque => None,
        }
    }
}

impl Display for TypeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sum(constructors) => {
                let constructors = constructors.iter().join("\n        ");
                write!(f, "{}", constructors)
            }
            Self::Opaque => write!(f, "opaque"),
        }
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.iter().map(|x| format!(" {x}")).join("");
        write!(f, "type {}{}", self.name, params)
    }
}

pub struct Iter {
    pub typ: Type,
}

impl Iterator for Iter {
    type Item = Type;

    fn next(&mut self) -> Option<Self::Item> {
        match &*self.typ.clone().flatten() {
            MonoType::Arrow(from, to) => {
                self.typ = to.clone();
                Some(from.clone())
            }
            _ => None,
        }
    }
}
