use std::{cell::RefCell, collections::HashMap, rc::Rc, fmt, hash::{Hasher, Hash}};

use super::context::Ctx;

#[derive(Debug)]
pub struct TypeScheme {
    pub names: Vec<String>,
    pub mono: Rc<MonoType>,
}

impl TypeScheme {
    pub fn instantiate(&self, ctx: Ctx) -> Rc<MonoType> {
        let substitutions = self
            .names
            .iter()
            .cloned()
            .map(|x| (x, MonoType::new_hole(ctx.new_name())))
            .collect::<HashMap<String, Rc<MonoType>>>();

        self.mono.substitute(&substitutions)
    }
}

impl fmt::Display for TypeScheme {
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

#[derive(Debug, Clone)]
pub enum Hole {
    Empty,
    Filled(Rc<MonoType>),
}

#[derive(Debug, Clone)]
pub struct RefItem {
    pub name: String,
    pub data: Hole,
}

impl RefItem {
    pub fn new(name: String) -> Self {
        Self {
            name,
            data: Hole::Empty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ref(Rc<RefCell<RefItem>>);

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        self.identifier() == other.identifier()
    }
}

impl Eq for Ref {}

impl Hash for Ref {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.identifier().hash(state);
    }
}

impl Ref {
    pub fn identifier(&self) -> u64 {
        &*self.0.as_ref().borrow() as *const _ as u64
    }

    pub fn new(name: String) -> Self {
        Self(Rc::new(RefCell::new(RefItem {
            name,
            data: Hole::Empty,
        })))
    }

    pub fn fill(&self, typ: Rc<MonoType>) {
        self.0.as_ref().borrow_mut().data = Hole::Filled(typ);
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.0.borrow().data, Hole::Empty)
    }

    pub fn is_filled(&self) -> bool {
        matches!(self.0.borrow().data, Hole::Filled(_))
    }

    pub fn get(&self) -> Hole {
        self.0.borrow().data.clone()
    }
}

#[derive(Debug, Clone)]
pub enum MonoType {
    Var(String),
    Arrow(Rc<MonoType>, Rc<MonoType>),
    Hole(Ref),
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(name) => write!(f, "{}", name),
            Self::Arrow(from, to) => write!(f, "({} -> {})", from, to),
            Self::Hole(item) => match item.get().clone() {
                Hole::Filled(typ) => write!(f, "{}", typ),
                Hole::Empty => write!(f, "^{}", item.0.borrow().name),
            },
        }
    }
}

impl MonoType {
    pub fn substitute(&self, substs: &HashMap<String, Rc<MonoType>>) -> Rc<Self> {
        match self {
            Self::Var(name) => substs
                .get(name)
                .cloned()
                .unwrap_or_else(|| Rc::new(Self::Var(name.clone()))),
            Self::Arrow(from, to) => {
                Rc::new(Self::Arrow(from.substitute(substs), to.substitute(substs)))
            }
            Self::Hole(item) => match item.get().clone() {
                Hole::Filled(typ) => typ.substitute(substs),
                Hole::Empty => Rc::new(Self::Hole(item.clone())),
            },
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

    pub fn arrow(from: Rc<Self>, to: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Arrow(from, to))
    }

    pub fn new_hole(name: String) -> Rc<Self> {
        Rc::new(Self::Hole(Ref::new(name)))
    }

    pub fn generalize_type(&self, ctx: Ctx, holes: &mut HashMap<Ref, String>) -> Rc<MonoType> {
        match self {
            Self::Var(_) => Rc::new(self.clone()),
            Self::Arrow(from, to) => Rc::new(Self::Arrow(
                from.generalize_type(ctx.clone(), holes),
                to.generalize_type(ctx, holes),
            )),
            Self::Hole(item) => match item.get().clone() {
                Hole::Filled(typ) => typ.generalize_type(ctx, holes),
                Hole::Empty => {
                    let name = holes.entry(item.clone()).or_insert_with(|| ctx.new_name());
                    MonoType::var(name.clone())
                }
            },
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