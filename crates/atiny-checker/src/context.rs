//! The context is primarily a list of bindings from variable names to type that is on the left side
//! of a type judgment.

use std::{cell::RefCell, fmt::Display, iter, rc::Rc};

use atiny_error::Error;
use atiny_location::ByteRange;
use atiny_tree::r#abstract::TypeDecl;
use itertools::Itertools;

use super::types::*;

#[derive(Clone, Default, Debug)]
pub struct Signatures {
    pub types: im_rc::OrdMap<String, TypeSignature>,
    pub values: im_rc::OrdMap<String, DeclSignature>,
    pub fields: im_rc::OrdMap<String, im_rc::OrdSet<String>>,
}

impl Display for Signatures {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Types:\n    {}", self.types.values().join("\n    "))?;
        writeln!(f, "\nValues:\n    {}", self.values.values().join("\n    "))
    }
}

#[derive(Clone)]
pub struct Ctx {
    counter: Rc<RefCell<usize>>,
    pub errors: Rc<RefCell<Vec<Error>>>,
    pub map: im_rc::OrdMap<String, Rc<TypeScheme>>,
    pub typ_map: im_rc::OrdSet<String>,
    pub location: ByteRange,
    pub level: usize,
    pub signatures: Signatures,
}

impl Default for Ctx {
    fn default() -> Self {
        let mut ctx = Self {
            counter: Default::default(),
            errors: Default::default(),
            map: Default::default(),
            typ_map: Default::default(),
            location: Default::default(),
            level: Default::default(),
            signatures: Default::default(),
        };

        ctx.signatures.types.insert(
            "Int".to_string(),
            TypeSignature::new_opaque("Int".to_string()),
        );

        let int = MonoType::typ("Int".to_string());
        let sig = MonoType::arrow(int.clone(), MonoType::arrow(int.clone(), int)).to_poly();

        ctx.map.extend([
            ("add".to_string(), sig.clone()),
            ("sub".to_string(), sig.clone()),
            ("mul".to_string(), sig.clone()),
            ("div".to_string(), sig),
        ]);

        ctx.extend_type_sigs(iter::once(TypeDecl::unit()));

        ctx
    }
}

impl Ctx {
    // Extends a context with a new binding.
    pub fn extend(&self, name: String, typ: Rc<TypeScheme>) -> Self {
        Self {
            map: self.map.update(name, typ),
            ..self.clone()
        }
    }

    /// Extends a context with a type name (for type variables).
    pub fn extend_type(&self, name: String) -> Self {
        Self {
            typ_map: self.typ_map.update(name),
            ..self.clone()
        }
    }

    /// Extends a context with a list of type names (for type variables).
    pub fn extend_types<'a, N: IntoIterator<Item = &'a String>>(&self, names: N) -> Self {
        Self {
            typ_map: self.typ_map.clone().union(names.into_iter().collect()),
            ..self.clone()
        }
    }

    /// Increase the level of the context, so it measures where we are inside the forall bindings.
    /// It only increases during let values because let values are always generalized so it counts
    /// as if we were inside a forall.
    pub fn level_up(&self) -> Self {
        Self {
            level: self.level + 1,
            ..self.clone()
        }
    }

    /// Sets the current location that we are type checking inside of the context.
    pub fn set_position(&self, location: ByteRange) -> Self {
        let mut c = self.clone();
        c.location = location;
        c
    }

    /// Creates a new name for a type variable.
    pub fn new_name(&self) -> String {
        fn format_radix(mut x: usize, radix: usize) -> String {
            let mut result = vec![];
            loop {
                let m = x % radix;
                x /= radix;
                result.push(std::char::from_u32((m + 96) as u32).unwrap());
                if x == 0 {
                    break;
                }
            }
            format!("'{}", result.into_iter().rev().collect::<String>())
        }

        *self.counter.borrow_mut() += 1;
        format_radix(*self.counter.borrow(), 26)
    }

    /// Looks up a type variable name in the context.
    pub fn lookup(&self, name: &str) -> Option<Rc<TypeScheme>> {
        self.map.get(name).cloned().or_else(|| {
            self.signatures.values.get(name).map(|decl| match decl {
                DeclSignature::Function(fun) => fun.entire_type.clone(),
                DeclSignature::Constructor(decl) => decl.typ.clone(),
            })
        })
    }

    pub fn lookup_cons(&self, name: &str) -> Option<Rc<ConstructorSignature>> {
        self.signatures
            .values
            .get(name)
            .and_then(|decl| match decl {
                DeclSignature::Function(_) => None,
                DeclSignature::Constructor(cons) => Some(cons.clone()),
            })
    }

    pub fn lookup_type(&self, name: &str) -> Option<&TypeSignature> {
        self.signatures.types.get(name)
    }

    pub fn error(&self, msg: String) {
        self.errors
            .borrow_mut()
            .push(Error::new(msg, self.location));
    }

    pub fn suggestion(&self, msg: String) {
        self.errors
            .borrow_mut()
            .push(Error::new_sugestion(msg, self.location));
    }

    pub fn dyn_error(&self, msg: impl Display + 'static) {
        self.errors
            .borrow_mut()
            .push(Error::new_dyn(msg, self.location));
    }

    pub fn take_errors(&self) -> Option<Vec<Error>> {
        let is_not_empty = { !self.errors.borrow().is_empty() };

        is_not_empty.then_some({
            let mut result = vec![];
            result.append(&mut self.errors.borrow_mut());
            result
        })
    }

    pub fn err_count(&self) -> usize {
        self.errors.borrow().len()
    }

    /// Creates a new hole type.
    pub fn new_hole(&self) -> Type {
        MonoType::new_hole(self.new_name(), self.level)
    }
}

pub trait InferError<T> {
    fn new_error(&self, msg: String) -> T;
}

impl InferError<Type> for Ctx {
    fn new_error(&self, msg: String) -> Type {
        self.error(msg);
        Rc::new(MonoType::Error)
    }
}
