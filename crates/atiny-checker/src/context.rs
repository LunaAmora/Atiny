//! The context is primarily a list of bindings from variable names to type that is on the left side
//! of a type judgment.

use std::{cell::RefCell, rc::Rc};

use atiny_error::Error;
use atiny_location::ByteRange;

use super::types::{MonoType, TypeScheme};

#[derive(Debug, Clone)]
pub struct Ctx {
    counter: Rc<RefCell<usize>>,
    pub map: im_rc::HashMap<String, Rc<TypeScheme>>,
    pub typ_map: im_rc::HashSet<String>,
    pub location: ByteRange,
    pub level: usize,
}

impl Default for Ctx {
    fn default() -> Self {
        Self {
            counter: Rc::new(RefCell::new(0)),
            map: Default::default(),
            typ_map: Default::default(),
            location: Default::default(),
            level: 0,
        }
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
    pub fn extend_types(&self, names: &[String]) -> Self {
        Self {
            typ_map: self.typ_map.clone().union(names.iter().cloned().collect()),
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
        self.map.get(name).cloned()
    }

    pub fn error<T>(&self, msg: String) -> Result<T, Error> {
        Err(Error::new(msg, self.location))
    }

    /// Creates a new hole type.
    pub fn new_hole(&self) -> Rc<MonoType> {
        MonoType::new_hole(self.new_name(), self.level)
    }
}
