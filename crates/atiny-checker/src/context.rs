use std::{cell::RefCell, rc::Rc};

use atiny_error::Error;
use atiny_location::ByteRange;

use super::{
    types::{MonoType, TypeScheme},
    util,
};

#[derive(Debug, Clone)]
pub struct Ctx<'a> {
    counter: Rc<RefCell<usize>>,
    pub map: im_rc::HashMap<String, Rc<TypeScheme>>,
    pub typ_map: im_rc::HashSet<String>,
    pub code: &'a str,
    pub location: ByteRange,
    pub level: usize,
}

impl<'a> Ctx<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            counter: Rc::new(RefCell::new(0)),
            map: Default::default(),
            typ_map: Default::default(),
            code,
            location: Default::default(),
            level: 0,
        }
    }

    pub fn extend(&self, name: String, typ: Rc<TypeScheme>) -> Self {
        Self {
            map: self.map.update(name, typ),
            ..self.clone()
        }
    }

    pub fn extend_type(&self, name: String) -> Self {
        Self {
            typ_map: self.typ_map.update(name),
            ..self.clone()
        }
    }

    pub fn extend_types(&self, names: &[String]) -> Self {
        Self {
            typ_map: self.typ_map.clone().union(names.iter().cloned().collect()),
            ..self.clone()
        }
    }

    pub fn level_up(&self) -> Self {
        Self {
            level: self.level + 1,
            ..self.clone()
        }
    }

    pub fn set_position(&self, location: ByteRange) -> Self {
        let mut c = self.clone();
        c.location = location;
        c
    }

    pub fn new_name(&self) -> String {
        *self.counter.borrow_mut() += 1;
        util::format_radix(*self.counter.borrow(), 26)
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<TypeScheme>> {
        self.map.get(name).cloned()
    }

    pub fn error<T>(&self, msg: String) -> Result<T, Error<'a>> {
        Err(Error::new(msg, self.location.locate(self.code), self.code))
    }

    pub fn new_hole(&self) -> Rc<MonoType> {
        MonoType::new_hole(self.new_name(), self.level)
    }
}
