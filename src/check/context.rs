use std::{cell::RefCell, rc::Rc};

use crate::{
    error::Error,
    syntax::{Byte, ByteRange},
};

use super::{
    types::{MonoType, TypeScheme},
    util,
};

#[derive(Debug, Clone)]
pub struct Ctx<'a> {
    counter: Rc<RefCell<usize>>,
    pub map: im_rc::HashMap<String, Rc<TypeScheme>>,
    pub code: &'a str,
    pub location: ByteRange,
}

impl<'a> Ctx<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            counter: Rc::new(RefCell::new(0)),
            map: Default::default(),
            code,
            location: ByteRange(Byte(0), Byte(0)),
        }
    }

    pub fn extend(&self, name: String, typ: Rc<TypeScheme>) -> Self {
        Self {
            counter: self.counter.clone(),
            map: self.map.update(name, typ),
            code: self.code,
            location: self.location,
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
        MonoType::new_hole(self.new_name())
    }
}
