//! The context is primarily a list of bindings from variable names to type that is on the left side
//! of a type judgment.

use std::{cell::RefCell, fmt::Display, rc::Rc};

use atiny_error::{Error, ErrorBuilder};
use atiny_location::{ByteRange, Located, NodeId};
use atiny_tree::r#abstract::{Path, TypeDecl};
use itertools::Itertools;

use crate::{infer::Infer, program::Program};

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

#[derive(Clone, Debug)]
pub enum Imports {
    Star,
    Module,
    Items(Vec<String>),
}

impl Display for Imports {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Module => write!(f, "Module"),
            Self::Items(_) => write!(f, "Items(...)"),
        }
    }
}

#[derive(Clone)]
pub enum VariableKind {
    Function,
    Constructor,
    Local,
}

#[derive(Clone)]
pub struct Ctx {
    pub id: NodeId,
    pub program: Program,
    pub imports: Rc<RefCell<im_rc::HashMap<NodeId, Imports>>>,
    pub counter: Rc<RefCell<usize>>,
    pub map: im_rc::OrdMap<String, (VariableKind, Rc<TypeScheme>)>,
    pub typ_map: im_rc::OrdSet<String>,
    pub location: RefCell<ByteRange>,
    pub level: usize,
    pub signatures: Signatures,
}

impl Ctx {
    pub fn new(id: NodeId, program: Program) -> Self {
        let mut ctx = Self {
            id,
            program,
            imports: Default::default(),
            counter: Default::default(),
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

        let int = Type::typ("Int".to_string(), id);
        let sig = int.clone().arrow(int.clone().arrow(int, id), id).to_poly();

        ctx.map.extend([
            ("add".to_string(), (VariableKind::Function, sig.clone())),
            ("sub".to_string(), (VariableKind::Function, sig.clone())),
            ("mul".to_string(), (VariableKind::Function, sig.clone())),
            ("div".to_string(), (VariableKind::Function, sig)),
        ]);

        #[allow(clippy::let_unit_value)]
        {
            let cons = TypeDecl::unit().infer(&mut ctx);
            let _ = cons.infer(&mut ctx);
        }

        ctx
    }
}

impl Ctx {
    // Extends a context with a new binding.
    pub fn extend(&self, name: String, kind: VariableKind, typ: Rc<TypeScheme>) -> Self {
        Self {
            map: self.map.update(name, (kind, typ)),
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
    pub fn set_position(&self, location: ByteRange) {
        *self.location.borrow_mut() = location;
    }

    /// Creates a new name for a type variable.
    pub fn new_name(&self) -> String {
        let counter = {
            let mut counter = self.counter.borrow_mut();
            *counter += 1;
            *counter
        };

        format!("'{}", (97 + ((counter - 1) % 26)) as u8 as char)
    }

    pub fn generate_name(&self, name: &str) -> String {
        let counter = {
            let mut counter = self.counter.borrow_mut();
            *counter += 1;
            *counter
        };

        format!("{}{}", name, counter)
    }

    /// Looks up a type variable name in the context.
    pub fn lookup(&self, name: &str) -> Option<(VariableKind, Rc<TypeScheme>)> {
        self.map
            .get(name)
            .cloned()
            .or_else(|| {
                self.signatures.values.get(name).map(|decl| match decl {
                    DeclSignature::Function(fun) => {
                        (VariableKind::Function, fun.entire_type.clone())
                    }
                    DeclSignature::Constructor(decl) => {
                        (VariableKind::Constructor, decl.typ.clone())
                    }
                })
            })
            .or_else(|| self.lookup_on_imports(name, |ctx| ctx.lookup(name)))
    }

    pub fn lookup_cons(&self, name: &str) -> Option<Rc<ConstructorSignature>> {
        self.signatures
            .values
            .get(name)
            .and_then(|decl| match decl {
                DeclSignature::Function(_) => None,
                DeclSignature::Constructor(cons) => Some(cons.clone()),
            })
            .or_else(|| self.lookup_on_imports(name, |ctx| ctx.lookup_cons(name)))
    }

    pub fn lookup_type(&self, name: &str) -> Option<TypeSignature> {
        self.signatures
            .types
            .get(name)
            .cloned()
            .or_else(|| self.lookup_on_imports(name, |ctx| ctx.lookup_type(name)))
    }

    fn lookup_on_imports<T>(&self, name: &str, lookup: impl Fn(&Self) -> Option<T>) -> Option<T> {
        for (&id, import) in self.imports.borrow().iter() {
            match import {
                Imports::Items(items) => {
                    if items.iter().any(|i| name.eq(i.as_str())) {
                        return lookup(&self.get_ctx_by_id(id));
                    };
                }
                Imports::Star => match lookup(&self.get_ctx_by_id(id)) {
                    None => {}
                    some => return some,
                },
                Imports::Module => {}
            }
        }
        None
    }

    pub fn get_ctx_by_id(&self, id: NodeId) -> Self {
        if id == self.id {
            return self.clone();
        }

        self.program
            .get_ctx(id)
            .expect("ICE: context was not stored in the program")
    }

    pub fn ctx_from_path<R>(&self, path: &Path, f: impl FnMut(&mut Self) -> R) -> Option<R> {
        let Path(qualifier, item) = path;

        let Some(file) = self.get_file_from_qualifier(qualifier) else {
            return None;
        };

        match self.imports.borrow().get(&file.id) {
            Some(imports) => match imports {
                Imports::Items(i) if !i.contains(&item.data) => {
                    self.set_position(item.location);
                    self.error(format!(
                        "Module '{}' is not imported. Import it or the item '{}' directly",
                        qualifier, item.data
                    ));
                    None
                }

                imports => {
                    if matches!(imports, Imports::Star) {
                        //todo: Accept, but Lint that the use of a path is unecessary
                    }

                    self.program.update_ctx(self.clone());
                    self.program.get_infered_module(file.id, f)
                }
            },

            None => {
                self.set_position(qualifier.location().unwrap());
                self.error(format!("Invalid Path: {}", qualifier));
                None
            }
        }
    }

    pub fn path_map<R>(
        &mut self,
        path: &Path,
        mut f: impl FnMut(&mut Self) -> Option<R>,
    ) -> Option<R> {
        if path.0.is_empty() {
            f(self)
        } else {
            self.ctx_from_path(path, f).flatten()
        }
    }

    pub fn err_count(&self) -> usize {
        self.program.borrow().errors.len()
    }

    /// Creates a new hole type.
    pub fn new_hole(&self) -> Type {
        Type::new_hole(self.new_name(), self.level, self.id)
    }

    pub fn register_imports(&mut self, ctx: &Self, item: Option<Located<String>>) {
        let imports = match item {
            None => Imports::Module,

            Some(item) if item.data.eq("*") => Imports::Star,

            Some(item) if item.data.starts_with(|c: char| c.is_ascii_uppercase()) => {
                let mut names = Vec::new();

                match ctx.lookup_type(&item.data) {
                    Some(sig) => {
                        names.push(sig.name);
                        for name in sig.names {
                            names.push(name);
                        }
                    }

                    None => {
                        self.set_position(item.location);
                        self.error(format!("could not find Type '{}' import", item));
                        return;
                    }
                }

                Imports::Items(names)
            }

            Some(item) => Imports::Items(vec![item.data]),
        };

        self.update_imports(ctx.id, imports);
    }

    pub fn update_imports(&mut self, id: NodeId, new: Imports) {
        let mut imports = self.imports.borrow_mut();
        use Imports::*;

        match imports.get_mut(&id) {
            None => _ = imports.insert(id, new),

            Some(old) => match (old, new) {
                (Star, Star) => {}
                (Star, Items(_)) => {}
                (Module, Module) => {}

                (old @ Star, new @ Module)
                | (old @ Module, new @ Star)
                | (old @ Module, new @ Items(_))
                | (old @ Items(_), new @ Module) => self.error(format!(
                    "can not import as '{}' the same module that was already imported as '{}'",
                    new, old
                )),

                (Items(_), Star) => _ = imports.insert(id, Star),
                (Items(ref mut items), Items(names)) => items.extend(names),
            },
        }
    }
}

impl ErrorBuilder for Ctx {
    fn push_error(&self, error: Error) {
        self.program.borrow_mut().errors.push(error);
    }

    fn location(&self) -> ByteRange {
        *self.location.borrow()
    }
}

pub trait InferError<T> {
    fn new_error(&self, msg: String) -> T {
        self.error_message(msg);
        self.infer_error()
    }

    fn error_message(&self, msg: String);
    fn infer_error(&self) -> T;
}

impl InferError<Type> for Ctx {
    fn error_message(&self, msg: String) {
        self.error(msg);
    }

    fn infer_error(&self) -> Type {
        Type::new(MonoType::Error, self.id)
    }
}
