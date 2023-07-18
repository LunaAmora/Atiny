use std::io;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use std::{cell::RefCell, collections::HashMap, fs::read_to_string, path::PathBuf, rc::Rc};

use atiny_error::Error;
use atiny_location::NodeId;
use atiny_parser::atiny_fs::{File, FileSystem, VirtualFileSystem};
use atiny_parser::{Parser, Parsers};
use atiny_tree::elaborated::FnBody;

use crate::context::Ctx;
use crate::types::Type;

pub struct CtxGuard(Option<Ctx>, Program);

impl Deref for CtxGuard {
    type Target = Ctx;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl DerefMut for CtxGuard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

impl Drop for CtxGuard {
    fn drop(&mut self) {
        let Self(ctx, prog) = self;
        prog.update_ctx(std::mem::take(ctx).unwrap());
    }
}

#[derive(Clone)]
pub struct Program(Rc<RefCell<Prog>>);

impl Deref for Program {
    type Target = Rc<RefCell<Prog>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Program {
    pub fn new(file: PathBuf) -> io::Result<Self> {
        FileSystem::new_with(&file, |file| read_to_string(file).map(Rc::from))
            .map(|(file_system, entry_point)| Prog {
                file_system: Box::new(file_system),
                entry_point,
                modules: HashMap::new(),
                elaborated: HashMap::new(),
                parser: Parsers::default(),
                errors: Vec::new(),
            })
            .map(|prog| Self(Rc::new(RefCell::new(prog))))
    }

    pub fn print_errors(&self) {
        self.borrow_mut().print_errors();
    }

    pub fn take_errors(&self) -> Option<Vec<String>> {
        self.borrow_mut().take_errors()
    }

    pub fn update_ctx(&self, ctx: Ctx) {
        let prog = &mut self.borrow_mut();
        prog.modules.insert(ctx.id, Some(ctx));
    }

    pub fn get_ctx(&self, id: NodeId) -> Option<Ctx> {
        let prog = &mut self.borrow();
        prog.get_ctx(id)
    }

    pub fn get_ctx_mut(&self, id: NodeId) -> Option<CtxGuard> {
        self.get_ctx(id)
            .map(|ctx| CtxGuard(Some(ctx), self.clone()))
    }

    pub fn get_entry<Out, R>(&self, f: impl FnMut(&mut Ctx, Option<Out>) -> R) -> R
    where
        Parsers: Parser<Out>,
    {
        let entry_point = self.borrow().get_entry();
        self.get_module(entry_point, f)
    }

    pub fn get_module<Out, R>(&self, file: File, mut f: impl FnMut(&mut Ctx, Option<Out>) -> R) -> R
    where
        Parsers: Parser<Out>,
    {
        let (mut ctx, parsed) = self.take_or_parse(file);
        f(&mut ctx, parsed)
    }

    pub fn get_infered_module<R>(&self, id: NodeId, f: impl FnMut(&mut Ctx) -> R) -> Option<R> {
        self.get_ctx_mut(id).as_deref_mut().map(f)
    }

    fn take_or_parse<Out>(&self, file: File) -> (CtxGuard, Option<Out>)
    where
        Parsers: Parser<Out>,
    {
        if let Some(ctx) = self.get_ctx_mut(file.id) {
            return (ctx, None);
        }

        self.parse_file_as_ctx(file)
    }

    fn parse_file_as_ctx<Out>(&self, file: File) -> (CtxGuard, Option<Out>)
    where
        Parsers: Parser<Out>,
    {
        let parsed = { self.borrow_mut().parse_file(&file) };
        let ctx = Ctx::new(file.id, self.clone());
        self.update_ctx(ctx);

        let top = parsed.map_or_else(
            |err| {
                self.0.borrow_mut().errors.push(err);
                None
            },
            Some,
        );

        (self.get_ctx_mut(file.id).unwrap(), top)
    }
}

pub struct Prog {
    pub file_system: Box<dyn VirtualFileSystem<PathBuf, io::Error>>,
    pub elaborated: HashMap<NodeId, Vec<FnBody<Type>>>,
    pub errors: Vec<Error>,
    entry_point: NodeId,
    modules: HashMap<NodeId, Option<Ctx>>,
    parser: Parsers,
}

impl Prog {
    pub fn get_entry(&self) -> File {
        self.file_system.get_file(self.entry_point).unwrap()
    }

    fn get_ctx(&self, id: NodeId) -> Option<Ctx> {
        self.modules
            .get(&id)
            .map_or_else(|| None, |ctx| ctx.clone())
    }

    fn parse_file<Out>(&mut self, file: &File) -> Result<Out, Error>
    where
        Parsers: Parser<Out>,
    {
        let None = self.modules.insert(file.id, None) else {
            panic!("ICE: attempted to parse the same file twice")
        };

        self.parser.parse(file)
    }

    pub fn print_errors(&mut self) {
        if let Some(errs) = self.take_errors() {
            for err in errs {
                eprint!("{}", err);
            }
        }
    }

    pub fn take_errors(&mut self) -> Option<Vec<String>> {
        let result: Vec<String> = take(&mut self.errors)
            .into_iter()
            .map(|err| {
                let file = self.file_system.get_file(err.id()).unwrap();
                let name = self.file_system.get_node_full_name(err.id());
                err.with_code(&file.code, &name).to_string()
            })
            .collect();

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }
}
