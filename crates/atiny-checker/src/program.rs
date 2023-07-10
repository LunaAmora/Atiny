use std::io;
use std::mem::take;
use std::{cell::RefCell, collections::HashMap, fs::read_to_string, path::PathBuf, rc::Rc};

use atiny_parser::io::{File, FileSystem, NodeId, VirtualFileSystem};
use atiny_parser::{Parser, ParserOutput, Parsers};
use atiny_tree::elaborated::FnBody;

use crate::context::Ctx;
use crate::types::Type;

pub type Prog = Rc<RefCell<Program>>;

pub struct Program {
    pub file_system: Box<dyn VirtualFileSystem<PathBuf, io::Error>>,
    pub entry_point: NodeId,
    pub modules: HashMap<NodeId, Option<Ctx>>,
    pub elaborated: HashMap<NodeId, Vec<FnBody<Type>>>,
    pub parser: Parsers,
}

impl Program {
    pub fn new(file: PathBuf) -> io::Result<Prog> {
        FileSystem::new_with(&file, |file| read_to_string(file).map(Rc::from))
            .map(|(file_system, entry_point)| Self {
                file_system: Box::new(file_system),
                entry_point,
                modules: HashMap::new(),
                elaborated: HashMap::new(),
                parser: Parsers::default(),
            })
            .map(|prog| Rc::new(RefCell::new(prog)))
    }

    pub fn return_ctx(&mut self, ctx: Ctx) {
        self.modules.insert(ctx.id, Some(ctx));
    }

    pub fn print_errors(&mut self) {
        for err in self.take_errors().iter().flatten() {
            eprintln!("{}", err);
        }
    }

    pub fn take_errors(&mut self) -> Option<Vec<String>> {
        self.modules
            .values_mut()
            .flatten()
            .map(|ctx| {
                ctx.take_errors().map(|errs| {
                    errs.into_iter()
                        .map(|err| {
                            (
                                err,
                                self.file_system.get_file(ctx.id).unwrap(),
                                self.file_system.get_node_full_name(ctx.id),
                            )
                        })
                        .map(|(err, file, name)| err.with_code(&file.code, &name).to_string())
                        .collect()
                })
            })
            .collect()
    }

    pub fn get_entry(program: Prog) -> File {
        let prog = program.borrow();
        prog.file_system.get_file(prog.entry_point).unwrap()
    }

    pub fn get_entry_ctx<Out>(program: Prog, f: impl FnMut(&mut Ctx, Out)) -> Ctx
    where
        Parsers: Parser<Out>,
        Out: ParserOutput,
    {
        let entry_point = Self::get_entry(program.clone());
        Self::get_ctx(program, entry_point, f)
    }

    pub fn get_ctx<Out>(program: Prog, file: File, mut f: impl FnMut(&mut Ctx, Out)) -> Ctx
    where
        Parsers: Parser<Out>,
        Out: ParserOutput,
    {
        {
            let mut prog = program.borrow_mut();
            match prog.modules.get_mut(&file.id) {
                Some(None) => panic!("ICE: context was not stored in the program"),
                Some(ctx) => return take(ctx).unwrap(),
                None => {}
            }
        }

        let (mut ctx, parsed) = Self::parse_file_as_ctx(program, file);
        if let Some(parsed) = parsed {
            f(&mut ctx, parsed);
        }
        ctx
    }

    fn parse_file_as_ctx<Out>(program: Prog, file: File) -> (Ctx, Option<Out>)
    where
        Parsers: Parser<Out>,
        Out: ParserOutput,
    {
        let parsed = {
            let mut prog = program.borrow_mut();

            let None = prog.modules.insert(file.id, None) else {
                panic!("ICE: attempted to parse the same file twice")
            };

            prog.parser.parse(&file.code)
        };

        let ctx = Ctx::new(file.id, program);

        let top = parsed.map_or_else(
            |err| {
                ctx.errors.borrow_mut().push(err);
                None
            },
            Some,
        );

        (ctx, top)
    }
}
