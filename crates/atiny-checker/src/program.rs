use std::io::Result;
use std::mem::take;
use std::{cell::RefCell, collections::HashMap, fs::read_to_string, path::PathBuf, rc::Rc};

use atiny_parser::io::{File, NodeId, VirtualFileSystem};
use atiny_parser::{error::from_lalrpop, ProgramParser};
use atiny_tree::elaborated::FnBody;
use atiny_tree::r#abstract::TopLevel;

use crate::context::Ctx;
use crate::types::Type;

pub type Prog = Rc<RefCell<Program>>;

pub struct Program {
    pub file_system: VirtualFileSystem<'static, PathBuf>,
    pub entry_point: NodeId,
    pub modules: HashMap<NodeId, Option<Ctx>>,
    pub elaborated: HashMap<NodeId, Vec<FnBody<Type>>>,
    parser: ProgramParser,
}

impl Program {
    pub fn new(file: PathBuf) -> Result<Prog> {
        VirtualFileSystem::new_with(&file, |file| read_to_string(file).map(Rc::from)).map(
            |(file_system, entry_point)| {
                Rc::new(
                    Self {
                        file_system,
                        entry_point,
                        modules: HashMap::new(),
                        elaborated: HashMap::new(),
                        parser: ProgramParser::new(),
                    }
                    .into(),
                )
            },
        )
    }

    pub fn return_ctx(&mut self, ctx: Ctx) {
        self.modules.insert(ctx.id, Some(ctx));
    }

    pub fn print_errors(&mut self) {
        for ctx in self.modules.values_mut().flatten() {
            ctx.take_errors()
                .map_or(Ok(()), Err)
                .unwrap_or_else(|errs| {
                    let file = self.file_system.get_file(ctx.id).unwrap();
                    let name = self.file_system.get_node_full_name(ctx.id);

                    for err in errs {
                        eprint!("{}", err.with_code(&file.code, &name));
                    }
                });
        }
    }

    pub fn get_entry_or_parse(program: Prog, f: impl Fn(&mut Ctx, Vec<TopLevel>)) -> Ctx {
        let entry_point = {
            let prog = program.borrow();
            prog.file_system.get_file(prog.entry_point).unwrap()
        };

        Self::get_ctx_or_parse(program, entry_point, f)
    }

    pub fn get_ctx_or_parse(program: Prog, file: File, f: impl Fn(&mut Ctx, Vec<TopLevel>)) -> Ctx {
        {
            let mut prog = program.borrow_mut();
            match prog.modules.get_mut(&file.id) {
                Some(None) => panic!("ICE: context was not stored in the program"),
                Some(ctx) => return take(ctx).unwrap(),
                None => {}
            }
        }

        let (mut ctx, parsed) = Self::parse_file(program, file);
        if let Some(parsed) = parsed {
            f(&mut ctx, parsed);
        }
        ctx
    }

    pub fn parse_file(program: Prog, file: File) -> (Ctx, Option<Vec<TopLevel>>) {
        let parsed = {
            let mut prog = program.borrow_mut();

            let None = prog.modules.insert(file.id, None) else {
                panic!("ICE: attempted to parse the same file twice")
            };

            prog.parser.parse(&file.code).map_err(from_lalrpop)
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
