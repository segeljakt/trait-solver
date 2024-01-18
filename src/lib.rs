#![allow(dead_code)]

use ast::Expr;
use ast::Pat;
use ast::Program;
use ast::Stmt;
use ast::StmtImpl;
use ast::Trait;
use ast::Type;
use diag::Report;
use diag::Sources;
use lexer::Lexer;
use parser::Parser;
use util::trim;

pub mod annotate;
pub mod apply;
pub mod ast;
pub mod diag;
pub mod display;
pub mod helper;
pub mod infer;
pub mod instantiate;
pub mod lexer;
pub mod parser;
pub mod resolve;
pub mod solver;
pub mod util;

pub struct Compiler<'a> {
    ctx0: resolve::Context,
    ctx1: infer::Context,
    sources: Sources<'a>,
    report: Report,
}

impl<'a> Drop for Compiler<'a> {
    fn drop(&mut self) {
        if !self.report.is_empty() {
            self.report.print(&mut self.sources).unwrap();
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self {
            ctx0: resolve::Context::new(),
            ctx1: infer::Context::new(),
            sources: Sources::new(),
            report: Report::new(),
        }
    }

    pub fn parse<T>(
        &mut self,
        name: &str,
        input: &'a str,
        f: impl Fn(&mut Parser<'a, &mut Lexer<'a>>) -> T,
    ) -> Result<T, (T, String)> {
        let id = self.sources.add(name, input);
        let mut lexer = Lexer::new(id, input);
        let mut parser = Parser::new(&mut lexer);
        let result = f(&mut parser);
        self.report.merge(&mut parser.report);
        self.report.merge(&mut lexer.report);
        if self.report.is_empty() {
            Ok(result)
        } else {
            Err((result, self.report()))
        }
    }

    pub fn resolve(&mut self, name: &str, input: &'a str) -> Result<Program, (Program, String)> {
        let program = self.parse(name, input, |parser| parser.parse()).unwrap();
        let result = program.resolve(&mut self.ctx0);
        self.report.merge(&mut self.ctx0.report);
        if self.report.is_empty() {
            Ok(result)
        } else {
            Err((result, self.report()))
        }
    }

    pub fn infer(&mut self, name: &str, input: &'a str) -> Result<Program, (Program, String)> {
        let program = self
            .parse(name, input, |parser| parser.parse())
            .unwrap()
            .resolve(&mut self.ctx0)
            .infer(&mut self.ctx1);
        self.report.merge(&mut self.ctx1.report);
        if self.report.is_empty() {
            Ok(program)
        } else {
            Err((program, self.report()))
        }
    }

    pub fn report(&mut self) -> String {
        trim(self.report.string(&mut self.sources).unwrap())
    }
}

impl Expr {
    pub fn parse(input: &str) -> Expr {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Expr, (Option<Expr>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.expr())
            .map(|e| e.unwrap())
    }
}

impl Type {
    pub fn parse(input: &str) -> Type {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Type, (Option<Type>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.ty())
            .map(|t| t.unwrap())
    }
}

impl Stmt {
    pub fn parse(input: &str) -> Stmt {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Stmt, (Option<Stmt>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.stmt())
            .map(|s| s.unwrap())
    }
}

impl Pat {
    pub fn parse(input: &str) -> Pat {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Pat, (Option<Pat>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.pat())
            .map(|p| p.unwrap())
    }
}

impl Trait {
    pub fn parse(input: &str) -> Trait {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Trait, (Option<Trait>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.tr())
            .map(|t| t.unwrap())
    }
}

impl StmtImpl {
    pub fn parse(input: &str) -> StmtImpl {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<StmtImpl, (Option<StmtImpl>, String)> {
        Compiler::new()
            .parse("test", input, |parser| parser.stmt_impl())
            .map(|i| i.unwrap())
    }
}

impl Program {
    pub fn parse(input: &str) -> Program {
        Self::try_parse(input).unwrap_or_else(|(_, s)| panic!("{}", s))
    }

    pub fn diagnose(s: &str) -> String {
        Self::try_parse(s).unwrap_err().1
    }

    pub fn try_parse(input: &str) -> Result<Program, (Program, String)> {
        Compiler::new().parse("test", input, |parser| parser.parse())
    }

    pub fn try_resolve(input: &str) -> Result<Program, (Program, String)> {
        Compiler::new().resolve("test", input)
    }

    pub fn try_infer(input: &str) -> Result<Program, (Program, String)> {
        Compiler::new().infer("test", input)
    }
}
