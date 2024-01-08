use crate::data::Def;
use crate::data::Expr;
use crate::data::Impl;
use crate::data::Param;
use crate::data::Pred;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::Var;
use crate::parser::Parser;
use crate::token::Lexer;

pub fn program<const N: usize>(ss: [Stmt; N]) -> Program {
    Program::new(ss.iter().map(|s| s.clone()).collect())
}

// Create a Rule:
// impl<{gs}> {head} where {body}
pub fn imp<const N: usize, const M: usize, const K: usize>(
    gs: [&'static str; N],
    head: Pred,
    body: [Pred; M],
    defs: [Def; K],
) -> Impl {
    Impl::new(
        gs.iter().map(|s| s.to_string()).collect(),
        head,
        body.iter().map(|i| i.clone()).collect(),
        defs.iter().map(|d| d.clone()).collect(),
    )
}

// Create a Fact:
// impl<{gs}> {head}
pub fn fact<const N: usize, const M: usize>(
    gs: [&'static str; N],
    head: Pred,
    defs: [Def; M],
) -> Impl {
    Impl::new(
        gs.iter().map(|s| s.to_string()).collect(),
        head,
        vec![],
        defs.iter().map(|d| d.clone()).collect(),
    )
}

// Create a predicate:
// {t0}: {name}<{t1},..,{tn},{assoc}>
pub fn p<const N: usize, const M: usize>(
    name: &'static str,
    types: [Type; N],
    assoc: [(&'static str, Type); M],
) -> Pred {
    Pred::new(
        name.to_string(),
        types.iter().map(|t| t.clone()).collect(),
        assoc
            .iter()
            .map(|(s, t)| (s.to_string(), t.clone()))
            .collect(),
    )
}

pub fn t(name: &'static str) -> Type {
    Type::atom(name.to_string())
}

pub fn tc<const N: usize>(name: &'static str, types: [Type; N]) -> Type {
    Type::cons(name.to_string(), types.iter().map(|t| t.clone()).collect())
}

pub fn tv(name: &'static str) -> Type {
    Type::var(name.to_string())
}

pub fn ta(i: Pred, x: &'static str) -> Type {
    Type::assoc(i, x)
}

pub fn var(x: &'static str, t: Type, e: Expr) -> Var {
    Var::new(x.to_string(), t, e)
}

pub fn def<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Pred; M],
    ps: [(&'static str, Type); K],
    t: Type,
    e: Expr,
) -> Def {
    Def::new(
        x.to_string(),
        gs.iter().map(|s| s.to_string()).collect(),
        qs.iter().map(|q| q.clone()).collect(),
        ps.iter()
            .map(|(s, t)| Param::new(s.to_string(), t.clone()))
            .collect(),
        t,
        e,
    )
}

pub fn call<const N: usize>(e: Expr, es: [Expr; N]) -> Expr {
    Expr::Call(Type::Hole, e.into(), es.iter().map(|e| e.clone()).collect())
}

pub fn binop(e0: Expr, op: &'static str, e1: Expr) -> Expr {
    Expr::Call(
        Type::Hole,
        Expr::Var(Type::Hole, op.to_string()).into(),
        [e0, e1].iter().map(|e| e.clone()).collect(),
    )
}

pub fn ev(x: &'static str) -> Expr {
    Expr::Var(Type::Hole, x.to_string())
}

pub fn int(i: &'static str) -> Expr {
    Expr::Int(Type::Hole, i.to_owned())
}

pub fn float(f: &'static str) -> Expr {
    Expr::Float(Type::Hole, f.to_owned())
}

pub fn bool(b: bool) -> Expr {
    Expr::Bool(Type::Hole, b)
}

impl Expr {
    pub fn with(self, t: Type) -> Expr {
        match self {
            Expr::Int(_, v) => Expr::Int(t, v),
            Expr::Float(_, v) => Expr::Float(t, v),
            Expr::Bool(_, v) => Expr::Bool(t, v),
            Expr::Var(_, x) => Expr::Var(t, x),
            Expr::Call(_, e, es) => Expr::Call(t, e, es),
            Expr::Block(_, ss, e) => Expr::Block(t, ss, e),
            Expr::From(..) => todo!(),
        }
    }
}

pub fn block<const N: usize>(ss: [Stmt; N], e: Expr) -> Expr {
    Expr::Block(Type::Hole, ss.iter().map(|s| s.clone()).collect(), e.into())
}

pub fn stmt_expr(e: Expr) -> Stmt {
    Stmt::Expr(e)
}

impl Into<Stmt> for Var {
    fn into(self) -> Stmt {
        Stmt::Var(self)
    }
}

impl Into<Stmt> for Def {
    fn into(self) -> Stmt {
        Stmt::Def(self)
    }
}

impl Into<Stmt> for Impl {
    fn into(self) -> Stmt {
        Stmt::Impl(self)
    }
}

impl Into<Stmt> for Expr {
    fn into(self) -> Stmt {
        Stmt::Expr(self)
    }
}

impl Type {
    pub fn parse(s: &str) -> Type {
        Parser::new(Lexer::new(s)).ty().unwrap()
    }
}

impl Expr {
    pub fn parse(s: &str) -> Expr {
        Parser::new(Lexer::new(s)).expr().unwrap()
    }
}

impl Stmt {
    pub fn parse(s: &str) -> Stmt {
        Parser::new(Lexer::new(s)).stmt().unwrap()
    }
}

impl Def {
    pub fn parse(s: &str) -> Def {
        Parser::new(Lexer::new(s)).def().unwrap()
    }
}

impl Impl {
    pub fn parse(s: &str) -> Impl {
        Parser::new(Lexer::new(s)).imp().unwrap()
    }
}

impl Pred {
    pub fn parse(s: &str) -> Pred {
        Parser::new(Lexer::new(s)).pred().unwrap()
    }
}
