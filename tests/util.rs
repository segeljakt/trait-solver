#![allow(unused)]

use aqua::data::Expr;
use aqua::data::Name;
use aqua::data::Param;
use aqua::data::Program;
use aqua::data::Stmt;
use aqua::data::StmtDef;
use aqua::data::StmtEnum;
use aqua::data::StmtImpl;
use aqua::data::StmtStruct;
use aqua::data::StmtVar;
use aqua::data::Trait;
use aqua::data::Type;
use aqua::lexer::Span;

pub fn program<const N: usize>(ss: [Stmt; N]) -> Program {
    Program::new(ss.into_iter().collect())
}

// Create a Rule:
// impl<{gs}> {head} where {body}
pub fn stmt_impl<const N: usize, const M: usize, const K: usize>(
    gs: [&'static str; N],
    head: Trait,
    body: [Trait; M],
    defs: [StmtDef; K],
) -> Stmt {
    StmtImpl::new(
        Span::default(),
        gs.into_iter().map(Name::from).collect(),
        head,
        body.into_iter().collect(),
        defs.into_iter().collect(),
    )
    .into()
}

// Create a Fact:
// impl<{gs}> {head}
pub fn fact<const N: usize, const M: usize>(
    gs: [&'static str; N],
    head: Trait,
    defs: [StmtDef; M],
) -> StmtImpl {
    StmtImpl::new(
        Span::default(),
        gs.into_iter().map(Name::from).collect(),
        head,
        vec![],
        defs.into_iter().collect(),
    )
}

// Create a predicate:
// {t0}: {name}<{t1},..,{tn},{assoc}>
pub fn tr<const N: usize, const M: usize>(
    name: &'static str,
    types: [Type; N],
    assoc: [(&'static str, Type); M],
) -> Trait {
    Trait::new(
        Span::default(),
        Name::from(name),
        types.into_iter().collect(),
        assoc
            .into_iter()
            .map(|(s, t)| (Name::from(s), t.clone()))
            .collect(),
    )
}

pub fn t(name: &'static str) -> Type {
    Type::atom(name.to_string())
}

pub fn hole() -> Type {
    Type::Hole
}

pub fn tc<const N: usize>(name: &'static str, types: [Type; N]) -> Type {
    Type::cons(Name::from(name), types.into_iter().collect())
}

pub fn tv(name: &'static str) -> Type {
    Type::var(name.to_string())
}

pub fn type_assoc(i: Trait, x: &'static str) -> Type {
    Type::assoc(i, x)
}

pub fn expr_assoc(i: Trait, x: &'static str) -> Expr {
    Expr::Assoc(Span::default(), Type::Hole, i, Name::from(x))
}

pub fn stmt_var(x: &'static str, t: Type, e: Expr) -> Stmt {
    StmtVar::new(Span::default(), Name::from(x), t, e).into()
}

pub fn stmt_expr(e: Expr) -> Stmt {
    Stmt::Expr(e)
}

pub fn unit() -> Expr {
    Expr::Unit(Span::default(), Type::Hole)
}

pub fn expr_struct<const N: usize>(x: &'static str, xes: [(&'static str, Expr); N]) -> Expr {
    Expr::Struct(
        Span::default(),
        Type::Hole,
        Name::from(x),
        xes.into_iter().map(|(x, e)| (Name::from(x), e)).collect(),
    )
}

pub fn expr_tuple<const N: usize>(es: [Expr; N]) -> Expr {
    Expr::Tuple(Span::default(), Type::Hole, es.into_iter().collect())
}

pub fn field(e: Expr, x: &'static str) -> Expr {
    Expr::Field(Span::default(), Type::Hole, Box::new(e), Name::from(x))
}

pub fn variant(x0: &'static str, x1: &'static str, e: Expr) -> Expr {
    Expr::Enum(
        Span::default(),
        Type::Hole,
        Name::from(x0),
        Name::from(x1),
        Box::new(e),
    )
}

pub fn stmt_def<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Trait; M],
    ps: [(&'static str, Type); K],
    t: Type,
    e: Expr,
) -> Stmt {
    def(x, gs, qs, ps, t, e).into()
}

pub fn def<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Trait; M],
    ps: [(&'static str, Type); K],
    t: Type,
    e: Expr,
) -> StmtDef {
    StmtDef::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        qs.into_iter().collect(),
        ps.into_iter()
            .map(|(s, t)| Param::new(Span::default(), Name::from(s), t))
            .collect(),
        t,
        e,
    )
}

pub fn stmt_struct<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Trait; M],
    ps: [(&'static str, Type); K],
) -> Stmt {
    StmtStruct::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        qs.into_iter().collect(),
        ps.into_iter().map(|(s, t)| (Name::from(s), t)).collect(),
    )
    .into()
}

pub fn stmt_enum<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Trait; M],
    ps: [(&'static str, Type); K],
) -> Stmt {
    StmtEnum::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        qs.into_iter().collect(),
        ps.into_iter().map(|(s, t)| (Name::from(s), t)).collect(),
    )
    .into()
}

pub fn call<const N: usize>(e: Expr, es: [Expr; N]) -> Expr {
    Expr::Call(
        Span::default(),
        Type::Hole,
        Box::new(e),
        es.into_iter().collect(),
    )
}

pub fn binop(e0: Expr, op: &'static str, e1: Expr) -> Expr {
    Expr::Call(
        Span::default(),
        Type::Hole,
        Box::new(Expr::Var(Span::default(), Type::Hole, Name::from(op))),
        [e0, e1].into_iter().collect(),
    )
}

pub fn ev(x: &'static str) -> Expr {
    Expr::Var(Span::default(), Type::Hole, Name::from(x))
}

pub fn int(i: &'static str) -> Expr {
    Expr::Int(Span::default(), Type::Hole, i.to_owned())
}

pub fn float(f: &'static str) -> Expr {
    Expr::Float(Span::default(), Type::Hole, f.to_owned())
}

pub fn bool(b: bool) -> Expr {
    Expr::Bool(Span::default(), Type::Hole, b)
}

pub fn block<const N: usize>(ss: [Stmt; N], e: Expr) -> Expr {
    Expr::Block(
        Span::default(),
        Type::Hole,
        ss.into_iter().collect(),
        Box::new(e),
    )
}

pub fn expr_err() -> Expr {
    Expr::Err(Span::default(), Type::Hole)
}
