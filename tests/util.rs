#![allow(unused)]

use aqua::ast::Expr;
use aqua::ast::Index;
use aqua::ast::Name;
use aqua::ast::Param;
use aqua::ast::Program;
use aqua::ast::Stmt;
use aqua::ast::StmtDef;
use aqua::ast::StmtEnum;
use aqua::ast::StmtImpl;
use aqua::ast::StmtStruct;
use aqua::ast::StmtVar;
use aqua::ast::Trait;
use aqua::ast::Type;
use aqua::ast::Variant;
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

pub fn expr_assoc<const N: usize>(i: Trait, x: &'static str, ts: [Type; N]) -> Expr {
    Expr::Assoc(
        Span::default(),
        Type::Hole,
        i,
        Name::from(x),
        ts.into_iter().collect(),
    )
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

pub fn expr_array<const N: usize>(es: [Expr; N]) -> Expr {
    Expr::Array(Span::default(), Type::Hole, es.into_iter().collect())
}

pub fn expr_index(e1: Expr, i: Index) -> Expr {
    Expr::Index(Span::default(), Type::Hole, Box::new(e1), i)
}

pub fn index(i: &'static str) -> Index {
    Index::new(Span::default(), i.parse().unwrap())
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
    impl_def(x, gs, qs, ps, t, e).into()
}

pub fn impl_def<const N: usize, const M: usize, const K: usize>(
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

pub fn stmt_struct<const N: usize, const M: usize>(
    x: &'static str,
    gs: [&'static str; N],
    ps: [(&'static str, Type); M],
) -> Stmt {
    StmtStruct::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        ps.into_iter().map(|(s, t)| (Name::from(s), t)).collect(),
    )
    .into()
}

pub fn stmt_enum<const N: usize, const M: usize>(
    x: &'static str,
    gs: [&'static str; N],
    ps: [(&'static str, Type); M],
) -> Stmt {
    StmtEnum::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        ps.into_iter()
            .map(|(s, t)| Variant::new(Span::default(), Name::from(s), t))
            .collect(),
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

pub fn binop(tr: &'static str, op: &'static str, e0: Expr, e1: Expr) -> Expr {
    Expr::Call(
        Span::default(),
        Type::Hole,
        Box::new(Expr::Assoc(
            Span::default(),
            Type::Hole,
            Trait::new(Span::default(), Name::from(tr), vec![], vec![]),
            Name::from(op),
            vec![],
        )),
        [e0, e1].into_iter().collect(),
    )
}

pub fn add(e0: Expr, e1: Expr) -> Expr {
    binop("Add", "add", e0, e1)
}

pub fn sub(e0: Expr, e1: Expr) -> Expr {
    binop("Sub", "sub", e0, e1)
}

pub fn mul(e0: Expr, e1: Expr) -> Expr {
    binop("Mul", "mul", e0, e1)
}

pub fn div(e0: Expr, e1: Expr) -> Expr {
    binop("Div", "div", e0, e1)
}

pub fn eq(e0: Expr, e1: Expr) -> Expr {
    binop("PartialEq", "eq", e0, e1)
}

pub fn ne(e0: Expr, e1: Expr) -> Expr {
    binop("PartialEq", "ne", e0, e1)
}

pub fn lt(e0: Expr, e1: Expr) -> Expr {
    binop("PartialOrd", "lt", e0, e1)
}

pub fn le(e0: Expr, e1: Expr) -> Expr {
    binop("PartialOrd", "le", e0, e1)
}

pub fn gt(e0: Expr, e1: Expr) -> Expr {
    binop("PartialOrd", "gt", e0, e1)
}

pub fn ge(e0: Expr, e1: Expr) -> Expr {
    binop("PartialOrd", "ge", e0, e1)
}

pub fn and(e0: Expr, e1: Expr) -> Expr {
    binop("Logic", "and", e0, e1)
}

pub fn or(e0: Expr, e1: Expr) -> Expr {
    binop("Logic", "or", e0, e1)
}

pub fn get(e0: Expr, e1: Expr) -> Expr {
    binop("Index", "get", e0, e1)
}

pub fn var(x: &'static str) -> Expr {
    Expr::Var(Span::default(), Type::Hole, Name::from(x), vec![])
}

pub fn def<const N: usize>(x: &'static str, ts: [Type; N]) -> Expr {
    Expr::Var(
        Span::default(),
        Type::Hole,
        Name::from(x),
        ts.into_iter().collect(),
    )
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
