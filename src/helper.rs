use crate::data::Param;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::StmtDef;
use crate::data::StmtEnum;
use crate::data::Expr;
use crate::data::StmtImpl;
use crate::data::StmtStruct;
use crate::data::StmtVar;
use crate::data::Trait;
use crate::data::Type;

pub fn program<const N: usize>(ss: [Stmt; N]) -> Program {
    Program::new(ss.iter().map(|s| s.clone()).collect())
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
        gs.iter().map(|s| s.to_string()).collect(),
        head,
        body.iter().map(|i| i.clone()).collect(),
        defs.iter().map(|d| d.clone()).collect(),
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
) -> Trait {
    Trait::new(
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

pub fn hole() -> Type {
    Type::Hole
}

pub fn tc<const N: usize>(name: &'static str, types: [Type; N]) -> Type {
    Type::cons(name.to_string(), types.iter().map(|t| t.clone()).collect())
}

pub fn tv(name: &'static str) -> Type {
    Type::var(name.to_string())
}

pub fn type_assoc(i: Trait, x: &'static str) -> Type {
    Type::assoc(i, x)
}

pub fn expr_assoc(i: Trait, x: &'static str) -> Expr {
    Expr::Assoc(Type::Hole, i, x.to_string())
}

pub fn stmt_var(x: &'static str, t: Type, e: Expr) -> Stmt {
    StmtVar::new(x.to_string(), t, e).into()
}

pub fn stmt_expr(e: Expr) -> Stmt {
    Stmt::Expr(e)
}

pub fn unit() -> Expr {
    Expr::Unit(Type::Hole)
}

pub fn expr_struct<const N: usize>(
    x: &'static str,
    xes: [(&'static str, Expr); N],
) -> Expr {
    Expr::Struct(
        Type::Hole,
        x.to_string(),
        xes.iter()
            .map(|(x, e)| (x.to_string(), e.clone()))
            .collect(),
    )
}

pub fn expr_tuple<const N: usize>(es: [Expr; N]) -> Expr {
    Expr::Tuple(Type::Hole, es.iter().map(|e| e.clone()).collect())
}

pub fn field(e: Expr, x: &'static str) -> Expr {
    Expr::Field(Type::Hole, e.into(), x.to_string())
}

pub fn variant(x0: &'static str, x1: &'static str, e: Expr) -> Expr {
    Expr::Enum(Type::Hole, x0.to_string(), x1.to_string(), Box::new(e))
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

pub fn stmt_struct<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    qs: [Trait; M],
    ps: [(&'static str, Type); K],
) -> Stmt {
    StmtStruct::new(
        x.to_string(),
        gs.iter().map(|s| s.to_string()).collect(),
        qs.iter().map(|q| q.clone()).collect(),
        ps.iter().map(|(s, t)| (s.to_string(), t.clone())).collect(),
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
        x.to_string(),
        gs.iter().map(|s| s.to_string()).collect(),
        qs.iter().map(|q| q.clone()).collect(),
        ps.iter().map(|(s, t)| (s.to_string(), t.clone())).collect(),
    )
    .into()
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
            Expr::String(_, v) => Expr::String(t, v),
            Expr::Unit(_) => Expr::Unit(t),
            Expr::Var(_, x) => Expr::Var(t, x),
            Expr::Call(_, e, es) => Expr::Call(t, e, es),
            Expr::Block(_, ss, e) => Expr::Block(t, ss, e),
            Expr::From(..) => todo!(),
            Expr::Struct(_, _, _) => todo!(),
            Expr::Enum(_, _, _, _) => todo!(),
            Expr::Field(_, _, _) => todo!(),
            Expr::Tuple(_, _) => todo!(),
            Expr::Assoc(_, _, _) => todo!(),
        }
    }
}

pub fn block<const N: usize>(ss: [Stmt; N], e: Expr) -> Expr {
    Expr::Block(Type::Hole, ss.iter().map(|s| s.clone()).collect(), e.into())
}
