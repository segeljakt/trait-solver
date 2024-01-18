#![allow(unused)]

use crate::ast::Arm;
use crate::ast::Expr;
use crate::ast::Index;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Pat;
use crate::ast::Program;
use crate::ast::Query;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtDefDecl;
use crate::ast::StmtEnum;
use crate::ast::StmtImpl;
use crate::ast::StmtStruct;
use crate::ast::StmtTrait;
use crate::ast::StmtType;
use crate::ast::StmtTypeDecl;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;
use crate::lexer::Span;

pub fn program<const N: usize>(ss: [Stmt; N]) -> Program {
    Program::new(ss.into_iter().collect())
}

pub fn stmt_trait<const N: usize, const M: usize, const K: usize, const L: usize>(
    x: &'static str,
    gs: [&'static str; N],
    body: [Trait; M],
    defs: [StmtDefDecl; K],
    types: [StmtTypeDecl; L],
) -> Stmt {
    StmtTrait::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        body.into_iter().collect(),
        defs.into_iter().collect(),
        types.into_iter().collect(),
    )
    .into()
}

// Create a Rule:
// impl<{gs}> {head} where {body}
pub fn stmt_impl<const N: usize, const M: usize, const K: usize, const L: usize>(
    gs: [&'static str; N],
    head: Trait,
    body: [Trait; M],
    defs: [Stmt; K],
    types: [Stmt; L],
) -> Stmt {
    StmtImpl::new(
        Span::default(),
        gs.into_iter().map(Name::from).collect(),
        head,
        body.into_iter().collect(),
        defs.into_iter()
            .map(|s| {
                if let Stmt::Def(s) = s {
                    s
                } else {
                    unreachable!()
                }
            })
            .collect(),
        types
            .into_iter()
            .map(|s| {
                if let Stmt::Type(s) = s {
                    s
                } else {
                    unreachable!()
                }
            })
            .collect(),
    )
    .into()
}

// Create a Fact:
// impl<{gs}> {head}
pub fn fact<const N: usize, const M: usize, const K: usize>(
    gs: [&'static str; N],
    head: Trait,
    defs: [StmtDef; M],
    types: [StmtType; K],
) -> StmtImpl {
    StmtImpl::new(
        Span::default(),
        gs.into_iter().map(Name::from).collect(),
        head,
        vec![],
        defs.into_iter().collect(),
        types.into_iter().collect(),
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
    Type::Cons(Name::from(name), vec![])
}

pub fn type_err() -> Type {
    Type::Err
}

pub fn hole() -> Type {
    Type::Hole
}

pub fn tc<const N: usize>(name: &'static str, types: [Type; N]) -> Type {
    Type::Cons(Name::from(name), types.into_iter().collect())
}

pub fn ttuple<const N: usize>(ts: [Type; N]) -> Type {
    Type::Tuple(ts.into_iter().collect())
}

pub fn tfun<const N: usize>(ts: [Type; N], t: Type) -> Type {
    Type::Fun(ts.into_iter().collect(), Box::new(t))
}

pub fn tv(name: &'static str) -> Type {
    Type::Var(Name::from(name))
}

pub fn tg(name: &'static str) -> Type {
    Type::Gen(Name::from(name))
}

pub fn type_assoc(i: Trait, x: &'static str) -> Type {
    Type::Assoc(i, Name::from(x))
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

pub fn stmt_type<const N: usize>(x: &'static str, generics: [&'static str; N], t: Type) -> Stmt {
    StmtType::new(
        Span::default(),
        Name::from(x),
        generics.into_iter().map(Name::from).collect(),
        t,
    )
    .into()
}

pub fn stmt_expr(e: Expr) -> Stmt {
    Stmt::Expr(e)
}

pub fn expr_assign(e0: Expr, e1: Expr) -> Expr {
    Expr::Assign(Span::default(), Type::Hole, Box::new(e0), Box::new(e1))
}

pub fn unit() -> Expr {
    Expr::Tuple(Span::default(), Type::Hole, vec![])
}

pub fn expr_struct<const N: usize, const M: usize>(
    x: &'static str,
    ts: [Type; N],
    xes: [(&'static str, Expr); M],
) -> Expr {
    Expr::Struct(
        Span::default(),
        Type::Hole,
        Name::from(x),
        ts.into_iter().collect(),
        xes.into_iter().map(|(x, e)| (Name::from(x), e)).collect(),
    )
}

pub fn etuple<const N: usize>(es: [Expr; N]) -> Expr {
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

pub fn variant<const N: usize>(x0: &'static str, ts: [Type; N], x1: &'static str, e: Expr) -> Expr {
    Expr::Enum(
        Span::default(),
        Type::Hole,
        Name::from(x0),
        ts.into_iter().collect(),
        Name::from(x1),
        Box::new(e),
    )
}

pub fn stmt_def<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    ps: [(&'static str, Type); K],
    qs: [Trait; M],
    t: Type,
    e: Expr,
) -> Stmt {
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
    .into()
}

pub fn stmt_def_decl<const N: usize, const M: usize, const K: usize>(
    x: &'static str,
    gs: [&'static str; N],
    ps: [(&'static str, Type); K],
    qs: [Trait; M],
    t: Type,
) -> StmtDefDecl {
    StmtDefDecl::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
        qs.into_iter().collect(),
        ps.into_iter()
            .map(|(s, t)| Param::new(Span::default(), Name::from(s), t))
            .collect(),
        t,
    )
}

pub fn stmt_type_decl<const N: usize>(x: &'static str, gs: [&'static str; N]) -> StmtTypeDecl {
    StmtTypeDecl::new(
        Span::default(),
        Name::from(x),
        gs.into_iter().map(Name::from).collect(),
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

pub fn dcall<const N: usize, const M: usize>(
    x: &'static str,
    ts: [Type; N],
    es: [Expr; M],
) -> Expr {
    Expr::Call(
        Span::default(),
        Type::Hole,
        Box::new(Expr::Var(
            Span::default(),
            Type::Hole,
            Name::from(x),
            ts.into_iter().collect(),
        )),
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

pub fn unop(tr: &'static str, op: &'static str, e: Expr) -> Expr {
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
        [e].into_iter().collect(),
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
    Expr::And(Span::default(), Type::Hole, Box::new(e0), Box::new(e1))
}

pub fn or(e0: Expr, e1: Expr) -> Expr {
    Expr::Or(Span::default(), Type::Hole, Box::new(e0), Box::new(e1))
}

pub fn get(e0: Expr, e1: Expr) -> Expr {
    binop("Index", "get", e0, e1)
}

pub fn not(e: Expr) -> Expr {
    unop("Not", "not", e)
}

pub fn neg(e: Expr) -> Expr {
    unop("Neg", "neg", e)
}

pub fn var(x: &'static str) -> Expr {
    Expr::Var(Span::default(), Type::Hole, Name::from(x), vec![])
}

pub fn pv(x: &'static str) -> Pat {
    Pat::Var(Span::default(), Type::Hole, Name::from(x))
}

pub fn pat_int(i: &'static str) -> Pat {
    Pat::Int(Span::default(), Type::Hole, i.to_owned())
}

pub fn pat_string(s: &'static str) -> Pat {
    Pat::String(Span::default(), Type::Hole, s.to_owned())
}

pub fn pat_unit() -> Pat {
    Pat::Tuple(Span::default(), Type::Hole, vec![])
}

pub fn pat_bool(b: bool) -> Pat {
    Pat::Bool(Span::default(), Type::Hole, b)
}

pub fn pat_wild() -> Pat {
    Pat::Wildcard(Span::default(), Type::Hole)
}

pub fn pat_tuple<const N: usize>(ps: [Pat; N]) -> Pat {
    Pat::Tuple(Span::default(), Type::Hole, ps.into_iter().collect())
}

pub fn pat_enum<const N: usize>(x0: &'static str, ts: [Type; N], x1: &'static str, p: Pat) -> Pat {
    Pat::Enum(
        Span::default(),
        Type::Hole,
        Name::from(x0),
        ts.into_iter().collect(),
        Name::from(x1),
        Box::new(p),
    )
}

pub fn pat_struct<const N: usize, const M: usize>(
    x: &'static str,
    ts: [Type; N],
    xps: [(&'static str, Pat); M],
) -> Pat {
    Pat::Struct(
        Span::default(),
        Type::Hole,
        Name::from(x),
        ts.into_iter().collect(),
        xps.into_iter().map(|(x, p)| (Name::from(x), p)).collect(),
    )
}

pub fn pat_annot(t: Type, p: Pat) -> Pat {
    p.with(t)
}

pub fn expr_match<const N: usize>(e: Expr, arms: [(Pat, Expr); N]) -> Expr {
    Expr::Match(
        Span::default(),
        Type::Hole,
        Box::new(e),
        arms.into_iter().collect(),
    )
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

pub fn expr_bool(b: bool) -> Expr {
    Expr::Bool(Span::default(), Type::Hole, b)
}

pub fn string(s: &'static str) -> Expr {
    Expr::String(Span::default(), Type::Hole, s.to_owned())
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

pub fn expr_fun<const N: usize>(ps: [&'static str; N], e: Expr) -> Expr {
    Expr::Fun(
        Span::default(),
        Type::Hole,
        ps.into_iter()
            .map(|s| Param::new(Span::default(), Name::from(s), Type::Hole))
            .collect(),
        Type::Hole,
        Box::new(e),
    )
}

pub fn typed_expr_fun<const N: usize>(ps: [(&'static str, Type); N], t: Type, e: Expr) -> Expr {
    Expr::Fun(
        Span::default(),
        Type::Hole,
        ps.into_iter()
            .map(|(s, t)| Param::new(Span::default(), Name::from(s), t))
            .collect(),
        t,
        Box::new(e),
    )
}

pub fn expr_return(e: Expr) -> Expr {
    Expr::Return(Span::default(), Type::Hole, Box::new(e))
}

pub fn expr_continue() -> Expr {
    Expr::Continue(Span::default(), Type::Hole)
}

pub fn expr_break() -> Expr {
    Expr::Break(Span::default(), Type::Hole)
}

pub fn query<const N: usize>(qs: [Query; N]) -> Expr {
    Expr::Query(Span::default(), Type::Hole, qs.into_iter().collect())
}

pub fn query_select<const N: usize>(fields: [(&'static str, Expr); N]) -> Query {
    Query::Select(
        Span::default(),
        Type::Hole,
        fields
            .into_iter()
            .map(|(s, e)| (Name::from(s), e))
            .collect(),
    )
}

pub fn query_where(e: Expr) -> Query {
    Query::Where(Span::default(), Type::Hole, Box::new(e))
}

pub fn query_from<const N: usize>(xes: [(&'static str, Expr); N]) -> Query {
    Query::From(
        Span::default(),
        Type::Hole,
        xes.into_iter().map(|(s, e)| (Name::from(s), e)).collect(),
    )
}

pub fn query_into<const N: usize>(es: [Expr; N]) -> Query {
    Query::Into(Span::default(), Type::Hole, es.into_iter().collect())
}

pub fn query_with<const N: usize>(xes: [(&'static str, Expr); N]) -> Query {
    Query::With(
        Span::default(),
        Type::Hole,
        xes.into_iter().map(|(s, e)| (Name::from(s), e)).collect(),
    )
}

pub fn query_join<const N: usize>(xes: [(&'static str, Expr); N], e: Expr) -> Query {
    Query::Join(
        Span::default(),
        Type::Hole,
        xes.into_iter().map(|(s, e)| (Name::from(s), e)).collect(),
        Box::new(e),
    )
}

pub fn query_group<const N: usize, const M: usize>(xs: [&'static str; N], qs: [Query; M]) -> Query {
    Query::Group(
        Span::default(),
        Type::Hole,
        xs.into_iter().map(Name::from).collect(),
        qs.into_iter().collect(),
    )
}

pub fn query_over<const N: usize>(e: Expr, qs: [Query; N]) -> Query {
    Query::Over(
        Span::default(),
        Type::Hole,
        Box::new(e),
        qs.into_iter().collect(),
    )
}

pub fn query_compute<const N: usize>(aggs: [(&'static str, Expr, Expr); N]) -> Query {
    Query::Compute(
        Span::default(),
        Type::Hole,
        aggs.into_iter()
            .map(|(x, e0, e1)| (Name::from(x), e0, e1))
            .collect(),
    )
}

pub fn expr_while(e0: Expr, e1: Expr) -> Expr {
    Expr::While(Span::default(), Type::Hole, Box::new(e0), Box::new(e1))
}

pub fn gen() -> Span {
    Span::default()
}

pub fn trim(s: String) -> String {
    // Trim space right before \n on each line
    s.trim_end()
        .lines()
        .map(|line| line.trim_end().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}
