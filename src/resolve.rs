#![allow(unused)]
use std::fmt::Arguments;

use crate::ast::Expr;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Pat;
use crate::ast::Program;
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
use crate::diag::Report;

#[derive(Debug)]
pub struct Context {
    stack: Stack,
    pub report: Report,
}

impl Context {
    pub fn new() -> Context {
        let mut stack = Stack(vec![vec![]]);
        // Types
        stack.bind("i32", Binding::TypeBuiltin(0));
        stack.bind("f32", Binding::TypeBuiltin(0));
        stack.bind("i64", Binding::TypeBuiltin(0));
        stack.bind("bool", Binding::TypeBuiltin(0));
        stack.bind("Vec", Binding::TypeBuiltin(1));
        stack.bind("VecIterator", Binding::TypeBuiltin(1));
        stack.bind("Stream", Binding::TypeBuiltin(1));
        stack.bind("StreamIterator", Binding::TypeBuiltin(1));
        stack.bind("Option", Binding::TypeBuiltin(1));
        // Traits
        stack.bind("Iterator", Binding::Trait(1, vec![Name::from("Item")]));
        stack.bind(
            "IntoIterator",
            Binding::Trait(1, vec![Name::from("Item"), Name::from("IntoIter")]),
        );
        stack.bind("Add", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Sub", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Mul", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Div", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Eq", Binding::Trait(1, vec![]));
        stack.bind("Ord", Binding::Trait(1, vec![]));
        stack.bind("Clone", Binding::Trait(1, vec![]));
        stack.bind("Copy", Binding::Trait(1, vec![]));
        stack.bind("Display", Binding::Trait(1, vec![]));
        let diags = Report::new();
        Context {
            stack,
            report: diags,
        }
    }

    fn scoped<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.stack.0.push(vec![]);
        let t = f(self);
        self.stack.0.pop();
        t
    }

    fn not_found(&mut self, name: &Name, expected: &'static str) {
        self.report.err(
            name.span,
            format!("Name `{}` not found.", name),
            format!("Expected {}.", expected),
        );
    }

    fn unexpected(&mut self, name: &Name, found: Binding, expected: &'static str) {
        self.report.err(
            name.span,
            format!("Unexpected {} `{name}`.", found.name()),
            format!("Expected {expected}."),
        );
    }

    fn wrong_arity(&mut self, name: &Name, found: usize, expected: usize) {
        self.report.err(
            name.span,
            format!(
                "Wrong number of arguments. Found {}, expected {}",
                found, expected
            ),
            format!("Expected {} arguments.", expected),
        );
    }

    fn wrong_fields<T>(&mut self, name: &Name, found: &[(Name, T)], expected: &[Name]) {
        let found = names_to_string(found, |(x, _)| format!("{x}"));
        let expected = names_to_string(expected, |x| format!("{x}"));
        self.report.err(
            name.span,
            format!("Wrong fields provided. Found {{ {found} }}, expected {{ {expected} }}",),
            format!("Expected {{ {expected} }} fields."),
        );
    }

    fn wrong_variant<T>(&mut self, name: &Name, found: &(Name, T), expected: &[Name]) {
        let found = &found.0;
        let expected = names_to_string(expected, |x| format!("{x}"));
        self.report.err(
            name.span,
            format!("Wrong variant provided. Found {found}, expected {expected}",),
            format!("Expected one of {{ {expected} }} variants."),
        );
    }
}

fn names_to_string<T>(names: &[T], f: impl FnMut(&T) -> String) -> String {
    names.iter().map(f).collect::<Vec<_>>().join(", ")
}

impl std::ops::Deref for Context {
    type Target = Stack;
    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

impl std::ops::DerefMut for Context {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}

#[derive(Debug)]
pub struct Stack(Vec<Vec<(Name, Binding)>>);

impl Stack {
    fn bind(&mut self, name: impl Into<Name>, binding: Binding) {
        self.0.last_mut().unwrap().push((name.into(), binding));
    }

    fn get(&self, x: &Name) -> Option<&Binding> {
        self.0.iter().rev().find_map(|s| {
            s.iter()
                .rev()
                .find_map(|(y, b)| if y == x { Some(b) } else { None })
        })
    }
}

impl Program {
    pub fn resolve(&self, ctx: &mut Context) -> Program {
        let stmts = self
            .stmts
            .iter()
            .flat_map(|stmt| stmt.resolve(ctx))
            .collect();
        Program::new(stmts)
    }
}

impl Stmt {
    pub fn resolve(&self, ctx: &mut Context) -> Option<Stmt> {
        match self {
            Stmt::Var(s) => Some(Stmt::Var(s.resolve(ctx))),
            Stmt::Def(s) => Some(Stmt::Def(s.resolve(ctx))),
            Stmt::Trait(s) => Some(Stmt::Trait(s.resolve(ctx))),
            Stmt::Impl(s) => Some(Stmt::Impl(s.resolve(ctx)?)),
            Stmt::Struct(s) => Some(Stmt::Struct(s.resolve(ctx))),
            Stmt::Enum(s) => Some(Stmt::Enum(s.resolve(ctx))),
            Stmt::Type(s) => {
                s.resolve(ctx);
                None
            }
            Stmt::Expr(s) => Some(Stmt::Expr(s.resolve(ctx))),
        }
    }
}

impl StmtVar {
    pub fn resolve(&self, ctx: &mut Context) -> StmtVar {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.resolve(ctx);
        let expr = self.expr.resolve(ctx);
        ctx.bind(name.clone(), Binding::ExprVar);
        StmtVar::new(span, name, ty, expr)
    }
}

impl StmtDef {
    pub fn resolve(&self, ctx: &mut Context) -> StmtDef {
        let span = self.span;
        let name = self.name.clone();
        ctx.bind(name.clone(), Binding::ExprVar);
        ctx.scoped(|ctx| {
            let generics = self.generics.clone();
            generics
                .iter()
                .for_each(|g| ctx.bind(g.clone(), Binding::TypeGen));
            let preds = self
                .where_clause
                .iter()
                .flat_map(|p| p.resolve(ctx))
                .collect();
            let params = self.params.iter().map(|p| p.resolve(ctx)).collect();
            let ty = self.ty.resolve(ctx);
            let expr = self.expr.resolve(ctx);
            StmtDef::new(span, name, generics, preds, params, ty, expr)
        })
    }
}

impl StmtTrait {
    pub fn resolve(&self, ctx: &mut Context) -> StmtTrait {
        let span = self.span;
        let name = self.name.clone();
        ctx.bind(name.clone(), Binding::Trait(self.generics.len(), vec![]));
        for def in &self.defs {
            ctx.bind(def.name.clone(), Binding::ExprDef(def.generics.len()));
        }
        ctx.scoped(|ctx| {
            let generics = self.generics.clone();
            generics
                .iter()
                .for_each(|g| ctx.bind(g.clone(), Binding::TypeGen));
            let body = self.body.iter().flat_map(|p| p.resolve(ctx)).collect();
            let defs = self.defs.iter().map(|d| d.resolve(ctx)).collect();
            let types = self.types.clone();
            StmtTrait::new(span, name, generics, body, defs, types)
        })
    }
}

impl StmtDefDecl {
    pub fn resolve(&self, ctx: &mut Context) -> StmtDefDecl {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let preds = self
            .where_clause
            .iter()
            .flat_map(|p| p.resolve(ctx))
            .collect();
        let params = self.params.iter().map(|p| p.resolve(ctx)).collect();
        let ty = self.ty.resolve(ctx);
        StmtDefDecl::new(span, name, generics, preds, params, ty)
    }
}

impl StmtImpl {
    pub fn resolve(&self, ctx: &mut Context) -> Option<StmtImpl> {
        let span = self.span;
        let generics = self.generics.clone();
        let head = self.head.resolve(ctx);
        let body = self
            .where_clause
            .iter()
            .flat_map(|p| p.resolve(ctx))
            .collect();
        let defs = self.defs.iter().map(|d| d.resolve(ctx)).collect();
        let types = self.types.iter().map(|t| t.resolve(ctx)).collect();
        if let Some(head) = head {
            Some(StmtImpl::new(span, generics, head, body, defs, types))
        } else {
            None
        }
    }
}

impl StmtStruct {
    pub fn resolve(&self, ctx: &mut Context) -> StmtStruct {
        let span = self.span;
        let name = self.name.clone();
        let stmt = ctx.scoped(|ctx| {
            let generics = self.generics.clone();
            for generic in &generics {
                ctx.bind(generic.clone(), Binding::TypeGen);
            }
            let fields = self
                .fields
                .iter()
                .map(|(x, t)| (x.clone(), t.resolve(ctx)))
                .collect();
            StmtStruct::new(span, name.clone(), generics, fields)
        });
        let xs = stmt.fields.iter().map(|(x, _)| x.clone()).collect();
        ctx.bind(name, Binding::TypeStruct(stmt.generics.len(), xs));
        stmt
    }
}

impl StmtEnum {
    pub fn resolve(&self, ctx: &mut Context) -> StmtEnum {
        let span = self.span;
        let name = self.name.clone();
        let stmt = ctx.scoped(|ctx| {
            let generics = self.generics.clone();
            for generic in &generics {
                ctx.bind(generic.clone(), Binding::TypeGen);
            }
            let variants = self
                .variants
                .iter()
                .map(|(x, t)| (x.clone(), t.resolve(ctx)))
                .collect();
            StmtEnum::new(span, name.clone(), generics, variants)
        });
        let xs = stmt.variants.iter().map(|(x, _)| x.clone()).collect();
        ctx.bind(name, Binding::TypeEnum(stmt.generics.len(), xs));
        stmt
    }
}

impl StmtType {
    pub fn resolve(&self, ctx: &mut Context) -> StmtType {
        let span = self.span;
        let name = self.name.clone();
        let (generics, ty) = ctx.scoped(|ctx| {
            let generics = self.generics.clone();
            generics
                .iter()
                .for_each(|g| ctx.bind(g.clone(), Binding::TypeGen));
            let ty = self.ty.resolve(ctx);
            (generics, ty)
        });
        ctx.bind(
            name.clone(),
            Binding::TypeAlias(generics.clone(), ty.clone()),
        );
        StmtType::new(span, name, generics, ty)
    }
}

impl Param {
    pub fn resolve(&self, ctx: &mut Context) -> Param {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.resolve(ctx);
        ctx.bind(name.clone(), Binding::ExprVar);
        Param::new(span, name, ty)
    }
}

impl Type {
    pub fn resolve(&self, ctx: &mut Context) -> Type {
        match self {
            Type::Cons(x, ts) => match ctx.get(x) {
                Some(Binding::TypeGen) => {
                    if ts.is_empty() {
                        Type::Gen(x.clone())
                    } else {
                        ctx.wrong_arity(x, ts.len(), 0);
                        Type::Err
                    }
                }
                Some(
                    Binding::TypeEnum(n, _) | Binding::TypeStruct(n, _) | Binding::TypeBuiltin(n),
                ) => {
                    if ts.len() == *n {
                        let x = x.clone();
                        let ts = ts.iter().map(|t| t.resolve(ctx)).collect::<Vec<_>>();
                        Type::Cons(x, ts)
                    } else {
                        ctx.wrong_arity(x, ts.len(), *n);
                        Type::Err
                    }
                }
                Some(Binding::TypeAlias(gs, t)) => {
                    if gs.len() == ts.len() {
                        let sub = gs
                            .clone()
                            .into_iter()
                            .zip(ts.clone().into_iter())
                            .collect::<Vec<_>>();
                        t.apply_generic(&sub)
                    } else {
                        ctx.wrong_arity(x, ts.len(), gs.len());
                        Type::Err
                    }
                }
                Some(b) => {
                    ctx.unexpected(x, b.clone(), "type");
                    Type::Err
                }
                None => {
                    ctx.not_found(x, "type");
                    Type::Err
                }
            },
            Type::Assoc(tr, x) => {
                let tr = tr.resolve(ctx);
                let x = x.clone();
                if let Some(tr) = tr {
                    Type::Assoc(tr, x)
                } else {
                    Type::Err
                }
            }
            Type::Hole => Type::Hole,
            Type::Var(_) => unreachable!("Type::Var should not yet be annotated."),
            Type::Err => Type::Err,
            Type::Gen(x) => Type::Gen(x.clone()),
            Type::Fun(ts, t) => {
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                let t = t.resolve(ctx);
                Type::Fun(ts, Box::new(t))
            }
            Type::Tuple(ts) => {
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                Type::Tuple(ts)
            }
            Type::Record(xts) => {
                let xts = xts
                    .iter()
                    .map(|(x, t)| (x.clone(), t.resolve(ctx)))
                    .collect();
                Type::Record(xts)
            }
        }
    }
}

impl Expr {
    pub fn resolve(&self, ctx: &mut Context) -> Expr {
        match self {
            Expr::Var(s, t, x, ts) => match ctx.get(x) {
                Some(Binding::ExprVar) => {
                    if ts.len() == 0 {
                        let t = t.resolve(ctx);
                        let x = x.clone();
                        let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                        Expr::Var(*s, t, x, ts)
                    } else {
                        ctx.wrong_arity(x, ts.len(), 0);
                        Expr::Err(*s, t.clone())
                    }
                }
                Some(Binding::ExprDef(n)) => {
                    if ts.len() == *n || ts.len() == 0 {
                        let t = t.resolve(ctx);
                        let x = x.clone();
                        let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                        Expr::Var(*s, t, x, ts)
                    } else {
                        ctx.wrong_arity(x, ts.len(), *n);
                        Expr::Err(*s, t.clone())
                    }
                }
                Some(Binding::TypeStruct(n, xs)) => {
                    if ts.len() == *n && xs.len() == 0 {
                        let t = t.resolve(ctx);
                        let x = x.clone();
                        let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                        Expr::Struct(*s, t, x, ts, vec![])
                    } else {
                        ctx.wrong_arity(x, ts.len(), *n);
                        Expr::Err(*s, t.clone())
                    }
                }
                Some(b) => {
                    ctx.unexpected(x, b.clone(), "expression");
                    Expr::Err(*s, t.clone())
                }
                None => {
                    ctx.not_found(x, "expression");
                    Expr::Err(*s, t.clone())
                }
            },
            Expr::Int(s, t, v) => {
                let t = t.resolve(ctx);
                let v = v.clone();
                Expr::Int(*s, t, v)
            }
            Expr::Float(s, t, v) => {
                let t = t.resolve(ctx);
                let v = v.clone();
                Expr::Float(*s, t, v)
            }
            Expr::Bool(s, t, v) => {
                let t = t.resolve(ctx);
                Expr::Bool(*s, t, *v)
            }
            Expr::Tuple(s, t, es) => {
                let t = t.resolve(ctx);
                let es = es.iter().map(|e| e.resolve(ctx)).collect();
                Expr::Tuple(*s, t, es)
            }
            Expr::Struct(s, t, x, ts, xes) => {
                let t = t.resolve(ctx);
                let ts: Vec<_> = ts.iter().map(|t| t.resolve(ctx)).collect();
                let x = x.clone();
                let xes: Vec<_> = xes
                    .iter()
                    .map(|(x, e)| (x.clone(), e.resolve(ctx)))
                    .collect();
                match ctx.get(&x) {
                    Some(Binding::TypeStruct(n, xs)) => {
                        if ts.len() == *n {
                            if fields_are_defined(&xs, &xes) {
                                Expr::Struct(*s, t, x, ts, xes)
                            } else {
                                let s = *s;
                                let xs = xs.clone();
                                ctx.wrong_fields(&x, &xes, &xs);
                                Expr::Err(s, t)
                            }
                        } else {
                            ctx.wrong_arity(&x, ts.len(), *n);
                            Expr::Err(*s, t)
                        }
                    }
                    Some(b) => todo!(),
                    None => todo!(),
                }
            }
            Expr::Enum(s, t, x0, ts, x1, e) => {
                let t = t.resolve(ctx);
                let x0 = x0.clone();
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                let x1 = x1.clone();
                let e = e.resolve(ctx);
                Expr::Enum(*s, t, x0, ts, x1, Box::new(e))
            }
            Expr::Call(s, t, f, es) => {
                let t = t.resolve(ctx);
                let f = f.resolve(ctx);
                let es = es.iter().map(|e| e.resolve(ctx)).collect();
                Expr::Call(*s, t, Box::new(f), es)
            }
            Expr::String(s, t, v) => {
                let t = t.resolve(ctx);
                let v = v.clone();
                Expr::String(*s, t, v)
            }
            Expr::Field(s, t, e, x) => {
                let t = t.resolve(ctx);
                let e = e.resolve(ctx);
                let x = x.clone();
                Expr::Field(*s, t, Box::new(e), x)
            }
            Expr::Block(s, t, ss, e) => {
                let t = t.resolve(ctx);
                let ss = ss.iter().flat_map(|s| s.resolve(ctx)).collect();
                let e = e.resolve(ctx);
                Expr::Block(*s, t, ss, Box::new(e))
            }
            Expr::Query(..) => {
                todo!()
            }
            Expr::Assoc(s, t, tr, x, ts) => {
                let t = t.resolve(ctx);
                let tr = tr.resolve(ctx);
                let x = x.clone();
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                if let Some(tr) = tr {
                    Expr::Assoc(*s, t, tr, x, ts)
                } else {
                    Expr::Err(*s, t.clone())
                }
            }
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Err(s, t) => {
                let t = t.resolve(ctx);
                Expr::Err(*s, t)
            }
            Expr::Assign(s, t, e0, e1) => {
                let t = t.resolve(ctx);
                let e0 = e0.resolve(ctx);
                let e1 = e1.resolve(ctx);
                let e1 = if is_lvalue(&e0) {
                    e1
                } else {
                    Expr::Err(*s, Type::Hole)
                };
                Expr::Assign(*s, t, Box::new(e0), Box::new(e1))
            }
            Expr::Return(s, t, e) => {
                let t = t.resolve(ctx);
                let e = e.resolve(ctx);
                Expr::Return(*s, t, Box::new(e))
            }
            Expr::Continue(s, t) => {
                let t = t.resolve(ctx);
                Expr::Continue(*s, t)
            }
            Expr::Break(s, t) => {
                let t = t.resolve(ctx);
                Expr::Break(*s, t)
            }
            Expr::Fun(s, t, ps, t1, e) => {
                let t = t.resolve(ctx);
                let ps = ps.iter().map(|p| p.resolve(ctx)).collect();
                let t1 = t1.resolve(ctx);
                let e = e.resolve(ctx);
                Expr::Fun(*s, t, ps, t1, Box::new(e))
            }
            Expr::And(s, t, e0, e1) => {
                let t = t.resolve(ctx);
                let e0 = e0.resolve(ctx);
                let e1 = e1.resolve(ctx);
                Expr::And(*s, t, Box::new(e0), Box::new(e1))
            }
            Expr::Or(s, t, e0, e1) => {
                let t = t.resolve(ctx);
                let e0 = e0.resolve(ctx);
                let e1 = e1.resolve(ctx);
                Expr::Or(*s, t, Box::new(e0), Box::new(e1))
            }
            Expr::Match(s, t, e, xps) => {
                let t = t.resolve(ctx);
                let e = e.resolve(ctx);
                let xps = xps
                    .iter()
                    .map(|(p, e)| (p.resolve(ctx), e.resolve(ctx)))
                    .collect();
                Expr::Match(*s, t, Box::new(e), xps)
            }
            Expr::While(s, t, e0, e1) => {
                let t = t.resolve(ctx);
                let e0 = e0.resolve(ctx);
                let e1 = e1.resolve(ctx);
                Expr::While(*s, t, Box::new(e0), Box::new(e1))
            }
            Expr::Record(s, t, xes) => {
                let t = t.resolve(ctx);
                let xes = xes
                    .iter()
                    .map(|(x, e)| (x.clone(), e.resolve(ctx)))
                    .collect();
                Expr::Record(*s, t, xes)
            }
        }
    }
}

impl Pat {
    fn resolve(&self, ctx: &mut Context) -> Pat {
        match self {
            Pat::Var(s, t, j) => todo!(),
            Pat::Tuple(s, t, ts) => todo!(),
            Pat::Struct(s, t, _, _, _) => todo!(),
            Pat::Enum(s, t, _, _, _, _) => todo!(),
            Pat::Int(s, t, v) => todo!(),
            Pat::String(s, t, v) => todo!(),
            Pat::Wildcard(s, t) => todo!(),
            Pat::Bool(s, t, v) => todo!(),
            Pat::Err(s, t) => todo!(),
        }
    }
}

fn is_lvalue(e: &Expr) -> bool {
    match e {
        Expr::Var(..) => true,
        Expr::Field(_, _, e, _) => is_lvalue(e),
        Expr::Index(_, _, e, _) => is_lvalue(e),
        _ => false,
    }
}

fn fields_are_defined<T>(expected: &[Name], provided: &[(Name, T)]) -> bool {
    provided.iter().all(|(x, _)| expected.contains(x))
        && expected
            .iter()
            .all(|x| provided.iter().any(|(y, _)| x == y))
}

fn variant_exists(expected: &[Name], provided: &(Name, Type)) -> bool {
    expected.contains(&provided.0)
}

impl Trait {
    pub fn resolve(&self, ctx: &mut Context) -> Option<Trait> {
        let span = self.span;
        match ctx.get(&self.name) {
            Some(Binding::Trait(n, xs)) => {
                let name = self.name.clone();
                let types = self.types.iter().map(|t| t.resolve(ctx)).collect();
                let assocs = self
                    .assocs
                    .iter()
                    .map(|(x, t)| (x.clone(), t.resolve(ctx)))
                    .collect();
                Some(Trait::new(span, name, types, assocs))
            }
            Some(b) => {
                ctx.unexpected(&self.name, b.clone(), "trait");
                None
            }
            None => {
                ctx.not_found(&self.name, "trait");
                None
            }
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum Binding {
    TypeEnum(usize, Vec<Name>),
    TypeStruct(usize, Vec<Name>),
    TypeBuiltin(usize),
    TypeGen,
    TypeAlias(Vec<Name>, Type),
    Trait(usize, Vec<Name>),
    ExprVar,
    ExprDef(usize),
}

impl Binding {
    fn name(&self) -> &'static str {
        match self {
            Binding::TypeEnum(_, _) => "enum",
            Binding::TypeStruct(_, _) => "struct",
            Binding::TypeBuiltin(_) => "builtin",
            Binding::TypeGen => "type variable",
            Binding::TypeAlias(..) => "type alias",
            Binding::Trait(..) => "trait",
            Binding::ExprVar => "variable",
            Binding::ExprDef(..) => "definition",
        }
    }

    fn arity(&self) -> Option<usize> {
        match self {
            Binding::TypeEnum(n, _) => Some(*n),
            Binding::TypeStruct(n, _) => Some(*n),
            Binding::TypeBuiltin(n) => Some(*n),
            Binding::TypeGen => None,
            Binding::TypeAlias(generics, _) => Some(generics.len()),
            Binding::Trait(n, _) => Some(*n),
            Binding::ExprVar => None,
            Binding::ExprDef(n) => Some(*n),
        }
    }

    fn members(&self) -> Option<&[Name]> {
        match self {
            Binding::TypeEnum(_, xs) => Some(xs),
            Binding::TypeStruct(_, xs) => Some(xs),
            Binding::TypeBuiltin(_) => None,
            Binding::TypeGen => None,
            Binding::TypeAlias(..) => None,
            Binding::Trait(_, xs) => Some(xs),
            Binding::ExprVar => None,
            Binding::ExprDef(..) => None,
        }
    }
}

// generics
//     .iter()
//     .for_each(|g| self.bind(g.clone(), Binding::TypeVar));
// self.bind(name.clone(), Binding::TypeAlias(generics, ty));
// self.bind(name.clone(), Binding::ExprVar);
// self.bind(stmt.name.clone(), Binding::TypeStruct(stmt.generics.len()));
// self.bind(name.clone(), Binding::ExprVar);
// fn type_name(&mut self, name: Name) -> Option<Type> {
//     match self.lookup(&name)? {
//         Binding::TypeAlias(generics, ty) => {
//             let generics = generics.clone();
//             let ty = ty.clone();
//             let tys = self.tys()?;
//             if generics.len() == tys.len() {
//                 let sub = generics
//                     .into_iter()
//                     .zip(tys.into_iter())
//                     .collect::<Vec<_>>();
//                 Some(ty.apply(&sub))
//             } else {
//                 None
//             }
//         }
//         Binding::TypeVar => Some(Type::Var(name)),
//         Binding::TypeStruct(n) => {
//             let n = *n;
//             let tys = self.tys()?;
//             if tys.len() == n {
//                 Some(Type::Cons(name, tys))
//             } else if tys.is_empty() {
//                 Some(Type::Cons(name, (0..n).map(|_| Type::Hole).collect()))
//             } else {
//                 None
//             }
//         }
//         Binding::TypeEnum(n) => {
//             let n = *n;
//             let tys = self.tys()?;
//             if tys.len() == n {
//                 Some(Type::Cons(name, tys))
//             } else if tys.is_empty() {
//                 Some(Type::Cons(name, (0..n).map(|_| Type::Hole).collect()))
//             } else {
//                 None
//             }
//         }
//         Binding::TypeBuiltin(n) => {
//             let n = *n;
//             let tys = self.tys()?;
//             if tys.len() == n {
//                 Some(Type::Cons(name, tys))
//             } else {
//                 None
//             }
//         }
//         Binding::ExprVar => None,
//         Binding::Trait(n, assocs) => {
//             let assocs = assocs.clone();
//             let n = *n;
//             let tys = self.tys()?;
//             if tys.len() == n {
//                 let s = self.expect(Token::Dot)?;
//                 let name1 = self.name()?;
//                 let xts = assocs.into_iter().map(|x| (x, Type::Hole)).collect();
//                 let tr = Trait::new(s, name, tys, xts);
//                 Some(Type::Assoc(tr, name1))
//             } else {
//                 None
//             }
//         }
//     }
// }
//
// fn expr_name(&mut self, name: Name) -> Option<Expr> {
//     let s0 = name.span;
//     match self.lookup(&name)? {
//         Binding::ExprVar => Some(Expr::Var(s0, Type::Hole, name)),
//         Binding::TypeEnum(_) => {
//             if self.expect(Token::Dot).is_some() {
//                 let variant_name = self.name()?;
//                 let expr = if self.peek() == Token::LParen {
//                     let exprs =
//                         self.sep(|this| this.expr(Self::expr4), Token::LParen, Token::RParen)?;
//                     if exprs.len() == 1 {
//                         exprs.into_iter().next().unwrap()
//                     } else {
//                         Expr::Tuple(s0, Type::Hole, exprs)
//                     }
//                 } else {
//                     Expr::Unit(s0, Type::Hole)
//                 };
//                 Some(Expr::Enum(
//                     s0,
//                     Type::Hole,
//                     name,
//                     variant_name,
//                     Box::new(expr),
//                 ))
//             } else {
//                 None
//             }
//         }
//         Binding::TypeStruct(_) => {
//             if self.peek() == Token::LBrace {
//                 let fields = self.sep(Self::expr_field, Token::LBrace, Token::RBrace)?;
//                 Some(Expr::Struct(s0, Type::Hole, name, fields))
//             } else {
//                 None
//             }
//         }
//         Binding::TypeAlias(..) => None,
//         Binding::TypeVar => None,
//         Binding::TypeBuiltin(..) => None,
//         Binding::Trait(n, xs) => {
//             let n = *n;
//             let xs = xs.clone();
//             let tys = self.tys()?;
//             if tys.len() == n {
//                 self.expect(Token::Dot)?;
//                 let name1 = self.name()?;
//                 let xts = xs.into_iter().map(|x| (x, Type::Hole)).collect();
//                 let tr = Trait::new(s0, name, tys, xts);
//                 Some(Expr::Assoc(s0, Type::Hole, tr, name1))
//             } else {
//                 None
//             }
//         }
//     }
// }
//
