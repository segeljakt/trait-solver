#![allow(unused)]
use crate::ast::Expr;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtEnum;
use crate::ast::StmtImpl;
use crate::ast::StmtStruct;
use crate::ast::StmtTrait;
use crate::ast::StmtType;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;
use crate::ast::Variant;
use crate::diag::Diags;

pub struct Resolver {
    stack: Stack,
    pub diags: Diags,
}

impl Resolver {
    pub fn new() -> Resolver {
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
        let diags = Diags::new();
        Resolver { stack, diags }
    }
}

impl std::ops::Deref for Resolver {
    type Target = Stack;
    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

impl std::ops::DerefMut for Resolver {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}

pub struct Stack(Vec<Vec<(Name, Binding)>>);

impl Stack {
    fn bind(&mut self, name: impl Into<Name>, binding: Binding) {
        self.0.last_mut().unwrap().push((name.into(), binding));
    }

    fn lookup(&self, x: &Name) -> Option<&Binding> {
        self.0.iter().rev().find_map(|s| {
            s.iter()
                .rev()
                .find_map(|(y, b)| if y == x { Some(b) } else { None })
        })
    }

    fn check<const N: usize>(&self, x: &Name, bs: [Binding; N]) -> bool {
        self.lookup(x).is_some_and(|b| bs.iter().any(|b1| b == b1))
    }
}

impl Program {
    pub fn resolve(&self, ctx: &mut Resolver) -> Program {
        let stmts = self.stmts.iter().map(|stmt| stmt.resolve(ctx)).collect();
        Program::new(stmts)
    }
}

impl Stmt {
    pub fn resolve(&self, ctx: &mut Resolver) -> Stmt {
        match self {
            Stmt::Var(s) => Stmt::Var(s.resolve(ctx)),
            Stmt::Def(s) => Stmt::Def(s.resolve(ctx)),
            Stmt::Trait(s) => Stmt::Trait(s.resolve(ctx)),
            Stmt::Impl(s) => Stmt::Impl(s.resolve(ctx)),
            Stmt::Struct(s) => Stmt::Struct(s.resolve(ctx)),
            Stmt::Enum(s) => Stmt::Enum(s.resolve(ctx)),
            Stmt::Type(s) => Stmt::Type(s.resolve(ctx)),
            Stmt::Expr(s) => Stmt::Expr(s.resolve(ctx)),
        }
    }
}

impl StmtVar {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtVar {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.resolve(ctx);
        let expr = self.expr.resolve(ctx);
        ctx.bind(name.clone(), Binding::ExprVar);
        StmtVar::new(span, name, ty, expr)
    }
}

impl StmtDef {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtDef {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let preds = self.preds.iter().map(|p| p.resolve(ctx)).collect();
        let params = self.params.iter().map(|p| p.resolve(ctx)).collect();
        let ty = self.ty.resolve(ctx);
        let expr = self.expr.resolve(ctx);
        ctx.bind(name.clone(), Binding::ExprVar);
        StmtDef::new(span, name, generics, preds, params, ty, expr)
    }
}

impl StmtTrait {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtTrait {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let body = self.body.iter().map(|p| p.resolve(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.resolve(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.resolve(ctx)))
            .collect();
        StmtTrait::new(span, name, generics, body, defs, assocs)
    }
}

impl StmtImpl {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtImpl {
        let span = self.span;
        let generics = self.generics.clone();
        let head = self.head.resolve(ctx);
        let body = self.body.iter().map(|p| p.resolve(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.resolve(ctx)).collect();
        StmtImpl::new(span, generics, head, body, defs)
    }
}

impl StmtStruct {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtStruct {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let fields = self
            .fields
            .iter()
            .map(|(x, t)| (x.clone(), t.resolve(ctx)))
            .collect();
        StmtStruct::new(span, name, generics, fields)
    }
}

impl StmtEnum {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtEnum {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let variants = self.variants.iter().map(|v| v.resolve(ctx)).collect();
        StmtEnum::new(span, name, generics, variants)
    }
}

impl Variant {
    pub fn resolve(&self, ctx: &mut Resolver) -> Variant {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.resolve(ctx);
        Variant::new(span, name, ty)
    }
}

impl StmtType {
    pub fn resolve(&self, ctx: &mut Resolver) -> StmtType {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let ty = self.ty.resolve(ctx);
        StmtType::new(span, name, generics, ty)
    }
}

impl Param {
    pub fn resolve(&self, ctx: &mut Resolver) -> Param {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.resolve(ctx);
        Param::new(span, name, ty)
    }
}

impl Type {
    pub fn resolve(&self, ctx: &mut Resolver) -> Type {
        match self {
            Type::Cons(x, ts) => {
                let x = x.clone();
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect::<Vec<_>>();
                Type::Cons(x, ts)
            }
            Type::Var(x) => Type::Var(x.clone()),
            Type::Assoc(i, x) => {
                let i = i.resolve(ctx);
                let x = x.clone();
                Type::Assoc(i, x)
            }
            Type::Hole => Type::Hole,
        }
    }
}

impl Expr {
    pub fn resolve(&self, ctx: &mut Resolver) -> Expr {
        match self {
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
            Expr::Var(s, t, v, ts) => {
                let t = t.resolve(ctx);
                let v = v.clone();
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                Expr::Var(*s, t, v, ts)
            }
            Expr::Tuple(s, t, es) => {
                let t = t.resolve(ctx);
                let es = es.iter().map(|e| e.resolve(ctx)).collect();
                Expr::Tuple(*s, t, es)
            }
            Expr::Struct(s, t, x, fs) => {
                let t = t.resolve(ctx);
                let x = x.clone();
                let fs = fs
                    .iter()
                    .map(|(f, e)| (f.clone(), e.resolve(ctx)))
                    .collect();
                Expr::Struct(*s, t, x, fs)
            }
            Expr::Enum(s, t, x, v, e) => {
                let t = t.resolve(ctx);
                let x = x.clone();
                let v = v.clone();
                let e = e.resolve(ctx);
                Expr::Enum(*s, t, x, v, Box::new(e))
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
            Expr::Unit(s, t) => {
                let t = t.resolve(ctx);
                Expr::Unit(*s, t)
            }
            Expr::Field(s, t, e, x) => {
                let t = t.resolve(ctx);
                let e = e.resolve(ctx);
                let x = x.clone();
                Expr::Field(*s, t, Box::new(e), x)
            }
            Expr::Block(s, t, ss, e) => {
                let t = t.resolve(ctx);
                let ss = ss.iter().map(|s| s.resolve(ctx)).collect();
                let e = e.resolve(ctx);
                Expr::Block(*s, t, ss, Box::new(e))
            }
            Expr::From(..) => {
                todo!()
            }
            Expr::Assoc(s, t, tr, x, ts) => {
                let t = t.resolve(ctx);
                let tr = tr.resolve(ctx);
                let x = x.clone();
                let ts = ts.iter().map(|t| t.resolve(ctx)).collect();
                Expr::Assoc(*s, t, tr, x, ts)
            }
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Err(s, t) => {
                let t = t.resolve(ctx);
                Expr::Err(*s, t)
            }
            Expr::Assign(_, _, _, _) => todo!(),
            Expr::Return(_, _, _) => todo!(),
            Expr::Continue(_, _) => todo!(),
            Expr::Break(_, _) => todo!(),
            Expr::Fun(_, _, _, _, _) => todo!(),
        }
    }
}

impl Trait {
    pub fn resolve(&self, ctx: &mut Resolver) -> Trait {
        let span = self.span;
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.resolve(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.resolve(ctx)))
            .collect();
        Trait::new(span, name, types, assocs)
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum Binding {
    TypeEnum(usize),
    TypeStruct(usize),
    TypeBuiltin(usize),
    TypeVar,
    TypeAlias(Vec<Name>, Type),
    Trait(usize, Vec<Name>),
    ExprVar,
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
