use crate::ast::Expr;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtImpl;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;

impl Program {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Program {
        let stmts = self.stmts.iter().map(|s| s.apply(sub)).collect::<Vec<_>>();
        Program::new(stmts)
    }
}

impl Stmt {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Stmt {
        match self {
            Stmt::Var(v) => Stmt::Var(v.apply(sub)),
            Stmt::Def(d) => Stmt::Def(d.apply(sub)),
            Stmt::Impl(i) => Stmt::Impl(i.apply(sub)),
            Stmt::Expr(e) => Stmt::Expr(e.apply(sub)),
            Stmt::Struct(s) => Stmt::Struct(s.clone()),
            Stmt::Enum(s) => Stmt::Enum(s.clone()),
            Stmt::Type(s) => Stmt::Type(s.clone()),
            Stmt::Trait(s) => Stmt::Trait(s.clone()),
        }
    }
}

impl StmtVar {
    pub fn apply(&self, sub: &[(Name, Type)]) -> StmtVar {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.apply(sub);
        let expr = self.expr.apply(sub);
        StmtVar::new(span, name, ty, expr)
    }
}

impl StmtDef {
    pub fn apply(&self, sub: &[(Name, Type)]) -> StmtDef {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let qs = self.preds.iter().map(|p| p.apply(sub)).collect();
        let ps = self.params.iter().map(|p| p.apply(sub)).collect();
        let t = self.ty.apply(sub);
        let e = self.expr.apply(sub);
        StmtDef::new(span, name, generics, qs, ps, t, e)
    }
}

impl StmtImpl {
    pub fn apply(&self, sub: &[(Name, Type)]) -> StmtImpl {
        let span = self.span;
        let generics = self.generics.clone();
        let head = self.head.apply(sub);
        let body = self.body.iter().map(|p| p.apply(sub)).collect();
        let defs = self.defs.iter().map(|d| d.apply(sub)).collect();
        StmtImpl::new(span, generics, head, body, defs)
    }
}

impl Type {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Type {
        match self {
            Type::Cons(x, ts) => {
                let x = x.clone();
                let ts = ts.iter().map(|t| t.apply(sub)).collect::<Vec<_>>();
                Type::Cons(x, ts)
            }
            Type::Var(x) => {
                if let Some((_, t)) = sub.iter().find(|(n, _)| n == x) {
                    t.clone()
                } else {
                    let x = x.clone();
                    Type::Var(x)
                }
            }
            Type::Assoc(i, x) => {
                let i = i.apply(sub);
                let x = x.clone();
                Type::Assoc(i, x)
            }
            Type::Hole => unreachable!(),
        }
    }
}

impl Trait {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Trait {
        let span = self.span;
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.apply(sub)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.apply(sub)))
            .collect();
        Trait::new(span, name, types, assocs)
    }
}

impl Expr {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Expr {
        match self {
            Expr::Int(s, t, v) => {
                let t = t.apply(sub);
                let v = v.clone();
                Expr::Int(*s, t, v)
            }
            Expr::Float(s, t, v) => {
                let t = t.apply(sub);
                let v = v.clone();
                Expr::Float(*s, t, v)
            }
            Expr::Bool(s, t, v) => {
                let t = t.apply(sub);
                Expr::Bool(*s, t, *v)
            }
            Expr::String(s, t, v) => {
                let t = t.apply(sub);
                let v = v.clone();
                Expr::String(*s, t, v)
            }
            Expr::Unit(s, t) => {
                let t = t.apply(sub);
                Expr::Unit(*s, t)
            }
            Expr::Var(s, t, x, ts) => {
                let t = t.apply(sub);
                let x = x.clone();
                let ts = ts.into_iter().map(|t| t.apply(sub)).collect();
                Expr::Var(*s, t, x, ts)
            }
            Expr::Call(s, t, e, es) => {
                let t = t.apply(sub);
                let e = Box::new(e.apply(sub));
                let es = es.iter().map(|e| e.apply(sub)).collect();
                Expr::Call(*s, t, e, es)
            }
            Expr::Block(s, t, ss, e) => {
                let t = t.apply(sub);
                let ss = ss.iter().map(|s| s.apply(sub)).collect();
                let e = Box::new(e.apply(sub));
                Expr::Block(*s, t, ss, e)
            }
            Expr::From(..) => todo!(),
            Expr::Struct(_, _, _, _) => todo!(),
            Expr::Enum(_, _, _, _, _) => todo!(),
            Expr::Field(_, _, _, _) => todo!(),
            Expr::Tuple(_, _, _) => todo!(),
            Expr::Assoc(_, _, _, _, _) => todo!(),
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Err(s, t) => {
                let t = t.apply(sub);
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

impl Param {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Param {
        let span = self.span;
        let name = self.name.clone();
        let t = self.ty.apply(sub);
        Param::new(span, name, t)
    }
}
