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
            Stmt::Var(v) => {
                let v = v.apply(sub);
                Stmt::Var(v)
            }
            Stmt::Def(d) => {
                let d = d.apply(sub);
                Stmt::Def(d)
            }
            Stmt::Impl(i) => {
                let i = i.apply(sub);
                // let ds = ds.iter().map(|d| d.apply(sub)).collect();
                Stmt::Impl(i)
            }
            Stmt::Expr(e) => {
                let e = e.apply(sub);
                Stmt::Expr(e)
            }
            Stmt::Struct(_) => todo!(),
            Stmt::Enum(_) => todo!(),
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
            Expr::Var(s, t, x) => {
                let t = t.apply(sub);
                let x = x.clone();
                Expr::Var(*s, t, x)
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
            Expr::Assoc(_, _, _, _) => todo!(),
            Expr::Err(s, t) => {
                let t = t.apply(sub);
                Expr::Err(*s, t)
            }
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
