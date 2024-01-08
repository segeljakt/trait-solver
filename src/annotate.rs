use crate::data::Context;
use crate::data::Def;
use crate::data::Expr;
use crate::data::Impl;
use crate::data::Param;
use crate::data::Pred;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::Var;

// Replace all holes with fresh type variables.

impl Type {
    pub fn annotate(&self, ctx: &mut Context) -> Type {
        match self {
            Type::Cons(x, ts) => {
                let x = x.clone();
                let ts = ts.into_iter().map(|t| t.annotate(ctx)).collect();
                Type::Cons(x, ts)
            }
            Type::Assoc(p, x) => {
                let p = p.annotate(ctx);
                let x = x.clone();
                Type::Assoc(p, x)
            }
            Type::Var(x) => {
                let x = x.clone();
                Type::Var(x)
            }
            Type::Hole => ctx.new_tyvar(),
        }
    }
}

impl Program {
    pub fn annotate(&self, ctx: &mut Context) -> Program {
        let stmts = self.stmts.iter().map(|s| s.annotate(ctx)).collect();
        Program::new(stmts)
    }
}

impl Stmt {
    pub fn annotate(&self, ctx: &mut Context) -> Stmt {
        match self {
            Stmt::Var(v) => {
                let v = v.annotate(ctx);
                Stmt::Var(v)
            }
            Stmt::Def(d) => {
                let d = d.annotate(ctx);
                Stmt::Def(d)
            }
            Stmt::Impl(i) => {
                let i = i.annotate(ctx);
                // let ds = ds.iter().map(|d| d.annotate(ctx)).collect();
                Stmt::Impl(i)
            }
            Stmt::Expr(e) => {
                let e = e.annotate(ctx);
                Stmt::Expr(e)
            }
        }
    }
}

impl Var {
    pub fn annotate(&self, ctx: &mut Context) -> Var {
        let x = self.name.clone();
        let t = self.ty.annotate(ctx);
        let e = self.expr.annotate(ctx);
        Var::new(x, t, e)
    }
}

impl Expr {
    pub fn annotate(&self, ctx: &mut Context) -> Expr {
        match self {
            Expr::Int(t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::Int(t, v)
            }
            Expr::Float(t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::Float(t, v)
            }
            Expr::Bool(t, v) => {
                let t = t.annotate(ctx);
                Expr::Bool(t, *v)
            }
            Expr::Var(t, x) => {
                let t = t.annotate(ctx);
                let x = x.clone();
                Expr::Var(t, x)
            }
            Expr::Call(t, e, es) => {
                let t = t.annotate(ctx);
                let e = Box::new(e.as_ref().annotate(ctx));
                let es = es.into_iter().map(|e| e.annotate(ctx)).collect();
                Expr::Call(t, e, es)
            }
            Expr::Block(t, ss, e) => {
                let t = t.annotate(ctx);
                let ss = ss.into_iter().map(|s| s.annotate(ctx)).collect();
                let e = Box::new(e.as_ref().annotate(ctx));
                Expr::Block(t, ss, e)
            }
            Expr::From(..) => todo!(),
        }
    }
}

impl Def {
    pub fn annotate(&self, ctx: &mut Context) -> Def {
        let name = self.name.clone();
        let generics = self.generics.clone();
        let preds = self.preds.iter().map(|p| p.annotate(ctx)).collect();
        let params = self.params.iter().map(|p| p.annotate(ctx)).collect();
        let ty = self.ty.annotate(ctx);
        let expr = self.expr.annotate(ctx);
        Def::new(name, generics, preds, params, ty, expr)
    }
}

impl Param {
    pub fn annotate(&self, ctx: &mut Context) -> Param {
        let name = self.name.clone();
        let ty = self.ty.annotate(ctx);
        Param::new(name, ty)
    }
}

impl Impl {
    pub fn annotate(&self, ctx: &mut Context) -> Impl {
        let generics = self.generics.clone();
        let head = self.head.annotate(ctx);
        let body = self.body.iter().map(|p| p.annotate(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.annotate(ctx)).collect();
        Impl::new(generics, head, body, defs)
    }
}

impl Pred {
    pub fn annotate(&self, ctx: &mut Context) -> Pred {
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.annotate(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.annotate(ctx)))
            .collect();
        Pred::new(name, types, assocs)
    }
}
