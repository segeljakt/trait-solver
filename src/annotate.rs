use crate::data::StmtDef;
use crate::data::Expr;
use crate::data::StmtImpl;
use crate::data::Param;
use crate::data::Trait;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::StmtVar;
use crate::infer::Context;

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
            Stmt::Struct(_) => todo!(),
            Stmt::Enum(_) => todo!(),
        }
    }
}

impl StmtVar {
    pub fn annotate(&self, ctx: &mut Context) -> StmtVar {
        let x = self.name.clone();
        let t = self.ty.annotate(ctx);
        let e = self.expr.annotate(ctx);
        StmtVar::new(x, t, e)
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
            Expr::String(t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::String(t, v)
            }
            Expr::Unit(t) => {
                let t = t.annotate(ctx);
                Expr::Unit(t)
            }
            Expr::Var(t, x) => {
                let t = t.annotate(ctx);
                let x = x.clone();
                Expr::Var(t, x)
            }
            Expr::Call(t, e, es) => {
                let t = t.annotate(ctx);
                let e = Box::new(e.annotate(ctx));
                let es = es.into_iter().map(|e| e.annotate(ctx)).collect();
                Expr::Call(t, e, es)
            }
            Expr::Block(t, ss, e) => {
                let t = t.annotate(ctx);
                let ss = ss.into_iter().map(|s| s.annotate(ctx)).collect();
                let e = Box::new(e.annotate(ctx));
                Expr::Block(t, ss, e)
            }
            Expr::From(..) => todo!(),
            Expr::Struct(..) => todo!(),
            Expr::Enum(..) => todo!(),
            Expr::Field(t, e, x) => {
                let t = t.annotate(ctx);
                let e = Box::new(e.annotate(ctx));
                let x = x.clone();
                Expr::Field(t, e, x)
            }
            Expr::Tuple(_, _) => todo!(),
            Expr::Assoc(_, _, _) => todo!(),
        }
    }
}

impl StmtDef {
    pub fn annotate(&self, ctx: &mut Context) -> StmtDef {
        let name = self.name.clone();
        let generics = self.generics.clone();
        let preds = self.preds.iter().map(|p| p.annotate(ctx)).collect();
        let params = self.params.iter().map(|p| p.annotate(ctx)).collect();
        let ty = self.ty.annotate(ctx);
        let expr = self.expr.annotate(ctx);
        StmtDef::new(name, generics, preds, params, ty, expr)
    }
}

impl Param {
    pub fn annotate(&self, ctx: &mut Context) -> Param {
        let name = self.name.clone();
        let ty = self.ty.annotate(ctx);
        Param::new(name, ty)
    }
}

impl StmtImpl {
    pub fn annotate(&self, ctx: &mut Context) -> StmtImpl {
        let generics = self.generics.clone();
        let head = self.head.annotate(ctx);
        let body = self.body.iter().map(|p| p.annotate(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.annotate(ctx)).collect();
        StmtImpl::new(generics, head, body, defs)
    }
}

impl Trait {
    pub fn annotate(&self, ctx: &mut Context) -> Trait {
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.annotate(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.annotate(ctx)))
            .collect();
        Trait::new(name, types, assocs)
    }
}
