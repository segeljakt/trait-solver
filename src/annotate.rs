use crate::ast::Expr;
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
                Stmt::Impl(i)
            }
            Stmt::Expr(e) => {
                let e = e.annotate(ctx);
                Stmt::Expr(e)
            }
            Stmt::Struct(s) => {
                let s = s.annotate(ctx);
                Stmt::Struct(s)
            }
            Stmt::Enum(e) => {
                let e = e.annotate(ctx);
                Stmt::Enum(e)
            }
            Stmt::Type(s) => {
                let s = s.annotate(ctx);
                Stmt::Type(s)
            }
            Stmt::Trait(s) => {
                let s = s.annotate(ctx);
                Stmt::Trait(s)
            }
        }
    }
}

impl StmtStruct {
    pub fn annotate(&self, ctx: &mut Context) -> StmtStruct {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let fields = self
            .fields
            .iter()
            .map(|(x, t)| (x.clone(), t.annotate(ctx)))
            .collect();
        StmtStruct::new(span, name, generics, fields)
    }
}

impl StmtType {
    pub fn annotate(&self, ctx: &mut Context) -> StmtType {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let ty = self.ty.annotate(ctx);
        StmtType::new(span, name, generics, ty)
    }
}

impl StmtEnum {
    pub fn annotate(&self, ctx: &mut Context) -> StmtEnum {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let variants = self.variants.iter().map(|v| v.annotate(ctx)).collect();
        StmtEnum::new(span, name, generics, variants)
    }
}

impl Variant {
    pub fn annotate(&self, ctx: &mut Context) -> Variant {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.annotate(ctx);
        Variant::new(span, name, ty)
    }
}

impl StmtVar {
    pub fn annotate(&self, ctx: &mut Context) -> StmtVar {
        let span = self.span;
        let x = self.name.clone();
        let t = self.ty.annotate(ctx);
        let e = self.expr.annotate(ctx);
        StmtVar::new(span, x, t, e)
    }
}

impl StmtDef {
    pub fn annotate(&self, ctx: &mut Context) -> StmtDef {
        let span = self.span;
        let name = self.name.clone();
        let generics = self.generics.clone();
        let preds = self.preds.iter().map(|p| p.annotate(ctx)).collect();
        let params = self.params.iter().map(|p| p.annotate(ctx)).collect();
        let ty = self.ty.annotate(ctx);
        let expr = self.expr.annotate(ctx);
        StmtDef::new(span, name, generics, preds, params, ty, expr)
    }
}

impl StmtImpl {
    pub fn annotate(&self, ctx: &mut Context) -> StmtImpl {
        let span = self.span;
        let generics = self.generics.clone();
        let head = self.head.annotate(ctx);
        let body = self.body.iter().map(|p| p.annotate(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.annotate(ctx)).collect();
        StmtImpl::new(span, generics, head, body, defs)
    }
}

impl StmtTrait {
    fn annotate(&self, ctx: &mut Context) -> StmtTrait {
        let span = self.span;
        let generics = self.generics.clone();
        let name = self.name.clone();
        let body = self.body.iter().map(|p| p.annotate(ctx)).collect();
        let defs = self.defs.iter().map(|d| d.annotate(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.annotate(ctx)))
            .collect();
        StmtTrait::new(span, name, generics, body, defs, assocs)
    }
}

impl Expr {
    pub fn annotate(&self, ctx: &mut Context) -> Expr {
        match self {
            Expr::Int(s, t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::Int(*s, t, v)
            }
            Expr::Float(s, t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::Float(*s, t, v)
            }
            Expr::Bool(s, t, v) => {
                let t = t.annotate(ctx);
                Expr::Bool(*s, t, *v)
            }
            Expr::String(s, t, v) => {
                let t = t.annotate(ctx);
                let v = v.clone();
                Expr::String(*s, t, v)
            }
            Expr::Unit(s, t) => {
                let t = t.annotate(ctx);
                Expr::Unit(*s, t)
            }
            Expr::Var(s, t, x, ts) => {
                let t = t.annotate(ctx);
                let x = x.clone();
                let ts = ts.into_iter().map(|t| t.annotate(ctx)).collect();
                Expr::Var(*s, t, x, ts)
            }
            Expr::Call(s, t, e, es) => {
                let t = t.annotate(ctx);
                let e = Box::new(e.annotate(ctx));
                let es = es.into_iter().map(|e| e.annotate(ctx)).collect();
                Expr::Call(*s, t, e, es)
            }
            Expr::Block(s, t, ss, e) => {
                let t = t.annotate(ctx);
                let ss = ss.into_iter().map(|s| s.annotate(ctx)).collect();
                let e = Box::new(e.annotate(ctx));
                Expr::Block(*s, t, ss, e)
            }
            Expr::From(..) => todo!(),
            Expr::Struct(..) => todo!(),
            Expr::Enum(..) => todo!(),
            Expr::Field(s, t, e, x) => {
                let t = t.annotate(ctx);
                let e = Box::new(e.annotate(ctx));
                let x = x.clone();
                Expr::Field(*s, t, e, x)
            }
            Expr::Tuple(..) => todo!(),
            Expr::Assoc(..) => todo!(),
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Err(s, t) => {
                let t = t.annotate(ctx);
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
    pub fn annotate(&self, ctx: &mut Context) -> Param {
        let span = self.span;
        let name = self.name.clone();
        let ty = self.ty.annotate(ctx);
        Param::new(span, name, ty)
    }
}

impl Trait {
    pub fn annotate(&self, ctx: &mut Context) -> Trait {
        let span = self.span;
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.annotate(ctx)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.annotate(ctx)))
            .collect();
        Trait::new(span, name, types, assocs)
    }
}
