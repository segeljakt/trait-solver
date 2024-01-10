use crate::data::StmtDef;
use crate::data::Expr;
use crate::data::StmtImpl;
use crate::data::Name;
use crate::data::Param;
use crate::data::Trait;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::StmtVar;

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
        let name = self.name.clone();
        let ty = self.ty.apply(sub);
        let expr = self.expr.apply(sub);
        StmtVar::new(name, ty, expr)
    }
}

impl StmtDef {
    pub fn apply(&self, sub: &[(Name, Type)]) -> StmtDef {
        let name = self.name.clone();
        let generics = self.generics.clone();
        let qs = self.preds.iter().map(|p| p.apply(sub)).collect();
        let ps = self.params.iter().map(|p| p.apply(sub)).collect();
        let t = self.ty.apply(sub);
        let e = self.expr.apply(sub);
        StmtDef::new(name, generics, qs, ps, t, e)
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
        let name = self.name.clone();
        let types = self.types.iter().map(|t| t.apply(sub)).collect();
        let assocs = self
            .assocs
            .iter()
            .map(|(x, t)| (x.clone(), t.apply(sub)))
            .collect();
        Trait::new(name, types, assocs)
    }
}

impl Expr {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Expr {
        match self {
            Expr::Int(t, v) => {
                let t = t.apply(sub);
                let v = v.clone();
                Expr::Int(t, v)
            }
            Expr::Float(t, v) => {
                let t = t.apply(sub);
                let v = v.clone();
                Expr::Float(t, v)
            }
            Expr::Bool(t, b) => {
                let t = t.apply(sub);
                Expr::Bool(t, *b)
            }
            Expr::String(t, s) => {
                let t = t.apply(sub);
                let s = s.clone();
                Expr::String(t, s)
            }
            Expr::Unit(t) => {
                let t = t.apply(sub);
                Expr::Unit(t)
            }
            Expr::Var(t, x) => {
                let t = t.apply(sub);
                let x = x.clone();
                Expr::Var(t, x)
            }
            Expr::Call(t, e, es) => {
                let t = t.apply(sub);
                let e = Box::new(e.apply(sub));
                let es = es.iter().map(|e| e.apply(sub)).collect();
                Expr::Call(t, e, es)
            }
            Expr::Block(t, ss, e) => {
                let t = t.apply(sub);
                let ss = ss.iter().map(|s| s.apply(sub)).collect();
                let e = Box::new(e.apply(sub));
                Expr::Block(t, ss, e)
            }
            Expr::From(..) => todo!(),
            Expr::Struct(_, _, _) => todo!(),
            Expr::Enum(_, _, _, _) => todo!(),
            Expr::Field(_, _, _) => todo!(),
            Expr::Tuple(_, _) => todo!(),
            Expr::Assoc(_, _, _) => todo!(),
        }
    }
}

impl Param {
    pub fn apply(&self, sub: &[(Name, Type)]) -> Param {
        let name = self.name.clone();
        let t = self.ty.apply(sub);
        Param::new(name, t)
    }
}

impl StmtImpl {
    pub fn apply(&self, sub: &[(Name, Type)]) -> StmtImpl {
        let generics = self.generics.clone();
        let head = self.head.apply(sub);
        let body = self.body.iter().map(|p| p.apply(sub)).collect();
        let defs = self.defs.iter().map(|d| d.apply(sub)).collect();
        StmtImpl::new(generics, head, body, defs)
    }
}
