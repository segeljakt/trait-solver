use crate::data::Binding;
use crate::data::CompilerError;
use crate::data::Context;
use crate::data::Def;
use crate::data::Expr;
use crate::data::Impl;
use crate::data::Name;
use crate::data::Pred;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::Var;
use crate::solve;
use crate::unify;

impl Program {
    pub fn infer(&self) -> Result<Program, Vec<CompilerError>> {
        let mut ctx = Context::new();
        let mut goals = Vec::new();
        let mut sub = Vec::new();
        let program = self.annotate(&mut ctx);
        let stmts = program
            .stmts
            .into_iter()
            .map(|s| s.infer(&mut sub, &mut goals, &mut ctx))
            .collect::<Vec<_>>();
        let program = Program::new(stmts);
        let impls = ctx.impls();
        for goal in goals {
            if let Err(e) = solve(&goal, &impls, &[], &mut sub, &mut ctx) {
                ctx.errors.push(e);
            }
        }
        if ctx.errors.is_empty() {
            let program = program.apply(&sub);
            Ok(program)
        } else {
            Err(ctx.errors)
        }
    }
}

impl Stmt {
    pub fn infer(
        &self,
        sub: &mut Vec<(Name, Type)>,
        goals: &mut Vec<Pred>,
        ctx: &mut Context,
    ) -> Stmt {
        match self {
            Stmt::Var(v) => Stmt::Var(v.infer(sub, goals, ctx)),
            Stmt::Def(d) => Stmt::Def(d.infer(ctx)),
            Stmt::Impl(i) => Stmt::Impl(i.infer(ctx)),
            Stmt::Expr(e) => {
                let e = e.infer(sub, goals, ctx);
                Stmt::Expr(e)
            }
        }
    }
}

impl Var {
    pub fn infer(
        &self,
        sub: &mut Vec<(Name, Type)>,
        goals: &mut Vec<Pred>,
        ctx: &mut Context,
    ) -> Var {
        let e = self.expr.infer(sub, goals, ctx);
        ctx.recover(unify(sub, &self.ty, e.type_of()));
        ctx.bind(self.name.clone(), Binding::Var(self.ty.clone()));
        Var::new(self.name.clone(), self.ty.clone(), e)
    }
}

impl Impl {
    pub fn infer(&self, _ctx: &mut Context) -> Impl {
        // ctx.add_impl(self.clone());
        // for d in ds {
        //     // let impl_def = ImplDef
        //     d.infer(ctx);
        // }
        // let ds = ds.into_iter().map(|d| d.infer(ctx)).collect::<Vec<_>>();
        // Stmt::Impl(i.clone())
        todo!()
    }
}

impl Def {
    pub fn infer(&self, ctx: &mut Context) -> Def {
        let mut sub = vec![];
        let mut goals = vec![];
        for p in &self.params {
            ctx.bind(p.name.clone(), Binding::Var(p.ty.clone()));
        }
        let e = self.expr.infer(&mut sub, &mut goals, ctx);
        let ts = self.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>();
        ctx.recover(unify(&mut sub, &self.ty, e.type_of()));
        let d = self.apply(&sub);
        let impls = ctx.impls();
        for goal in goals {
            // TODO: Try to solve each goal multiple times
            if let Err(e) = solve(&goal, &impls, &self.preds, &mut sub, ctx) {
                ctx.errors.push(e);
            }
        }
        ctx.bind(
            self.name.clone(),
            Binding::Def(
                self.generics.clone(),
                self.preds.clone(),
                ts,
                self.ty.clone(),
            ),
        );
        d
    }
}

impl Expr {
    pub fn infer(
        &self,
        sub: &mut Vec<(Name, Type)>,
        goals: &mut Vec<Pred>,
        ctx: &mut Context,
    ) -> Expr {
        match self {
            Expr::Int(t0, v) => {
                let t1 = Type::Cons("i32".to_string(), vec![]);
                let v = v.clone();
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Int(t0.clone(), v)
            }
            Expr::Float(t0, v) => {
                let t1 = Type::Cons("f32".to_string(), vec![]);
                let v = v.clone();
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Float(t0.clone(), v)
            }
            Expr::Bool(t0, v) => {
                let t1 = Type::Cons("bool".to_string(), vec![]);
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Bool(t0.clone(), *v)
            }
            Expr::Var(t0, x) => match ctx.get(&x).expect("variable not found").clone() {
                Binding::Var(t1) => {
                    ctx.recover(unify(sub, &t0, &t1));
                    Expr::Var(t0.clone(), x.clone())
                }
                Binding::Def(gs, preds, ts, t1) => {
                    let gsub = gs
                        .iter()
                        .map(|x| (x.clone(), ctx.new_tyvar()))
                        .collect::<Vec<_>>();
                    preds.into_iter().for_each(|p| {
                        p.apply(&gsub);
                        goals.push(p);
                    });
                    let mut ts = ts.into_iter().map(|t| t.apply(&gsub)).collect::<Vec<_>>();
                    let t1 = t1.apply(&gsub);
                    ts.push(t1);
                    let t2 = Type::Cons("fun".to_string(), ts);
                    ctx.recover(unify(sub, &t0, &t2));
                    Expr::Var(t0.clone(), x.clone())
                }
                Binding::Impl(_) => todo!(),
            },
            Expr::Call(t0, e, es) => {
                let e = e.infer(sub, goals, ctx);
                let es = es
                    .into_iter()
                    .map(|e| e.infer(sub, goals, ctx))
                    .collect::<Vec<_>>();
                let mut ts = vec![t0.clone()];
                let ts0 = es.iter().map(|e| e.type_of().clone()).collect::<Vec<_>>();
                ts.extend(ts0);
                let t2 = Type::Cons("fun".to_string(), ts.clone());
                ctx.recover(unify(sub, &e.type_of(), &t2));
                Expr::Call(t0.clone(), e.into(), es)
            }
            Expr::Block(t0, ss, e) => {
                let ss = ss.into_iter().map(|s| s.infer(sub, goals, ctx)).collect();
                let e = e.infer(sub, goals, ctx);
                ctx.recover(unify(sub, &t0, &e.type_of()));
                Expr::Block(t0.clone(), ss, e.into())
            }
            Expr::From(..) => todo!(),
        }
    }
}
