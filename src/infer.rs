use crate::ast::Expr;
use crate::ast::Name;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtImpl;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;
use crate::diag::Report;
use crate::lexer::Span;
use crate::solver::solve;
use crate::solver::unify;
use crate::util::t;

pub struct Context {
    tvars: usize,
    stack: Vec<Scope>,
    pub report: Report,
}

pub struct Scope {
    impls: Vec<StmtImpl>,
    binds: Vec<(Name, Binding)>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            impls: vec![],
            binds: vec![],
        }
    }
}

// pub struct Goal {}

// pub struct ImplDef {
//     generics: Vec<Name>,
//     def: Def,
//     imp: Pred,
// }

#[derive(Debug, Clone)]
pub enum Binding {
    Var(Span, Type),
    Def(Span, Vec<Name>, Vec<Trait>, Vec<Type>, Type),
    Struct(Span, Vec<Name>, Vec<(Name, Type)>),
    Enum(Span, Vec<Name>, Vec<(Name, Type)>),
    Trait(Span, StmtImpl),
}

impl Context {
    pub fn new() -> Context {
        Context {
            tvars: 0,
            stack: vec![Scope::new()],
            report: Report::new(),
        }
    }

    pub fn recover<T>(&mut self, s0: Span, s1: Span, r: Result<T, (Type, Type)>) {
        if let Err((t0, t1)) = r {
            self.report.err2(
                s0,
                s1,
                "Type mismatch",
                format!("Expected {t0}"),
                format!("Found {t1}"),
            );
        }
    }

    pub fn new_tyvar(&mut self) -> Type {
        self.tvars += 1;
        Type::Var(Name::from(format!("_T{}", self.tvars - 1)))
    }

    pub fn get(&self, x1: &Name) -> Option<&Binding> {
        self.stack
            .iter()
            .rev()
            .find_map(|s| s.binds.iter().find(|(x0, _)| x0 == x1).map(|(_, b)| b))
    }

    pub fn bind(&mut self, name: Name, b: Binding) {
        self.stack.last_mut().unwrap().binds.push((name, b));
    }

    pub fn add_impl(&mut self, i: StmtImpl) {
        self.stack.last_mut().unwrap().impls.push(i);
    }

    pub fn impls(&self) -> Vec<StmtImpl> {
        self.stack
            .iter()
            .rev()
            .flat_map(|s| s.impls.clone())
            .collect()
    }
}

impl Program {
    pub fn infer(&self, ctx: &mut Context) -> Program {
        let mut goals = Vec::new();
        let mut sub = Vec::new();
        let program = self.annotate(ctx);
        let stmts = program
            .stmts
            .into_iter()
            .map(|s| s.infer(&mut sub, &mut goals, ctx))
            .collect::<Vec<_>>();
        let program = Program::new(stmts);
        let impls = ctx.impls();
        for goal in goals {
            if solve(&goal, &impls, &[], &mut sub, ctx).is_none() {
                ctx.report
                    .err(goal.span, "Unsolved goal", "Could not solve goal");
            }
        }
        program.apply(&sub)
    }
}

impl Stmt {
    pub fn infer(
        &self,
        sub: &mut Vec<(Name, Type)>,
        goals: &mut Vec<Trait>,
        ctx: &mut Context,
    ) -> Stmt {
        match self {
            Stmt::Var(s) => Stmt::Var(s.infer(sub, goals, ctx)),
            Stmt::Def(s) => Stmt::Def(s.infer(ctx)),
            Stmt::Impl(s) => Stmt::Impl(s.infer(ctx)),
            Stmt::Expr(s) => Stmt::Expr(s.infer(sub, goals, ctx)),
            Stmt::Struct(s) => Stmt::Struct(s.clone()),
            Stmt::Enum(s) => Stmt::Enum(s.clone()),
            Stmt::Type(s) => Stmt::Type(s.clone()),
            Stmt::Trait(s) => Stmt::Trait(s.clone()),
        }
    }
}

impl StmtVar {
    pub fn infer(
        &self,
        sub: &mut Vec<(Name, Type)>,
        goals: &mut Vec<Trait>,
        ctx: &mut Context,
    ) -> StmtVar {
        let e = self.expr.infer(sub, goals, ctx);
        ctx.recover(self.span, e.span(), unify(sub, &self.ty, e.ty()));
        ctx.bind(self.name.clone(), Binding::Var(self.span, self.ty.clone()));
        StmtVar::new(e.span(), self.name.clone(), self.ty.clone(), e)
    }
}

impl StmtImpl {
    pub fn infer(&self, _ctx: &mut Context) -> StmtImpl {
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

impl StmtDef {
    pub fn infer(&self, ctx: &mut Context) -> StmtDef {
        let mut sub = vec![];
        let mut goals = vec![];
        for p in &self.params {
            ctx.bind(p.name.clone(), Binding::Var(p.span, p.ty.clone()));
        }
        let e = self.expr.infer(&mut sub, &mut goals, ctx);
        let ts = self.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>();
        ctx.recover(self.span, e.span(), unify(&mut sub, &self.ty, e.ty()));
        let d = self.apply(&sub);
        let impls = ctx.impls();
        for goal in goals {
            // TODO: Try to solve each goal multiple times
            if solve(&goal, &impls, &self.where_clause, &mut sub, ctx).is_none() {
                ctx.report
                    .err(goal.span, "Unsolved goal", "Could not solve goal");
            }
        }
        ctx.bind(
            self.name.clone(),
            Binding::Def(
                self.span,
                self.generics.clone(),
                self.where_clause.clone(),
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
        goals: &mut Vec<Trait>,
        ctx: &mut Context,
    ) -> Expr {
        let s = self.span();
        match self {
            Expr::Int(_, t0, v) => {
                let t1 = t("i32");
                let v = v.clone();
                ctx.recover(s, s, unify(sub, &t0, &t1));
                Expr::Int(s, t0.clone(), v)
            }
            Expr::Float(_, t0, v) => {
                let t1 = t("f32");
                let v = v.clone();
                ctx.recover(s, s, unify(sub, &t0, &t1));
                Expr::Float(s, t0.clone(), v)
            }
            Expr::Bool(_, t0, v) => {
                let t1 = t("bool");
                let v = *v;
                ctx.recover(s, s, unify(sub, &t0, &t1));
                Expr::Bool(s, t0.clone(), v)
            }
            Expr::String(_, t0, v) => {
                let t1 = t("String");
                let v = v.clone();
                ctx.recover(s, s, unify(sub, &t0, &t1));
                Expr::String(s, t0.clone(), v)
            }
            Expr::Struct(_, _t0, _x, _ts, _xes) => {
                todo!()
            }
            Expr::Enum(_, _t0, _x, _ts, _x1, _xes) => {
                todo!()
            }
            Expr::Tuple(_, _t0, _xes) => {
                todo!()
            }
            Expr::Var(_, t0, x, ts0) => match ctx.get(&x).unwrap().clone() {
                Binding::Var(s1, t1) => {
                    ctx.recover(s, s1, unify(sub, &t0, &t1));
                    Expr::Var(s, t0.clone(), x.clone(), ts0.clone())
                }
                Binding::Def(s1, gs, preds, ts, t1) => {
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
                    let t2 = Type::Cons(Name::from("Fun"), ts);
                    ctx.recover(s, s1, unify(sub, &t0, &t2));
                    Expr::Var(s, t0.clone(), x.clone(), ts0.clone())
                }
                _ => unreachable!(),
            },
            Expr::Call(s, t0, e, es) => {
                let e = e.infer(sub, goals, ctx);
                let es = es
                    .into_iter()
                    .map(|e| e.infer(sub, goals, ctx))
                    .collect::<Vec<_>>();
                let mut ts = es.iter().map(|e| e.ty().clone()).collect::<Vec<_>>();
                ts.push(t0.clone());
                let t2 = Type::Cons(Name::from("Fun"), ts);
                ctx.recover(*s, e.span(), unify(sub, &t2, &e.ty()));
                Expr::Call(self.span(), t0.clone(), e.into(), es)
            }
            Expr::Block(s, t0, ss, e) => {
                let ss = ss.into_iter().map(|s| s.infer(sub, goals, ctx)).collect();
                let e = e.infer(sub, goals, ctx);
                ctx.recover(*s, e.span(), unify(sub, &t0, &e.ty()));
                Expr::Block(*s, t0.clone(), ss, e.into())
            }
            Expr::Query(..) => todo!(),
            Expr::Field(..) => todo!(),
            Expr::Assoc(..) => todo!(),
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Err(s, t) => Expr::Err(*s, t.clone()),
            Expr::Assign(_, _, _, _) => todo!(),
            Expr::Return(_, _, _) => todo!(),
            Expr::Continue(_, _) => todo!(),
            Expr::Break(_, _) => todo!(),
            Expr::Fun(_, _, _, _, _) => todo!(),
            Expr::And(_, _, _, _) => todo!(),
            Expr::Or(_, _, _, _) => todo!(),
            Expr::Match(_, _, _, _) => todo!(),
            Expr::While(_, _, _, _) => todo!(),
            Expr::Record(_, _, _) => todo!(),
        }
    }
}
