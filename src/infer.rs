use crate::data::CompilerError;
use crate::data::StmtDef;
use crate::data::Expr;
use crate::data::StmtImpl;
use crate::data::Name;
use crate::data::Trait;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::StmtVar;
use crate::solve;
use crate::unify;

pub struct Context {
    tvars: usize,
    stack: Vec<Scope>,
    pub errors: Vec<CompilerError>,
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
    Var(Type),
    Def(Vec<Name>, Vec<Trait>, Vec<Type>, Type),
    Impl(StmtImpl),
}

impl Context {
    pub fn new() -> Context {
        Context {
            tvars: 0,
            stack: vec![Scope::new()],
            errors: vec![],
        }
    }

    pub fn recover<T>(&mut self, r: Result<T, CompilerError>) {
        if let Err(e) = r {
            self.errors.push(e);
        }
    }

    pub fn new_tyvar(&mut self) -> Type {
        self.tvars += 1;
        Type::Var(format!("?T{}", self.tvars - 1))
    }

    pub fn get(&self, x1: &str) -> Option<&Binding> {
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
        goals: &mut Vec<Trait>,
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
            Stmt::Struct(_) => todo!(),
            Stmt::Enum(_) => todo!(),
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
        ctx.recover(unify(sub, &self.ty, e.type_of()));
        ctx.bind(self.name.clone(), Binding::Var(self.ty.clone()));
        StmtVar::new(self.name.clone(), self.ty.clone(), e)
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
        goals: &mut Vec<Trait>,
        ctx: &mut Context,
    ) -> Expr {
        match self {
            Expr::Int(t0, v) => {
                let t1 = Type::atom("i32".to_string());
                let v = v.clone();
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Int(t0.clone(), v)
            }
            Expr::Float(t0, v) => {
                let t1 = Type::atom("f32".to_string());
                let v = v.clone();
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Float(t0.clone(), v)
            }
            Expr::Bool(t0, v) => {
                let t1 = Type::atom("bool".to_string());
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Bool(t0.clone(), *v)
            }
            Expr::String(t0, v) => {
                let t1 = Type::atom("String".to_string());
                let v = v.clone();
                ctx.recover(unify(sub, &t0, &t1));
                Expr::String(t0.clone(), v)
            }
            Expr::Unit(t0) => {
                let t1 = Type::atom("()".to_string());
                ctx.recover(unify(sub, &t0, &t1));
                Expr::Unit(t0.clone())
            }
            Expr::Struct(_t0, _xes, _) => {
                todo!()
            }
            Expr::Enum(_t0, _x, _xes, _) => {
                todo!()
            }
            Expr::Tuple(_t0, _xes) => {
                todo!()
            }
            Expr::Var(t0, x) => match ctx.get(&x).unwrap().clone() {
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
            Expr::Field(_, _, _) => todo!(),
            Expr::Assoc(_, _, _) => todo!(),
        }
    }
}
