use crate::data::Def;
use crate::data::Expr;
use crate::data::Impl;
use crate::data::Param;
use crate::data::Pred;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::Var;

macro_rules! fns {
    { $($arg:ty,)* } => {
        $(
            impl $arg {
                pub fn display(&self) -> Printer<Self> {
                    Printer { ctx: Context::default(), data: self }
                }
                pub fn display_with_types(&self) -> Printer<Self> {
                    Printer { ctx: Context::new(0, true), data: self }
                }
                #[allow(unused)]
                fn inner<T>(&self, d: &Printer<T>) -> Printer<Self> {
                    Printer { ctx: d.ctx, data: self }
                }
            }
        )*
    }
}

fns! {
    Program,
    Stmt,
    Var,
    Def,
    Param,
    Pred,
    Type,
    Expr,
    Impl,
}

pub struct Printer<'a, T> {
    ctx: Context,
    data: &'a T,
}

#[derive(Default, Copy, Clone)]
pub struct Context {
    indent: usize,
    type_info: bool,
}

impl<'a, T> Printer<'a, T> {
    fn tab(&'a self) -> Printer<'a, T> {
        Printer {
            ctx: Context::new(self.ctx.indent + 1, self.ctx.type_info),
            data: self.data,
        }
    }
    fn indent(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        (0..self.ctx.indent).try_for_each(|_| write!(f, "    "))
    }
}

impl Context {
    fn new(indent: usize, type_info: bool) -> Context {
        Context { indent, type_info }
    }
}

impl<'a, T> std::ops::Deref for Printer<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> std::fmt::Display for Printer<'a, Impl> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.head.inner(self))?;
        let mut iter = self.body.iter();
        if let Some(t) = iter.next() {
            write!(f, " :- {}", t.inner(self))?;
            for t in iter {
                write!(f, ", {}", t.inner(self))?;
            }
        }
        write!(f, ".")?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Pred> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        let mut iter = self.types.iter();
        if let Some(t) = iter.next() {
            write!(f, "(")?;
            write!(f, "{}", t.inner(self))?;
            for t in iter {
                write!(f, ", {}", t.inner(self))?;
            }
            let mut iter = self.assocs.iter();
            if let Some((x, t)) = iter.next() {
                write!(f, ", {}={}", x, t.inner(self))?;
                for (x, t) in iter {
                    write!(f, ", {}={}", x, t.inner(self))?;
                }
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.data {
            Type::Cons(x, ts) => {
                write!(f, "{x}")?;
                let mut iter = ts.iter();
                if let Some(t) = iter.next() {
                    write!(f, "[")?;
                    write!(f, "{}", t.inner(self))?;
                    for t in iter {
                        write!(f, ", {}", t.inner(self))?;
                    }
                    write!(f, "]")?;
                }
            }
            Type::Assoc(i, x) => {
                write!(f, "{}::{}", i.inner(self), x)?;
            }
            Type::Var(x) => {
                write!(f, "{x}")?;
            }
            Type::Hole => {
                write!(f, "_")?;
            }
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Stmt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data {
            Stmt::Var(v) => write!(f, "{}", v.inner(self))?,
            Stmt::Def(d) => write!(f, "{}", d.inner(self))?,
            Stmt::Impl(i) => write!(f, "{}", i.inner(self))?,
            Stmt::Expr(e) => write!(f, "{};", e.inner(self))?,
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {}: {} = {};",
            self.name,
            self.ty.inner(self),
            self.expr.inner(self)
        )
    }
}

impl<'a> std::fmt::Display for Printer<'a, Def> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "def {}", self.name)?;
        let mut iter = self.generics.iter();
        if let Some(g) = iter.next() {
            write!(f, "<")?;
            write!(f, "{g}")?;
            for g in iter {
                write!(f, ", {g}")?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        let mut iter = self.params.iter();
        if let Some(p) = iter.next() {
            write!(f, "{}", p.inner(self))?;
            for p in iter {
                write!(f, ", {}", p.inner(self))?;
            }
        }
        write!(f, "): {}", self.ty.inner(self))?;
        let mut iter = self.preds.iter();
        if let Some(i) = iter.next() {
            write!(f, " where {}", i.inner(self))?;
            for i in iter {
                write!(f, ", {}", i.inner(self))?;
            }
        }
        write!(f, " = {}; ", self.expr.inner(self))?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Param> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.inner(self))
    }
}

impl<'a> std::fmt::Display for Printer<'a, Expr> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.ctx.type_info {
            write!(f, "<")?;
        }
        match self.data {
            Expr::Int(_, i) => {
                write!(f, "{i}")?;
            }
            Expr::Float(_, i) => {
                write!(f, "{i}")?;
            }
            Expr::Bool(_, b) => {
                write!(f, "{b}")?;
            }
            Expr::Var(_, x) => {
                write!(f, "{x}")?;
            }
            Expr::Call(_, e, es) => {
                write!(f, "{}", e.inner(self))?;
                write!(f, "(")?;
                let mut iter = es.iter();
                if let Some(e) = iter.next() {
                    write!(f, "{}", e.inner(self))?;
                    for e in iter {
                        write!(f, ", {}", e.inner(self))?;
                    }
                }
                write!(f, ")")?;
            }
            Expr::Block(_, ss, e) => {
                write!(f, "do {{")?;
                for s in ss {
                    self.indent(f)?;
                    write!(f, "{}", s.inner(&self.tab()))?;
                }
                write!(f, "{}", e.inner(self))?;
                write!(f, "}}")?;
            }
            Expr::From(..) => todo!(),
        }
        if self.ctx.type_info {
            write!(f, ":{}>", self.type_of().inner(self))?;
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Program> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.stmts {
            write!(f, "{}", s.inner(self))?;
        }
        Ok(())
    }
}
