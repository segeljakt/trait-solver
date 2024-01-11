use crate::data::Name;
use crate::data::StmtDef;
use crate::data::StmtEnum;
use crate::data::Expr;
use crate::data::StmtImpl;
use crate::data::Param;
use crate::data::Trait;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::StmtStruct;
use crate::data::Type;
use crate::data::StmtVar;

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
            impl std::fmt::Display for $arg {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    self.display().fmt(f)
                }
            }
        )*
    }
}

fns! {
    Program,
    Stmt,
    StmtVar,
    StmtDef,
    Param,
    Trait,
    StmtStruct,
    StmtEnum,
    Type,
    Expr,
    StmtImpl,
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

impl<'a> std::fmt::Display for Printer<'a, StmtImpl> {
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

impl<'a> std::fmt::Display for Printer<'a, Trait> {
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
                write!(f, "{}.{}", i.inner(self), x)?;
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
            Stmt::Var(v) => write!(f, "{}", v.inner(self)),
            Stmt::Def(d) => write!(f, "{}", d.inner(self)),
            Stmt::Impl(i) => write!(f, "{}", i.inner(self)),
            Stmt::Expr(e) => write!(f, "{};", e.inner(self)),
            Stmt::Struct(s) => write!(f, "{}", s.inner(self)),
            Stmt::Enum(e) => write!(f, "{}", e.inner(self)),
        }
    }
}

impl<'a> std::fmt::Display for Printer<'a, StmtVar> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "var {}: {} = {};",
            self.name,
            self.ty.inner(self),
            self.expr.inner(self)
        )
    }
}

impl<'a> std::fmt::Display for Printer<'a, StmtDef> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "def {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            sep(f, &self.generics, |f, g| write!(f, "{g}"))?;
            write!(f, "]")?;
        }
        write!(f, "(")?;
        sep(f, &self.params, |f, p| write!(f, "{}", p.inner(self)))?;
        write!(f, "): {}", self.ty.inner(self))?;
        if !self.preds.is_empty() {
            write!(f, " where ")?;
            sep(f, &self.preds, |f, p| write!(f, "{}", p.inner(self)))?;
        }
        write!(f, " = {}; ", self.expr.inner(self))?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Param> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.inner(self))?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, StmtStruct> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            sep(f, &self.generics, |f, g| write!(f, "{g}"))?;
            write!(f, "]")?;
        }
        write!(f, " {{")?;
        sep(f, &self.fields, |f, (x, t)| {
            write!(f, "{}: {}", x, t.inner(self))
        })?;
        write!(f, "}}")?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, StmtEnum> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            sep(f, &self.generics, |f, g| write!(f, "{g}"))?;
            write!(f, "]")?;
        }
        write!(f, " {{")?;
        sep(f, &self.variants, |f, (x, t)| {
            write!(f, "{}({})", x, t.inner(self))
        })?;
        write!(f, "}}")?;
        Ok(())
    }
}

fn sep<'a, T: 'a>(
    f: &mut std::fmt::Formatter,
    iter: impl IntoIterator<Item = &'a T>,
    fun: impl Fn(&mut std::fmt::Formatter, &'a T) -> std::fmt::Result,
) -> std::fmt::Result {
    let mut iter = iter.into_iter();
    if let Some(x) = iter.next() {
        fun(f, x)?;
        for x in iter {
            write!(f, ", ")?;
            fun(f, x)?;
        }
    }
    Ok(())
}

impl<'a> std::fmt::Display for Printer<'a, Expr> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.ctx.type_info {
            write!(f, "<")?;
        }
        match self.data {
            Expr::Int(_, _, i) => {
                write!(f, "{i}")?;
            }
            Expr::Float(_, _, i) => {
                write!(f, "{i}")?;
            }
            Expr::Bool(_, _, b) => {
                write!(f, "{b}")?;
            }
            Expr::String(_, _, s) => {
                write!(f, r#""{s}""#)?;
            }
            Expr::Unit(_, _) => {
                write!(f, "()")?;
            }
            Expr::Field(_, _, e, x) => {
                write!(f, "{}.{}", e.inner(self), x)?;
            }
            Expr::Tuple(_, _, es) => {
                write!(f, "(")?;
                sep(f, es, |f, e| write!(f, "{}", e.inner(self)))?;
                write!(f, ")")?;
            }
            Expr::Struct(_, _, x, xes) => {
                write!(f, "{}", x)?;
                write!(f, "{{")?;
                sep(f, xes, |f, (x, e)| {
                    write!(f, "{x}: {}", e.inner(&self.tab()))
                })?;
                write!(f, "}}")?;
            }
            Expr::Enum(_, _, _, _, _s) => {
                todo!()
            }
            Expr::Var(_, _, x) => {
                write!(f, "{x}")?;
            }
            Expr::Call(_, _, e, es) => {
                write!(f, "{}", e.inner(self))?;
                write!(f, "(")?;
                sep(f, es, |f, e| write!(f, "{}", e.inner(self)))?;
                write!(f, ")")?;
            }
            Expr::Block(_, _, ss, e) => {
                write!(f, "do {{")?;
                for s in ss {
                    self.indent(f)?;
                    write!(f, "{}", s.inner(&self.tab()))?;
                }
                write!(f, "{}", e.inner(self))?;
                write!(f, "}}")?;
            }
            Expr::From(..) => todo!(),
            Expr::Assoc(_, _, tr, x) => write!(f, "{}.{}", tr, x)?,
            Expr::Err(_, _) => todo!(),
        }
        if self.ctx.type_info {
            write!(f, ":{}>", self.ty().inner(self))?;
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

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}
