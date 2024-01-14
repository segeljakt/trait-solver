use crate::ast::Expr;
use crate::ast::Index;
use crate::ast::Name;
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.display().fmt(f)
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.display().fmt(f)
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display().fmt(f)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display().fmt(f)
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display().fmt(f)
    }
}

pub trait Print: Sized {
    fn display(&self) -> Printer<Self> {
        Printer {
            ctx: Context::default(),
            data: self,
        }
    }
    fn display_with_types(&self) -> Printer<Self> {
        Printer {
            ctx: Context::new(0, true),
            data: self,
        }
    }
    fn inner<T>(&self, d: &Printer<T>) -> Printer<Self> {
        Printer {
            ctx: d.ctx,
            data: self,
        }
    }
}

impl Print for Expr {}
impl Print for Param {}
impl Print for Program {}
impl Print for Stmt {}
impl Print for StmtDef {}
impl Print for StmtEnum {}
impl Print for StmtImpl {}
impl Print for StmtStruct {}
impl Print for StmtType {}
impl Print for StmtVar {}
impl Print for StmtTrait {}
impl Print for Trait {}
impl Print for Type {}
impl Print for Name {}
impl Print for Variant {}
impl Print for Index {}

pub struct Printer<'a, T> {
    ctx: Context,
    data: &'a T,
}

#[derive(Default, Copy, Clone)]
pub struct Context {
    indent: usize,
    type_info: bool,
}

impl<'a, T> Printer<'a, T>
where
    T: Print,
    Printer<'a, T>: std::fmt::Display,
{
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
            Stmt::Var(s) => write!(f, "{}", s.inner(self)),
            Stmt::Def(s) => write!(f, "{}", s.inner(self)),
            Stmt::Impl(s) => write!(f, "{}", s.inner(self)),
            Stmt::Expr(s) => write!(f, "{};", s.inner(self)),
            Stmt::Struct(s) => write!(f, "{}", s.inner(self)),
            Stmt::Enum(s) => write!(f, "{}", s.inner(self)),
            Stmt::Type(s) => write!(f, "{}", s.inner(self)),
            Stmt::Trait(s) => write!(f, "{}", s.inner(self)),
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

impl<'a> std::fmt::Display for Printer<'a, StmtType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            sep(f, &self.generics, |f, g| write!(f, "{g}"))?;
            write!(f, "]")?;
        }
        write!(f, " = {};", self.ty.inner(self))?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, StmtTrait> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "trait {}", self.name)?;
        if !self.generics.is_empty() {
            brack(f, |f| sep(f, &self.generics, |f, g| write!(f, "{g}")))?;
        }
        write!(f, " ")?;
        brace(f, |f| {
            sep(f, &self.defs, |f, d| write!(f, "{}", d.inner(self)))
        })?;
        Ok(())
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
        write!(f, " ")?;
        write!(f, "{{")?;
        sep(f, &self.variants, |f, v| write!(f, "{}", v.inner(self)))?;
        write!(f, "}}")?;
        Ok(())
    }
}

impl<'a> std::fmt::Display for Printer<'a, Variant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.name, self.ty.inner(self))
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

fn group(
    f: &mut std::fmt::Formatter,
    fun: impl Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
    l: &str,
    r: &str,
) -> std::fmt::Result {
    write!(f, "{l}")?;
    fun(f)?;
    write!(f, "{r}")?;
    Ok(())
}

fn brack(
    f: &mut std::fmt::Formatter,
    fun: impl Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
) -> std::fmt::Result {
    group(f, fun, "[", "]")
}

#[allow(unused)]
fn paren(
    f: &mut std::fmt::Formatter,
    fun: impl Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
) -> std::fmt::Result {
    group(f, fun, "(", ")")
}

fn brace(
    f: &mut std::fmt::Formatter,
    fun: impl Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
) -> std::fmt::Result {
    group(f, fun, "{", "}")
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
            Expr::Var(_, _, x, ts) => {
                write!(f, "{x}")?;
                sep(f, ts, |f, t| write!(f, "{}", t.inner(self)))?;
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
            Expr::Assoc(_, _, tr, x, ts) => {
                write!(f, "{}::{}", tr.display(), x)?;
                if !ts.is_empty() {
                    write!(f, "[")?;
                    sep(f, ts, |f, t| write!(f, "{}", t.inner(self)))?;
                    write!(f, "]")?;
                }
            }
            Expr::Index(_, _, e, i) => {
                write!(f, "{}[{}]", e.inner(self), i.inner(self))?;
            }
            Expr::Array(_, _, es) => {
                write!(f, "[")?;
                sep(f, es, |f, e| write!(f, "{}", e.inner(self)))?;
                write!(f, "]")?;
            }
            Expr::Err(_, _) => write!(f, "<err>")?,
            Expr::Assign(_, _, e0, e1) => {
                write!(f, "{} = {}", e0.inner(self), e1.inner(self))?;
            }
            Expr::Return(_, _, e) => {
                write!(f, "return {}", e.inner(self))?;
            }
            Expr::Continue(_, _) => {
                write!(f, "continue")?;
            }
            Expr::Break(_, _) => write!(f, "break")?,
            Expr::Fun(_, _, _, _, _) => todo!(),
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

impl<'a> std::fmt::Display for Printer<'a, Name> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.data)
    }
}

impl<'a> std::fmt::Display for Printer<'a, Index> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.index)
    }
}
