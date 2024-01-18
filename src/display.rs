#![allow(unused)]
use crate::ast::Expr;
use crate::ast::Index;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Pat;
use crate::ast::Program;
use crate::ast::Query;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtDefDecl;
use crate::ast::StmtEnum;
use crate::ast::StmtImpl;
use crate::ast::StmtStruct;
use crate::ast::StmtTrait;
use crate::ast::StmtType;
use crate::ast::StmtTypeDecl;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;

pub struct Wrapper<T> {
    v: T,
}

impl<'a> std::fmt::Display for Wrapper<&'a Program> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut p = Printer::new(f);
        p.type_info = true;
        p.program(self.v)
    }
}

impl Program {
    pub fn display_types(&self) -> Wrapper<&Self> {
        Wrapper { v: self }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Printer::new(f).expr(self)
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Printer::new(f).program(self)
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).stmt(self)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).ty(self)
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).name(self)
    }
}

impl std::fmt::Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).pat(self)
    }
}

impl std::fmt::Display for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).tr(self)
    }
}

impl std::fmt::Display for StmtImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Printer::new(f).stmt_impl(self)
    }
}

pub struct Printer<'a, 'b> {
    f: &'a mut std::fmt::Formatter<'b>,
    noindent: bool,
    indent: usize,
    type_info: bool,
}

impl<'a, 'b> Printer<'a, 'b> {
    fn new(f: &'a mut std::fmt::Formatter<'b>) -> Printer<'a, 'b> {
        Printer {
            f,
            noindent: false,
            indent: 0,
            type_info: false,
        }
    }

    fn indent(&mut self) -> std::fmt::Result {
        if !self.noindent {
            for _ in 0..self.indent {
                write!(self.f, "    ")?;
            }
        }
        Ok(())
    }

    fn kw(&mut self, s: &str) -> std::fmt::Result {
        write!(self.f, "{}", s)
    }

    fn space(&mut self) -> std::fmt::Result {
        write!(self.f, " ")
    }

    fn punct(&mut self, s: &str) -> std::fmt::Result {
        write!(self.f, "{}", s)
    }

    fn newline(&mut self) -> std::fmt::Result {
        if !self.noindent {
            write!(self.f, "\n")?;
        }
        self.indent()
    }

    fn comma_sep<'c, T: 'c>(
        &mut self,
        iter: impl IntoIterator<Item = &'c T>,
        f: impl Fn(&mut Self, &'c T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        let mut iter = iter.into_iter();
        if let Some(x) = iter.next() {
            f(self, x)?;
            for x in iter {
                self.punct(",")?;
                self.space()?;
                f(self, x)?;
            }
        }
        Ok(())
    }

    fn comma_sep_trailing<'c, T: 'c>(
        &mut self,
        iter: impl IntoIterator<Item = &'c T>,
        f: impl Fn(&mut Self, &'c T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        let mut iter = iter.into_iter();
        if let Some(x) = iter.next() {
            f(self, x)?;
            self.punct(",")?;
            if let Some(x) = iter.next() {
                self.space()?;
                f(self, x)?;
                for x in iter {
                    self.punct(",")?;
                    self.space()?;
                    f(self, x)?;
                }
            }
        }
        Ok(())
    }

    fn newline_comma_sep<'c, T: 'c>(
        &mut self,
        iter: impl IntoIterator<Item = &'c T>,
        f: impl Fn(&mut Self, &'c T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        let mut iter = iter.into_iter();
        if let Some(x) = iter.next() {
            self.newline()?;
            f(self, x)?;
            self.punct(",")?;
            for x in iter {
                self.newline()?;
                f(self, x)?;
                self.punct(",")?;
            }
        }
        Ok(())
    }

    fn newline_sep<'c, T: 'c>(
        &mut self,
        iter: impl IntoIterator<Item = &'c T>,
        f: impl Fn(&mut Self, &'c T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        let mut iter = iter.into_iter();
        if let Some(x) = iter.next() {
            self.newline()?;
            f(self, x)?;
            for x in iter {
                self.newline()?;
                f(self, x)?;
            }
        }
        Ok(())
    }

    fn group(
        &mut self,
        fun: impl Fn(&mut Self) -> std::fmt::Result,
        l: &str,
        r: &str,
    ) -> std::fmt::Result {
        self.punct(l)?;
        fun(self)?;
        self.punct(r)?;
        Ok(())
    }

    fn indented(&mut self, fun: impl Fn(&mut Self) -> std::fmt::Result) -> std::fmt::Result {
        self.indent += 1;
        fun(self)?;
        self.indent -= 1;
        Ok(())
    }

    fn brace(&mut self, fun: impl Fn(&mut Self) -> std::fmt::Result) -> std::fmt::Result {
        self.group(fun, "{", "}")
    }

    fn paren(&mut self, fun: impl Fn(&mut Self) -> std::fmt::Result) -> std::fmt::Result {
        self.group(fun, "(", ")")
    }

    fn brack(&mut self, fun: impl Fn(&mut Self) -> std::fmt::Result) -> std::fmt::Result {
        self.group(fun, "[", "]")
    }

    fn name(&mut self, s: &Name) -> std::fmt::Result {
        write!(self.f, "{}", &s.data)
    }

    fn param(&mut self, p: &Param) -> std::fmt::Result {
        self.name(&p.name)?;
        self.punct(":")?;
        self.space()?;
        self.ty(&p.ty)
    }

    fn program(&mut self, p: &Program) -> std::fmt::Result {
        self.newline_sep(&p.stmts, Self::stmt)
    }

    fn stmt(&mut self, s: &Stmt) -> std::fmt::Result {
        match s {
            Stmt::Var(s) => self.stmt_var(s),
            Stmt::Def(s) => self.stmt_def(s),
            Stmt::Impl(s) => self.stmt_impl(s),
            Stmt::Expr(s) => self.stmt_expr(s),
            Stmt::Struct(s) => self.stmt_struct(s),
            Stmt::Enum(s) => self.stmt_enum(s),
            Stmt::Type(s) => self.stmt_type(s),
            Stmt::Trait(s) => self.stmt_trait(s),
        }
    }

    fn stmt_var(&mut self, s: &StmtVar) -> std::fmt::Result {
        self.kw("var")?;
        self.space()?;
        self.name(&s.name)?;
        self.punct(":")?;
        self.space()?;
        self.ty(&s.ty)?;
        self.space()?;
        self.punct("=")?;
        self.space()?;
        self.expr(&s.expr)?;
        self.punct(";")
    }

    fn stmt_def(&mut self, s: &StmtDef) -> std::fmt::Result {
        self.kw("def")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.paren(|this| this.comma_sep(&s.params, Self::param))?;
        self.punct(":")?;
        self.space()?;
        self.ty(&s.ty)?;
        self.space()?;
        self.punct("=")?;
        self.space()?;
        self.expr(&s.expr)?;
        self.punct(";")
    }

    fn stmt_impl(&mut self, s: &StmtImpl) -> std::fmt::Result {
        self.kw("impl")?;
        self.generics(&s.generics)?;
        self.space()?;
        self.tr(&s.head)?;
        self.where_clause(&s.where_clause)?;
        self.space()?;
        self.brace(|this| this.comma_sep(&s.defs, Self::stmt_def))
    }

    fn where_clause(&mut self, ts: &[Trait]) -> std::fmt::Result {
        if !ts.is_empty() {
            self.space()?;
            self.kw("where")?;
            self.space()?;
            self.comma_sep(ts, Self::tr)?;
        }
        Ok(())
    }

    fn stmt_expr(&mut self, s: &Expr) -> std::fmt::Result {
        self.expr(s)?;
        self.punct(";")
    }

    fn stmt_struct(&mut self, s: &StmtStruct) -> std::fmt::Result {
        self.kw("struct")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.space()?;
        self.members(&s.fields, ": ", Self::ty)
    }

    fn stmt_enum(&mut self, s: &StmtEnum) -> std::fmt::Result {
        self.kw("enum")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.space()?;
        self.members(&s.variants, "", Self::variant)
    }

    fn variant(&mut self, t: &Type) -> std::fmt::Result {
        self.paren(|this| this.ty(&t))
    }

    fn stmt_type(&mut self, s: &StmtType) -> std::fmt::Result {
        self.kw("type")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.space()?;
        self.punct("=")?;
        self.space()?;
        self.ty(&s.ty)?;
        self.punct(";")
    }

    fn stmt_trait(&mut self, s: &StmtTrait) -> std::fmt::Result {
        self.kw("trait")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.space()?;
        self.brace(|this| {
            if !s.defs.is_empty() || !s.types.is_empty() {
                this.indented(|this| {
                    this.newline()?;
                    this.newline_sep(&s.defs, Self::stmt_def_decl)?;
                    this.newline_sep(&s.types, Self::stmt_type_decl)
                })?;
                this.newline()?;
            }
            Ok(())
        })
    }

    fn stmt_def_decl(&mut self, s: &StmtDefDecl) -> std::fmt::Result {
        self.kw("def")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.paren(|this| this.comma_sep(&s.params, Self::param))?;
        self.punct(":")?;
        self.space()?;
        self.ty(&s.ty)?;
        self.punct(";")
    }

    fn stmt_type_decl(&mut self, s: &StmtTypeDecl) -> std::fmt::Result {
        self.kw("type")?;
        self.space()?;
        self.name(&s.name)?;
        self.generics(&s.generics)?;
        self.space()?;
        self.punct(";")
    }

    fn generics(&mut self, gs: &[Name]) -> std::fmt::Result {
        if !gs.is_empty() {
            self.brack(|this| this.comma_sep(gs, Self::name))?;
        }
        Ok(())
    }

    fn turbo(&mut self, ts: &[Type]) -> std::fmt::Result {
        if !ts.is_empty() {
            self.punct("::")?;
            self.brack(|this| this.comma_sep(ts, Self::ty))?;
        }
        Ok(())
    }

    fn type_args(&mut self, ts: &[Type]) -> std::fmt::Result {
        if !ts.is_empty() {
            self.brack(|this| this.comma_sep(ts, Self::ty))?;
        }
        Ok(())
    }

    fn index(&mut self, i: &Index) -> std::fmt::Result {
        write!(self.f, "{}", i.data)
    }

    fn lit(&mut self, s: impl std::fmt::Display) -> std::fmt::Result {
        write!(self.f, "{}", s)
    }

    fn _expr(&mut self, expr: &Expr) -> std::fmt::Result {
        match expr {
            Expr::Int(_, _, v) => {
                self.lit(v)?;
            }
            Expr::Float(_, _, v) => {
                self.lit(v)?;
            }
            Expr::Bool(_, _, v) => {
                self.lit(v)?;
            }
            Expr::String(_, _, s) => {
                self.punct("\"")?;
                self.lit(s)?;
                self.punct("\"")?;
            }
            Expr::Field(_, _, e, x) => {
                self.expr(e)?;
                self.punct(".")?;
                self.name(x)?;
            }
            Expr::Tuple(_, _, es) => {
                self.paren(|this| this.comma_sep_trailing(es, Self::expr))?;
            }
            Expr::Struct(_, _, x, ts, xes) => {
                self.name(x)?;
                self.turbo(ts)?;
                self.space()?;
                self.members(xes, " = ", Self::expr)?;
            }
            Expr::Enum(_, _, x0, ts, x1, e) => {
                self.name(x0)?;
                self.turbo(ts)?;
                self.punct("::")?;
                self.name(x1)?;
                self.paren(|this| this.expr(e))?;
            }
            Expr::Var(_, _, x, ts) => {
                self.name(x)?;
                self.type_args(ts)?;
            }
            Expr::Call(_, _, e, es) => {
                self.expr(e)?;
                self.paren(|this| this.comma_sep(es, Self::expr))?;
            }
            Expr::Block(_, _, ss, e) => {
                self.brace(|this| {
                    this.indented(|this| {
                        this.newline_sep(ss, |this, s| this.stmt(s))?;
                        this.newline()?;
                        this.expr(e)
                    })
                })?;
            }
            Expr::Query(_, _, qs) => {
                self.newline_sep(qs, Self::query_stmt)?;
            }
            Expr::Assoc(_, _, tr, x, ts) => {
                self.punct("@")?;
                self.tr(tr)?;
                self.punct("::")?;
                self.name(x)?;
                self.turbo(ts)?;
            }
            Expr::Index(_, _, e, i) => {
                self.expr(e)?;
                self.punct(".")?;
                self.index(i)?;
            }
            Expr::Array(_, _, es) => {
                self.brack(|this| this.comma_sep(es, Self::expr))?;
            }
            Expr::Err(_, _) => {
                self.kw("<err>")?;
            }
            Expr::Assign(_, _, e0, e1) => {
                self.expr(e0)?;
                self.space()?;
                self.punct("=")?;
                self.space()?;
                self.expr(e1)?;
            }
            Expr::Return(_, _, e) => {
                self.kw("return")?;
                self.space()?;
                self.expr(e)?;
            }
            Expr::Continue(_, _) => {
                self.kw("continue")?;
            }
            Expr::Break(_, _) => {
                self.kw("break")?;
            }
            Expr::Fun(_, _, ps, t, e) => {
                self.kw("fun")?;
                self.paren(|this| this.comma_sep(ps, Self::param))?;
                self.punct(":")?;
                self.space()?;
                self.ty(t)?;
                self.space()?;
                self.punct("=")?;
                self.space()?;
                self.expr(e)?;
            }
            Expr::And(_, _, e0, e1) => {
                self.expr(e0)?;
                self.space()?;
                self.punct("and")?;
                self.space()?;
                self.expr(e1)?;
            }
            Expr::Or(_, _, e0, e1) => {
                self.expr(e0)?;
                self.space()?;
                self.punct("or")?;
                self.space()?;
                self.expr(e1)?;
            }
            Expr::Match(_, _, e, arms) => {
                self.kw("match")?;
                self.space()?;
                self.expr(e)?;
                self.space()?;
                self.brace(|this| {
                    this.indented(|this| {
                        this.comma_sep(arms, |this, (p, e)| {
                            this.pat(&p)?;
                            this.space()?;
                            this.punct("=>")?;
                            this.space()?;
                            this.expr(&e)
                        })
                    })
                })?;
            }
            Expr::While(_, _, e0, e1) => {
                self.kw("while")?;
                self.space()?;
                self.expr(e0)?;
                self.space()?;
                self.expr(e1)?;
            }
            Expr::Record(_, _, xts) => {
                self.members(xts, " = ", Self::expr)?;
            }
        }
        Ok(())
    }

    fn members<T>(
        &mut self,
        xts: &[(Name, T)],
        t: &str,
        f: impl Fn(&mut Self, &T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        self.brace(|this| {
            if !xts.is_empty() {
                this.indented(|this| {
                    this.newline_comma_sep(xts, |this, (x, e)| {
                        this.name(x)?;
                        this.punct(t)?;
                        f(this, e)
                    })
                })?;
                this.newline()?;
            }
            Ok(())
        })?;
        Ok(())
    }

    fn members2<T>(
        &mut self,
        xts: &[(Name, T, T)],
        t: &str,
        f: impl Fn(&mut Self, &T, &T) -> std::fmt::Result,
    ) -> std::fmt::Result {
        self.brace(|this| {
            if !xts.is_empty() {
                this.indented(|this| {
                    this.newline_comma_sep(xts, |this, (x, e0, e1)| {
                        this.name(x)?;
                        this.punct(t)?;
                        f(this, e0, e1)
                    })
                })?;
                this.newline()?;
            }
            Ok(())
        })?;
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> std::fmt::Result {
        if self.type_info {
            self.paren(|this| {
                this._expr(expr)?;
                this.punct(":")?;
                this.ty(&expr.ty())
            })?;
        } else {
            self._expr(expr)?;
        }
        Ok(())
    }

    fn query_stmt(&mut self, q: &Query) -> std::fmt::Result {
        match q {
            Query::From(_, _, xes) => {
                self.kw("from")?;
                self.space()?;
                self.members(xes, " in ", Self::expr)?;
            }
            Query::Where(_, _, e) => {
                self.kw("where")?;
                self.space()?;
                self.expr(e)?;
            }
            Query::Select(_, _, xes) => {
                self.kw("select")?;
                self.space()?;
                self.members(xes, " = ", Self::expr)?;
            }
            Query::Join(_, _, xes, e) => {
                self.kw("join")?;
                self.space()?;
                self.members(xes, " in ", Self::expr)?;
                self.space()?;
                self.punct("on")?;
                self.space()?;
                self.expr(e)?;
            }
            Query::Group(_, _, xs, qs) => {
                self.kw("group")?;
                self.space()?;
                self.comma_sep(xs, Self::name)?;
                self.space()?;
                self.brace(|this| {
                    if !qs.is_empty() {
                        this.indented(|this| this.newline_comma_sep(qs, Self::query_stmt))?;
                        this.newline()?;
                    }
                    Ok(())
                })?;
            }
            Query::Over(_, _, e, qs) => {
                self.kw("over")?;
                self.space()?;
                self.expr(e)?;
                self.brace(|this| {
                    this.indented(|this| {
                        this.newline()?;
                        this.comma_sep(qs, Self::query_stmt)?;
                        this.newline()
                    })
                })?;
            }
            Query::Order(_, _, os) => {
                self.kw("order")?;
                self.space()?;
                self.comma_sep(os, Self::ordering)?;
            }
            Query::With(_, _, xes) => {
                self.kw("with")?;
                self.space()?;
                self.members(xes, " = ", Self::expr)?;
            }
            Query::Into(_, _, e) => {
                self.kw("into")?;
                self.space()?;
                self.comma_sep(e, Self::expr)?;
            }
            Query::Compute(_, _, aggs) => {
                self.kw("compute")?;
                self.space()?;
                self.members2(aggs, " = ", Self::agg)?;
            }
        }
        Ok(())
    }

    fn ordering(&mut self, (x, d): &(Name, bool)) -> std::fmt::Result {
        self.name(x)?;
        if *d {
            self.space()?;
            self.kw("desc")?;
        }
        Ok(())
    }

    fn agg(&mut self, e0: &Expr, e1: &Expr) -> std::fmt::Result {
        self.expr(e0)?;
        self.space()?;
        self.kw("of")?;
        self.space()?;
        self.expr(e1)
    }

    fn tr(&mut self, t: &Trait) -> std::fmt::Result {
        self.name(&t.name)?;
        if !t.types.is_empty() || !t.assocs.is_empty() {
            self.brack(|this| {
                this.comma_sep(&t.types, Self::ty)?;
                if !t.assocs.is_empty() {
                    if !t.types.is_empty() {
                        this.punct(",")?;
                        this.space()?;
                    }
                    this.comma_sep(&t.assocs, |this, (x, t)| {
                        this.name(x)?;
                        this.punct("=")?;
                        this.ty(t)
                    })?;
                }
                Ok(())
            })?;
        }
        Ok(())
    }

    fn ty(&mut self, t: &Type) -> std::fmt::Result {
        match t {
            Type::Cons(x, ts) => {
                self.name(x)?;
                self.type_args(ts)?;
            }
            Type::Assoc(i, x) => {
                self.tr(i)?;
                self.punct("::")?;
                self.name(x)?;
            }
            Type::Var(x) => {
                self.punct("?")?;
                self.name(x)?;
            }
            Type::Hole => {
                self.punct("_")?;
            }
            Type::Err => {
                self.kw("<err>")?;
            }
            Type::Gen(x) => {
                self.name(x)?;
            }
            Type::Fun(ts, t) => {
                self.kw("fun")?;
                self.paren(|this| this.comma_sep(ts, Self::ty))?;
                self.punct(":")?;
                self.space()?;
                self.ty(t)?;
            }
            Type::Tuple(ts) => {
                self.paren(|this| this.comma_sep_trailing(ts, Self::ty))?;
            }
            Type::Record(xts) => {
                self.members(xts, ": ", Self::ty)?;
            }
        }
        Ok(())
    }

    fn pat(&mut self, p: &Pat) -> std::fmt::Result {
        if self.type_info {
            self.paren(|this| {
                this._pat(p)?;
                this.punct(":")?;
                this.ty(&p.ty())
            })
        } else {
            self._pat(p)
        }
    }

    fn _pat(&mut self, p: &Pat) -> std::fmt::Result {
        match p {
            Pat::Var(_, _, x) => {
                self.name(x)?;
            }
            Pat::Int(_, _, v) => {
                write!(self.f, "{}", v)?;
            }
            Pat::Bool(_, _, v) => {
                write!(self.f, "{}", v)?;
            }
            Pat::String(_, _, v) => {
                write!(self.f, r#""{}""#, v)?;
            }
            Pat::Wildcard(_, _) => {
                self.punct("_")?;
            }
            Pat::Tuple(_, _, ps) => {
                self.paren(|this| this.comma_sep_trailing(ps, Self::pat))?;
            }
            Pat::Struct(_, _, x, ts, xps) => {
                self.name(x)?;
                self.turbo(ts)?;
                self.members(xps, " = ", Self::pat)?;
            }
            Pat::Enum(_, _, x0, ts, x1, p) => {
                self.name(x0)?;
                self.turbo(ts)?;
                self.punct("::")?;
                self.name(x1)?;
                self.paren(|this| this.pat(p))?;
            }
            Pat::Err(_, _) => {
                self.kw("<err>")?;
            }
        }
        Ok(())
    }
}
