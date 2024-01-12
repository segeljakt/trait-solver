use crate::ast::Expr;
use crate::ast::Name;
use crate::ast::Param;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::StmtDef;
use crate::ast::StmtEnum;
use crate::ast::StmtImpl;
use crate::ast::StmtStruct;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;
use crate::diag::Diags;
use crate::lexer::Lexer;
use crate::lexer::Span;
use crate::lexer::Token;

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
    stack: Stack,
    pub diags: Diags,
}

impl<'a> std::ops::Deref for Parser<'a> {
    type Target = Stack;

    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

impl<'a> std::ops::DerefMut for Parser<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}

pub struct Stack(Vec<Vec<(Name, Binding)>>);

impl Stack {
    fn bind(&mut self, name: impl Into<Name>, binding: Binding) {
        self.0.last_mut().unwrap().push((name.into(), binding));
    }

    fn lookup(&self, x: &Name) -> Option<&Binding> {
        self.0.iter().rev().find_map(|s| {
            s.iter()
                .rev()
                .find_map(|(y, b)| if y == x { Some(b) } else { None })
        })
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum Binding {
    TypeEnum(usize),
    TypeStruct(usize),
    TypeBuiltin(usize),
    TypeVar,
    TypeAlias(Vec<Name>, Type),
    Trait(usize, Vec<Name>),
    ExprVar,
}

impl<'a> Drop for Parser<'a> {
    fn drop(&mut self) {
        if !self.diags.0.is_empty() {
            eprintln!("{:?}", self.diags);
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: Lexer<'a>) -> Self {
        let lexer = iter.peekable();
        let mut stack = Stack(vec![vec![]]);
        // Types
        stack.bind("i32", Binding::TypeBuiltin(0));
        stack.bind("f32", Binding::TypeBuiltin(0));
        stack.bind("i64", Binding::TypeBuiltin(0));
        stack.bind("bool", Binding::TypeBuiltin(0));
        stack.bind("Vec", Binding::TypeBuiltin(1));
        stack.bind("VecIterator", Binding::TypeBuiltin(1));
        stack.bind("Stream", Binding::TypeBuiltin(1));
        stack.bind("StreamIterator", Binding::TypeBuiltin(1));
        stack.bind("Option", Binding::TypeBuiltin(1));
        // Traits
        stack.bind("Iterator", Binding::Trait(1, vec![Name::from("Item")]));
        stack.bind(
            "IntoIterator",
            Binding::Trait(1, vec![Name::from("Item"), Name::from("IntoIter")]),
        );
        stack.bind("Add", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Sub", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Mul", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Div", Binding::Trait(2, vec![Name::from("Output")]));
        stack.bind("Eq", Binding::Trait(1, vec![]));
        stack.bind("Ord", Binding::Trait(1, vec![]));
        stack.bind("Clone", Binding::Trait(1, vec![]));
        stack.bind("Copy", Binding::Trait(1, vec![]));
        stack.bind("Display", Binding::Trait(1, vec![]));
        let diags = Diags::new();
        Self {
            lexer,
            stack,
            diags,
        }
    }

    pub fn update(&mut self, lexer: Lexer<'a>) -> std::iter::Peekable<Lexer<'a>> {
        std::mem::replace(&mut self.lexer, lexer.peekable())
    }

    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();
        loop {
            if let Some(stmt) = self.stmt() {
                stmts.push(stmt);
            }
            if matches!(
                self.peek(),
                Token::SemiColon | Token::RBrace | Token::RBrack | Token::RParen
            ) {
                self.next();
            }
            if self.peek() == Token::Eof {
                break;
            }
        }
        Program::new(stmts)
    }

    fn peek(&mut self) -> Token {
        self.lexer.peek().map(|(_, t)| *t).unwrap_or(Token::Eof)
    }

    fn next(&mut self) -> Option<(Span, Token)> {
        self.lexer.next()
    }

    fn or_else<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        or: impl FnOnce(Span) -> T,
    ) -> Option<T> {
        if let Some(x) = f(self) {
            Some(x)
        } else {
            if let Some(s) = self.peek_span() {
                Some(or(s))
            } else {
                None
            }
        }
    }

    pub fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        self.0.push(vec![]);
        let x = f(self);
        self.0.pop();
        x
    }

    fn optional(&mut self, token: Token) -> Option<Span> {
        if self.peek() == token {
            Some(self.advance())
        } else {
            None
        }
    }

    fn advance(&mut self) -> Span {
        self.next().unwrap().0
    }

    fn peek_span(&mut self) -> Option<Span> {
        self.lexer.peek().map(|(s, _)| *s)
    }

    fn recover(&mut self, token: Token) -> Option<Span> {
        loop {
            match self.peek() {
                t if t == Token::Eof => return None,
                t if t == token => return Some(self.advance()),
                Token::SemiColon | Token::RBrace | Token::RBrack | Token::RParen => {
                    return None;
                }
                _ => self.next(),
            };
        }
    }

    fn expect(&mut self, token: Token) -> Option<Span> {
        match self.peek() {
            t if t == token => Some(self.advance()),
            Token::Eof => {
                self.diags.err(
                    Span::default(),
                    format!("Unexpected end of file"),
                    format!("Expected `{token}`"),
                );
                None
            }
            Token::RBrace | Token::RBrack | Token::RParen => None,
            t => {
                let label = format!("Unexpected token `{t}`");
                let msg = format!("Expected `{token}`");
                let s = self.advance();
                self.diags.err(s, label, msg);
                self.recover(token)
            }
        }
    }

    fn name(&mut self) -> Option<Name> {
        match self.peek() {
            Token::Name(name) => {
                let name = name.to_owned();
                let s = self.advance();
                let name = Name::new(s, name);
                Some(name)
            }
            _ => None,
        }
    }

    pub fn stmt(&mut self) -> Option<Stmt> {
        let stmt = loop {
            break match self.peek() {
                Token::Def => Stmt::Def(self.stmt_def()?),
                Token::Impl => Stmt::Impl(self.stmt_impl()?),
                Token::Var => Stmt::Var(self.stmt_var()?),
                Token::Struct => Stmt::Struct(self.stmt_struct()?),
                Token::Enum => Stmt::Enum(self.stmt_enum()?),
                Token::Type => {
                    self.stmt_type()?;
                    continue;
                }
                _ => Stmt::Expr(self.stmt_expr()?),
            };
        };
        Some(stmt)
    }

    pub fn stmt_type(&mut self) -> Option<()> {
        self.expect(Token::Type)?;
        let name = self.name()?;
        let generics = self.generics()?;
        generics
            .iter()
            .for_each(|g| self.bind(g.clone(), Binding::TypeVar));
        self.expect(Token::Eq)?;
        let ty = self.ty()?;
        self.expect(Token::SemiColon)?;
        self.bind(name.clone(), Binding::TypeAlias(generics, ty));
        Some(())
    }

    pub fn stmt_expr(&mut self) -> Option<Expr> {
        let expr = self.expr(Self::expr4)?;
        if !matches!(expr, Expr::Block(..)) {
            self.expect(Token::SemiColon)?;
        }
        Some(expr)
    }

    pub fn stmt_def(&mut self) -> Option<StmtDef> {
        let s0 = self.expect(Token::Def)?;
        let name = self.name()?;
        self.bind(name.clone(), Binding::ExprVar);
        self.scoped(|this| {
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let params = this.params()?;
            params
                .iter()
                .for_each(|p| this.bind(p.name.clone(), Binding::ExprVar));
            this.expect(Token::Colon)?;
            let ty = this.ty()?;
            let trs = this.where_clause(Token::Eq)?;
            this.expect(Token::Eq)?;
            let expr = this.stmt_expr()?;
            Some(StmtDef::new(
                s0 + expr.span(),
                name,
                generics,
                trs,
                params,
                ty,
                expr,
            ))
        })
    }

    fn where_clause(&mut self, r: Token) -> Option<Vec<Trait>> {
        if self.optional(Token::Where).is_some() {
            self.until(Self::tr, r)
        } else {
            Some(vec![])
        }
    }

    pub fn stmt_impl(&mut self) -> Option<StmtImpl> {
        self.scoped(|this| {
            let s0 = this.expect(Token::Impl)?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let mut head = this.head()?;
            let body = this.where_clause(Token::LBrace)?;
            this.expect(Token::LBrace)?;
            let mut defs = Vec::new();
            let mut assocs = Vec::new();
            loop {
                match this.peek() {
                    Token::Def => defs.push(this.stmt_def()?),
                    Token::Type => assocs.push(this.stmt_assoc_type()?),
                    _ => break,
                }
            }
            head.assocs = assocs;
            let s1 = this.expect(Token::RBrace)?;
            Some(StmtImpl::new(s0 + s1, generics, head, body, defs))
        })
    }

    pub fn stmt_struct(&mut self) -> Option<StmtStruct> {
        let stmt = self.scoped(move |this| {
            let s0 = this.expect(Token::Struct)?;
            let name = this.name()?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let trs = this.where_clause(Token::LBrace)?;
            let fields = this.sep(Self::type_field, Token::LBrace, Token::RBrace)?;
            Some(StmtStruct::new(s0, name, generics, trs, fields))
        })?;
        self.bind(stmt.name.clone(), Binding::TypeStruct(stmt.generics.len()));
        Some(stmt)
    }

    fn type_field(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        Some((name, ty))
    }

    pub fn stmt_enum(&mut self) -> Option<StmtEnum> {
        let stmt = self.scoped(move |this| {
            let s0 = this.expect(Token::Enum)?;
            let name = this.name()?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let trs = this.where_clause(Token::LBrace)?;
            let variants = this.sep(Self::enum_variants, Token::LBrace, Token::RBrace)?;
            Some(StmtEnum::new(s0, name, generics, trs, variants))
        })?;
        self.bind(stmt.name.clone(), Binding::TypeEnum(stmt.generics.len()));
        Some(stmt)
    }

    fn enum_variants(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        let ty = if let Token::LParen = self.peek() {
            let tys = self.sep(Self::ty, Token::LParen, Token::RParen)?;
            if tys.len() == 1 {
                tys.into_iter().next().unwrap()
            } else {
                Type::Cons(Name::from("Tuple"), tys)
            }
        } else {
            Type::Cons(Name::from("Unit"), vec![])
        };
        Some((name, ty))
    }

    fn stmt_assoc_type(&mut self) -> Option<(Name, Type)> {
        self.expect(Token::Type)?;
        let name = self.name()?;
        self.expect(Token::Eq)?;
        let ty = self.ty()?;
        self.expect(Token::SemiColon)?;
        Some((name, ty))
    }

    fn stmt_var(&mut self) -> Option<StmtVar> {
        let s0 = self.expect(Token::Var)?;
        let name = self.name()?;
        let ty = if self.optional(Token::Colon).is_some() {
            self.ty()?
        } else {
            Type::Hole
        };
        self.expect(Token::Eq)?;
        let expr = self.expr(Self::expr4)?;
        let s1 = self.expect(Token::SemiColon)?;
        self.bind(name.clone(), Binding::ExprVar);
        Some(StmtVar::new(s0 + s1, name, ty, expr))
    }

    pub fn head(&mut self) -> Option<Trait> {
        let name = self.name()?;
        let tys = self.tys()?;
        Some(Trait::new(name.span, name, tys, vec![]))
    }

    pub fn tr(&mut self) -> Option<Trait> {
        let name = self.name()?;
        let (tys, assocs) = self.trait_args()?;
        Some(Trait::new(name.span, name, tys, assocs))
    }

    fn until<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>, r: Token) -> Option<Vec<T>> {
        if self.peek() == r {
            return Some(vec![]);
        } else {
            let mut xs = Vec::new();
            xs.push(f(self)?);
            while self.optional(Token::Comma).is_some() {
                if self.peek() == r {
                    return Some(xs);
                } else {
                    xs.push(f(self)?);
                }
            }
            Some(xs)
        }
    }

    fn sep<T>(
        &mut self,
        f: impl FnMut(&mut Self) -> Option<T>,
        l: Token,
        r: Token,
    ) -> Option<Vec<T>> {
        self.expect(l)?;
        let xs = self.until(f, r)?;
        self.expect(r)?;
        Some(xs)
    }

    fn params(&mut self) -> Option<Vec<Param>> {
        self.sep(Self::param, Token::LParen, Token::RParen)
    }

    fn param(&mut self) -> Option<Param> {
        let name = self.name()?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        Some(Param::new(name.span, name, ty))
    }

    fn generics(&mut self) -> Option<Vec<Name>> {
        if self.peek() == Token::LBrack {
            self.sep(Self::name, Token::LBrack, Token::RBrack)
        } else {
            Some(vec![])
        }
    }

    pub fn ty(&mut self) -> Option<Type> {
        match self.peek() {
            Token::Name(name) => {
                let name = name.to_owned();
                let s = self.advance();
                let name = Name::new(s, name);
                self.type_name(name)
            }
            Token::Underscore => {
                self.advance();
                Some(Type::Hole)
            }
            Token::Question => {
                self.advance();
                let name = self.name()?;
                Some(Type::Var(name))
            }
            _ => None,
        }
    }

    fn tys(&mut self) -> Option<Vec<Type>> {
        if self.peek() == Token::LBrack {
            self.sep(Self::ty, Token::LBrack, Token::RBrack)
        } else {
            Some(vec![])
        }
    }

    fn trait_args(&mut self) -> Option<(Vec<Type>, Vec<(Name, Type)>)> {
        if self.peek() == Token::LBrack {
            self.next();
            let mut tys = Vec::new();
            let mut assocs = Vec::new();
            self.until(
                |this| this.ty_or_assoc(&mut tys, &mut assocs),
                Token::RBrack,
            )?;
            self.expect(Token::RBrack)?;
            Some((tys, assocs))
        } else {
            Some((vec![], vec![]))
        }
    }

    fn type_name(&mut self, name: Name) -> Option<Type> {
        match self.lookup(&name)? {
            Binding::TypeAlias(generics, ty) => {
                let generics = generics.clone();
                let ty = ty.clone();
                let tys = self.tys()?;
                if generics.len() == tys.len() {
                    let sub = generics
                        .into_iter()
                        .zip(tys.into_iter())
                        .collect::<Vec<_>>();
                    Some(ty.apply(&sub))
                } else {
                    None
                }
            }
            Binding::TypeVar => Some(Type::Var(name)),
            Binding::TypeStruct(n) => {
                let n = *n;
                let tys = self.tys()?;
                if tys.len() == n {
                    Some(Type::Cons(name, tys))
                } else if tys.is_empty() {
                    Some(Type::Cons(name, (0..n).map(|_| Type::Hole).collect()))
                } else {
                    None
                }
            }
            Binding::TypeEnum(n) => {
                let n = *n;
                let tys = self.tys()?;
                if tys.len() == n {
                    Some(Type::Cons(name, tys))
                } else if tys.is_empty() {
                    Some(Type::Cons(name, (0..n).map(|_| Type::Hole).collect()))
                } else {
                    None
                }
            }
            Binding::TypeBuiltin(n) => {
                let n = *n;
                let tys = self.tys()?;
                if tys.len() == n {
                    Some(Type::Cons(name, tys))
                } else {
                    None
                }
            }
            Binding::ExprVar => None,
            Binding::Trait(n, assocs) => {
                let assocs = assocs.clone();
                let n = *n;
                let tys = self.tys()?;
                if tys.len() == n {
                    let s = self.expect(Token::Dot)?;
                    let name1 = self.name()?;
                    let xts = assocs.into_iter().map(|x| (x, Type::Hole)).collect();
                    let tr = Trait::new(s, name, tys, xts);
                    Some(Type::Assoc(tr, name1))
                } else {
                    None
                }
            }
        }
    }

    fn expr_name(&mut self, name: Name) -> Option<Expr> {
        let s0 = name.span;
        match self.lookup(&name)? {
            Binding::ExprVar => Some(Expr::Var(s0, Type::Hole, name)),
            Binding::TypeEnum(_) => {
                if self.expect(Token::Dot).is_some() {
                    let variant_name = self.name()?;
                    let expr = if self.peek() == Token::LParen {
                        let exprs =
                            self.sep(|this| this.expr(Self::expr4), Token::LParen, Token::RParen)?;
                        if exprs.len() == 1 {
                            exprs.into_iter().next().unwrap()
                        } else {
                            Expr::Tuple(s0, Type::Hole, exprs)
                        }
                    } else {
                        Expr::Unit(s0, Type::Hole)
                    };
                    Some(Expr::Enum(
                        s0,
                        Type::Hole,
                        name,
                        variant_name,
                        Box::new(expr),
                    ))
                } else {
                    None
                }
            }
            Binding::TypeStruct(_) => {
                if self.peek() == Token::LBrace {
                    let fields = self.sep(Self::expr_field, Token::LBrace, Token::RBrace)?;
                    Some(Expr::Struct(s0, Type::Hole, name, fields))
                } else {
                    None
                }
            }
            Binding::TypeAlias(..) => None,
            Binding::TypeVar => None,
            Binding::TypeBuiltin(..) => None,
            Binding::Trait(n, xs) => {
                let n = *n;
                let xs = xs.clone();
                let tys = self.tys()?;
                if tys.len() == n {
                    self.expect(Token::Dot)?;
                    let name1 = self.name()?;
                    let xts = xs.into_iter().map(|x| (x, Type::Hole)).collect();
                    let tr = Trait::new(s0, name, tys, xts);
                    Some(Expr::Assoc(s0, Type::Hole, tr, name1))
                } else {
                    None
                }
            }
        }
    }

    fn ty_or_assoc(&mut self, tys: &mut Vec<Type>, assocs: &mut Vec<(Name, Type)>) -> Option<()> {
        if let Some(name) = self.name() {
            if self.optional(Token::Eq).is_some() {
                assocs.push((name, self.ty()?));
            } else {
                tys.push(self.type_name(name)?)
            }
        } else {
            tys.push(self.ty()?);
        }
        Some(())
    }

    fn expr_field(&mut self) -> Option<(Name, Expr)> {
        if let Token::Name(name) = self.peek() {
            let mut name = Name::new(Span::default(), name);
            if self.stack.lookup(&name).is_none() {
                name.span = self.advance();
                self.expect(Token::Colon)?;
                return Some((name, self.expr(Self::expr4)?));
            }
        }
        match self.expr(Self::expr4) {
            Some(Expr::Var(s, t, x)) => {
                if self.expect(Token::Colon).is_some() {
                    Some((x, self.expr(Self::expr4)?))
                } else {
                    Some((x.clone(), Expr::Var(s, t, x)))
                }
            }
            Some(Expr::Field(s, t, e, x)) => Some((x.clone(), Expr::Field(s, t, e, x))),
            _ => None,
        }
    }

    fn expr(&mut self, f: impl FnMut(&mut Self) -> Option<Expr>) -> Option<Expr> {
        self.or_else(f, |s| Expr::Err(s, Type::Hole))
    }

    fn expr_block(&mut self) -> Option<Expr> {
        let s0 = self.expect(Token::LBrace)?;
        self.scoped(move |this| {
            let mut stmts = Vec::new();
            let expr = loop {
                let stmt = match this.peek() {
                    Token::Def => Stmt::Def(this.stmt_def()?),
                    Token::Var => Stmt::Var(this.stmt_var()?),
                    Token::Impl => Stmt::Impl(this.stmt_impl()?),
                    Token::Struct => Stmt::Struct(this.stmt_struct()?),
                    Token::Enum => Stmt::Enum(this.stmt_enum()?),
                    Token::SemiColon => {
                        this.next();
                        continue;
                    }
                    Token::RBrace => {
                        let s1 = this.advance();
                        let expr = Expr::Unit(s0 + s1, Type::Hole);
                        break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                    }
                    _ => {
                        let expr = this.expr(Self::expr4)?;
                        if let Expr::Block(..) = expr {
                            if let Token::RBrace = this.peek() {
                                let s1 = this.advance();
                                break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                            } else {
                                Stmt::Expr(expr)
                            }
                        } else {
                            if this.expect(Token::SemiColon).is_some() {
                                while this.expect(Token::Comma).is_some() {}
                                Stmt::Expr(expr)
                            } else {
                                let s1 = this.expect(Token::RBrace)?;
                                break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                            }
                        }
                    }
                };
                stmts.push(stmt);
            };
            Some(expr)
        })
    }

    fn expr0(&mut self) -> Option<Expr> {
        match self.peek() {
            Token::Int(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Expr::Int(s, Type::Hole, v))
            }
            Token::Float(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Expr::Float(s, Type::Hole, v))
            }
            Token::True => {
                let s = self.advance();
                Some(Expr::Bool(s, Type::Hole, true))
            }
            Token::String(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Expr::String(s, Type::Hole, v))
            }
            Token::False => {
                let s = self.advance();
                Some(Expr::Bool(s, Type::Hole, false))
            }
            Token::Name(name) => {
                let name = name.to_owned();
                let s = self.advance();
                let name = Name::new(s, name);
                self.expr_name(name)
            }
            Token::LParen => {
                let es = self.sep(|this| this.expr(Self::expr4), Token::LParen, Token::RParen)?;
                match es.len() {
                    0 => Some(Expr::Unit(Span::default(), Type::Hole)),
                    1 => Some(es.into_iter().next().unwrap()),
                    _ => Some(Expr::Tuple(Span::default(), Type::Hole, es)),
                }
            }
            Token::LBrace => self.expr_block(),
            _ => None,
        }
    }

    fn expr2(&mut self) -> Option<Expr> {
        let mut expr = self.expr(Self::expr0)?;
        loop {
            match self.peek() {
                Token::LParen => {
                    let es =
                        self.sep(|this| this.expr(Self::expr4), Token::LParen, Token::RParen)?;
                    expr = Expr::Call(expr.span(), Type::Hole, Box::new(expr), es);
                }
                Token::Dot => {
                    let s = self.advance();
                    let name = self.name()?;
                    if self.peek() == Token::LParen {
                        let es =
                            self.sep(|this| this.expr(Self::expr4), Token::LParen, Token::RParen)?;
                        let span = expr.span() + name.span;
                        let es = std::iter::once(expr).chain(es).collect::<Vec<_>>();
                        let e1 = Expr::Var(name.span, Type::Hole, Name::from(name));
                        expr = Expr::Call(span, Type::Hole, Box::new(e1), es);
                    } else {
                        expr = Expr::Field(s, Type::Hole, Box::new(expr), name);
                    }
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn expr3(&mut self) -> Option<Expr> {
        let mut expr = self.expr(Self::expr2)?;
        loop {
            match self.peek() {
                Token::Star => {
                    let s = self.advance();
                    let rhs = self.expr(Self::expr2)?;
                    let fun = Expr::Var(s, Type::Hole, Name::from("__mul__"));
                    expr = Expr::Call(
                        expr.span() + rhs.span(),
                        Type::Hole,
                        Box::new(fun),
                        vec![expr, rhs],
                    );
                }
                Token::Slash => {
                    let s = self.advance();
                    let rhs = self.expr(Self::expr2)?;
                    let fun = Expr::Var(s, Type::Hole, Name::from("__div__"));
                    expr = Expr::Call(
                        expr.span() + rhs.span(),
                        Type::Hole,
                        Box::new(fun),
                        vec![expr, rhs],
                    );
                }
                _ => break,
            }
        }
        Some(expr)
    }

    pub fn expr4(&mut self) -> Option<Expr> {
        let mut expr = self.expr(Self::expr3)?;
        loop {
            match self.peek() {
                Token::Plus => {
                    let s = self.advance();
                    let rhs = self.expr(Self::expr3)?;
                    let fun = Expr::Var(s, Type::Hole, Name::from("__add__".to_owned()));
                    expr = Expr::Call(
                        expr.span() + rhs.span(),
                        Type::Hole,
                        Box::new(fun),
                        vec![expr, rhs],
                    );
                }
                Token::Minus => {
                    let s = self.advance();
                    let fun = Expr::Var(s, Type::Hole, Name::from("__sub__".to_owned()));
                    let rhs = self.expr(Self::expr3)?;
                    expr = Expr::Call(
                        expr.span() + rhs.span(),
                        Type::Hole,
                        Box::new(fun),
                        vec![expr, rhs],
                    );
                }
                _ => break,
            }
        }
        Some(expr)
    }
}

impl Type {
    pub fn parse(s: &str) -> Type {
        Parser::new(Lexer::new(0, s)).ty().unwrap()
    }
}

impl Expr {
    pub fn parse(s: &str) -> Expr {
        Parser::new(Lexer::new(0, s)).expr(Parser::expr4).unwrap()
    }
}

impl Stmt {
    pub fn parse(s: &str) -> Stmt {
        Parser::new(Lexer::new(0, s)).stmt().unwrap()
    }
}

impl StmtDef {
    pub fn parse(s: &str) -> StmtDef {
        Parser::new(Lexer::new(0, s)).stmt_def().unwrap()
    }
}

impl StmtImpl {
    pub fn parse(s: &str) -> StmtImpl {
        Parser::new(Lexer::new(0, s)).stmt_impl().unwrap()
    }
}

impl StmtStruct {
    pub fn parse(s: &str) -> StmtStruct {
        Parser::new(Lexer::new(0, s)).stmt_struct().unwrap()
    }
}

impl StmtEnum {
    pub fn parse(s: &str) -> StmtEnum {
        Parser::new(Lexer::new(0, s)).stmt_enum().unwrap()
    }
}

impl Trait {
    pub fn parse(s: &str) -> Trait {
        Parser::new(Lexer::new(0, s)).tr().unwrap()
    }
}

impl Program {
    pub fn parse(s: &str) -> Program {
        Parser::new(Lexer::new(0, s)).parse()
    }
}
