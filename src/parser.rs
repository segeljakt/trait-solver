use crate::data::Expr;
use crate::data::Name;
use crate::data::Param;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::StmtDef;
use crate::data::StmtEnum;
use crate::data::StmtImpl;
use crate::data::StmtStruct;
use crate::data::StmtVar;
use crate::data::Trait;
use crate::data::Type;
use crate::token::Lexer;
use crate::token::Span;
use crate::token::Token;

pub struct Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    iter: std::iter::Peekable<I>,
    stack: Stack,
}

impl<'a, I> std::ops::Deref for Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    type Target = Stack;

    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

impl<'a, I> std::ops::DerefMut for Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}

pub struct Stack(Vec<Vec<(Name, Binding)>>);

impl Stack {
    fn bind(&mut self, name: Name, binding: Binding) {
        self.0.last_mut().unwrap().push((name, binding));
    }

    fn lookup(&self, x: &str) -> Option<&Binding> {
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

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    pub fn new(iter: I) -> Self {
        let iter = iter.peekable();
        let mut stack = Stack(vec![vec![]]);
        // Types
        stack.bind("i32".to_owned(), Binding::TypeBuiltin(0));
        stack.bind("f32".to_owned(), Binding::TypeBuiltin(0));
        stack.bind("i64".to_owned(), Binding::TypeBuiltin(0));
        stack.bind("bool".to_owned(), Binding::TypeBuiltin(0));
        stack.bind("Vec".to_owned(), Binding::TypeBuiltin(1));
        stack.bind("VecIterator".to_owned(), Binding::TypeBuiltin(1));
        stack.bind("Stream".to_owned(), Binding::TypeBuiltin(1));
        stack.bind("StreamIterator".to_owned(), Binding::TypeBuiltin(1));
        stack.bind("Option".to_owned(), Binding::TypeBuiltin(1));
        // Traits
        stack.bind(
            "Iterator".to_owned(),
            Binding::Trait(1, vec!["Item".to_string()]),
        );
        stack.bind(
            "IntoIterator".to_owned(),
            Binding::Trait(1, vec!["Item".to_string(), "IntoIter".to_string()]),
        );
        stack.bind(
            "Add".to_owned(),
            Binding::Trait(2, vec!["Output".to_string()]),
        );
        stack.bind(
            "Sub".to_owned(),
            Binding::Trait(2, vec!["Output".to_string()]),
        );
        stack.bind(
            "Mul".to_owned(),
            Binding::Trait(2, vec!["Output".to_string()]),
        );
        stack.bind(
            "Div".to_owned(),
            Binding::Trait(2, vec!["Output".to_string()]),
        );
        stack.bind("Eq".to_owned(), Binding::Trait(1, vec![]));
        stack.bind("Ord".to_owned(), Binding::Trait(1, vec![]));
        stack.bind("Clone".to_owned(), Binding::Trait(1, vec![]));
        stack.bind("Copy".to_owned(), Binding::Trait(1, vec![]));
        stack.bind("Display".to_owned(), Binding::Trait(1, vec![]));
        Self { iter, stack }
    }

    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();
        while let Some(stmt) = self.stmt() {
            stmts.push(stmt);
        }
        Program::new(stmts)
    }

    fn peek(&mut self) -> Option<Token> {
        self.iter.peek().cloned().map(|(_, t)| t)
    }

    fn next(&mut self) -> Option<Token> {
        self.iter.next().map(|(_, t)| t)
    }

    pub fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        self.0.push(vec![]);
        let x = f(self);
        self.0.pop();
        x
    }

    fn eat(&mut self, token: Token) -> Option<()> {
        if self.peek() == Some(token) {
            self.next();
            Some(())
        } else {
            None
        }
    }

    fn name(&mut self) -> Option<Name> {
        match self.peek()? {
            Token::Name(name) => {
                let name = name.to_owned();
                self.next();
                Some(name)
            }
            _ => None,
        }
    }

    pub fn stmt(&mut self) -> Option<Stmt> {
        let stmt = loop {
            break match self.peek()? {
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
        self.eat(Token::Type)?;
        let name = self.name()?;
        let generics = self.generics()?;
        generics
            .iter()
            .for_each(|g| self.bind(g.clone(), Binding::TypeVar));
        self.eat(Token::Eq)?;
        let ty = self.ty()?;
        self.eat(Token::SemiColon)?;
        self.bind(name.clone(), Binding::TypeAlias(generics, ty));
        Some(())
    }

    pub fn stmt_expr(&mut self) -> Option<Expr> {
        let expr = self.expr()?;
        if !matches!(expr, Expr::Block(..)) {
            self.eat(Token::SemiColon)?;
        }
        Some(expr)
    }

    pub fn stmt_def(&mut self) -> Option<StmtDef> {
        self.eat(Token::Def)?;
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
            let trs = this.where_clause()?;
            this.eat(Token::Colon)?;
            let ty = this.ty()?;
            this.eat(Token::Eq)?;
            let expr = this.stmt_expr()?;
            Some(StmtDef::new(name, generics, trs, params, ty, expr))
        })
    }

    fn where_clause(&mut self) -> Option<Vec<Trait>> {
        if self.eat(Token::Where).is_some() {
            self.seq(Self::tr)
        } else {
            Some(vec![])
        }
    }

    pub fn stmt_impl(&mut self) -> Option<StmtImpl> {
        self.scoped(|this| {
            this.eat(Token::Impl)?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let mut head = this.head()?;
            let body = this.where_clause()?;
            this.eat(Token::LBrace)?;
            let mut defs = Vec::new();
            let mut assocs = Vec::new();
            loop {
                match this.peek()? {
                    Token::Def => defs.push(this.stmt_def()?),
                    Token::Type => assocs.push(this.stmt_assoc_type()?),
                    _ => break,
                }
            }
            head.assocs = assocs;
            this.eat(Token::RBrace)?;
            Some(StmtImpl::new(generics, head, body, defs))
        })
    }

    pub fn stmt_struct(&mut self) -> Option<StmtStruct> {
        let stmt = self.scoped(move |this| {
            this.eat(Token::Struct)?;
            let name = this.name()?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let trs = this.where_clause()?;
            this.eat(Token::LBrace)?;
            let fields = this.seq(Self::type_field)?;
            this.eat(Token::RBrace)?;
            Some(StmtStruct::new(name, generics, trs, fields))
        })?;
        self.bind(stmt.name.clone(), Binding::TypeStruct(stmt.generics.len()));
        Some(stmt)
    }

    fn type_field(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        self.eat(Token::Colon)?;
        let ty = self.ty()?;
        Some((name, ty))
    }

    pub fn stmt_enum(&mut self) -> Option<StmtEnum> {
        let stmt = self.scoped(move |this| {
            this.eat(Token::Enum)?;
            let name = this.name()?;
            let generics = this.generics()?;
            generics
                .iter()
                .for_each(|g| this.bind(g.clone(), Binding::TypeVar));
            let trs = this.where_clause()?;
            this.eat(Token::LBrace)?;
            let variants = this.seq(Self::enum_variants)?;
            this.eat(Token::RBrace)?;
            Some(StmtEnum::new(name, generics, trs, variants))
        })?;
        self.bind(stmt.name.clone(), Binding::TypeEnum(stmt.generics.len()));
        Some(stmt)
    }

    fn enum_variants(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        let ty = if let Some(Token::LParen) = self.peek() {
            self.next();
            let tys = self.seq_nonempty(Self::ty)?;
            self.eat(Token::RParen)?;
            if tys.len() == 1 {
                tys.into_iter().next().unwrap()
            } else {
                Type::Cons("Tuple".to_owned(), tys)
            }
        } else {
            Type::Cons("Unit".to_owned(), vec![])
        };
        Some((name, ty))
    }

    fn stmt_assoc_type(&mut self) -> Option<(Name, Type)> {
        self.eat(Token::Type)?;
        let name = self.name()?;
        self.eat(Token::Eq)?;
        let ty = self.ty()?;
        self.eat(Token::SemiColon)?;
        Some((name, ty))
    }

    fn stmt_var(&mut self) -> Option<StmtVar> {
        self.eat(Token::Var);
        let name = self.name()?;
        let ty = if self.eat(Token::Colon).is_some() {
            self.ty()?
        } else {
            Type::Hole
        };
        self.eat(Token::Eq)?;
        let expr = self.expr()?;
        self.eat(Token::SemiColon)?;
        self.bind(name.clone(), Binding::ExprVar);
        Some(StmtVar::new(name, ty, expr))
    }

    pub fn head(&mut self) -> Option<Trait> {
        let name = self.name()?;
        let tys = self.tys()?;
        Some(Trait::new(name, tys, vec![]))
    }

    pub fn tr(&mut self) -> Option<Trait> {
        let name = self.name()?;
        let (tys, assocs) = self.trait_args()?;
        Some(Trait::new(name, tys, assocs))
    }

    fn seq<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>) -> Option<Vec<T>> {
        let mut xs = Vec::new();
        if let Some(x) = f(self) {
            xs.push(x);
            while self.eat(Token::Comma).is_some() {
                xs.push(f(self)?);
            }
        }
        self.eat(Token::Comma);
        Some(xs)
    }

    fn seq_nonempty<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>) -> Option<Vec<T>> {
        let mut xs = Vec::new();
        let x = f(self)?;
        xs.push(x);
        while self.eat(Token::Comma).is_some() {
            xs.push(f(self)?);
        }
        self.eat(Token::Comma);
        Some(xs)
    }

    fn params(&mut self) -> Option<Vec<Param>> {
        self.eat(Token::LParen)?;
        let ps = self.seq(Self::param)?;
        self.eat(Token::RParen)?;
        Some(ps)
    }

    fn param(&mut self) -> Option<Param> {
        let name = self.name()?;
        self.eat(Token::Colon)?;
        let ty = self.ty()?;
        Some(Param::new(name, ty))
    }

    fn generics(&mut self) -> Option<Vec<Name>> {
        if self.eat(Token::LBrack).is_some() {
            let gs = self.seq_nonempty(Self::name)?;
            self.eat(Token::RBrack)?;
            Some(gs)
        } else {
            Some(vec![])
        }
    }

    pub fn ty(&mut self) -> Option<Type> {
        match self.next()? {
            Token::Name(name) => {
                let name = name.to_string();
                self.type_name(name)
            }
            Token::Underscore => Some(Type::Hole),
            Token::Question => {
                let name = self.name()?;
                Some(Type::Var(name))
            }
            _ => None,
        }
    }

    fn tys(&mut self) -> Option<Vec<Type>> {
        if self.eat(Token::LBrack).is_some() {
            let tys = self.seq_nonempty(Self::ty)?;
            self.eat(Token::RBrack)?;
            Some(tys)
        } else {
            Some(vec![])
        }
    }

    fn trait_args(&mut self) -> Option<(Vec<Type>, Vec<(Name, Type)>)> {
        if self.eat(Token::LBrack).is_some() {
            let (tys, assocs) = self.tys_and_assocs()?;
            self.eat(Token::RBrack)?;
            Some((tys, assocs))
        } else {
            Some((vec![], vec![]))
        }
    }

    fn tys_and_assocs(&mut self) -> Option<(Vec<Type>, Vec<(Name, Type)>)> {
        let mut tys = Vec::new();
        let mut assocs = Vec::new();
        self.seq_nonempty(|this| this.ty_or_assoc(&mut tys, &mut assocs))?;
        Some((tys, assocs))
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
                    self.eat(Token::Dot)?;
                    let name1 = self.name()?;
                    let xts = assocs.into_iter().map(|x| (x, Type::Hole)).collect();
                    let tr = Trait::new(name, tys, xts);
                    Some(Type::Assoc(tr, name1))
                } else {
                    None
                }
            }
        }
    }

    fn expr_name(&mut self, name: Name) -> Option<Expr> {
        match self.lookup(&name)? {
            Binding::ExprVar => Some(Expr::Var(Type::Hole, name)),
            Binding::TypeEnum(_) => {
                if self.eat(Token::Dot).is_some() {
                    let variant_name = self.name()?;
                    let expr = if self.eat(Token::LParen).is_some() {
                        let exprs = self.seq_nonempty(Self::expr)?;
                        self.eat(Token::RParen)?;
                        if exprs.len() == 1 {
                            exprs.into_iter().next().unwrap()
                        } else {
                            Expr::Tuple(Type::Hole, exprs)
                        }
                    } else {
                        Expr::Unit(Type::Hole)
                    };
                    Some(Expr::Enum(Type::Hole, name, variant_name, Box::new(expr)))
                } else {
                    None
                }
            }
            Binding::TypeStruct(_) => {
                if let Token::LBrace = self.peek()? {
                    self.next();
                    let fields = self.seq(Self::expr_field)?;
                    self.eat(Token::RBrace)?;
                    Some(Expr::Struct(Type::Hole, name, fields))
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
                    self.eat(Token::Dot)?;
                    let name1 = self.name()?;
                    let xts = xs.into_iter().map(|x| (x, Type::Hole)).collect();
                    let tr = Trait::new(name, tys, xts);
                    Some(Expr::Assoc(Type::Hole, tr, name1))
                } else {
                    None
                }
            }
        }
    }

    fn ty_or_assoc(&mut self, tys: &mut Vec<Type>, assocs: &mut Vec<(Name, Type)>) -> Option<()> {
        if let Some(name) = self.name() {
            let name = name.to_string();
            if self.eat(Token::Eq).is_some() {
                assocs.push((name, self.ty()?));
            } else {
                tys.push(self.type_name(name)?)
            }
        } else {
            tys.push(self.ty()?);
        }
        Some(())
    }

    fn expr0(&mut self) -> Option<Expr> {
        match self.peek()? {
            Token::Int(s) => {
                let s = s.to_owned();
                self.next();
                Some(Expr::Int(Type::Hole, s))
            }
            Token::True => {
                self.next();
                Some(Expr::Bool(Type::Hole, true))
            }
            Token::String(s) => {
                let s = s.to_owned();
                self.next();
                Some(Expr::String(Type::Hole, s))
            }
            Token::False => {
                self.next();
                Some(Expr::Bool(Type::Hole, false))
            }
            Token::Name(name) => {
                let name = name.to_owned();
                self.next();
                self.expr_name(name)
            }
            Token::LParen => {
                self.next();
                if self.eat(Token::RParen).is_some() {
                    Some(Expr::Unit(Type::Hole))
                } else {
                    let expr = self.expr()?;
                    if let Some(Token::Comma) = self.peek() {
                        let exprs = self.seq(Self::expr)?;
                        self.eat(Token::RParen)?;
                        Some(Expr::Tuple(
                            Type::Hole,
                            std::iter::once(expr).chain(exprs).collect(),
                        ))
                    } else {
                        self.eat(Token::RParen)?;
                        Some(expr)
                    }
                }
            }
            Token::LBrace => self.block(),
            _ => None,
        }
    }

    fn block(&mut self) -> Option<Expr> {
        self.eat(Token::LBrace);
        self.scoped(move |this| {
            let mut stmts = Vec::new();
            let expr = loop {
                if let Some(t) = this.peek() {
                    let stmt = match t {
                        Token::Def => Stmt::Def(this.stmt_def()?),
                        Token::Var => Stmt::Var(this.stmt_var()?),
                        Token::Impl => Stmt::Impl(this.stmt_impl()?),
                        Token::Struct => Stmt::Struct(this.stmt_struct()?),
                        Token::Enum => Stmt::Enum(this.stmt_enum()?),
                        Token::SemiColon => {
                            this.next();
                            continue;
                        }
                        Token::RBrace => break Expr::Unit(Type::Hole),
                        _ => {
                            let expr = this.expr().unwrap();
                            if let Expr::Block(..) = expr {
                                if let Some(Token::RBrace) = this.peek() {
                                    break expr;
                                } else {
                                    Stmt::Expr(expr)
                                }
                            } else {
                                if this.eat(Token::SemiColon).is_some() {
                                    while this.eat(Token::Comma).is_some() {}
                                    Stmt::Expr(expr)
                                } else {
                                    break expr;
                                }
                            }
                        }
                    };
                    stmts.push(stmt);
                } else {
                    break Expr::Unit(Type::Hole);
                }
            };
            this.eat(Token::RBrace)?;
            Some(Expr::Block(Type::Hole, stmts, Box::new(expr)))
        })
    }

    fn expr_field(&mut self) -> Option<(Name, Expr)> {
        if let Some((_, Token::Name(name))) = self.iter.peek() {
            if self.stack.lookup(&name).is_none() {
                let name = name.to_string();
                self.next();
                self.eat(Token::Colon)?;
                return Some((name, self.expr()?));
            }
        }
        match self.expr1() {
            Some(Expr::Var(t, x)) => {
                if self.eat(Token::Colon).is_some() {
                    Some((x, self.expr()?))
                } else {
                    Some((x.clone(), Expr::Var(t, x)))
                }
            }
            Some(Expr::Field(t, e, x)) => Some((x.clone(), Expr::Field(t, e, x))),
            _ => None,
        }
    }

    fn expr1(&mut self) -> Option<Expr> {
        let mut expr = self.expr0()?;
        while let Some(op) = self.peek() {
            match op {
                Token::LParen => {
                    self.next();
                    let args = self.seq(Self::expr)?;
                    self.eat(Token::RParen)?;
                    expr = Expr::Call(Type::Hole, Box::new(expr), args);
                }
                Token::Dot => {
                    self.next();
                    let name = self.name()?;
                    expr = Expr::Field(Type::Hole, Box::new(expr), name);
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn expr2(&mut self) -> Option<Expr> {
        let mut expr = self.expr1()?;
        while let Some(t) = self.peek() {
            match t {
                Token::Star => {
                    self.next();
                    let rhs = self.expr2()?;
                    let fun = Expr::Var(Type::Hole, "__mul__".to_owned());
                    expr = Expr::Call(Type::Hole, Box::new(fun), vec![expr, rhs]);
                }
                Token::Slash => {
                    self.next();
                    let rhs = self.expr2()?;
                    let fun = Expr::Var(Type::Hole, "__div__".to_owned());
                    expr = Expr::Call(Type::Hole, Box::new(fun), vec![expr, rhs]);
                }
                _ => break,
            }
        }
        Some(expr)
    }

    pub fn expr(&mut self) -> Option<Expr> {
        let mut expr = self.expr2()?;
        while let Some(op) = self.peek() {
            match op {
                Token::Plus => {
                    self.next();
                    let rhs = self.expr()?;
                    let fun = Expr::Var(Type::Hole, "__add__".to_owned());
                    expr = Expr::Call(Type::Hole, Box::new(fun), vec![expr, rhs]);
                }
                Token::Minus => {
                    self.next();
                    let rhs = self.expr()?;
                    let fun = Expr::Var(Type::Hole, "__sub__".to_owned());
                    expr = Expr::Call(Type::Hole, Box::new(fun), vec![expr, rhs]);
                }
                _ => break,
            }
        }
        Some(expr)
    }
}

impl Type {
    pub fn parse(s: &str) -> Type {
        Parser::new(Lexer::new(s)).ty().unwrap()
    }
}

impl Expr {
    pub fn parse(s: &str) -> Expr {
        Parser::new(Lexer::new(s)).expr().unwrap()
    }
}

impl Stmt {
    pub fn parse(s: &str) -> Stmt {
        Parser::new(Lexer::new(s)).stmt().unwrap()
    }
}

impl StmtDef {
    pub fn parse(s: &str) -> StmtDef {
        Parser::new(Lexer::new(s)).stmt_def().unwrap()
    }
}

impl StmtImpl {
    pub fn parse(s: &str) -> StmtImpl {
        Parser::new(Lexer::new(s)).stmt_impl().unwrap()
    }
}

impl StmtStruct {
    pub fn parse(s: &str) -> StmtStruct {
        Parser::new(Lexer::new(s)).stmt_struct().unwrap()
    }
}

impl StmtEnum {
    pub fn parse(s: &str) -> StmtEnum {
        Parser::new(Lexer::new(s)).stmt_enum().unwrap()
    }
}

impl Trait {
    pub fn parse(s: &str) -> Trait {
        Parser::new(Lexer::new(s)).tr().unwrap()
    }
}

impl Program {
    pub fn parse(s: &str) -> Program {
        Parser::new(Lexer::new(s)).parse()
    }
}
