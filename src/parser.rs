use crate::data::Def;
use crate::data::Expr;
use crate::data::Impl;
use crate::data::Name;
use crate::data::Param;
use crate::data::Pred;
use crate::data::Program;
use crate::data::Stmt;
use crate::data::Type;
use crate::data::Var;
use crate::token::Span;
use crate::token::Token;

pub struct Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    iter: std::iter::Peekable<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
        }
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
        let stmt = match self.peek()? {
            Token::Def => Stmt::Def(self.def()?),
            Token::Impl => Stmt::Impl(self.imp()?),
            Token::Var => Stmt::Var(self.var()?),
            _ => {
                let expr = self.expr()?;
                self.eat(Token::SemiColon);
                Stmt::Expr(expr)
            }
        };
        Some(stmt)
    }

    pub fn def(&mut self) -> Option<Def> {
        self.eat(Token::Def)?;
        let name = self.name()?;
        println!("name: {:?}", name);
        let params = self.params()?;
        let generics = self.generics()?;
        let preds = self.preds()?;
        self.eat(Token::Colon)?;
        let ty = self.ty()?;
        self.eat(Token::Eq)?;
        let expr = self.expr()?;
        println!("expr: {:?}", self.peek());
        self.eat(Token::SemiColon)?;
        println!("semicolon");
        let def = Def::new(name, generics, preds, params, ty, expr);
        Some(def)
    }

    fn preds(&mut self) -> Option<Vec<Pred>> {
        if self.eat(Token::Where).is_some() {
            let mut preds = Vec::new();
            if let Some(pred) = self.pred() {
                preds.push(pred);
            }
            while let Some(pred) = self.pred() {
                self.eat(Token::Comma);
                preds.push(pred);
            }
            Some(preds)
        } else {
            Some(vec![])
        }
    }

    pub fn imp(&mut self) -> Option<Impl> {
        self.eat(Token::Impl)?;
        let generics = self.generics()?;
        let head = self.pred()?;
        let body = self.preds()?;
        self.eat(Token::LBrace)?;
        let mut defs = Vec::new();
        let mut assocs = Vec::new();
        loop {
            match self.peek()? {
                Token::Def => defs.push(self.def()?),
                Token::Type => assocs.push(self.assoc()?),
                Token::RBrace => break,
                _ => return None,
            }
        }
        self.eat(Token::RBrace)?;
        let imp = Impl::new(generics, head, body, defs);
        Some(imp)
    }

    fn assoc(&mut self) -> Option<(Name, Type)> {
        self.eat(Token::Type)?;
        let name = self.name()?;
        self.eat(Token::Eq)?;
        let ty = self.ty()?;
        Some((name, ty))
    }

    fn var(&mut self) -> Option<Var> {
        self.eat(Token::Var);
        let name = self.name()?;
        let ty = self.ty()?;
        let expr = self.expr()?;
        let var = Var::new(name, ty, expr);
        Some(var)
    }

    pub fn pred(&mut self) -> Option<Pred> {
        let name = self.name()?;
        let (tys, assocs) = self.tys_and_assocs()?;
        Some(Pred::new(name, tys, assocs))
    }

    fn params(&mut self) -> Option<Vec<Param>> {
        self.eat(Token::LParen)?;
        let mut ps = Vec::new();
        if let Some(param) = self.param() {
            ps.push(param);
            while self.eat(Token::Comma).is_some() {
                let param = self.param()?;
                ps.push(param);
            }
            self.eat(Token::Comma);
        }
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
            let mut generics = Vec::new();
            if let Some(name) = self.name() {
                generics.push(name);
                while self.eat(Token::Comma).is_some() {
                    let name = self.name()?;
                    generics.push(name);
                }
                self.eat(Token::Comma);
            }
            self.eat(Token::RBrack)?;
            Some(generics)
        } else {
            Some(vec![])
        }
    }

    pub fn ty(&mut self) -> Option<Type> {
        match self.next()? {
            Token::Name(name) => {
                let name = name.to_owned();
                match self.peek() {
                    Some(Token::LBrack) => {
                        let tys = self.tys()?;
                        Some(Type::Cons(name, tys))
                    }
                    _ => Some(Type::Cons(name.to_owned(), vec![])),
                }
            }
            Token::Question => {
                let name = self.name()?;
                Some(Type::Var(name))
            }
            Token::Underscore => Some(Type::Hole),
            _ => None,
        }
    }

    fn tys(&mut self) -> Option<Vec<Type>> {
        if self.eat(Token::LBrack).is_some() {
            let mut tys = Vec::new();
            let ty = self.ty()?;
            tys.push(ty);
            while self.eat(Token::Comma).is_some() {
                let ty = self.ty()?;
                tys.push(ty);
            }
            self.eat(Token::Comma);
            self.eat(Token::RBrack)?;
            Some(tys)
        } else {
            Some(vec![])
        }
    }

    fn tys_and_assocs(&mut self) -> Option<(Vec<Type>, Vec<(Name, Type)>)> {
        if self.eat(Token::LBrack).is_some() {
            let mut tys = Vec::new();
            let mut assocs = Vec::new();
            if let Some(name) = self.name() {
                let name = name.to_string();
                if self.eat(Token::Eq).is_some() {
                    assocs.push((name, self.ty()?));
                } else {
                    tys.push(Type::Cons(name, vec![]));
                }
            } else {
                tys.push(self.ty()?);
            }
            while self.eat(Token::Comma).is_some() {
                if let Some(name) = self.name() {
                    let name = name.to_string();
                    if self.eat(Token::Eq).is_some() {
                        assocs.push((name, self.ty()?));
                    } else {
                        tys.push(Type::Cons(name, self.tys()?));
                    }
                } else {
                    tys.push(self.ty()?);
                }
            }
            self.eat(Token::Comma);
            self.eat(Token::RBrack)?;
            Some((tys, assocs))
        } else {
            Some((vec![], vec![]))
        }
    }

    fn expr0(&mut self) -> Option<Expr> {
        println!("expr0: {:?}", self.peek());
        match self.next()? {
            Token::Int(s) => Some(Expr::Int(Type::Hole, s.to_owned())),
            Token::True => Some(Expr::Bool(Type::Hole, true)),
            Token::False => Some(Expr::Bool(Type::Hole, false)),
            Token::Name(s) => Some(Expr::Var(Type::Hole, s.to_owned())),
            Token::RParen => {
                let expr = self.expr()?;
                self.eat(Token::LParen);
                Some(expr)
            }
            _ => None,
        }
    }

    fn expr1(&mut self) -> Option<Expr> {
        let mut expr = self.expr0()?;
        while let Some(op) = self.peek() {
            match op {
                Token::LParen => {
                    self.next();
                    let mut args = Vec::new();
                    if let Some(expr) = self.expr() {
                        args.push(expr);
                    }
                    while self.eat(Token::Comma).is_some() {
                        let expr = self.expr()?;
                        args.push(expr);
                    }
                    self.eat(Token::Comma);
                    self.eat(Token::RParen)?;
                    expr = Expr::Call(Type::Hole, Box::new(expr), args);
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
