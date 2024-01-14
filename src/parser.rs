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
use crate::ast::StmtType;
use crate::ast::StmtVar;
use crate::ast::Trait;
use crate::ast::Type;
use crate::ast::Variant;
use crate::diag::Diags;
use crate::lexer::Lexer;
use crate::lexer::Span;
use crate::lexer::Token;

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
    pub diags: Diags,
}

impl<'a> Drop for Parser<'a> {
    fn drop(&mut self) {
        if !self.diags.0.is_empty() {
            eprintln!("{:?}", self.diags);
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let lexer = lexer.peekable();
        let diags = Diags::new();
        Self { lexer, diags }
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
                let s = self.peek_span().unwrap();
                self.diags.err(
                    s,
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

    fn index(&mut self) -> Option<Index> {
        match self.peek() {
            Token::Int(v) => {
                let v = v.to_owned().parse().unwrap();
                let s = self.advance();
                let v = Index::new(s, v);
                Some(v)
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
                Token::Type => Stmt::Type(self.stmt_type()?),
                _ => Stmt::Expr(self.stmt_expr()?),
            };
        };
        Some(stmt)
    }

    pub fn stmt_type(&mut self) -> Option<StmtType> {
        self.expect(Token::Type)?;
        let name = self.name()?;
        let generics = self.generics()?;
        self.expect(Token::Eq)?;
        let ty = self.ty()?;
        self.expect(Token::SemiColon)?;
        Some(StmtType::new(name.span, name, generics, ty))
    }

    pub fn stmt_expr(&mut self) -> Option<Expr> {
        let expr = self.expr()?;
        if !matches!(expr, Expr::Block(..)) {
            self.expect(Token::SemiColon)?;
        }
        Some(expr)
    }

    pub fn stmt_def(&mut self) -> Option<StmtDef> {
        let s0 = self.expect(Token::Def)?;
        let name = self.name()?;
        let generics = self.generics()?;
        let params = self.params()?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        let trs = self.where_clause(Token::Eq)?;
        self.expect(Token::Eq)?;
        let expr = self.stmt_expr()?;
        Some(StmtDef::new(
            s0 + expr.span(),
            name,
            generics,
            trs,
            params,
            ty,
            expr,
        ))
    }

    fn where_clause(&mut self, r: Token) -> Option<Vec<Trait>> {
        if self.optional(Token::Where).is_some() {
            self.until(Self::tr, r)
        } else {
            Some(vec![])
        }
    }

    pub fn stmt_impl(&mut self) -> Option<StmtImpl> {
        let s0 = self.expect(Token::Impl)?;
        let generics = self.generics()?;
        let mut head = self.head()?;
        let body = self.where_clause(Token::LBrace)?;
        self.expect(Token::LBrace)?;
        let mut defs = Vec::new();
        let mut assocs = Vec::new();
        loop {
            match self.peek() {
                Token::Def => defs.push(self.stmt_def()?),
                Token::Type => assocs.push(self.stmt_assoc_type()?),
                _ => break,
            }
        }
        head.assocs = assocs;
        let s1 = self.expect(Token::RBrace)?;
        Some(StmtImpl::new(s0 + s1, generics, head, body, defs))
    }

    pub fn stmt_struct(&mut self) -> Option<StmtStruct> {
        let s0 = self.expect(Token::Struct)?;
        let name = self.name()?;
        let generics = self.generics()?;
        let fields = if self.peek() == Token::LBrace {
            self.sep(Self::type_field, Token::LBrace, Token::RBrace)?
        } else {
            self.expect(Token::SemiColon)?;
            vec![]
        };
        Some(StmtStruct::new(s0, name, generics, fields))
    }

    fn type_field(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        Some((name, ty))
    }

    pub fn stmt_enum(&mut self) -> Option<StmtEnum> {
        let s0 = self.expect(Token::Enum)?;
        let name = self.name()?;
        let generics = self.generics()?;
        let variants = self.sep(Self::variant, Token::LBrace, Token::RBrace)?;
        Some(StmtEnum::new(s0, name, generics, variants))
    }

    fn variant(&mut self) -> Option<Variant> {
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
        Some(Variant::new(name.span, name, ty))
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
        let expr = self.expr()?;
        let s1 = self.expect(Token::SemiColon)?;
        Some(StmtVar::new(s0 + s1, name, ty, expr))
    }

    pub fn head(&mut self) -> Option<Trait> {
        let name = self.name()?;
        let tys = self.type_args()?;
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
                let ts = self.type_args()?;
                if self.peek() == Token::Dot {
                    let s = self.advance();
                    let name1 = self.name()?;
                    let tr = Trait::new(s, name, ts, vec![]);
                    Some(Type::Assoc(tr, name1))
                } else {
                    Some(Type::Cons(name, ts))
                }
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

    fn type_args(&mut self) -> Option<Vec<Type>> {
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

    fn ty_or_assoc(&mut self, tys: &mut Vec<Type>, assocs: &mut Vec<(Name, Type)>) -> Option<()> {
        if let Some(name) = self.name() {
            if self.optional(Token::Eq).is_some() {
                assocs.push((name, self.ty()?));
            } else {
                tys.push(Type::Cons(name, self.type_args()?))
            }
        } else {
            tys.push(self.ty()?);
        }
        Some(())
    }

    fn expr_field(&mut self) -> Option<(Name, Expr)> {
        let expr = self.expr1()?;
        match expr {
            Expr::Field(s, t, e, x) => {
                if self.optional(Token::Colon).is_some() {
                    let expr = self.expr()?;
                    Some((x, expr))
                } else {
                    Some((x.clone(), Expr::Field(s, t, e, x)))
                }
            }
            Expr::Var(s, t, x, ts) => {
                if self.optional(Token::Colon).is_some() {
                    let expr = self.expr()?;
                    Some((x, expr))
                } else {
                    Some((x.clone(), Expr::Var(s, t, x, ts)))
                }
            }
            _ => None,
        }
    }

    fn expr_block(&mut self) -> Option<Expr> {
        let s0 = self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        let expr = loop {
            let stmt = match self.peek() {
                Token::Def => Stmt::Def(self.stmt_def()?),
                Token::Var => Stmt::Var(self.stmt_var()?),
                Token::Impl => Stmt::Impl(self.stmt_impl()?),
                Token::Struct => Stmt::Struct(self.stmt_struct()?),
                Token::Enum => Stmt::Enum(self.stmt_enum()?),
                Token::SemiColon => {
                    self.next();
                    continue;
                }
                Token::RBrace => {
                    let s1 = self.advance();
                    let expr = Expr::Unit(s0 + s1, Type::Hole);
                    break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                }
                _ => {
                    let expr = self.expr()?;
                    if let Expr::Block(..) = expr {
                        if let Token::RBrace = self.peek() {
                            let s1 = self.advance();
                            break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                        } else {
                            Stmt::Expr(expr)
                        }
                    } else {
                        if self.expect(Token::SemiColon).is_some() {
                            while self.expect(Token::Comma).is_some() {}
                            Stmt::Expr(expr)
                        } else {
                            let s1 = self.expect(Token::RBrace)?;
                            break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                        }
                    }
                }
            };
            stmts.push(stmt);
        };
        Some(expr)
    }

    fn turbo(&mut self) -> Option<Vec<Type>> {
        if self.peek() == Token::ColonColon {
            self.next();
            self.sep(Self::ty, Token::LBrack, Token::RBrack)
        } else {
            Some(vec![])
        }
    }

    fn expr_array(&mut self) -> Option<Expr> {
        let es = self.sep(Self::expr, Token::LBrack, Token::RBrack)?;
        Some(Expr::Array(Span::default(), Type::Hole, es))
    }

    fn expr_paren(&mut self) -> Option<Expr> {
        let es = self.args()?;
        match es.len() {
            0 => Some(Expr::Unit(Span::default(), Type::Hole)),
            1 => Some(es.into_iter().next().unwrap()),
            _ => Some(Expr::Tuple(Span::default(), Type::Hole, es)),
        }
    }

    fn expr_assoc(&mut self) -> Option<Expr> {
        let s = self.expect(Token::AtSign)?;
        let name0 = self.name()?;
        let ts0 = self.type_args()?;
        self.expect(Token::Dot)?;
        let name1 = self.name()?;
        let ts1 = self.turbo()?;
        let tr = Trait::new(s, name0, ts0, vec![]);
        Some(Expr::Assoc(s, Type::Hole, tr, name1, ts1))
    }

    fn args(&mut self) -> Option<Vec<Expr>> {
        self.sep(Self::expr, Token::LParen, Token::RParen)
    }

    fn expr_name(&mut self) -> Option<Expr> {
        let name = self.name()?;
        let s = name.span;
        match self.peek() {
            Token::LBrace => {
                let fields = self.sep(Self::expr_field, Token::LBrace, Token::RBrace)?;
                Some(Expr::Struct(s, Type::Hole, name, fields))
            }
            Token::ColonColon => {
                self.advance();
                if let Token::LBrack = self.peek() {
                    let ts = self.type_args()?;
                    Some(Expr::Var(s, Type::Hole, name, ts))
                } else {
                    let name1 = self.name()?;
                    if let Token::LParen = self.peek() {
                        let es = self.args()?;
                        let e = match es.len() {
                            0 => Expr::Unit(Span::default(), Type::Hole),
                            1 => es.into_iter().next().unwrap(),
                            _ => Expr::Tuple(Span::default(), Type::Hole, es),
                        };
                        Some(Expr::Enum(s, Type::Hole, name, name1, Box::new(e)))
                    } else {
                        Some(Expr::Enum(
                            s,
                            Type::Hole,
                            name,
                            name1,
                            Box::new(Expr::Unit(s, Type::Hole)),
                        ))
                    }
                }
            }
            _ => Expr::Var(s, Type::Hole, name, vec![]).into(),
        }
    }

    fn expr_array_get(&mut self, e0: Expr) -> Option<Expr> {
        self.expect(Token::LBrack)?;
        let e1 = self.expr()?;
        self.expect(Token::RBrack)?;
        let tr = Trait::new(Span::default(), Name::from("Index"), vec![], vec![]);
        let f = Expr::Assoc(Span::default(), Type::Hole, tr, Name::from("get"), vec![]);
        Some(Expr::Call(e0.span(), Type::Hole, Box::new(f), vec![e0, e1]))
    }

    fn expr_call(&mut self, e0: Expr) -> Option<Expr> {
        let es = self.args()?;
        Some(Expr::Call(e0.span(), Type::Hole, Box::new(e0), es))
    }

    fn tbinop(
        &mut self,
        lhs: Expr,
        t: Token,
        f: impl FnMut(&mut Self) -> Option<Expr>,
        trait_name: impl Into<Name>,
        def_name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let rhs = self.try_expr(f)?;
        let tr = Trait::new(s, trait_name.into(), vec![], vec![]);
        let op = Expr::Assoc(s, Type::Hole, tr, def_name.into(), vec![]);
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Call(s1, Type::Hole, Box::new(op), vec![lhs, rhs]))
    }

    fn def_binop(
        &mut self,
        lhs: Expr,
        t: Token,
        f: impl FnMut(&mut Self) -> Option<Expr>,
        def_name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let rhs = self.try_expr(f)?;
        let op = Expr::Var(s, Type::Hole, def_name.into(), vec![]);
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Call(s1, Type::Hole, Box::new(op), vec![lhs, rhs]))
    }

    fn unop(
        &mut self,
        t: Token,
        f: impl FnMut(&mut Self) -> Option<Expr>,
        name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let e = self.try_expr(f)?;
        let s1 = s + e.span();
        let op = Expr::Var(s, Type::Hole, name.into(), vec![]);
        Some(Expr::Call(s1, Type::Hole, Box::new(op), vec![e]))
    }

    fn expr_dot(&mut self, expr: Expr) -> Option<Expr> {
        let s = self.expect(Token::Dot)?;
        match self.peek() {
            Token::Name(_) => {
                let name = self.name()?;
                if let Token::ColonColon | Token::LParen = self.peek() {
                    let ts = self.turbo()?;
                    let es = self.args()?;
                    let span = expr.span() + name.span;
                    let es = std::iter::once(expr).chain(es).collect::<Vec<_>>();
                    let e1 = Expr::Var(name.span, Type::Hole, Name::from(name), ts);
                    Some(Expr::Call(span, Type::Hole, Box::new(e1), es))
                } else {
                    Some(Expr::Field(s, Type::Hole, Box::new(expr), name))
                }
            }
            Token::Int(_) => {
                let i = self.index()?;
                Some(Expr::Index(i.span, Type::Hole, Box::new(expr), i))
            }
            _ => None,
        }
    }

    fn expr_assign(&mut self, lhs: Expr) -> Option<Expr> {
        self.expect(Token::Eq)?;
        let rhs = self.try_expr(Self::expr8)?;
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Assign(s1, Type::Hole, Box::new(lhs), Box::new(rhs)))
    }

    fn expr_question(&mut self, _lhs: Expr) -> Option<Expr> {
        todo!()
    }

    fn expr_return(&mut self) -> Option<Expr> {
        let s = self.expect(Token::Return)?;
        if self.peek() == Token::SemiColon {
            let e = Expr::Unit(s, Type::Hole);
            Some(Expr::Return(s, Type::Hole, Box::new(e)))
        } else {
            let e = self.try_expr(Self::expr8)?;
            Some(Expr::Return(s, Type::Hole, Box::new(e)))
        }
    }

    fn expr_continue(&mut self) -> Option<Expr> {
        let s = self.expect(Token::Continue)?;
        Some(Expr::Continue(s, Type::Hole))
    }

    fn expr_break(&mut self) -> Option<Expr> {
        let s = self.expect(Token::Break)?;
        Some(Expr::Break(s, Type::Hole))
    }

    fn type_annot(&mut self) -> Option<Type> {
        if self.peek() == Token::Colon {
            self.next();
            self.ty()
        } else {
            Some(Type::Hole)
        }
    }

    fn expr_fun(&mut self) -> Option<Expr> {
        let s0 = self.expect(Token::Fun)?;
        let ps = self.params()?;
        let t = self.type_annot()?;
        self.expect(Token::Eq)?;
        let e = self.expr()?;
        Some(Expr::Fun(s0 + e.span(), Type::Hole, ps, t, Box::new(e)))
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
            Token::Name(_) => self.expr_name(),
            Token::AtSign => self.expr_assoc(),
            Token::LParen => self.expr_paren(),
            Token::LBrack => self.expr_array(),
            Token::LBrace => self.expr_block(),
            _ => None,
        }
    }

    fn expr1(&mut self) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr0)?;
        loop {
            expr = match self.peek() {
                Token::LParen => self.expr_call(expr)?,
                Token::LBrack => self.expr_array_get(expr)?,
                Token::Dot => self.expr_dot(expr)?,
                Token::Question => self.expr_question(expr)?,
                _ => break Some(expr),
            }
        }
    }

    fn expr2(&mut self) -> Option<Expr> {
        match self.peek() {
            Token::Minus => self.unop(Token::Minus, Self::expr2, "neg"),
            Token::Not => self.unop(Token::Not, Self::expr2, "not"),
            _ => self.try_expr(Self::expr1),
        }
    }

    fn expr3(&mut self) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr2)?;
        loop {
            expr = match self.peek() {
                Token::Star => self.tbinop(expr, Token::Star, Self::expr2, "Mul", "mul")?,
                Token::Slash => self.tbinop(expr, Token::Slash, Self::expr2, "Div", "div")?,
                _ => break Some(expr),
            }
        }
    }

    fn expr4(&mut self) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr3)?;
        loop {
            expr = match self.peek() {
                Token::Plus => self.tbinop(expr, Token::Plus, Self::expr3, "Add", "add")?,
                Token::Minus => self.tbinop(expr, Token::Minus, Self::expr3, "Sub", "sub")?,
                _ => break Some(expr),
            }
        }
    }

    fn expr5(&mut self) -> Option<Expr> {
        let expr = self.try_expr(Self::expr4)?;
        match self.peek() {
            Token::EqEq => self.tbinop(expr, Token::EqEq, Self::expr4, "PartialEq", "eq"),
            Token::NotEq => self.tbinop(expr, Token::NotEq, Self::expr4, "PartialEq", "ne"),
            Token::Lt => self.tbinop(expr, Token::Lt, Self::expr4, "PartialOrd", "lt"),
            Token::Gt => self.tbinop(expr, Token::Gt, Self::expr4, "PartialOrd", "gt"),
            Token::Le => self.tbinop(expr, Token::Le, Self::expr4, "PartialOrd", "le"),
            Token::Ge => self.tbinop(expr, Token::Ge, Self::expr4, "PartialOrd", "ge"),
            _ => Some(expr),
        }
    }

    fn expr6(&mut self) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr5)?;
        loop {
            expr = match self.peek() {
                Token::And => self.tbinop(expr, Token::And, Self::expr5, "Logic", "and")?,
                Token::Or => self.tbinop(expr, Token::Or, Self::expr5, "Logic", "or")?,
                _ => break Some(expr),
            }
        }
    }

    fn expr7(&mut self) -> Option<Expr> {
        let expr = self.try_expr(Self::expr6)?;
        match self.peek() {
            Token::DotDot => self.def_binop(expr, Token::DotDot, Self::expr6, "range"),
            _ => Some(expr),
        }
    }

    fn expr8(&mut self) -> Option<Expr> {
        let expr = self.try_expr(Self::expr7)?;
        match self.peek() {
            Token::Eq => self.expr_assign(expr),
            _ => Some(expr),
        }
    }

    fn expr9(&mut self) -> Option<Expr> {
        match self.peek() {
            Token::Return => self.expr_return(),
            Token::Continue => self.expr_continue(),
            Token::Break => self.expr_break(),
            Token::Fun => self.expr_fun(),
            _ => self.try_expr(Self::expr8),
        }
    }

    fn try_expr(&mut self, f: impl FnMut(&mut Self) -> Option<Expr>) -> Option<Expr> {
        self.or_else(f, |s| Expr::Err(s, Type::Hole))
    }

    fn expr(&mut self) -> Option<Expr> {
        self.try_expr(Self::expr9)
    }
}

impl Type {
    pub fn parse(s: &str) -> Type {
        Parser::new(Lexer::new(0, s)).ty().unwrap()
    }
}

impl Expr {
    pub fn parse(s: &str) -> Expr {
        Parser::new(Lexer::new(0, s)).expr().unwrap()
    }

    pub fn try_parse(s: &str) -> Option<Expr> {
        Parser::new(Lexer::new(0, s)).expr()
    }

    pub fn parse_diags(s: &str) -> Diags {
        let mut parser = Parser::new(Lexer::new(0, s));
        let _ = parser.expr();
        std::mem::take(&mut parser.diags)
    }
}

impl Stmt {
    pub fn parse(s: &str) -> Stmt {
        Parser::new(Lexer::new(0, s)).stmt().unwrap()
    }

    pub fn try_parse(s: &str) -> Option<Stmt> {
        Parser::new(Lexer::new(0, s)).stmt()
    }

    pub fn parse_diags(s: &str) -> Diags {
        let mut parser = Parser::new(Lexer::new(0, s));
        let _ = parser.stmt();
        std::mem::take(&mut parser.diags)
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
