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
use crate::diag::Report;
use crate::lexer::Span;
use crate::lexer::Token;
use crate::util::block;
use crate::util::call;
use crate::util::expr_assoc;
use crate::util::expr_bool;
use crate::util::expr_break;
use crate::util::expr_match;
use crate::util::expr_while;
use crate::util::hole;
use crate::util::pat_enum;
use crate::util::pat_wild;
use crate::util::stmt_var;
use crate::util::tr;
use crate::util::var;

pub struct Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    lexer: std::iter::Peekable<I>,
    pub report: Report,
}

#[derive(Clone, Copy)]
struct ExprConfig {
    brace: bool,
}

enum Either<A, B> {
    A(A),
    B(B),
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = (Span, Token<'a>)>,
{
    pub fn new(lexer: I) -> Self {
        let lexer = lexer.peekable();
        let report = Report::new();
        Self { lexer, report }
    }

    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();
        loop {
            if let Some(stmt) = self.stmt() {
                stmts.push(stmt);
            }
            while self
                .optional_any([
                    Token::SemiColon,
                    Token::RBrace,
                    Token::RBrack,
                    Token::RParen,
                ])
                .is_some()
            {}
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

    fn optional_any<const N: usize>(&mut self, tokens: [Token; N]) -> Option<Span> {
        let t = self.peek();
        if tokens.into_iter().any(|t1| t == t1) {
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

    fn recover<const N: usize>(&mut self, tokens: [Token; N]) -> Option<(Span, Token)> {
        loop {
            match self.peek() {
                t if t == Token::Eof => return None,
                t if tokens.contains(&t) => return self.next(),
                Token::SemiColon | Token::RBrace | Token::RBrack | Token::RParen => {
                    return None;
                }
                _ => self.next(),
            };
        }
    }

    fn expect_any<const N: usize>(&mut self, tokens: [Token; N]) -> Option<(Span, Token)> {
        match self.peek() {
            t if tokens.contains(&t) => Some(self.next().unwrap()),
            Token::Eof => {
                let s = self.peek_span().unwrap();
                self.report.err(
                    s,
                    format!("Unexpected end of file"),
                    format!(
                        "Expected {}",
                        tokens
                            .into_iter()
                            .map(|t| format!("`{}`", t.to_string()))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                );
                None
            }
            Token::RBrace | Token::RBrack | Token::RParen => None,
            t => {
                let label = format!("Unexpected token `{t}`");
                let msg = format!(
                    "Expected {}",
                    tokens
                        .into_iter()
                        .map(|t| format!("`{}`", t.to_string()))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                let s = self.advance();
                self.report.err(s, label, msg);
                self.recover(tokens)
            }
        }
    }

    fn expect(&mut self, token: Token) -> Option<Span> {
        Some(self.expect_any([token])?.0)
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
                Token::Trait => Stmt::Trait(self.stmt_trait()?),
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
        if !matches!(expr, Expr::Block(..) | Expr::Match(..)) {
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
        let expr = self.body(Self::stmt_expr)?;
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

    pub fn stmt_trait(&mut self) -> Option<StmtTrait> {
        let s0 = self.expect(Token::Trait)?;
        let name = self.name()?;
        let generics = self.generics()?;
        let body = self.where_clause(Token::LBrace)?;
        self.expect(Token::LBrace)?;
        let mut defs = Vec::new();
        let mut tys = Vec::new();
        loop {
            match self.peek() {
                Token::Def => defs.push(self.stmt_def_decl()?),
                Token::Type => tys.push(self.stmt_type_decl()?),
                _ => break,
            }
        }
        let s1 = self.expect(Token::RBrace)?;
        Some(StmtTrait::new(s0 + s1, name, generics, body, defs, tys))
    }

    fn stmt_def_decl(&mut self) -> Option<StmtDefDecl> {
        let s0 = self.expect(Token::Def)?;
        let name = self.name()?;
        let generics = self.generics()?;
        let params = self.params()?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        let trs = self.where_clause(Token::SemiColon)?;
        let s1 = self.expect(Token::SemiColon)?;
        Some(StmtDefDecl::new(s0 + s1, name, generics, trs, params, ty))
    }

    fn stmt_type_decl(&mut self) -> Option<StmtTypeDecl> {
        self.expect(Token::Type)?;
        let name = self.name()?;
        let generics = self.generics()?;
        self.expect(Token::SemiColon)?;
        Some(StmtTypeDecl::new(name.span, name, generics))
    }

    pub fn stmt_impl(&mut self) -> Option<StmtImpl> {
        let s0 = self.expect(Token::Impl)?;
        let generics = self.generics()?;
        let head = self.head()?;
        let body = self.where_clause(Token::LBrace)?;
        self.expect(Token::LBrace)?;
        let mut defs = Vec::new();
        let mut tys = Vec::new();
        loop {
            match self.peek() {
                Token::Def => defs.push(self.stmt_def()?),
                Token::Type => tys.push(self.stmt_type()?),
                _ => break,
            }
        }
        let s1 = self.expect(Token::RBrace)?;
        Some(StmtImpl::new(s0 + s1, generics, head, body, defs, tys))
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
        let variants = self.sep(Self::ty_variant, Token::LBrace, Token::RBrace)?;
        Some(StmtEnum::new(s0, name, generics, variants))
    }

    fn ty_variant(&mut self) -> Option<(Name, Type)> {
        let name = self.name()?;
        let ty = if let Token::LParen = self.peek() {
            self.ty_paren()?
        } else {
            Type::Cons(Name::from("Unit"), vec![])
        };
        Some((name, ty))
    }

    fn tuple<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Either<T, (Span, Vec<T>)>> {
        let s0 = self.expect(Token::LParen)?;
        if let Some(s1) = self.optional(Token::RParen) {
            Some(Either::B((s0 + s1, vec![])))
        } else {
            let v = f(self)?;
            if self.optional(Token::RParen).is_some() {
                Some(Either::A(v))
            } else {
                let mut vs = vec![v];
                while self.optional(Token::Comma).is_some() {
                    if self.peek() == Token::RParen {
                        break;
                    } else {
                        vs.push(f(self)?);
                    }
                }
                let s1 = self.expect(Token::RParen)?;
                Some(Either::B((s0 + s1, vs)))
            }
        }
    }

    fn ty_paren(&mut self) -> Option<Type> {
        match self.tuple(Self::ty)? {
            Either::A(t) => Some(t),
            Either::B((_, ts)) => Some(Type::Tuple(ts)),
        }
    }

    fn pat_paren(&mut self) -> Option<Pat> {
        match self.tuple(Self::pat)? {
            Either::A(p) => Some(p),
            Either::B((s, ps)) => Some(Pat::Tuple(s, Type::Hole, ps)),
        }
    }

    fn expr_paren(&mut self) -> Option<Expr> {
        match self.tuple(Self::expr)? {
            Either::A(e) => Some(e),
            Either::B((s, es)) => Some(Expr::Tuple(s, Type::Hole, es)),
        }
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
        let mut xs = vec![];
        if self.peek() != r {
            xs.push(f(self)?);
            while self.optional(Token::Comma).is_some() {
                if self.peek() == r {
                    break;
                } else {
                    xs.push(f(self)?);
                }
            }
        }
        Some(xs)
    }

    fn until_cond<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        mut c: impl FnMut(&mut Self) -> bool,
    ) -> Option<Vec<T>> {
        let mut xs = vec![];
        if !c(self) {
            xs.push(f(self)?);
            while self.optional(Token::Comma).is_some() {
                if !c(self) {
                    xs.push(f(self)?);
                } else {
                    break;
                }
            }
        }
        Some(xs)
    }

    fn sep_while<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        mut c: impl FnMut(&mut Self) -> bool,
    ) -> Option<Vec<T>> {
        let mut xs = vec![];
        if c(self) {
            xs.push(f(self)?);
            while self.optional(Token::Comma).is_some() {
                if c(self) {
                    xs.push(f(self)?);
                } else {
                    break;
                }
            }
        }
        Some(xs)
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

    fn sep_trailing<T>(
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
        let ty = self.type_annot()?;
        Some(Param::new(name.span, name, ty))
    }

    fn generics(&mut self) -> Option<Vec<Name>> {
        if self.peek() == Token::LBrack {
            self.sep(Self::name, Token::LBrack, Token::RBrack)
        } else {
            Some(vec![])
        }
    }

    fn ty_name(&mut self) -> Option<Type> {
        let name = self.name()?;
        let ts = self.type_args()?;
        if self.peek() == Token::ColonColon {
            let s = self.advance();
            let name1 = self.name()?;
            let tr = Trait::new(s, name, ts, vec![]);
            Some(Type::Assoc(tr, name1))
        } else {
            Some(Type::Cons(name, ts))
        }
    }

    fn ty_fun(&mut self) -> Option<Type> {
        self.expect(Token::Fun)?;
        let tys = self.sep(Self::ty, Token::LParen, Token::RParen)?;
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        Some(Type::Fun(tys, Box::new(ty)))
    }

    pub fn ty(&mut self) -> Option<Type> {
        self.or_else(Self::_ty, |_| Type::Err)
    }

    pub fn _ty(&mut self) -> Option<Type> {
        match self.peek() {
            Token::Name(_) => self.ty_name(),
            Token::Underscore => {
                self.advance();
                Some(Type::Hole)
            }
            Token::Question => {
                self.advance();
                let name = self.name()?;
                Some(Type::Var(name))
            }
            Token::LParen => self.ty_paren(),
            Token::Fun => self.ty_fun(),
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
        let expr = self.expr1(ExprConfig { brace: true })?;
        match expr {
            Expr::Field(s, t, e, x) => {
                if self.optional(Token::Eq).is_some() {
                    let expr = self.expr()?;
                    Some((x, expr))
                } else {
                    Some((x.clone(), Expr::Field(s, t, e, x)))
                }
            }
            Expr::Var(s, t, x, ts) => {
                if self.optional(Token::Eq).is_some() {
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
                Token::Trait => Stmt::Trait(self.stmt_trait()?),
                Token::Struct => Stmt::Struct(self.stmt_struct()?),
                Token::Enum => Stmt::Enum(self.stmt_enum()?),
                Token::SemiColon => {
                    self.next();
                    continue;
                }
                Token::RBrace => {
                    let s1 = self.advance();
                    let expr = Expr::Tuple(s0 + s1, Type::Hole, vec![]);
                    break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                }
                _ => {
                    let expr = self.expr()?;
                    if let Expr::Block(..) = expr {
                        // {{}}
                        if let Token::RBrace = self.peek() {
                            let s1 = self.advance();
                            break Expr::Block(s0 + s1, Type::Hole, stmts, Box::new(expr));
                        } else {
                            Stmt::Expr(expr)
                        }
                    } else {
                        if self.expect(Token::SemiColon).is_some() {
                            while self.optional(Token::SemiColon).is_some() {}
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

    fn expr_assoc(&mut self) -> Option<Expr> {
        let s = self.expect(Token::AtSign)?;
        let name0 = self.name()?;
        let ts0 = self.type_args()?;
        self.expect(Token::ColonColon)?;
        let name1 = self.name()?;
        let ts1 = self.turbo()?;
        let tr = Trait::new(s, name0, ts0, vec![]);
        Some(Expr::Assoc(s, Type::Hole, tr, name1, ts1))
    }

    fn args(&mut self) -> Option<Vec<Expr>> {
        self.sep(Self::expr, Token::LParen, Token::RParen)
    }

    fn expr_name(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let x0 = self.name()?;
        let s = x0.span;
        let t = Type::Hole;
        match self.peek() {
            Token::LBrace if cfg.brace => {
                let xes = self.expr_fields()?;
                Some(Expr::Struct(s, t, x0, vec![], xes))
            }
            Token::ColonColon => {
                self.advance();
                if let Token::LBrack = self.peek() {
                    let ts = self.type_args()?;
                    match self.peek() {
                        Token::LBrace => {
                            let xes = self.expr_fields()?;
                            Some(Expr::Struct(s, t, x0, ts, xes))
                        }
                        Token::ColonColon => {
                            self.advance();
                            let x1 = self.name()?;
                            let e = self.expr_variant()?;
                            Some(Expr::Enum(s, t, x0, ts, x1, Box::new(e)))
                        }
                        _ => Some(Expr::Var(s, t, x0, ts)),
                    }
                } else {
                    let x1 = self.name()?;
                    let e = self.expr_variant()?;
                    Some(Expr::Enum(s, t, x0, vec![], x1, Box::new(e)))
                }
            }
            _ => Expr::Var(s, Type::Hole, x0, vec![]).into(),
        }
    }

    fn expr_fields(&mut self) -> Option<Vec<(Name, Expr)>> {
        self.sep(Self::expr_field, Token::LBrace, Token::RBrace)
    }

    fn expr_array_get(&mut self, e0: Expr) -> Option<Expr> {
        let s0 = self.expect(Token::LBrack)?;
        let e1 = self.expr()?;
        let s1 = self.expect(Token::RBrack)?;
        let s = s0 + s1;
        let tr = Trait::new(s, Name::from("Index"), vec![], vec![]);
        let f = Expr::Assoc(s, Type::Hole, tr, Name::from("get"), vec![]);
        Some(Expr::Call(s, Type::Hole, Box::new(f), vec![e0, e1]))
    }

    fn expr_call(&mut self, e0: Expr) -> Option<Expr> {
        let es = self.args()?;
        Some(Expr::Call(e0.span(), Type::Hole, Box::new(e0), es))
    }

    fn tbinop(
        &mut self,
        cfg: ExprConfig,
        lhs: Expr,
        t: Token,
        f: impl FnMut(&mut Self, ExprConfig) -> Option<Expr>,
        trait_name: impl Into<Name>,
        def_name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let rhs = self.try_expr(f, cfg)?;
        let tr = Trait::new(s, trait_name.into(), vec![], vec![]);
        let op = Expr::Assoc(s, Type::Hole, tr, def_name.into(), vec![]);
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Call(s1, Type::Hole, Box::new(op), vec![lhs, rhs]))
    }

    fn def_binop(
        &mut self,
        cfg: ExprConfig,
        lhs: Expr,
        t: Token,
        f: impl FnMut(&mut Self, ExprConfig) -> Option<Expr>,
        def_name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let rhs = self.try_expr(f, cfg)?;
        let op = Expr::Var(s, Type::Hole, def_name.into(), vec![]);
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Call(s1, Type::Hole, Box::new(op), vec![lhs, rhs]))
    }

    fn unop(
        &mut self,
        cfg: ExprConfig,
        t: Token,
        f: impl FnMut(&mut Self, ExprConfig) -> Option<Expr>,
        trait_name: impl Into<Name>,
        name: impl Into<Name>,
    ) -> Option<Expr> {
        let s = self.expect(t)?;
        let e = self.try_expr(f, cfg)?;
        let s1 = s + e.span();
        let tr = Trait::new(s, trait_name.into(), vec![], vec![]);
        let op = Expr::Assoc(s, Type::Hole, tr, name.into(), vec![]);
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

    fn expr_assign(&mut self, lhs: Expr, cfg: ExprConfig) -> Option<Expr> {
        self.expect(Token::Eq)?;
        let rhs = self.try_expr(Self::expr9, cfg)?;
        let s1 = lhs.span() + rhs.span();
        Some(Expr::Assign(s1, Type::Hole, Box::new(lhs), Box::new(rhs)))
    }

    fn expr_question(&mut self, _lhs: Expr) -> Option<Expr> {
        todo!()
    }

    fn expr_end(&mut self) -> bool {
        matches!(
            self.peek(),
            Token::SemiColon
                | Token::Comma
                | Token::RBrace
                | Token::RBrack
                | Token::RParen
                | Token::Eof
        )
    }

    fn expr_return(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let s = self.expect(Token::Return)?;
        if self.expr_end() {
            let e = Expr::Tuple(s, Type::Hole, vec![]);
            Some(Expr::Return(s, Type::Hole, Box::new(e)))
        } else {
            let e = self.try_expr(Self::expr9, cfg)?;
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
        if self.optional(Token::Colon).is_some() {
            self.ty()
        } else {
            Some(Type::Hole)
        }
    }

    fn expr_fun(&mut self) -> Option<Expr> {
        let s = self.expect(Token::Fun)?;
        let ps = self.params()?;
        let t = self.type_annot()?;
        let e = self.body(Self::expr)?;
        Some(Expr::Fun(s + e.span(), Type::Hole, ps, t, Box::new(e)))
    }

    fn expr_record(&mut self) -> Option<Expr> {
        let xts = self.sep(Self::expr_field, Token::LBrace, Token::RBrace)?;
        Expr::Record(Span::default(), Type::Hole, xts).into()
    }

    // Start of expr (except `from`)
    fn start_of_expr(&mut self) -> bool {
        matches!(
            self.peek(),
            Token::Int(..)
                | Token::IntSuffix(..)
                | Token::Float(..)
                | Token::FloatSuffix(..)
                | Token::Name(..)
                | Token::True
                | Token::False
                | Token::String(..)
                | Token::AtSign
                | Token::LParen
                | Token::LBrack
                | Token::LBrace
                | Token::Match
                | Token::If
                | Token::For
                | Token::While
        )
    }

    fn expr0(&mut self, cfg: ExprConfig) -> Option<Expr> {
        match self.peek() {
            Token::Int(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Expr::Int(s, Type::Hole, v))
            }
            Token::IntSuffix(v0, v1) => {
                let v0 = v0.to_owned();
                let v1 = format!("postfix_{v1}");
                let s = self.advance();
                Some(Expr::Call(
                    s,
                    Type::Hole,
                    Box::new(Expr::Var(s, Type::Hole, Name::from(v1), vec![])),
                    vec![Expr::Int(s, Type::Hole, v0)],
                ))
            }
            Token::Float(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Expr::Float(s, Type::Hole, v))
            }
            Token::FloatSuffix(v0, v1) => {
                let v0 = v0.to_owned();
                let v1 = format!("postfix_{v1}");
                let s = self.advance();
                Some(Expr::Call(
                    s,
                    Type::Hole,
                    Box::new(Expr::Var(s, Type::Hole, Name::from(v1), vec![])),
                    vec![Expr::Float(s, Type::Hole, v0)],
                ))
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
            Token::Name(_) => self.expr_name(cfg),
            Token::AtSign => self.expr_assoc(),
            Token::LParen => self.expr_paren(),
            Token::LBrack => self.expr_array(),
            Token::LBrace => self.expr_block(),
            Token::Match => self.expr_match(),
            Token::If => self.expr_if(),
            Token::From => self.expr_query(),
            Token::For => self.expr_for(),
            Token::While => self.expr_while(),
            _ => None,
        }
    }

    fn expr_query(&mut self) -> Option<Expr> {
        let qs = self.query_stmts()?;
        let s = qs.first().unwrap().span() + qs.last().unwrap().span();
        Some(Expr::Query(s, Type::Hole, qs))
    }

    fn end_of_query_stmt(&mut self) -> bool {
        matches!(
            self.peek(),
            Token::From
                | Token::Where
                | Token::Select
                | Token::Group
                | Token::Order
                | Token::Join
                | Token::With
                | Token::Over
                | Token::Into
                | Token::Compute
                | Token::SemiColon
                | Token::RBrace
                | Token::RBrack
                | Token::RParen
                | Token::Eof
        )
    }

    fn query_stmts(&mut self) -> Option<Vec<Query>> {
        let mut qs = Vec::new();
        loop {
            let q = match self.peek() {
                Token::From => self.query_from()?,
                Token::Where => self.query_where()?,
                Token::Select => self.query_select()?,
                Token::Group => self.query_group()?,
                Token::Order => self.query_order()?,
                Token::Join => self.query_join()?,
                Token::With => self.query_with()?,
                Token::Over => self.query_over()?,
                Token::Into => self.query_into()?,
                Token::Compute => self.query_compute()?,
                _ => break,
            };
            qs.push(q);
        }
        Some(qs)
    }

    fn name_in_expr(&mut self) -> Option<(Name, Expr)> {
        let name = self.name()?;
        self.expect(Token::In)?;
        let expr = self.expr()?;
        Some((name, expr))
    }

    fn query_from(&mut self) -> Option<Query> {
        let s = self.expect(Token::From)?;
        let xes = if self.peek() == Token::LBrace {
            self.sep(Self::name_in_expr, Token::LBrace, Token::RBrace)?
        } else {
            self.until_cond(Self::name_in_expr, Self::end_of_query_stmt)?
        };
        Some(Query::From(s, Type::Hole, xes))
    }

    fn query_select(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::Select)?;
        let xes = if self.peek() == Token::LBrace {
            self.sep(Self::expr_field, Token::LBrace, Token::RBrace)?
        } else {
            self.until_cond(Self::expr_field, Self::end_of_query_stmt)?
        };
        Some(Query::Select(s0, Type::Hole, xes))
    }

    fn name_eq_expr(&mut self) -> Option<(Name, Expr)> {
        let name = self.name()?;
        self.expect(Token::Eq)?;
        let expr = self.expr()?;
        Some((name, expr))
    }

    fn query_with(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::With)?;
        let xes = if self.peek() == Token::LBrace {
            self.sep(Self::name_eq_expr, Token::LBrace, Token::RBrace)?
        } else {
            self.until_cond(Self::name_eq_expr, Self::end_of_query_stmt)?
        };
        Some(Query::With(s0, Type::Hole, xes))
    }

    fn query_compute(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::Compute)?;
        let aggs = if self.peek() == Token::LBrace {
            self.sep(Self::agg, Token::LBrace, Token::RBrace)?
        } else {
            self.until_cond(Self::agg, Self::end_of_query_stmt)?
        };
        Some(Query::Compute(s0, Type::Hole, aggs))
    }

    fn agg(&mut self) -> Option<(Name, Expr, Expr)> {
        let x = self.name()?;
        self.expect(Token::Eq)?;
        let e0 = self.expr()?;
        self.expect(Token::Of)?;
        let e1 = self.expr()?;
        Some((x, e0, e1))
    }

    fn query_where(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::Where)?;
        let e = self.expr()?;
        let s = s0 + e.span();
        Some(Query::Where(s, Type::Hole, Box::new(e)))
    }

    fn query_group(&mut self) -> Option<Query> {
        let s = self.expect(Token::Group)?;
        let xs = self.query_stmt_args(Self::name)?;
        self.expect(Token::LBrace)?;
        let qs = self.query_stmts()?;
        self.expect(Token::RBrace)?;
        Some(Query::Group(s, Type::Hole, xs, qs))
    }

    fn query_over(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::Over)?;
        let e = self.expr_without_brace()?;
        self.expect(Token::LBrace)?;
        let qs = self.query_stmts()?;
        self.expect(Token::RBrace)?;
        let s = s0 + e.span();
        Some(Query::Over(s, Type::Hole, Box::new(e), qs))
    }

    fn query_order(&mut self) -> Option<Query> {
        let s0 = self.expect(Token::Order)?;
        let os = self.query_stmt_args(Self::ordering)?;
        Some(Query::Order(s0, Type::Hole, os))
    }

    fn ordering(&mut self) -> Option<(Name, bool)> {
        let x = self.name()?;
        let k = self.optional(Token::Desc).is_some();
        Some((x, k))
    }

    fn query_join(&mut self) -> Option<Query> {
        let s = self.expect(Token::Join)?;
        let xes = self.query_stmt_args(Self::source)?;
        self.expect(Token::On)?;
        let e = self.expr()?;
        let t = Type::Hole;
        Some(Query::Join(s, t, xes, Box::new(e)))
    }

    fn source(&mut self) -> Option<(Name, Expr)> {
        let x = self.name()?;
        self.expect(Token::In)?;
        let e = self.expr()?;
        Some((x, e))
    }

    fn query_stmt_args<T>(&mut self, f: impl FnMut(&mut Self) -> Option<T>) -> Option<Vec<T>> {
        if self.peek() == Token::LBrace {
            self.sep(f, Token::LBrace, Token::RBrace)
        } else {
            self.until_cond(f, Self::end_of_query_stmt)
        }
    }

    fn query_into(&mut self) -> Option<Query> {
        let s = self.expect(Token::Into)?;
        let es = self.query_stmt_args(Self::expr)?;
        Some(Query::Into(s, Type::Hole, es))
    }

    fn expr_for(&mut self) -> Option<Expr> {
        self.expect(Token::For)?;
        let x = self.name()?;
        self.expect(Token::In)?;
        let e0 = self.expr_without_brace()?;
        let e1 = self.expr_block()?;
        // {
        //   let it = @IntoIterator::into_iter(e0);
        //   while true {
        //     match it.next() {
        //       Option::Some(x) => e1,
        //       Option::None => break
        //     }
        //   }
        // }
        Some(block(
            [stmt_var(
                "it",
                hole(),
                call(
                    expr_assoc(tr("IntoIterator", [], []), "into_iter", []),
                    [e0],
                ),
            )],
            expr_while(
                expr_bool(true),
                expr_match(
                    call(var("next"), [var("it")]),
                    [
                        (
                            pat_enum("Option", [], "Some", Pat::Var(x.span, hole(), x)),
                            e1,
                        ),
                        (pat_enum("Option", [], "None", pat_wild()), expr_break()),
                    ],
                ),
            ),
        ))
    }

    fn expr_while(&mut self) -> Option<Expr> {
        self.expect(Token::While)?;
        let e = self.expr_without_brace()?;
        let e1 = self.expr_block_or_expr()?;
        let s = e.span() + e1.span();
        Some(Expr::While(s, Type::Hole, Box::new(e), Box::new(e1)))
    }

    fn expr_match(&mut self) -> Option<Expr> {
        self.expect(Token::Match)?;
        let e = self.expr_without_brace()?;
        let arms = self.sep(Self::arm, Token::LBrace, Token::RBrace)?;
        Some(Expr::Match(e.span(), Type::Hole, Box::new(e), arms))
    }

    fn expr_block_or_expr(&mut self) -> Option<Expr> {
        match self.expr_block()? {
            Expr::Block(_, _, ss, e) if ss.is_empty() => Some(*e),
            e => Some(e),
        }
    }

    fn expr_if(&mut self) -> Option<Expr> {
        self.expect(Token::If)?;
        let e = self.expr_without_brace()?;
        let e1 = self.expr_block_or_expr()?;
        let e2 = if self.optional(Token::Else).is_some() {
            self.expr_block_or_expr()?
        } else {
            Expr::Tuple(Span::default(), Type::Hole, vec![])
        };
        let p1 = Pat::Bool(Span::default(), Type::Hole, true);
        let p2 = Pat::Wildcard(Span::default(), Type::Hole);
        Some(Expr::Match(
            e.span() + e1.span() + e2.span(),
            Type::Hole,
            Box::new(e),
            vec![(p1, e1), (p2, e2)],
        ))
    }

    fn body(&mut self, mut f: impl FnMut(&mut Self) -> Option<Expr>) -> Option<Expr> {
        if self.peek() == Token::LBrace {
            self.expr_block()
        } else {
            self.expect(Token::Eq)?;
            f(self)
        }
    }

    fn arm(&mut self) -> Option<(Pat, Expr)> {
        let p = self.pat()?;
        self.expect(Token::FatArrow)?;
        let e = self.expr()?;
        Some((p, e))
    }

    fn expr_annot(&mut self, e: Expr) -> Option<Expr> {
        self.expect(Token::Colon)?;
        let t = self.ty()?;
        Some(e.with(t))
    }

    fn expr1(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr0, cfg)?;
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

    fn expr2(&mut self, cfg: ExprConfig) -> Option<Expr> {
        match self.peek() {
            Token::Minus => self.unop(cfg, Token::Minus, Self::expr2, "Neg", "neg"),
            Token::Not => self.unop(cfg, Token::Not, Self::expr2, "Not", "not"),
            _ => self.try_expr(Self::expr1, cfg),
        }
    }

    fn expr3(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr2, cfg)?;
        loop {
            expr = match self.peek() {
                Token::Colon => self.expr_annot(expr)?,
                _ => break Some(expr),
            }
        }
    }

    fn expr4(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr3, cfg)?;
        loop {
            expr = match self.peek() {
                Token::Star => self.tbinop(cfg, expr, Token::Star, Self::expr3, "Mul", "mul")?,
                Token::Slash => self.tbinop(cfg, expr, Token::Slash, Self::expr3, "Div", "div")?,
                _ => break Some(expr),
            }
        }
    }

    fn expr5(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr4, cfg)?;
        loop {
            expr = match self.peek() {
                Token::Plus => self.tbinop(cfg, expr, Token::Plus, Self::expr4, "Add", "add")?,
                Token::Minus => self.tbinop(cfg, expr, Token::Minus, Self::expr4, "Sub", "sub")?,
                _ => break Some(expr),
            }
        }
    }

    fn expr6(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let expr = self.try_expr(Self::expr5, cfg)?;
        match self.peek() {
            Token::EqEq => self.tbinop(cfg, expr, Token::EqEq, Self::expr5, "PartialEq", "eq"),
            Token::NotEq => self.tbinop(cfg, expr, Token::NotEq, Self::expr5, "PartialEq", "ne"),
            Token::Lt => self.tbinop(cfg, expr, Token::Lt, Self::expr5, "PartialOrd", "lt"),
            Token::Gt => self.tbinop(cfg, expr, Token::Gt, Self::expr5, "PartialOrd", "gt"),
            Token::Le => self.tbinop(cfg, expr, Token::Le, Self::expr5, "PartialOrd", "le"),
            Token::Ge => self.tbinop(cfg, expr, Token::Ge, Self::expr5, "PartialOrd", "ge"),
            _ => Some(expr),
        }
    }

    fn expr7(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let mut expr = self.try_expr(Self::expr6, cfg)?;
        loop {
            expr = match self.peek() {
                Token::And => {
                    let s = self.advance();
                    let rhs = self.try_expr(Self::expr6, cfg)?;
                    let t = Type::Hole;
                    Expr::And(s, t, Box::new(expr), Box::new(rhs))
                }
                Token::Or => {
                    let s = self.advance();
                    let rhs = self.try_expr(Self::expr6, cfg)?;
                    let t = Type::Hole;
                    Expr::Or(s, t, Box::new(expr), Box::new(rhs))
                }
                _ => break Some(expr),
            }
        }
    }

    fn expr8(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let expr = self.try_expr(Self::expr7, cfg)?;
        match self.peek() {
            Token::DotDot => self.def_binop(cfg, expr, Token::DotDot, Self::expr7, "range"),
            _ => Some(expr),
        }
    }

    fn expr9(&mut self, cfg: ExprConfig) -> Option<Expr> {
        let expr = self.try_expr(Self::expr8, cfg)?;
        match self.peek() {
            Token::Eq => self.expr_assign(expr, cfg),
            _ => Some(expr),
        }
    }

    fn expr10(&mut self, cfg: ExprConfig) -> Option<Expr> {
        match self.peek() {
            Token::Return => self.expr_return(cfg),
            Token::Continue => self.expr_continue(),
            Token::Break => self.expr_break(),
            Token::Fun => self.expr_fun(),
            _ => self.try_expr(Self::expr9, cfg),
        }
    }

    fn try_expr(
        &mut self,
        mut f: impl FnMut(&mut Self, ExprConfig) -> Option<Expr>,
        cfg: ExprConfig,
    ) -> Option<Expr> {
        self.or_else(|this| f(this, cfg), |s| Expr::Err(s, Type::Hole))
    }

    fn expr_without_brace(&mut self) -> Option<Expr> {
        self.try_expr(Self::expr10, ExprConfig { brace: false })
    }

    pub fn expr(&mut self) -> Option<Expr> {
        self.try_expr(Self::expr10, ExprConfig { brace: true })
    }

    pub fn pat0(&mut self) -> Option<Pat> {
        match self.peek() {
            Token::Name(_) => self.pat_name(),
            Token::LParen => self.pat_paren(),
            Token::Underscore => self.pat_underscore(),
            Token::Int(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Pat::Int(s, Type::Hole, v))
            }
            Token::String(v) => {
                let v = v.to_owned();
                let s = self.advance();
                Some(Pat::String(s, Type::Hole, v))
            }
            _ => None,
        }
    }

    pub fn pat1(&mut self) -> Option<Pat> {
        let p = self.pat0()?;
        match self.peek() {
            Token::Colon => {
                self.advance();
                let t = self.ty()?;
                Some(p.with(t))
            }
            _ => Some(p),
        }
    }

    pub fn pat(&mut self) -> Option<Pat> {
        self.or_else(Self::pat1, |s| Pat::Err(s, Type::Hole))
    }

    fn pat_underscore(&mut self) -> Option<Pat> {
        let s = self.expect(Token::Underscore)?;
        Some(Pat::Wildcard(s, Type::Hole))
    }

    fn pat_name(&mut self) -> Option<Pat> {
        let x0 = self.name()?;
        let s = x0.span;
        let t = Type::Hole;
        match self.peek() {
            Token::LBrace => {
                let xps = self.pat_fields()?;
                Some(Pat::Struct(s, t, x0, vec![], xps))
            }
            Token::ColonColon => {
                self.advance();
                if self.peek() == Token::LBrack {
                    let ts = self.type_args()?;
                    match self.peek() {
                        Token::ColonColon => {
                            self.advance();
                            let x1 = self.name()?;
                            let p = self.pat_variant()?;
                            Some(Pat::Enum(s, t, x0, ts, x1, Box::new(p)))
                        }
                        Token::LBrace => {
                            let xps = self.pat_fields()?;
                            Some(Pat::Struct(s, t, x0, ts, xps))
                        }
                        _ => Some(Pat::Struct(s, t, x0, ts, vec![])),
                    }
                } else {
                    let x1 = self.name()?;
                    let p = self.pat_variant()?;
                    Some(Pat::Enum(s, t, x0, vec![], x1, Box::new(p)))
                }
            }
            _ => Some(Pat::Var(x0.span, t, x0)),
        }
    }

    fn expr_variant(&mut self) -> Option<Expr> {
        if self.peek() == Token::LParen {
            let es = self.sep(Self::expr, Token::LParen, Token::RParen)?;
            if es.len() == 1 {
                Some(es.into_iter().next().unwrap())
            } else {
                Some(Expr::Tuple(Span::default(), Type::Hole, es))
            }
        } else {
            Some(Expr::Tuple(Span::default(), Type::Hole, vec![]))
        }
    }

    fn pat_variant(&mut self) -> Option<Pat> {
        if self.peek() == Token::LParen {
            let ps = self.sep(Self::pat, Token::LParen, Token::RParen)?;
            if ps.len() == 1 {
                Some(ps.into_iter().next().unwrap())
            } else {
                Some(Pat::Tuple(Span::default(), Type::Hole, ps))
            }
        } else {
            Some(Pat::Tuple(Span::default(), Type::Hole, vec![]))
        }
    }

    fn pat_fields(&mut self) -> Option<Vec<(Name, Pat)>> {
        self.sep(Self::pat_field, Token::LBrace, Token::RBrace)
    }

    fn pat_field(&mut self) -> Option<(Name, Pat)> {
        let pat = self.pat()?;
        match pat {
            Pat::Var(s, t, name) => {
                if self.optional(Token::Eq).is_some() {
                    let pat = self.pat()?;
                    Some((name, pat))
                } else {
                    Some((name.clone(), Pat::Var(s, t, name)))
                }
            }
            _ => None,
        }
    }
}
