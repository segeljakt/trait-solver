use smol_str::SmolStr;

use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct Name {
    pub span: Span,
    pub data: SmolStr,
}

impl Eq for Name {}
impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl From<&str> for Name {
    fn from(s: &str) -> Name {
        Name {
            span: Span::default(),
            data: SmolStr::from(s),
        }
    }
}

impl From<String> for Name {
    fn from(s: String) -> Name {
        Name {
            span: Span::default(),
            data: SmolStr::from(s),
        }
    }
}

impl Name {
    pub fn new(span: Span, data: impl Into<SmolStr>) -> Name {
        Name {
            span,
            data: data.into(),
        }
    }
}

impl Index {
    pub fn new(span: Span, index: usize) -> Index {
        Index { span, data: index }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

// An impl is like a rule
// forall <quantifiers> <head> :- <body>.
// impl<quantifiers> <head> where <body>
// i.e., impl<T> Clone for Vec<T> where T: Clone {}
#[derive(Debug, Clone, Eq)]
pub struct StmtImpl {
    pub span: Span,
    pub generics: Vec<Name>,
    pub head: Trait,
    pub where_clause: Vec<Trait>,
    pub defs: Vec<StmtDef>,
    pub types: Vec<StmtType>,
}

impl PartialEq for StmtImpl {
    fn eq(&self, other: &Self) -> bool {
        self.generics == other.generics
            && self.head == other.head
            && self.where_clause == other.where_clause
            && self.defs == other.defs
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtTrait {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub body: Vec<Trait>,
    pub defs: Vec<StmtDefDecl>,
    pub types: Vec<StmtTypeDecl>,
}

// A trait is like a predicate
// <name>(<types>)
// e.g., Clone(i32)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trait {
    pub span: Span,
    pub name: Name,
    pub types: Vec<Type>,
    pub assocs: Vec<(Name, Type)>,
}

// A type is like a proposition
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Cons(Name, Vec<Type>),
    Assoc(Trait, Name),
    Var(Name),
    Gen(Name),
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Record(Vec<(Name, Type)>),
    Hole,
    Err,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub span: Span,
    pub name: Name,
    pub ty: Type,
}

pub enum Candidate {
    // An implementation of a trait for a type.
    Impl(StmtImpl),
    // A bound in a where clause
    Pred(Trait),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    Var(StmtVar),
    Def(StmtDef),
    Trait(StmtTrait),
    Impl(StmtImpl),
    Struct(StmtStruct),
    Enum(StmtEnum),
    Type(StmtType),
    Expr(Expr),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtTypeDecl {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtDefDecl {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub where_clause: Vec<Trait>,
    pub params: Vec<Param>,
    pub ty: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtVar {
    pub span: Span,
    pub name: Name,
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtDef {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub where_clause: Vec<Trait>,
    pub params: Vec<Param>,
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtStruct {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub fields: Vec<(Name, Type)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtEnum {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub variants: Vec<(Name, Type)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtType {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub ty: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Index {
    pub span: Span,
    pub data: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Int(Span, Type, String),
    Float(Span, Type, String),
    Bool(Span, Type, bool),
    String(Span, Type, String),
    Struct(Span, Type, Name, Vec<Type>, Vec<(Name, Expr)>),
    Tuple(Span, Type, Vec<Expr>),
    Record(Span, Type, Vec<(Name, Expr)>),
    Enum(Span, Type, Name, Vec<Type>, Name, Box<Expr>),
    Field(Span, Type, Box<Expr>, Name),
    Index(Span, Type, Box<Expr>, Index),
    Var(Span, Type, Name, Vec<Type>),
    Call(Span, Type, Box<Expr>, Vec<Expr>),
    Block(Span, Type, Vec<Stmt>, Box<Expr>),
    Query(Span, Type, Vec<Query>),
    Assoc(Span, Type, Trait, Name, Vec<Type>),
    Match(Span, Type, Box<Expr>, Vec<(Pat, Expr)>),
    Array(Span, Type, Vec<Expr>),
    Assign(Span, Type, Box<Expr>, Box<Expr>),
    Return(Span, Type, Box<Expr>),
    Continue(Span, Type),
    Break(Span, Type),
    While(Span, Type, Box<Expr>, Box<Expr>),
    Fun(Span, Type, Vec<Param>, Type, Box<Expr>),
    And(Span, Type, Box<Expr>, Box<Expr>),
    Or(Span, Type, Box<Expr>, Box<Expr>),
    Err(Span, Type),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Arm {
    pub span: Span,
    pub pat: Pat,
    pub expr: Expr,
}

impl Arm {
    pub fn new(span: Span, pat: Pat, expr: Expr) -> Arm {
        Arm { span, pat, expr }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pat {
    Var(Span, Type, Name),
    Tuple(Span, Type, Vec<Pat>),
    Struct(Span, Type, Name, Vec<Type>, Vec<(Name, Pat)>),
    Enum(Span, Type, Name, Vec<Type>, Name, Box<Pat>),
    Int(Span, Type, String),
    String(Span, Type, String),
    Wildcard(Span, Type),
    Bool(Span, Type, bool),
    Err(Span, Type),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query {
    From(Span, Type, Vec<(Name, Expr)>),
    Where(Span, Type, Box<Expr>),
    Select(Span, Type, Vec<(Name, Expr)>),
    Join(Span, Type, Vec<(Name, Expr)>, Box<Expr>),
    Group(Span, Type, Vec<Name>, Vec<Query>),
    Over(Span, Type, Box<Expr>, Vec<Query>),
    Order(Span, Type, Vec<(Name, bool)>),
    With(Span, Type, Vec<(Name, Expr)>),
    Into(Span, Type, Vec<Expr>),
    Compute(Span, Type, Vec<(Name, Expr, Expr)>),
}

impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Program {
        Program { stmts }
    }
}

impl StmtImpl {
    pub fn new(
        span: Span,
        generics: Vec<Name>,
        head: Trait,
        where_clause: Vec<Trait>,
        defs: Vec<StmtDef>,
        types: Vec<StmtType>,
    ) -> StmtImpl {
        StmtImpl {
            span,
            generics,
            where_clause,
            types,
            head,
            defs,
        }
    }
}

impl StmtType {
    pub fn new(span: Span, name: Name, generics: Vec<Name>, ty: Type) -> StmtType {
        StmtType {
            span,
            name,
            generics,
            ty,
        }
    }
}

impl StmtTrait {
    pub fn new(
        span: Span,
        name: Name,
        generics: Vec<Name>,
        body: Vec<Trait>,
        defs: Vec<StmtDefDecl>,
        types: Vec<StmtTypeDecl>,
    ) -> StmtTrait {
        StmtTrait {
            span,
            name,
            generics,
            body,
            defs,
            types,
        }
    }
}

impl StmtTypeDecl {
    pub fn new(span: Span, name: Name, generics: Vec<Name>) -> Self {
        Self {
            span,
            name,
            generics,
        }
    }
}

impl Param {
    pub fn new(span: Span, name: Name, ty: Type) -> Param {
        Param { span, name, ty }
    }
}

impl StmtVar {
    pub fn new(span: Span, name: Name, ty: Type, expr: Expr) -> StmtVar {
        StmtVar {
            span,
            name,
            ty,
            expr,
        }
    }
}

impl StmtDef {
    pub fn new(
        span: Span,
        name: Name,
        generics: Vec<Name>,
        preds: Vec<Trait>,
        params: Vec<Param>,
        ty: Type,
        expr: Expr,
    ) -> StmtDef {
        StmtDef {
            span,
            name,
            generics,
            where_clause: preds,
            params,
            ty,
            expr,
        }
    }
}

impl StmtStruct {
    pub fn new(
        span: Span,
        name: Name,
        generics: Vec<Name>,
        fields: Vec<(Name, Type)>,
    ) -> StmtStruct {
        StmtStruct {
            span,
            name,
            generics,
            fields,
        }
    }
}

impl StmtEnum {
    pub fn new(
        span: Span,
        name: Name,
        generics: Vec<Name>,
        variants: Vec<(Name, Type)>,
    ) -> StmtEnum {
        StmtEnum {
            span,
            name,
            generics,
            variants,
        }
    }
}

impl StmtDefDecl {
    pub fn new(
        span: Span,
        name: Name,
        generics: Vec<Name>,
        where_clause: Vec<Trait>,
        params: Vec<Param>,
        ty: Type,
    ) -> Self {
        Self {
            span,
            name,
            generics,
            where_clause,
            params,
            ty,
        }
    }
}

impl Trait {
    pub fn new(span: Span, name: Name, types: Vec<Type>, assocs: Vec<(Name, Type)>) -> Trait {
        Trait {
            span,
            name,
            types,
            assocs,
        }
    }
}

impl From<StmtVar> for Stmt {
    fn from(v: StmtVar) -> Stmt {
        Stmt::Var(v)
    }
}

impl From<StmtDef> for Stmt {
    fn from(d: StmtDef) -> Stmt {
        Stmt::Def(d)
    }
}
impl From<StmtImpl> for Stmt {
    fn from(i: StmtImpl) -> Stmt {
        Stmt::Impl(i)
    }
}

impl From<Expr> for Stmt {
    fn from(e: Expr) -> Stmt {
        Stmt::Expr(e)
    }
}

impl From<StmtStruct> for Stmt {
    fn from(s: StmtStruct) -> Stmt {
        Stmt::Struct(s)
    }
}

impl From<StmtEnum> for Stmt {
    fn from(e: StmtEnum) -> Stmt {
        Stmt::Enum(e)
    }
}

impl From<StmtType> for Stmt {
    fn from(t: StmtType) -> Stmt {
        Stmt::Type(t)
    }
}

impl From<StmtTrait> for Stmt {
    fn from(t: StmtTrait) -> Stmt {
        Stmt::Trait(t)
    }
}

impl Expr {
    pub fn with(self, t: Type) -> Expr {
        match self {
            Expr::Int(s, _, v) => Expr::Int(s, t, v),
            Expr::Float(s, _, v) => Expr::Float(s, t, v),
            Expr::Bool(s, _, v) => Expr::Bool(s, t, v),
            Expr::String(s, _, v) => Expr::String(s, t, v),
            Expr::Var(s, _, x, ts) => Expr::Var(s, t, x, ts),
            Expr::Call(s, _, e, es) => Expr::Call(s, t, e, es),
            Expr::Block(s, _, ss, e) => Expr::Block(s, t, ss, e),
            Expr::Query(s, _, qs) => Expr::Query(s, t, qs),
            Expr::Struct(s, _, x, ts, xes) => Expr::Struct(s, t, x, ts, xes),
            Expr::Enum(s, _, x0, ts, x1, e) => Expr::Enum(s, t, x0, ts, x1, e),
            Expr::Field(s, _, e, x) => Expr::Field(s, t, e, x),
            Expr::Tuple(s, _, es) => Expr::Tuple(s, t, es),
            Expr::Assoc(s, _, tr, x, ts) => Expr::Assoc(s, t, tr, x, ts),
            Expr::Index(s, _, e, i) => Expr::Index(s, t, e, i),
            Expr::Array(s, _, es) => Expr::Array(s, t, es),
            Expr::Assign(s, _, e0, e1) => Expr::Assign(s, t, e0, e1),
            Expr::Return(s, _, e) => Expr::Return(s, t, e),
            Expr::Continue(s, _) => Expr::Continue(s, t),
            Expr::Break(s, _) => Expr::Break(s, t),
            Expr::Fun(s, _, ps, t1, e) => Expr::Fun(s, t, ps, t1, e),
            Expr::And(s, _, e0, e1) => Expr::And(s, t, e0, e1),
            Expr::Or(s, _, e0, e1) => Expr::Or(s, t, e0, e1),
            Expr::Match(s, _, e, pes) => Expr::Match(s, t, e, pes),
            Expr::Err(s, _) => Expr::Err(s, t),
            Expr::While(s, _, e0, e1) => Expr::While(s, t, e0, e1),
            Expr::Record(_, _, _) => todo!(),
        }
    }
}

impl Pat {
    pub fn with(self, t: Type) -> Pat {
        match self {
            Pat::Var(s, _, x) => Pat::Var(s, t, x),
            Pat::Tuple(s, _, ps) => Pat::Tuple(s, t, ps),
            Pat::Struct(s, _, x, ts, xps) => Pat::Struct(s, t, x, ts, xps),
            Pat::Enum(s, _, x0, ts, x1, p) => Pat::Enum(s, t, x0, ts, x1, p),
            Pat::Int(s, _, v) => Pat::Int(s, t, v),
            Pat::String(s, _, v) => Pat::String(s, t, v),
            Pat::Wildcard(s, _) => Pat::Wildcard(s, t),
            Pat::Bool(s, _, v) => Pat::Bool(s, t, v),
            Pat::Err(s, _) => Pat::Err(s, t),
        }
    }
}

impl Expr {
    pub fn ty(&self) -> &Type {
        match self {
            Expr::Int(_, t, ..) => t,
            Expr::Float(_, t, ..) => t,
            Expr::Bool(_, t, ..) => t,
            Expr::String(_, t, ..) => t,
            Expr::Struct(_, t, ..) => t,
            Expr::Tuple(_, t, ..) => t,
            Expr::Enum(_, t, ..) => t,
            Expr::Var(_, t, ..) => t,
            Expr::Call(_, t, ..) => t,
            Expr::Block(_, t, ..) => t,
            Expr::Query(_, t, ..) => t,
            Expr::Field(_, t, ..) => t,
            Expr::Assoc(_, t, ..) => t,
            Expr::Err(_, t) => t,
            Expr::Index(_, t, ..) => t,
            Expr::Array(_, t, ..) => t,
            Expr::Assign(_, t, ..) => t,
            Expr::Return(_, t, ..) => t,
            Expr::Continue(_, t) => t,
            Expr::Break(_, t) => t,
            Expr::Fun(_, t, ..) => t,
            Expr::And(_, t, ..) => t,
            Expr::Or(_, t, ..) => t,
            Expr::Match(_, t, ..) => t,
            Expr::While(_, t, ..) => t,
            Expr::Record(_, _, _) => todo!(),
        }
    }
}

impl Pat {
    pub fn ty(&self) -> &Type {
        match self {
            Pat::Var(_, t, ..) => t,
            Pat::Tuple(_, t, ..) => t,
            Pat::Struct(_, t, ..) => t,
            Pat::Enum(_, t, ..) => t,
            Pat::Int(_, t, ..) => t,
            Pat::Wildcard(_, t, ..) => t,
            Pat::String(_, t, ..) => t,
            Pat::Bool(_, t, ..) => t,
            Pat::Err(_, t) => t,
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(s, ..) => *s,
            Expr::Float(s, ..) => *s,
            Expr::Bool(s, ..) => *s,
            Expr::String(s, ..) => *s,
            Expr::Struct(s, ..) => *s,
            Expr::Tuple(s, ..) => *s,
            Expr::Enum(s, ..) => *s,
            Expr::Field(s, ..) => *s,
            Expr::Var(s, ..) => *s,
            Expr::Call(s, ..) => *s,
            Expr::Block(s, ..) => *s,
            Expr::Query(s, ..) => *s,
            Expr::Assoc(s, ..) => *s,
            Expr::Index(s, ..) => *s,
            Expr::Array(s, ..) => *s,
            Expr::Assign(s, ..) => *s,
            Expr::Return(s, ..) => *s,
            Expr::Continue(s, ..) => *s,
            Expr::Break(s, ..) => *s,
            Expr::Fun(s, ..) => *s,
            Expr::And(s, ..) => *s,
            Expr::Or(s, ..) => *s,
            Expr::Match(s, ..) => *s,
            Expr::Err(s, ..) => *s,
            Expr::While(s, ..) => *s,
            Expr::Record(_, _, _) => todo!(),
        }
    }
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Var(s, ..) => *s,
            Pat::Tuple(s, ..) => *s,
            Pat::Struct(s, ..) => *s,
            Pat::Enum(s, ..) => *s,
            Pat::Int(s, ..) => *s,
            Pat::Wildcard(s, ..) => *s,
            Pat::String(s, ..) => *s,
            Pat::Bool(s, ..) => *s,
            Pat::Err(s, _) => *s,
        }
    }
}

impl Query {
    pub fn span(&self) -> Span {
        match self {
            Query::From(s, ..) => *s,
            Query::Where(s, ..) => *s,
            Query::Select(s, ..) => *s,
            Query::Into(s, ..) => *s,
            Query::Join(s, ..) => *s,
            Query::Group(s, ..) => *s,
            Query::Over(s, ..) => *s,
            Query::Order(s, ..) => *s,
            Query::With(s, ..) => *s,
            Query::Compute(s, ..) => *s,
        }
    }
}
