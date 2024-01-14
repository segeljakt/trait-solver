use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct Name {
    pub span: Span,
    pub data: String,
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
            data: s.to_string(),
        }
    }
}

impl From<String> for Name {
    fn from(s: String) -> Name {
        Name {
            span: Span::default(),
            data: s,
        }
    }
}

impl Name {
    pub fn new(span: Span, data: impl Into<String>) -> Name {
        Name {
            span,
            data: data.into(),
        }
    }
}

impl Index {
    pub fn new(span: Span, index: usize) -> Index {
        Index { span, index }
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
    pub body: Vec<Trait>,
    pub defs: Vec<StmtDef>,
}

impl PartialEq for StmtImpl {
    fn eq(&self, other: &Self) -> bool {
        self.generics == other.generics
            && self.head == other.head
            && self.body == other.body
            && self.defs == other.defs
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtTrait {
    pub span: Span,
    pub name: Name,
    pub generics: Vec<Name>,
    pub body: Vec<Trait>,
    pub defs: Vec<StmtDef>,
    pub assocs: Vec<(Name, Type)>,
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
    Hole,
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
    pub preds: Vec<Trait>,
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
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variant {
    pub span: Span,
    pub name: Name,
    pub ty: Type,
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
    pub index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Int(Span, Type, String),
    Float(Span, Type, String),
    Bool(Span, Type, bool),
    String(Span, Type, String),
    Unit(Span, Type),
    Struct(Span, Type, Name, Vec<(Name, Expr)>),
    Tuple(Span, Type, Vec<Expr>),
    Enum(Span, Type, Name, Name, Box<Expr>),
    Field(Span, Type, Box<Expr>, Name),
    Index(Span, Type, Box<Expr>, Index),
    Var(Span, Type, Name, Vec<Type>),
    Call(Span, Type, Box<Expr>, Vec<Expr>),
    Block(Span, Type, Vec<Stmt>, Box<Expr>),
    From(Span, Type, Name, Box<Expr>, Box<Query>),
    Assoc(Span, Type, Trait, Name, Vec<Type>),
    Err(Span, Type),
    Array(Span, Type, Vec<Expr>),
    Assign(Span, Type, Box<Expr>, Box<Expr>),
    Return(Span, Type, Box<Expr>),
    Continue(Span, Type),
    Break(Span, Type),
    Fun(Span, Type, Vec<Param>, Type, Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query {
    From(Span, Name, Box<Expr>, Box<Query>),
    Where(Span, WhereExpr, Box<Query>),
    Select(Span, Vec<Expr>, Box<Query>),
    Into(Span, Name, Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WhereExpr {
    And(Span, Box<WhereExpr>, Box<WhereExpr>),
    Or(Span, Box<WhereExpr>, Box<WhereExpr>),
    Not(Span, Box<WhereExpr>),
    Lt(Span, Expr, Expr),
    Leq(Span, Expr, Expr),
    Var(Span, Name),
    Call(Span, Expr, Vec<Expr>),
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
        body: Vec<Trait>,
        defs: Vec<StmtDef>,
    ) -> StmtImpl {
        StmtImpl {
            span,
            generics,
            body,
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
        defs: Vec<StmtDef>,
        assocs: Vec<(Name, Type)>,
    ) -> StmtTrait {
        StmtTrait {
            span,
            name,
            generics,
            body,
            defs,
            assocs,
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
            preds,
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
        variants: Vec<Variant>,
    ) -> StmtEnum {
        StmtEnum {
            span,
            name,
            generics,
            variants,
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

impl Variant {
    pub fn new(span: Span, name: Name, ty: Type) -> Variant {
        Variant { span, name, ty }
    }
}

impl Type {
    pub fn cons(x: impl Into<Name>, ts: Vec<Type>) -> Type {
        Type::Cons(x.into(), ts)
    }

    pub fn var(x: impl Into<Name>) -> Type {
        Type::Var(x.into())
    }

    pub fn atom(x: impl Into<Name>) -> Type {
        Type::Cons(x.into(), Vec::new())
    }

    pub fn assoc(i: Trait, x: impl Into<Name>) -> Type {
        Type::Assoc(i, x.into())
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

impl Expr {
    pub fn with(self, t: Type) -> Expr {
        match self {
            Expr::Int(s, _, v) => Expr::Int(s, t, v),
            Expr::Float(s, _, v) => Expr::Float(s, t, v),
            Expr::Bool(s, _, v) => Expr::Bool(s, t, v),
            Expr::String(s, _, v) => Expr::String(s, t, v),
            Expr::Unit(s, _) => Expr::Unit(s, t),
            Expr::Var(s, _, x, ts) => Expr::Var(s, t, x, ts),
            Expr::Call(s, _, e, es) => Expr::Call(s, t, e, es),
            Expr::Block(s, _, ss, e) => Expr::Block(s, t, ss, e),
            Expr::From(..) => todo!(),
            Expr::Struct(_s, _, _, _) => todo!(),
            Expr::Enum(_s, _, _, _, _) => todo!(),
            Expr::Field(_s, _, _, _) => todo!(),
            Expr::Tuple(_s, _, _) => todo!(),
            Expr::Assoc(_s, _, _, _, _) => todo!(),
            Expr::Err(s, _) => Expr::Err(s, t),
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Assign(_, _, _, _) => todo!(),
            Expr::Return(_, _, _) => todo!(),
            Expr::Continue(_, _) => todo!(),
            Expr::Break(_, _) => todo!(),
            Expr::Fun(_, _, _, _, _) => todo!(),
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
            Expr::Unit(_, t) => t,
            Expr::Struct(_, t, ..) => t,
            Expr::Tuple(_, t, ..) => t,
            Expr::Enum(_, t, ..) => t,
            Expr::Var(_, t, ..) => t,
            Expr::Call(_, t, ..) => t,
            Expr::Block(_, t, ..) => t,
            Expr::From(_, t, ..) => t,
            Expr::Field(_, t, ..) => t,
            Expr::Assoc(_, t, _, _, _) => t,
            Expr::Err(_, t) => t,
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Assign(_, _, _, _) => todo!(),
            Expr::Return(_, _, _) => todo!(),
            Expr::Continue(_, _) => todo!(),
            Expr::Break(_, _) => todo!(),
            Expr::Fun(_, _, _, _, _) => todo!(),
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
            Expr::Unit(s, ..) => *s,
            Expr::Struct(s, ..) => *s,
            Expr::Tuple(s, ..) => *s,
            Expr::Enum(s, ..) => *s,
            Expr::Field(s, ..) => *s,
            Expr::Var(s, ..) => *s,
            Expr::Call(s, ..) => *s,
            Expr::Block(s, ..) => *s,
            Expr::From(s, ..) => *s,
            Expr::Assoc(s, ..) => *s,
            Expr::Err(s, _) => *s,
            Expr::Index(_, _, _, _) => todo!(),
            Expr::Array(_, _, _) => todo!(),
            Expr::Assign(_, _, _, _) => todo!(),
            Expr::Return(_, _, _) => todo!(),
            Expr::Continue(_, _) => todo!(),
            Expr::Break(_, _) => todo!(),
            Expr::Fun(_, _, _, _, _) => todo!(),
        }
    }
}
