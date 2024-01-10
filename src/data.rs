pub type Name = String;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

// An impl is like a rule
// forall <quantifiers> <head> :- <body>.
// impl<quantifiers> <head> where <body>
// i.e., impl<T> Clone for Vec<T> where T: Clone {}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtImpl {
    pub generics: Vec<Name>,
    pub head: Trait,
    pub body: Vec<Trait>,
    pub defs: Vec<StmtDef>,
}

impl StmtImpl {
    pub fn new(generics: Vec<Name>, head: Trait, body: Vec<Trait>, defs: Vec<StmtDef>) -> StmtImpl {
        StmtImpl {
            generics,
            body,
            head,
            defs,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtTrait {
    pub generics: Vec<Name>,
    pub head: Trait,
    pub body: Vec<Trait>,
    pub defs: Vec<StmtDef>,
    pub assocs: Vec<(Name, Type)>,
}

// A trait is like a predicate
// <name>(<types>)
// e.g., Clone(i32)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trait {
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
    pub name: Name,
    pub ty: Type,
}

impl Param {
    pub fn new(name: impl Into<Name>, ty: Type) -> Param {
        Param {
            name: name.into(),
            ty,
        }
    }
}

// TODO: Need to check traits whenever we unify.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CompilerError {
    TypeMismatch(Type, Type),
    TraitNotFound,
    TraitIsAmbiguous,
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
    Impl(StmtImpl),
    Struct(StmtStruct),
    Enum(StmtEnum),
    Expr(Expr),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtVar {
    pub name: Name,
    pub ty: Type,
    pub expr: Expr,
}

impl StmtVar {
    pub fn new(name: impl Into<Name>, ty: Type, expr: Expr) -> StmtVar {
        StmtVar {
            name: name.into(),
            ty,
            expr,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtDef {
    pub name: Name,
    pub generics: Vec<Name>,
    pub preds: Vec<Trait>,
    pub params: Vec<Param>,
    pub ty: Type,
    pub expr: Expr,
}

impl StmtDef {
    pub fn new(
        name: impl Into<Name>,
        generics: Vec<Name>,
        preds: Vec<Trait>,
        params: Vec<Param>,
        ty: Type,
        expr: Expr,
    ) -> StmtDef {
        StmtDef {
            name: name.into(),
            generics,
            preds,
            params,
            ty,
            expr,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtStruct {
    pub name: Name,
    pub generics: Vec<Name>,
    pub preds: Vec<Trait>,
    pub fields: Vec<(Name, Type)>,
}

impl StmtStruct {
    pub fn new(
        name: impl Into<Name>,
        generics: Vec<Name>,
        preds: Vec<Trait>,
        fields: Vec<(Name, Type)>,
    ) -> StmtStruct {
        StmtStruct {
            name: name.into(),
            generics,
            preds,
            fields,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StmtEnum {
    pub name: Name,
    pub generics: Vec<Name>,
    pub preds: Vec<Trait>,
    pub variants: Vec<(Name, Type)>,
}

impl StmtEnum {
    pub fn new(
        name: impl Into<Name>,
        generics: Vec<Name>,
        preds: Vec<Trait>,
        variants: Vec<(Name, Type)>,
    ) -> StmtEnum {
        StmtEnum {
            name: name.into(),
            generics,
            preds,
            variants,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Int(Type, String),
    Float(Type, String),
    Bool(Type, bool),
    String(Type, String),
    Unit(Type),
    Struct(Type, Name, Vec<(Name, Expr)>),
    Tuple(Type, Vec<Expr>),
    Enum(Type, Name, Name, Box<Expr>),
    Field(Type, Box<Expr>, Name),
    Var(Type, Name),
    Call(Type, Box<Expr>, Vec<Expr>),
    Block(Type, Vec<Stmt>, Box<Expr>),
    From(Type, Name, Box<Expr>, Box<Query>),
    Assoc(Type, Trait, Name),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query {
    From(Name, Box<Expr>, Box<Query>),
    Where(WhereExpr, Box<Query>),
    Select(Vec<Expr>, Box<Query>),
    Into(Name, Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WhereExpr {
    And(Box<WhereExpr>, Box<WhereExpr>),
    Or(Box<WhereExpr>, Box<WhereExpr>),
    Not(Box<WhereExpr>),
    Lt(Expr, Expr),
    Leq(Expr, Expr),
    Var(Name),
    Call(Expr, Vec<Expr>),
}

impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Program {
        Program { stmts }
    }
}

impl Trait {
    pub fn new(name: impl Into<Name>, types: Vec<Type>, assocs: Vec<(Name, Type)>) -> Trait {
        Trait {
            name: name.into(),
            types,
            assocs,
        }
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

impl Expr {
    pub fn type_of(&self) -> &Type {
        match self {
            Expr::Int(t, ..) => t,
            Expr::Float(t, ..) => t,
            Expr::Bool(t, ..) => t,
            Expr::String(t, ..) => t,
            Expr::Unit(t) => t,
            Expr::Struct(t, ..) => t,
            Expr::Tuple(t, ..) => t,
            Expr::Enum(t, ..) => t,
            Expr::Var(t, ..) => t,
            Expr::Call(t, ..) => t,
            Expr::Block(t, ..) => t,
            Expr::From(t, ..) => t,
            Expr::Field(t, ..) => t,
            Expr::Assoc(t, _, _) => t,
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
