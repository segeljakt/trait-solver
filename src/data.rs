pub type Name = String;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

// forall <quantifiers> <head> :- <body>.
// impl<quantifiers> <head> where <body>
// i.e., impl<T> Clone for Vec<T> where T: Clone {}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Impl {
    pub generics: Vec<Name>,
    pub head: Pred,
    pub body: Vec<Pred>,
    pub defs: Vec<Def>,
}

// <name>(<types>)
// e.g., Clone(i32)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Pred {
    pub name: Name,
    pub types: Vec<Type>,
    pub assocs: Vec<(Name, Type)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Cons(Name, Vec<Type>),
    Assoc(Pred, Name),
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
    Impl(Impl),
    // A bound in a where clause
    Pred(Pred),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    Var(Var),
    Def(Def),
    Impl(Impl),
    Expr(Expr),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Var {
    pub name: Name,
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: Name,
    pub generics: Vec<Name>,
    pub preds: Vec<Pred>,
    pub params: Vec<Param>,
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Int(Type, String),
    Float(Type, String),
    Bool(Type, bool),
    Var(Type, Name),
    Call(Type, Box<Expr>, Vec<Expr>),
    Block(Type, Vec<Stmt>, Box<Expr>),
    From(Type, Name, Box<Expr>, Box<Query>),
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

pub struct Context {
    tvars: usize,
    stack: Vec<Scope>,
    pub errors: Vec<CompilerError>,
}

pub struct Scope {
    impls: Vec<Impl>,
    binds: Vec<(Name, Binding)>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            impls: vec![],
            binds: vec![],
        }
    }
}

// pub struct Goal {}

// pub struct ImplDef {
//     generics: Vec<Name>,
//     def: Def,
//     imp: Pred,
// }

#[derive(Debug, Clone)]
pub enum Binding {
    Var(Type),
    Def(Vec<Name>, Vec<Pred>, Vec<Type>, Type),
    Impl(Impl),
}

impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Program {
        Program { stmts }
    }
}

impl Var {
    pub fn new(name: impl Into<Name>, ty: Type, expr: Expr) -> Var {
        Var {
            name: name.into(),
            ty,
            expr,
        }
    }
}

impl Def {
    pub fn new(
        name: impl Into<Name>,
        generics: Vec<Name>,
        preds: Vec<Pred>,
        params: Vec<Param>,
        ty: Type,
        expr: Expr,
    ) -> Def {
        Def {
            name: name.into(),
            generics,
            preds,
            params,
            ty,
            expr,
        }
    }
}

impl Context {
    pub fn new() -> Context {
        Context {
            tvars: 0,
            stack: vec![Scope::new()],
            errors: vec![],
        }
    }

    pub fn recover<T>(&mut self, r: Result<T, CompilerError>) {
        if let Err(e) = r {
            self.errors.push(e);
        }
    }

    pub fn new_tyvar(&mut self) -> Type {
        self.tvars += 1;
        Type::Var(format!("?T{}", self.tvars - 1))
    }

    pub fn get(&self, x1: &str) -> Option<&Binding> {
        self.stack
            .iter()
            .rev()
            .find_map(|s| s.binds.iter().find(|(x0, _)| x0 == x1).map(|(_, b)| b))
    }

    pub fn bind(&mut self, name: Name, b: Binding) {
        self.stack.last_mut().unwrap().binds.push((name, b));
    }

    pub fn add_impl(&mut self, i: Impl) {
        self.stack.last_mut().unwrap().impls.push(i);
    }

    pub fn impls(&self) -> Vec<Impl> {
        self.stack
            .iter()
            .rev()
            .flat_map(|s| s.impls.clone())
            .collect()
    }
}

impl Pred {
    pub fn new(name: impl Into<Name>, types: Vec<Type>, assocs: Vec<(Name, Type)>) -> Pred {
        Pred {
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

    pub fn assoc(i: Pred, x: impl Into<Name>) -> Type {
        Type::Assoc(i, x.into())
    }
}

impl Impl {
    pub fn new(generics: Vec<Name>, head: Pred, body: Vec<Pred>, defs: Vec<Def>) -> Impl {
        Impl {
            generics,
            body,
            head,
            defs,
        }
    }
}

impl Expr {
    pub fn type_of(&self) -> &Type {
        match self {
            Expr::Int(t, ..) => t,
            Expr::Float(t, ..) => t,
            Expr::Bool(t, ..) => t,
            Expr::Var(t, ..) => t,
            Expr::Call(t, ..) => t,
            Expr::Block(t, ..) => t,
            Expr::From(t, ..) => t,
        }
    }
}
