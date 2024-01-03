pub type Name = String;

// forall <quantifiers>. <head> :- <body>
// impl<quantifiers> <head> for <body>
// i.e., impl<T> Clone for Vec<T> where T: Clone {}
#[derive(Debug)]
pub struct Rule {
    pub quantifiers: Vec<Name>,
    pub head: Impl,
    pub body: Vec<Impl>,
}

// <name>(<types>)
// e.g., Clone(i32)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub name: Name,
    pub types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Cons(Name, Vec<Type>),
    Var(Name),
}

pub struct Context {
    tvars: usize,
}

impl Context {
    pub fn new() -> Context {
        Context { tvars: 0 }
    }

    pub fn new_tyvar(&mut self) -> Type {
        self.tvars += 1;
        Type::Var(format!("T{}", self.tvars))
    }
}

impl Impl {
    pub fn new(name: impl Into<Name>, types: Vec<Type>) -> Impl {
        Impl {
            name: name.into(),
            types,
        }
    }
}

impl Type {
    pub fn cons(name: impl Into<Name>, args: Vec<Type>) -> Type {
        Type::Cons(name.into(), args)
    }

    pub fn var(name: impl Into<Name>) -> Type {
        Type::Var(name.into())
    }

    pub fn atom(name: impl Into<Name>) -> Type {
        Type::Cons(name.into(), Vec::new())
    }
}

impl Rule {
    pub fn new(quantifiers: Vec<Name>, head: Impl, body: Vec<Impl>) -> Rule {
        Rule { quantifiers, body, head }
    }
}
