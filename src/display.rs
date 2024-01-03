use crate::data::Impl;
use crate::data::Rule;
use crate::data::Type;

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} :- ", self.head)?;
        let mut iter = self.body.iter();
        if let Some(t) = iter.next() {
            write!(f, "{}", t)?;
        }
        for t in iter {
            write!(f, ", {}", t)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Impl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name,)?;
        let mut iter = self.types.iter();
        if let Some(t) = iter.next() {
            write!(f, "(")?;
            write!(f, "{}", t)?;
            for t in iter {
                write!(f, ", {}", t)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Cons(name, types) => {
                write!(f, "{}", name,)?;
                let mut iter = types.iter();
                if let Some(t) = iter.next() {
                    write!(f, "<")?;
                    write!(f, "{}", t)?;
                    for t in iter {
                        write!(f, ", {}", t)?;
                    }
                    write!(f, ">")?;
                }
            }
            Type::Var(name) => {
                write!(f, "?{}", name)?;
            }
        }
        Ok(())
    }
}
