use crate::data::Impl;
use crate::data::Rule;
use crate::data::Type;

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.head)?;
        let mut iter = self.body.iter();
        if let Some(t) = iter.next() {
            write!(f, " :- {t}")?;
            for t in iter {
                write!(f, ", {t}")?;
            }
        }
        write!(f, ".")?;
        Ok(())
    }
}

impl std::fmt::Display for Impl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        let mut iter = self.types.iter();
        if let Some(t) = iter.next() {
            write!(f, "(")?;
            write!(f, "{t}")?;
            for t in iter {
                write!(f, ", {t}")?;
            }
            let mut iter = self.assocs.iter();
            if let Some((x, t)) = iter.next() {
                write!(f, ", {x}={t}")?;
                for (x, t) in iter {
                    write!(f, ", {x}={t}")?;
                }
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Cons(x, ts) => {
                write!(f, "{x}")?;
                let mut iter = ts.iter();
                if let Some(t) = iter.next() {
                    write!(f, "<")?;
                    write!(f, "{t}")?;
                    for t in iter {
                        write!(f, ", {t}")?;
                    }
                    write!(f, ">")?;
                }
            }
            Type::Assoc(i, x) => {
                write!(f, "{i}::{x}")?;
            }
            Type::Var(x) => {
                write!(f, "{x}")?;
            }
        }
        Ok(())
    }
}
