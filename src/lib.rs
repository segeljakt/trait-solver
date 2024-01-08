pub mod annotate;
pub mod apply;
pub mod data;
pub mod display;
pub mod helper;
pub mod infer;
pub mod instantiate;
pub mod parser;
pub mod token;

use data::Candidate;
use data::CompilerError;
use data::Context;
use data::Impl;
use data::Name;
use data::Pred;
use data::Type;
use instantiate::instantiate_impl;

// For each function matching the goal, solve the subgoals.
// Max one solution is allowed.
//
// trait Foo<T> {
//     fn foo<U>(&self, x: T, y: U) -> T;
// }
//
// <T> fn foo<U>(x: T, y: U) -> T where Foo<T>;
//
// forall<T> {
//     fn foo<U>(x: T, y: U) -> T :- Foo<T>;
// }

pub fn solve(
    goal: &Pred,
    impls: &[Impl],
    preds: &[Pred],
    sub: &mut Vec<(Name, Type)>,
    ctx: &mut Context,
) -> Result<Candidate, CompilerError> {
    let mut solutions = vec![];
    for p in preds {
        if matches(sub, goal, p) {
            solutions.push(Candidate::Pred(p.clone()));
        }
    }
    for i in impls {
        let i = instantiate_impl(i, ctx);
        let mut sub = sub.clone();
        if matches(&mut sub, goal, &i.head) {
            if i.body
                .iter()
                .all(|subgoal| solve(subgoal, impls, preds, &mut sub, ctx).is_ok())
            {
                solutions.push(Candidate::Impl(i));
            }
        }
    }
    match solutions.len() {
        0 => Err(CompilerError::TraitNotFound),
        1 => Ok(solutions.pop().unwrap()),
        _ => Err(CompilerError::TraitIsAmbiguous),
    }
}

// Checks if i0 and i1 match, and if so, returns a substitution.
fn matches(s: &mut Vec<(Name, Type)>, i0: &Pred, i1: &Pred) -> bool {
    i0.name == i1.name
        && i0.types.len() == i1.types.len()
        && i1
            .types
            .iter()
            .zip(i0.types.iter())
            .all(|(t0, t1)| unify(s, &t0, &t1).is_ok())
        && i1
            .assocs
            .iter()
            .zip(i0.assocs.iter())
            .all(|((_, t0), (_, t1))| unify(s, &t0, &t1).is_ok())
}

pub fn unify(s0: &mut Vec<(Name, Type)>, t0: &Type, t1: &Type) -> Result<(), CompilerError> {
    let t0 = t0.apply(&s0);
    let t1 = t1.apply(&s0);
    let s1 = mgu(&t0, &t1)?;
    *s0 = compose(s0.clone(), s1);
    Ok(())
}

fn compose(s0: Vec<(Name, Type)>, s1: Vec<(Name, Type)>) -> Vec<(Name, Type)> {
    s1.into_iter()
        .map(|(x, t)| (x, t.apply(&s0)))
        .chain(s0.clone())
        .collect()
}

fn mgu(t0: &Type, t1: &Type) -> Result<Vec<(Name, Type)>, CompilerError> {
    match (t0, t1) {
        (Type::Cons(x0, ts0), Type::Cons(x1, ts1)) => {
            if x0 == x1 && ts0.len() == ts1.len() {
                ts0.iter()
                    .zip(ts1.iter())
                    .try_fold(vec![], |mut s, (t0, t1)| {
                        s.extend(mgu(t0, t1)?);
                        Ok(s)
                    })
            } else {
                Err(CompilerError::TypeMismatch(t0.clone(), t1.clone()))
            }
        }
        (t1, Type::Assoc(i, x)) | (Type::Assoc(i, x), t1) => {
            let (_, t2) = i.assocs.iter().find(|(n, _)| n == x).unwrap();
            mgu(t1, t2)
        }
        (t1, Type::Var(x)) | (Type::Var(x), t1) => Ok(vec![(x.clone(), t1.clone())]),
        (_, Type::Hole) | (Type::Hole, _) => unreachable!(),
    }
}
