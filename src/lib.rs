pub mod annotate;
pub mod apply;
pub mod ast;
pub mod diag;
pub mod display;
pub mod helper;
pub mod infer;
pub mod instantiate;
pub mod lexer;
pub mod parser;

use ast::Candidate;
use ast::Name;
use ast::StmtImpl;
use ast::Trait;
use ast::Type;
use infer::Context;
use instantiate::instantiate_impl;

pub fn solve(
    goal: &Trait,
    impls: &[StmtImpl],
    where_clause: &[Trait],
    sub: &mut Vec<(Name, Type)>,
    ctx: &mut Context,
) -> Option<Candidate> {
    let mut solutions = vec![];
    for tr in where_clause {
        if matches(sub, goal, tr) {
            solutions.push(Candidate::Pred(tr.clone()));
        }
    }
    for i in impls {
        let i = instantiate_impl(i, ctx);
        let mut sub = sub.clone();
        if matches(&mut sub, goal, &i.head) {
            if i.body
                .iter()
                .all(|subgoal| solve(subgoal, impls, where_clause, &mut sub, ctx).is_some())
            {
                solutions.push(Candidate::Impl(i));
            }
        }
    }
    // TODO: Return the current solutions
    match solutions.len() {
        0 => None,
        1 => Some(solutions.pop().unwrap()),
        _ => None,
    }
}

// Checks if i0 and i1 match, and if so, returns a substitution.
fn matches(s: &mut Vec<(Name, Type)>, i0: &Trait, i1: &Trait) -> bool {
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

pub fn unify(s0: &mut Vec<(Name, Type)>, t0: &Type, t1: &Type) -> Result<(), (Type, Type)> {
    let t0 = t0.apply(&s0);
    let t1 = t1.apply(&s0);
    if let Some(s1) = mgu(&t0, &t1) {
        *s0 = compose(s0.clone(), s1);
        Ok(())
    } else {
        Err((t0, t1))
    }
}

fn compose(s0: Vec<(Name, Type)>, s1: Vec<(Name, Type)>) -> Vec<(Name, Type)> {
    s1.into_iter()
        .map(|(x, t)| (x, t.apply(&s0)))
        .chain(s0.clone())
        .collect()
}

fn mgu(t0: &Type, t1: &Type) -> Option<Vec<(Name, Type)>> {
    match (t0, t1) {
        (Type::Cons(x0, ts0), Type::Cons(x1, ts1)) => {
            if x0 == x1 && ts0.len() == ts1.len() {
                ts0.iter()
                    .zip(ts1.iter())
                    .try_fold(vec![], |mut s, (t2, t3)| {
                        s.extend(mgu(t2, t3)?);
                        Some(s)
                    })
            } else {
                None
            }
        }
        (t2, Type::Assoc(i, x)) | (Type::Assoc(i, x), t2) => {
            let (_, t3) = i.assocs.iter().find(|(n, _)| n == x).unwrap();
            mgu(t2, t3)
        }
        (t, Type::Var(x)) | (Type::Var(x), t) => {
            if t0 != t1 {
                Some(vec![(x.clone(), t.clone())])
            } else {
                Some(vec![])
            }
        }
        (_, Type::Hole) | (Type::Hole, _) => unreachable!(),
    }
}
