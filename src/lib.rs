pub mod data;
mod display;

use data::Context;
use data::Impl;
use data::Name;
use data::Rule;
use data::Type;

pub fn solve(
    substs: Vec<(Name, Type)>,
    goal: &Impl,
    rules: &[Rule],
    ctx: &mut Context,
) -> Option<Vec<(Name, Type)>> {
    let mut solutions = vec![];
    for rule in rules {
        let rule = instantiate(rule, ctx);
        if let Some(s1) = matches(substs.clone(), goal, &rule.head) {
            if let Some(s2) = rule
                .body
                .iter()
                .try_fold(s1, |s1, subgoal| solve(s1, subgoal, rules, ctx))
            {
                solutions.push(s2);
            }
        }
    }
    match solutions.len() {
        0 => None,
        1 => Some(solutions.pop().unwrap()),
        _ => None,
    }
}

fn matches(s: Vec<(Name, Type)>, i0: &Impl, i1: &Impl) -> Option<Vec<(Name, Type)>> {
    if i0.name == i1.name && i0.types.len() == i1.types.len() {
        i1.types
            .iter()
            .zip(i0.types.iter())
            .try_fold(s, |s, (t0, t1)| unify(s, &t0, &t1))
    } else {
        None
    }
}

fn instantiate(rule: &Rule, ctx: &mut Context) -> Rule {
    let s = rule
        .quantifiers
        .iter()
        .map(|q| {
            let t = ctx.new_tyvar();
            (q.clone(), t)
        })
        .collect::<Vec<_>>();

    let head = apply_impl(&s, &rule.head);
    let body = rule
        .body
        .iter()
        .map(|i| apply_impl(&s, &i))
        .collect::<Vec<_>>();

    Rule::new(vec![], head, body)
}

fn apply_impl(s: &[(Name, Type)], i: &Impl) -> Impl {
    let types = i.types.iter().map(|t| apply(s, t)).collect();
    Impl::new(i.name.clone(), types)
}

fn apply(s: &[(Name, Type)], t: &Type) -> Type {
    match t {
        Type::Cons(x, ts) => {
            let ts = ts.iter().map(|t| apply(s, t)).collect::<Vec<_>>();
            Type::Cons(x.clone(), ts)
        }
        Type::Var(x) => {
            if let Some((_, t)) = s.iter().find(|(n, _)| n == x) {
                t.clone()
            } else {
                Type::Var(x.clone())
            }
        }
    }
}

pub fn unify(s0: Vec<(Name, Type)>, t0: &Type, t1: &Type) -> Option<Vec<(Name, Type)>> {
    let t0 = apply(&s0, t0);
    let t1 = apply(&s0, t1);
    let s1 = mgu(&t0, &t1)?;
    let s2 = compose(s0, s1);
    Some(s2)
}

fn compose(s0: Vec<(Name, Type)>, s1: Vec<(Name, Type)>) -> Vec<(Name, Type)> {
    s1.into_iter()
        .map(|(x, t)| (x, apply(&s0, &t)))
        .chain(s0.clone())
        .collect()
}

fn mgu(t0: &Type, t1: &Type) -> Option<Vec<(Name, Type)>> {
    match (t0, t1) {
        (Type::Cons(x0, ts0), Type::Cons(x1, ts1)) => {
            if x0 == x1 && ts0.len() == ts1.len() {
                ts0.iter()
                    .zip(ts1.iter())
                    .try_fold(vec![], |mut s, (t0, t1)| {
                        s.extend(mgu(t0, t1)?);
                        Some(s)
                    })
            } else {
                None
            }
        }
        (t, Type::Var(x)) | (Type::Var(x), t) => Some(vec![(x.clone(), t.clone())]),
    }
}
