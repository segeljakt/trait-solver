use trait_solver::data::Context;
use trait_solver::data::Impl;
use trait_solver::data::Rule;
use trait_solver::data::Type;
use trait_solver::solve;
use trait_solver::unify;

// Some helper functions to make the tests more readable.

// Create a Rule:
// impl<{gs}> {head} where {body}
fn rule<const N: usize, const M: usize>(
    gs: [&'static str; N],
    head: Impl,
    body: [Impl; M],
) -> Rule {
    Rule::new(
        gs.iter().map(|s| s.to_string()).collect(),
        head,
        body.iter().map(|i| i.clone()).collect(),
    )
}

// Create a Fact:
// impl<{gs}> {head}
fn fact<const N: usize>(gs: [&'static str; N], head: Impl) -> Rule {
    Rule::new(gs.iter().map(|s| s.to_string()).collect(), head, vec![])
}

// Create an Impl:
// {name}<{types}> where {assoc}
fn imp<const N: usize, const M: usize>(
    name: &'static str,
    types: [Type; N],
    assoc: [(&'static str, Type); M],
) -> Impl {
    Impl::new(
        name.to_string(),
        types.iter().map(|t| t.clone()).collect(),
        assoc
            .iter()
            .map(|(s, t)| (s.to_string(), t.clone()))
            .collect(),
    )
}

fn t(name: &'static str) -> Type {
    Type::atom(name.to_string())
}

fn tc<const N: usize>(name: &'static str, types: [Type; N]) -> Type {
    Type::cons(name.to_string(), types.iter().map(|t| t.clone()).collect())
}

fn tv(name: &'static str) -> Type {
    Type::var(name.to_string())
}

fn ta(i: Impl, x: &'static str) -> Type {
    Type::assoc(i, x)
}

// Rules:
// * impl<T> Clone for T {}
//
// Goal:
// * <i32 as Clone>
#[test]
fn test_solve1() {
    let rules = [fact(["T"], imp("Clone", [tv("T")], []))];
    let substs = vec![];

    let goal = imp("Clone", [t("i32")], []);
    let mut context = Context::new();

    assert!(solve(substs, &goal, &rules, &mut context).is_some());
}

// Rules:
// * impl Clone for i32 {}
//
// Goal:
// * <i32 as Clone>
#[test]
fn test_solve2() {
    let rules = [fact([], imp("Clone", [t("i32")], []))];
    let substs = vec![];

    let goal = imp("Clone", [t("i32")], []);
    let mut context = Context::new();
    assert!(solve(substs, &goal, &rules, &mut context).is_some());
}

// Rules:
// * impl<T> Clone for Vec<T> where T: Clone {}
// * impl Clone for i32 {}
//
// Goal:
// * <Vec<i32> as Clone>
#[test]
fn test_solve3() {
    let rules = [
        rule(
            ["T"],
            imp("Clone", [tc("Vec", [tv("T")])], []),
            [imp("Clone", [tv("T")], [])],
        ),
        fact([], imp("Clone", [t("i32")], [])),
    ];
    let substs = vec![];

    let goal = imp("Clone", [tc("Vec", [t("i32")])], []);
    let mut context = Context::new();

    assert!(solve(substs, &goal, &rules, &mut context).is_some());
}

// Rules:
// * impl<T> Clone for Vec<T> where T: Clone {}
// * impl Clone for i32 {}
//
// Goal:
// * <Vec<Vec<i32>> as Clone>
#[test]
fn test_solve4() {
    let rules = vec![
        rule(
            ["T"],
            imp("Clone", [tc("Vec", [tv("T")])], []),
            [imp("Clone", [tv("T")], [])],
        ),
        rule([], imp("Clone", [t("i32")], []), []),
    ];

    let substs = vec![];

    let goal = imp("Clone", [tc("Vec", [tc("Vec", [t("i32")])])], []);

    let mut context = Context::new();

    assert!(solve(substs, &goal, &rules, &mut context).is_some());
}

// Rules:
// * impl<T> Iterator for Vec<T> { type Item = T; }
//
// Goal:
// * <Vec<i32> as Iterator>::Item = i32
#[test]
fn test_solve5() {
    let rules = [rule(
        ["T"],
        imp("Iterator", [tc("Vec", [tv("T")])], [("Item", tv("T"))]),
        [],
    )];
    let substs = vec![];
    let goal = imp("Iterator", [tc("Vec", [t("i32")])], [("Item", tv("?0"))]);
    let mut context = Context::new();
    let s0 = solve(substs, &goal, &rules, &mut context).unwrap();
    let s1 = unify(s0, &ta(goal, "Item"), &t("i32"));
    assert!(s1.is_some());
}

// Rules:
// * impl Add<i32> for i32 { type Output = i32; }
// * impl Add<f32> for f32 { type Output = f32; }
//
// Goal:
// * <i32 as Add<i32>> = i32
#[test]
fn test_solve6() {
    let rules = [
        fact([], imp("Add", [t("i32"), t("i32")], [("Output", t("i32"))])),
        fact([], imp("Add", [t("f32"), t("f32")], [("Output", t("f32"))])),
    ];
    let substs = vec![];
    let goal = imp("Add", [t("i32"), t("i32")], [("Output", tv("?0"))]);
    let mut context = Context::new();
    let s0 = solve(substs, &goal, &rules, &mut context).unwrap();
    let s1 = unify(s0, &ta(goal, "Output"), &t("i32"));
    assert!(s1.is_some());
}

// Rules:
// * impl Add<i32> for i32 { type Output = i32; }
// * impl Add<T,R> for Vec<T> where T: Add<R> { type Output = Vec<<T as Add<R>>::Output>; }
//
// Goal:
// * <Vec<i32> as Add<i32>>::Output = i32
#[test]
fn test_solve7() {
    let rules = [
        fact([], imp("Add", [t("i32"), t("i32")], [("Output", t("i32"))])),
        rule(
            ["L", "R"],
            imp(
                "Add",
                [tc("Vec", [tv("L")]), tv("R")],
                [(
                    "Output",
                    tc(
                        "Vec",
                        [ta(
                            imp("Add", [tv("L"), tv("R")], [("Output", tv("?0"))]),
                            "Output",
                        )],
                    ),
                )],
            ),
            [imp("Add", [tv("L"), tv("R")], [("Output", tv("?0"))])],
        ),
    ];
    let substs = vec![];
    let goal = imp(
        "Add",
        [tc("Vec", [t("i32")]), t("i32")],
        [("Output", tv("?1"))],
    );
    let mut context = Context::new();
    let s0 = solve(substs, &goal, &rules, &mut context).unwrap();
    let s1 = unify(s0, &ta(goal, "Output"), &tc("Vec", [t("i32")]));
    assert!(s1.is_some());
}
