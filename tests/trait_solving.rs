use trait_solver::data::Context;
use trait_solver::data::Impl;
use trait_solver::data::Rule;
use trait_solver::data::Type;
use trait_solver::solve;

// Rules:
// * impl<T> Clone for T {}
//
// Goal:
// * <i32 as Clone>
#[test]
fn test_solve1() {
    let rules = vec![Rule::new(
        vec!["T".into()],
        Impl::new("Clone", vec![Type::var("T")]),
        vec![],
    )];
    let substs = vec![];

    let goal = Impl::new("Clone", vec![Type::atom("i32")]);
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
    let rules = vec![Rule::new(
        vec![],
        Impl::new("Clone", vec![Type::atom("i32")]),
        vec![],
    )];
    let substs = vec![];

    let goal = Impl::new("Clone", vec![Type::atom("i32")]);
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
    let rules = vec![
        Rule::new(
            vec!["T".into()],
            Impl::new("Clone", vec![Type::cons("Vec", vec![Type::var("T")])]),
            vec![Impl::new("Clone", vec![Type::var("T")])],
        ),
        Rule::new(vec![], Impl::new("Clone", vec![Type::atom("i32")]), vec![]),
    ];
    let substs = vec![];

    let goal = Impl::new("Clone", vec![Type::cons("Vec", vec![Type::atom("i32")])]);
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
        Rule::new(
            vec!["T".into()],
            Impl::new("Clone", vec![Type::cons("Vec", vec![Type::var("T")])]),
            vec![Impl::new("Clone", vec![Type::var("T")])],
        ),
        Rule::new(vec![], Impl::new("Clone", vec![Type::atom("i32")]), vec![]),
    ];

    let substs = vec![];

    let goal = Impl::new(
        "Clone",
        vec![Type::cons(
            "Vec",
            vec![Type::cons("Vec", vec![Type::atom("i32")])],
        )],
    );

    let mut context = Context::new();

    assert!(solve(substs, &goal, &rules, &mut context).is_some());
}
