use trait_solver::data::Context;
use trait_solver::data::Impl;
use trait_solver::data::Pred;
use trait_solver::helper::fact;
use trait_solver::helper::imp;
use trait_solver::helper::p;
use trait_solver::helper::t;
use trait_solver::helper::ta;
use trait_solver::helper::tc;
use trait_solver::helper::tv;
use trait_solver::solve;
use trait_solver::unify;

// Some helper functions to make the tests more readable.

// Rules:
// * impl<T> Clone for T {}
//
// Goal:
// * <i32 as Clone>
#[test]
fn test_solve1() {
    let rules = [Impl::parse("impl[T] Clone[?T] {}")];
    let mut substs = vec![];
    let goal = Pred::parse("Clone[i32]");
    let mut context = Context::new();

    assert!(solve(&goal, &rules, &[], &mut substs, &mut context).is_ok());
}

// Rules:
// * impl Clone for i32 {}
//
// Goal:
// * <i32 as Clone>
#[test]
fn test_solve2() {
    let rules = [Impl::parse("impl Clone[i32] {}")];
    let mut substs = vec![];

    let goal = Pred::parse("Clone[i32]");
    let mut context = Context::new();

    assert!(solve(&goal, &rules, &[], &mut substs, &mut context).is_ok());
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
        Impl::parse("impl[T] Clone[Vec[?T]] where ?T: Clone {}"),
        Impl::parse("impl Clone[i32] {}"),
    ];
    let mut substs = vec![];

    let goal = Pred::parse("Clone[Vec[i32]]");
    let mut context = Context::new();

    assert!(solve(&goal, &rules, &[], &mut substs, &mut context).is_ok());
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
        Impl::parse("impl[T] Clone[Vec[?T]] where ?T: Clone {}"),
        Impl::parse("impl Clone[i32]"),
    ];

    let mut substs = vec![];
    let goal = Pred::parse("Clone[Vec[Vec[i32]]]");

    let mut context = Context::new();

    assert!(solve(&goal, &rules, &[], &mut substs, &mut context).is_ok());
}

// Rules:
// * impl<T> Iterator for Vec<T> { type Item = T; }
//
// Goal:
// * <Vec<i32> as Iterator>::Item = i32
#[test]
fn test_solve5() {
    let rules = [Impl::parse("impl[T] Iterator[Vec[?T]] { type Item = ?T; }")];
    let mut sub = vec![];
    let goal = Pred::parse("Iterator[Vec[i32], Item = ?0]");
    let mut context = Context::new();
    assert!(solve(&goal, &rules, &[], &mut sub, &mut context).is_ok());
    assert!(unify(&mut sub, &ta(goal, "Item"), &t("i32")).is_ok());
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
        fact(
            [],
            p("Add", [t("i32"), t("i32")], [("Output", t("i32"))]),
            [],
        ),
        fact(
            [],
            p("Add", [t("f32"), t("f32")], [("Output", t("f32"))]),
            [],
        ),
    ];
    let mut sub = vec![];
    let goal = p("Add", [t("i32"), t("i32")], [("Output", tv("?0"))]);
    let mut context = Context::new();
    assert!(solve(&goal, &rules, &[], &mut sub, &mut context).is_ok());
    assert!(unify(&mut sub, &ta(goal, "Output"), &t("i32")).is_ok());
}

// Rules:
// * impl Add<i32> for i32 { type Output = i32; }
// * impl Add<T,R> for Vec<T> where T: Add<R> { type Output = Vec<<T as Add<R>>::Output>; }
//
// Goal:
// * <Vec<i32> as Add<i32>>::Output = Vec<i32>
#[test]
fn test_solve7() {
    let rules = [
        fact(
            [],
            p("Add", [t("i32"), t("i32")], [("Output", t("i32"))]),
            [],
        ),
        imp(
            ["L", "R"],
            p(
                "Add",
                [tc("Vec", [tv("L")]), tv("R")],
                [(
                    "Output",
                    tc(
                        "Vec",
                        [ta(
                            p("Add", [tv("L"), tv("R")], [("Output", tv("?0"))]),
                            "Output",
                        )],
                    ),
                )],
            ),
            [p("Add", [tv("L"), tv("R")], [("Output", tv("?0"))])],
            [],
        ),
    ];
    let mut sub = vec![];
    let goal = p(
        "Add",
        [tc("Vec", [t("i32")]), t("i32")],
        [("Output", tv("?1"))],
    );
    let mut context = Context::new();
    assert!(solve(&goal, &rules, &[], &mut sub, &mut context).is_ok());
    assert!(unify(&mut sub, &ta(goal, "Output"), &tc("Vec", [t("i32")])).is_ok());
}

// Rules:
// * impl Add<i32> for i32 { type Output = i32; }
// * impl Add<i64> for i32 { type Output = i64; }
//
// Goal:
// * <i32 as Add<i64>>::Output = i64
#[test]
fn test_solve8() {
    let rules = [
        fact(
            [],
            p("Add", [t("i32"), t("i32")], [("Output", t("i32"))]),
            [],
        ),
        fact(
            [],
            p("Add", [t("i64"), t("i32")], [("Output", t("i64"))]),
            [],
        ),
    ];
    let mut sub = vec![];
    let goal = p("Add", [t("i64"), t("i32")], [("Output", tv("?0"))]);
    let mut context = Context::new();
    assert!(solve(&goal, &rules, &[], &mut sub, &mut context).is_ok());
    assert!(unify(&mut sub, &ta(goal, "Output"), &t("i64")).is_ok());
}

// Rules:
// * impl<T> IntoIterator for Vec<T> { type Item = T; type IntoIter = VecIterator<T>; }
// * impl<T> Iterator for VecIterator<T> { type Item = T; }
//
// Goal:
// * <Vec<i32> as IntoIterator>::IntoIter = VecIterator<i32>
#[test]
fn test_solve9() {
    let rules = [
        imp(
            ["T"],
            p(
                "IntoIterator",
                [tc("Vec", [tv("T")])],
                [
                    ("Item", tv("T")),
                    ("IntoIter", tc("VecIterator", [tv("T")])),
                ],
            ),
            [],
            [],
        ),
        imp(
            ["T"],
            p(
                "Iterator",
                [tc("VecIterator", [tv("T")])],
                [("Item", tv("T"))],
            ),
            [],
            [],
        ),
    ];
    let mut sub = vec![];
    let goal = p(
        "IntoIterator",
        [tc("Vec", [t("i32")])],
        [("IntoIter", tv("?0"))],
    );
    let mut context = Context::new();
    assert!(solve(&goal, &rules, &[], &mut sub, &mut context).is_ok());
    assert!(unify(
        &mut sub,
        &ta(goal, "IntoIter"),
        &tc("VecIterator", [t("i32")])
    )
    .is_ok());
}
