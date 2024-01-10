use trait_solver::data::StmtImpl;
use trait_solver::data::Trait;
use trait_solver::data::Type;
use trait_solver::infer::Context;
use trait_solver::solve;
use trait_solver::unify;

#[test]
fn test_trait1() {
    let impls = [StmtImpl::parse("impl[T] Clone[T] {}")];
    let mut sub = vec![];
    let goal = Trait::parse("Clone[i32]");
    let mut context = Context::new();

    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
}

#[test]
fn test_trait2() {
    let impls = [StmtImpl::parse("impl Clone[i32] {}")];
    let mut sub = vec![];

    let goal = Trait::parse("Clone[i32]");
    let mut context = Context::new();

    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
}

#[test]
fn test_trait3() {
    let impls = [
        StmtImpl::parse("impl[T] Clone[Vec[T]] where Clone[T] {}"),
        StmtImpl::parse("impl Clone[i32] {}"),
    ];
    let mut sub = vec![];

    let goal = Trait::parse("Clone[Vec[i32]]");
    let mut context = Context::new();

    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
}

#[test]
fn test_trait4() {
    let impls = [
        StmtImpl::parse("impl[T] Clone[Vec[T]] where Clone[T] {}"),
        StmtImpl::parse("impl Clone[i32] {}"),
    ];

    let mut sub = vec![];
    let goal = Trait::parse("Clone[Vec[Vec[i32]]]");

    let mut context = Context::new();

    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
}

#[test]
fn test_trait5() {
    let impls = [StmtImpl::parse(
        "impl[T] Iterator[Vec[T]] { type Item = T; }",
    )];
    let mut sub = vec![];
    let goal = Trait::parse("Iterator[Vec[i32], Item = ?X]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
    let t0 = Type::parse("Iterator[Vec[i32]].Item");
    let t1 = Type::parse("i32");
    assert!(unify(&mut sub, &t0, &t1).is_ok());
}

#[test]
fn test_trait6() {
    let impls = [
        StmtImpl::parse("impl Add[i32, i32] { type Output = i32; }"),
        StmtImpl::parse("impl Add[f32, f32] { type Output = f32; }"),
    ];
    let mut sub = vec![];
    let goal = Trait::parse("Add[i32, i32, Output = ?X]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
    let t0 = Type::parse("Add[i32, i32].Output");
    let t1 = Type::parse("i32");
    assert!(unify(&mut sub, &t0, &t1).is_ok());
}

#[test]
fn test_trait7() {
    let impls = [
        StmtImpl::parse(
            "impl Add[i32, i32] {
                 type Output = i32;
             }",
        ),
        StmtImpl::parse(
            "impl[T, R] Add[Vec[T], R] where Add[T, R] {
                 type Output = Vec[Add[T, R].Output];
             }",
        ),
    ];
    let mut sub = vec![];
    let goal = Trait::parse("Add[Vec[i32], i32, Output = ?Y]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
    let t0 = Type::parse("Add[Vec[i32], i32].Output");
    let t1 = Type::parse("Vec[i32]");
    assert!(unify(&mut sub, &t0, &t1).is_ok());
}

#[test]
fn test_trait8() {
    let impls = [
        StmtImpl::parse("impl Add[i32, i32] { type Output = i32; }"),
        StmtImpl::parse("impl Add[i64, i32] { type Output = i64; }"),
    ];
    let mut sub = vec![];
    let goal = Trait::parse("Add[i64, i32, Output = ?X]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
    let t0 = Type::parse("Add[i64, i32].Output");
    let t1 = Type::parse("i64");
    assert!(unify(&mut sub, &t0, &t1).is_ok());
}

#[test]
fn test_trait9() {
    let impls = [
        StmtImpl::parse(
            "impl[T] IntoIterator[Vec[T]] {
                 type Item = T;
                 type IntoIter = VecIterator[T];
             }",
        ),
        StmtImpl::parse(
            "impl[T] Iterator[VecIterator[T]] {
                 type Item = T;
             }",
        ),
    ];
    let mut sub = vec![];
    let goal = Trait::parse("IntoIterator[Vec[i32], Item = ?A, IntoIter = ?B]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
    let t0 = Type::parse("IntoIterator[Vec[i32]].IntoIter");
    let t1 = Type::parse("VecIterator[i32]");
    assert!(unify(&mut sub, &t0, &t1).is_ok());
}

#[test]
fn test_trait10() {
    let impls = [
        StmtImpl::parse(
            "impl[T] IntoIterator[Vec[T]] {
                type Item = T;
                type IntoIter = VecIterator[T];
             }",
        ),
        StmtImpl::parse(
            "impl[T] IntoIterator[Stream[T]] {
                type Item = T;
                type IntoIter = StreamIterator[T];
             }",
        ),
    ];

    let mut sub = vec![];
    let goal = Trait::parse("IntoIterator[Vec[i32], Item = ?X, IntoIter = ?Y]");
    let mut context = Context::new();
    assert!(solve(&goal, &impls, &[], &mut sub, &mut context).is_ok());
}
