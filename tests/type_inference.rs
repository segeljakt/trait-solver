use trait_solver::data::Type;
use trait_solver::unify;

#[test]
fn test_unify() {
    let substs = vec![];
    let t0 = Type::atom("i32");
    let t1 = Type::atom("i32");
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_unify2() {
    let substs = vec![];
    let t0 = Type::var("T");
    let t1 = Type::atom("i32");
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_unify3() {
    let substs = vec![];
    let t0 = Type::atom("i32");
    let t1 = Type::var("T");
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_unify4() {
    let substs = vec![("T".into(), Type::atom("i32")), ("U".into(), Type::atom("i32"))];
    let t0 = Type::var("T");
    let t1 = Type::var("U");
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_unify5() {
    let substs = vec![("T".into(), Type::atom("i32"))];
    let t0 = Type::cons("Vec", vec![Type::var("T")]);
    let t1 = Type::cons("Vec", vec![Type::atom("i32")]);
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_unify6() {
    let substs = Vec::new();
    let t0 = Type::cons("Vec", vec![Type::var("T")]);
    let t1 = Type::cons("Vec", vec![Type::var("U")]);
    assert!(unify(substs, &t0, &t1).is_some());
}

#[test]
fn test_not_unify() {
    let substs = vec![("T".into(), Type::atom("i32"))];
    let t0 = Type::cons("Vec", vec![Type::var("T")]);
    let t1 = Type::cons("Vec", vec![Type::atom("i64")]);
    assert!(unify(substs, &t0, &t1).is_none());
}
