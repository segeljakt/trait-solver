use trait_solver::data::Type;
use trait_solver::unify;

#[test]
fn test_unify() {
    let mut substs = vec![];
    let t0 = Type::parse("i32");
    let t1 = Type::parse("i32");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify2() {
    let mut substs = vec![];
    let t0 = Type::parse("?T");
    let t1 = Type::parse("i32");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify3() {
    let mut substs = vec![];
    let t0 = Type::parse("i32");
    let t1 = Type::parse("?T");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify4() {
    let mut substs = vec![
        ("T".into(), Type::parse("i32")),
        ("U".into(), Type::parse("i32")),
    ];
    let t0 = Type::parse("?T");
    let t1 = Type::parse("?U");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify5() {
    let mut substs = vec![("T".into(), Type::parse("i32"))];
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[i32]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify6() {
    let mut substs = Vec::new();
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[?U]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_not_unify() {
    let mut substs = vec![("T".into(), Type::parse("i32"))];
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[i64]");
    assert!(unify(&mut substs, &t0, &t1).is_err());
}
