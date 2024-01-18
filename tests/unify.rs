use aqua::ast::Type;
use aqua::solver::unify;

use aqua::util::t;
use aqua::util::tc;

#[test]
fn test_unify_atom0() {
    let mut substs = vec![];
    let t0 = Type::parse("i32");
    let t1 = Type::parse("i32");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
    assert!(substs.is_empty());
}

#[test]
fn test_unify_atom1() {
    let mut substs = vec![];
    let t0 = Type::parse("i32");
    let t1 = Type::parse("i64");
    assert_eq!(unify(&mut substs, &t0, &t1), Err((t("i32"), t("i64"))));
    assert!(substs.is_empty());
}

#[test]
fn test_unify_var0() {
    let mut substs = vec![];
    let t0 = Type::parse("?T");
    let t1 = Type::parse("i32");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_var1() {
    let mut substs = vec![];
    let t0 = Type::parse("i32");
    let t1 = Type::parse("?T");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_var2() {
    let mut substs = vec![];
    let t0 = Type::parse("?T");
    let t1 = Type::parse("?U");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_var3() {
    let mut substs = vec![
        ("T".into(), Type::parse("i32")),
        ("U".into(), Type::parse("i32")),
    ];
    let t0 = Type::parse("?T");
    let t1 = Type::parse("?U");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_tc0() {
    let mut substs = vec![("T".into(), Type::parse("i32"))];
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[i32]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_tc1() {
    let mut substs = Vec::new();
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[?U]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_tc2() {
    let mut substs = vec![("T".into(), Type::parse("i32"))];
    let t0 = Type::parse("Vec[Vec[?T]]");
    let t1 = Type::parse("Vec[Vec[?U]]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_tc3() {
    let mut substs = vec![("T".into(), Type::parse("Vec[i32]"))];
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[Vec[i32]]");
    assert!(unify(&mut substs, &t0, &t1).is_ok());
}

#[test]
fn test_unify_tc4() {
    let mut substs = vec![("T".into(), Type::parse("i32"))];
    let t0 = Type::parse("Vec[?T]");
    let t1 = Type::parse("Vec[i64]");
    assert_eq!(
        unify(&mut substs, &t0, &t1),
        Err((tc("Vec", [t("i32")]), t1))
    );
}
