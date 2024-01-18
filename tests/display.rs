use aqua::util::add;
use aqua::util::div;
use aqua::util::eq;
use aqua::util::etuple;
use aqua::util::expr_bool;
use aqua::util::int;
use aqua::util::mul;
use aqua::util::ne;
use aqua::util::string;
use aqua::util::sub;
use aqua::util::t;
use aqua::util::tfun;
use aqua::util::ttuple;

#[test]
fn test_display_int0() {
    let a = int("1").to_string();
    let b = "1";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_float1() {
    let a = int("1.0").to_string();
    let b = "1.0";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_string1() {
    let a = string("abc").to_string();
    let b = "\"abc\"";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_string2() {
    let a = string("abc\ndef").to_string();
    let b = "\"abc\ndef\"";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_bool0() {
    let a = expr_bool(true).to_string();
    let b = "true";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_bool1() {
    let a = expr_bool(false).to_string();
    let b = "false";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop0() {
    let a = add(int("1"), int("2")).to_string();
    let b = "@Add::add(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop1() {
    let a = sub(int("1"), int("2")).to_string();
    let b = "@Sub::sub(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop2() {
    let a = mul(int("1"), int("2")).to_string();
    let b = "@Mul::mul(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop3() {
    let a = div(int("1"), int("2")).to_string();
    let b = "@Div::div(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop4() {
    let a = eq(int("1"), int("2")).to_string();
    let b = "@PartialEq::eq(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop5() {
    let a = ne(int("1"), int("2")).to_string();
    let b = "@PartialEq::ne(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_binop6() {
    let a = add(int("1"), add(int("2"), int("3"))).to_string();
    let b = "@Add::add(1, @Add::add(2, 3))";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_expr_tuple0() {
    let a = etuple([]).to_string();
    let b = "()";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_expr_tuple1() {
    let a = etuple([int("1")]).to_string();
    let b = "(1,)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_expr_tuple2() {
    let a = etuple([int("1"), int("2")]).to_string();
    let b = "(1, 2)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_type_tuple0() {
    let a = ttuple([]).to_string();
    let b = "()";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_type_tuple1() {
    let a = ttuple([t("i32")]).to_string();
    let b = "(i32,)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_type_tuple2() {
    let a = ttuple([t("i32"), t("i32")]).to_string();
    let b = "(i32, i32)";
    assert!(a == b, "{}\n{}", a, b);
}

#[test]
fn test_display_type_fun0() {
    let a = tfun([t("i32")], t("i32")).to_string();
    let b = "fun(i32): i32";
    assert!(a == b, "{}\n{}", a, b);
}
