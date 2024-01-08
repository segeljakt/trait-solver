use trait_solver::helper::binop;
use trait_solver::helper::bool;
use trait_solver::helper::call;
use trait_solver::helper::def;
use trait_solver::helper::ev;
use trait_solver::helper::imp;
use trait_solver::helper::int;
use trait_solver::helper::p;
use trait_solver::helper::program;
use trait_solver::helper::t;
use trait_solver::parser::Parser;
use trait_solver::token::Lexer;

#[test]
fn test_parser0() {
    let input = "1 + 2;";
    let program = program([binop(int("1"), "__add__", int("2").into()).into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser1() {
    let input = "1 + 2 * 3;";
    let program =
        program([binop(int("1"), "__add__", binop(int("2"), "__mul__", int("3"))).into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser2() {
    let input = "1 * 2 + 3;";
    let program =
        program([binop(binop(int("1"), "__mul__", int("2")), "__add__", int("3")).into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser3() {
    let input = "def id(x: i32): i32 = x;";
    let program = program([def("id", [], [], [("x", t("i32"))], t("i32"), ev("x")).into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser4() {
    let input = "def id(x: i32, y: i32): i32 = x;";
    let program = program([def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        ev("x"),
    )
    .into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser5() {
    let input = "def id(x: i32, y: i32): i32 = x + y;";
    let program = program([def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        binop(ev("x"), "__add__", ev("y")),
    )
    .into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser6() {
    let input = "def id(x: i32, y: i32): i32 = x + y * 2;";
    let program = program([def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        binop(ev("x"), "__add__", binop(ev("y"), "__mul__", int("2"))),
    )
    .into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser7() {
    let input = r#"
        def id(x: i32): i32 = x;
        def main(): i32 = id(42);
    "#;
    let program = program([
        def("id", [], [], [("x", t("i32"))], t("i32"), ev("x")).into(),
        def(
            "main",
            [],
            [],
            [],
            t("i32"),
            call(ev("id"), [int("42")]).into(),
        )
        .into(),
    ]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}

#[test]
fn test_parser8() {
    let input = r#"
        impl Eq[bool] {
            def eq(x: bool, y: bool): bool = true;
        }
    "#;
    let program = program([imp(
        [],
        p("Eq", [t("bool")], []),
        [],
        [def(
            "eq",
            [],
            [],
            [("x", t("bool")), ("y", t("bool"))],
            t("bool"),
            bool(true),
        )
        .into()],
    )
    .into()]);
    assert_eq!(Parser::new(Lexer::new(input)).parse(), program);
}
