use trait_solver::data::Expr;
use trait_solver::data::Program;
use trait_solver::data::StmtImpl;
use trait_solver::data::Type;
use trait_solver::helper::binop;
use trait_solver::helper::block;
use trait_solver::helper::bool;
use trait_solver::helper::call;
use trait_solver::helper::def;
use trait_solver::helper::ev;
use trait_solver::helper::expr_assoc;
use trait_solver::helper::expr_struct;
use trait_solver::helper::expr_tuple;
use trait_solver::helper::field;
use trait_solver::helper::hole;
use trait_solver::helper::int;
use trait_solver::helper::p;
use trait_solver::helper::program;
use trait_solver::helper::stmt_def;
use trait_solver::helper::stmt_enum;
use trait_solver::helper::stmt_expr;
use trait_solver::helper::stmt_impl;
use trait_solver::helper::stmt_struct;
use trait_solver::helper::stmt_var;
use trait_solver::helper::t;
use trait_solver::helper::tc;
use trait_solver::helper::type_assoc;
use trait_solver::helper::unit;
use trait_solver::helper::variant;

#[test]
fn test_parser_binop0() {
    let p0 = Program::parse("1 + 2;");
    let p1 = program([stmt_expr(binop(int("1"), "__add__", int("2")))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_binop1() {
    let p0 = Program::parse("1 + 2 * 3;");
    let p1 = program([stmt_expr(binop(
        int("1"),
        "__add__",
        binop(int("2"), "__mul__", int("3")),
    ))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_binop2() {
    let p0 = Program::parse("1 * 2 + 3;");
    let p1 = program([stmt_expr(binop(
        binop(int("1"), "__mul__", int("2")),
        "__add__",
        int("3"),
    ))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_def0() {
    let p0 = Program::parse("def id(x: i32): i32 = x;");
    let p1 = program([stmt_def("id", [], [], [("x", t("i32"))], t("i32"), ev("x"))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_def1() {
    let p0 = Program::parse("def id(x: i32, y: i32): i32 = x;");
    let p1 = program([stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        ev("x"),
    )]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_def2() {
    let p0 = Program::parse("def id(x: i32, y: i32): i32 = x + y;");
    let p1 = program([stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        binop(ev("x"), "__add__", ev("y")),
    )]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_def3() {
    let p0 = Program::parse("def id(x: i32, y: i32): i32 = x + y * 2;");
    let p1 = program([stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        binop(ev("x"), "__add__", binop(ev("y"), "__mul__", int("2"))),
    )]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_def4() {
    let p0 = Program::parse(
        "def id(x: i32): i32 = x;
         def main(): i32 = id(42);",
    );
    let p1 = program([
        stmt_def("id", [], [], [("x", t("i32"))], t("i32"), ev("x")),
        stmt_def("main", [], [], [], t("i32"), call(ev("id"), [int("42")])),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_impl0() {
    let p0 = Program::parse(
        "impl Eq[bool] {
             def eq(x: bool, y: bool): bool = true;
         }",
    );
    let p1 = program([stmt_impl(
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
        )],
    )]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_impl1() {
    StmtImpl::parse(
        "impl[T, R] Add[Vec[T], R] where Add[T, R] {
             type Output = Vec[Add[T, R].Output];
         }",
    );
}

#[test]
fn test_parser_trait0() {
    let t0 = Type::parse("Iterator[Vec[i32]].Item");
    let t1 = type_assoc(
        p("Iterator", [tc("Vec", [t("i32")])], [("Item", hole())]),
        "Item",
    );
    assert_eq!(t0, t1);
}

#[test]
fn test_parser_var0() {
    let p0 = Program::parse("var x = 1;");
    let p1 = program([stmt_var("x", hole(), int("1"))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_var1() {
    let p0 = Program::parse("var x: i32 = 1;");
    let p1 = program([stmt_var("x", t("i32"), int("1"))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_var2() {
    let p0 = Program::parse("var x = 1; var y = x;");
    let p1 = program([
        stmt_var("x", hole(), int("1")),
        stmt_var("y", hole(), ev("x")),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_struct0() {
    let p0 = Program::parse(
        "struct S { x: i32, y: i32 }
         var s = S { x: 1, y: 2 };
         s.x;
         ",
    );
    let p1 = program([
        stmt_struct("S", [], [], [("x", t("i32")), ("y", t("i32"))]),
        stmt_var(
            "s",
            hole(),
            expr_struct("S", [("x", int("1")), ("y", int("2"))]),
        ),
        stmt_expr(field(ev("s"), "x")),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_struct1() {
    let p0 = Program::parse(
        "struct S { x: i32 }
         var x = 0;
         var s = S { x };
         ",
    );
    let p1 = program([
        stmt_struct("S", [], [], [("x", t("i32"))]),
        stmt_var("x", hole(), int("0")),
        stmt_var("s", hole(), expr_struct("S", [("x", ev("x"))])),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_struct2() {
    let p0 = Program::parse(
        "struct S { x: i32 }
         var a = S { x: 0 };
         var b = S { a.x };
         ",
    );
    let p1 = program([
        stmt_struct("S", [], [], [("x", t("i32"))]),
        stmt_var("a", hole(), expr_struct("S", [("x", int("0"))])),
        stmt_var("b", hole(), expr_struct("S", [("x", field(ev("a"), "x"))])),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_struct3() {
    let p0 = Program::parse(
        "struct S { a: i32 }
         var a = S { a: 0 };
         var b = S { a: a.a };",
    );
    let p1 = program([
        stmt_struct("S", [], [], [("a", t("i32"))]),
        stmt_var("a", hole(), expr_struct("S", [("a", int("0"))])),
        stmt_var("b", hole(), expr_struct("S", [("a", field(ev("a"), "a"))])),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_enum0() {
    let p0 = Program::parse(
        "enum E { A(i32), B(i32) }
         var x = E.A(1);",
    );
    let p1 = program([
        stmt_enum("E", [], [], [("A", t("i32")), ("B", t("i32"))]),
        stmt_var("x", hole(), variant("E", "A", int("1"))),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_enum1() {
    let p0 = Program::parse(
        "enum E { A(i32, i32) }
         var x = E.A(1, 2);",
    );
    let p1 = program([
        stmt_enum("E", [], [], [("A", tc("Tuple", [t("i32"), t("i32")]))]),
        stmt_var(
            "x",
            hole(),
            variant("E", "A", expr_tuple([int("1"), int("2")])),
        ),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace0() {
    let p0 = Program::parse("{1}");
    let p1 = program([stmt_expr(block([], int("1")))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace1() {
    let p0 = Program::parse("{1} {2}");
    let p1 = program([
        stmt_expr(block([], int("1"))),
        stmt_expr(block([], int("2"))),
    ]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace2() {
    let p0 = Program::parse("{{1}}");
    let p1 = program([stmt_expr(block([], block([], int("1"))))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace3() {
    let p0 = Program::parse("{{1} {2}}");
    let p1 = program([stmt_expr(block(
        [stmt_expr(block([], int("1")))],
        block([], int("2")),
    ))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace4() {
    let p0 = Program::parse("{{1};{2}}");
    let p1 = program([stmt_expr(block(
        [stmt_expr(block([], int("1")))],
        block([], int("2")),
    ))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace5() {
    let p0 = Program::parse("{{1};{2};}");
    let p1 = program([stmt_expr(block(
        [
            stmt_expr(block([], int("1"))),
            stmt_expr(block([], int("2"))),
        ],
        unit(),
    ))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace6() {
    let p0 = Program::parse("{;}");
    let p1 = program([stmt_expr(block([], unit()))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_brace7() {
    let p0 = Program::parse("{;;;;;;;;}");
    let p1 = program([stmt_expr(block([], unit()))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_paren0() {
    let p0 = Program::parse("();");
    let p1 = program([stmt_expr(unit())]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_paren1() {
    let p0 = Program::parse("(());");
    let p1 = program([stmt_expr(unit())]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_paren2() {
    let p0 = Program::parse("({});");
    let p1 = program([stmt_expr(block([], unit()))]);
    assert_eq!(p0, p1);
}

#[test]
fn test_parser_assoc0() {
    let e0 = Expr::parse("Iterator[Vec[i32]].next");
    let e1 = expr_assoc(
        p("Iterator", [tc("Vec", [t("i32")])], [("Item", hole())]),
        "next",
    );
    assert_eq!(e0, e1);
}

#[test]
fn test_parser_assoc1() {
    let e0 = Expr::parse("Add[i32, i32].add;");
    let e1 = expr_assoc(p("Add", [t("i32"), t("i32")], [("Output", hole())]), "add");
    assert_eq!(e0, e1);
}

#[test]
fn test_parser_assoc2() {
    let e0 = Type::parse("Iterator[Vec[i32]].Item");
    let e1 = type_assoc(
        p("Iterator", [tc("Vec", [t("i32")])], [("Item", hole())]),
        "Item",
    );
    assert_eq!(e0, e1);
}

#[test]
fn test_parser_query0() {
    let _p0 = Program::parse(
        "from x in [1, 2, 3]
         select {x:1, y:2}
         where x > 1;",
    );
}
