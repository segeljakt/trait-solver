use aqua::ast::Expr;
use aqua::ast::Pat;
use aqua::ast::Program;
use aqua::ast::Stmt;
use aqua::ast::StmtImpl;
use aqua::ast::Type;
use aqua::diag::Sources;
use aqua::lexer::Lexer;
use aqua::parser::Parser;
use aqua::util::add;
use aqua::util::and;
use aqua::util::block;
use aqua::util::call;
use aqua::util::dcall;
use aqua::util::def;
use aqua::util::div;
use aqua::util::eq;
use aqua::util::etuple;
use aqua::util::expr_array;
use aqua::util::expr_assign;
use aqua::util::expr_assoc;
use aqua::util::expr_bool;
use aqua::util::expr_break;
use aqua::util::expr_continue;
use aqua::util::expr_err;
use aqua::util::expr_fun;
use aqua::util::expr_index;
use aqua::util::expr_match;
use aqua::util::expr_return;
use aqua::util::expr_struct;
use aqua::util::field;
use aqua::util::float;
use aqua::util::ge;
use aqua::util::get;
use aqua::util::gt;
use aqua::util::hole;
use aqua::util::index;
use aqua::util::int;
use aqua::util::le;
use aqua::util::lt;
use aqua::util::mul;
use aqua::util::ne;
use aqua::util::neg;
use aqua::util::not;
use aqua::util::or;
use aqua::util::pat_bool;
use aqua::util::pat_enum;
use aqua::util::pat_int;
use aqua::util::pat_struct;
use aqua::util::pat_wild;
use aqua::util::program;
use aqua::util::pv;
use aqua::util::query;
use aqua::util::query_compute;
use aqua::util::query_from;
use aqua::util::query_group;
use aqua::util::query_into;
use aqua::util::query_over;
use aqua::util::query_select;
use aqua::util::query_where;
use aqua::util::query_with;
use aqua::util::stmt_def;
use aqua::util::stmt_enum;
use aqua::util::stmt_expr;
use aqua::util::stmt_impl;
use aqua::util::stmt_struct;
use aqua::util::stmt_type;
use aqua::util::stmt_var;
use aqua::util::sub;
use aqua::util::t;
use aqua::util::tc;
use aqua::util::tfun;
use aqua::util::tr;
use aqua::util::ttuple;
use aqua::util::type_assoc;
use aqua::util::type_err;
use aqua::util::typed_expr_fun;
use aqua::util::unit;
use aqua::util::var;
use aqua::util::variant;

#[test]
fn test_parser_int0() {
    let a = Expr::parse("1");
    let b = int("1");
    assert_eq!(a, b);
}

#[test]
fn test_parser_int1() {
    let a = Expr::parse("123s");
    let b = dcall("postfix_s", [], [int("123")]);
    assert_eq!(a, b);
}

#[test]
fn test_parser_float0() {
    let a = Expr::parse("1.0");
    let b = float("1.0");
    assert_eq!(a, b);
}

#[test]
fn test_parser_float1() {
    let a = Expr::parse("1.0s");
    let b = dcall("postfix_s", [], [float("1.0")]);
    assert_eq!(a, b);
}

#[test]
fn test_parser_binop0() {
    let a = Expr::parse("1 + 2");
    let b = add(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop1() {
    let a = Expr::parse("1 - 2");
    let b = sub(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop2() {
    let a = Expr::parse("1 * 2");
    let b = mul(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop3() {
    let a = Expr::parse("1 / 2");
    let b = div(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop4() {
    let a = Expr::parse("1 == 2");
    let b = eq(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop5() {
    let a = Expr::parse("1 != 2");
    let b = ne(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop6() {
    let a = Expr::parse("1 <= 2");
    let b = le(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop7() {
    let a = Expr::parse("1 >= 2");
    let b = ge(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop8() {
    let a = Expr::parse("1 < 2");
    let b = lt(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop9() {
    let a = Expr::parse("1 > 2");
    let b = gt(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop10() {
    let a = Expr::parse("1 and 2");
    let b = and(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop11() {
    let a = Expr::parse("1 or 2");
    let b = or(int("1"), int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_precedence0() {
    let a = Expr::parse("1 + 2 * 3");
    let b = add(int("1"), mul(int("2"), int("3")));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_precedence1() {
    let a = Expr::parse("1 * 2 + 3");
    let b = add(mul(int("1"), int("2")), int("3"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_precedence2() {
    let a = Expr::parse("1 + 2 / 3");
    let b = add(int("1"), div(int("2"), int("3")));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_precedence3() {
    let a = Expr::parse("1 / 2 + 3");
    let b = add(div(int("1"), int("2")), int("3"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_precedence4() {
    let a = Expr::parse("1 * 2 / 3");
    let b = div(mul(int("1"), int("2")), int("3"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_associativity0() {
    let a = Expr::parse("1 + 2 + 3");
    let b = add(add(int("1"), int("2")), int("3"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_binop_associativity1() {
    let a = Stmt::diagnose("1 == 2 == 3;");
    assert_eq!(
        a,
        indoc::indoc! {"
            Error: Unexpected token `==`
               ╭─[test:1:8]
               │
             1 │ 1 == 2 == 3;
               │        ─┬
               │         ╰── Expected `;`
            ───╯"},
    );
}

#[test]
fn test_parser_unop0() {
    let a = Expr::parse("-1");
    let b = neg(int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_unop1() {
    let a = Expr::parse("!1");
    let b = not(int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_call0() {
    let a = Expr::parse("x(1)");
    let b = dcall("x", [], [int("1")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_call1() {
    let a = Expr::parse("1(x)");
    let b = call(int("1"), [var("x")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_var0() {
    let a = Expr::parse("x");
    let b = var("x");
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_var1() {
    let a = Expr::parse("x::[i32]");
    let b = def("x", [t("i32")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern0() {
    let a = Pat::parse("x");
    let b = pv("x");
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern1() {
    let a = Pat::parse("S::V(x)");
    let b = pat_enum("S", [], "V", pv("x"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern2() {
    let a = Pat::parse("S::[i32]::V(x)");
    let b = pat_enum("S", [t("i32")], "V", pv("x"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern3() {
    let a = Pat::parse("S {x, y}");
    let b = pat_struct("S", [], [("x", pv("x")), ("y", pv("y"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern4() {
    let a = Pat::parse("S::[i32] {x, y}");
    let b = pat_struct("S", [t("i32")], [("x", pv("x")), ("y", pv("y"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_pattern5() {
    let a = Pat::parse("S::[i32]");
    let b = pat_struct("S", [t("i32")], []);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_if0() {
    let a = Expr::parse("if true { 1 }");
    let b = expr_match(
        expr_bool(true),
        [(pat_bool(true), int("1")), (pat_wild(), unit())],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_if1() {
    let a = Program::parse("if true { 1 } if false { 2 }");
    let b = program([
        stmt_expr(expr_match(
            expr_bool(true),
            [(pat_bool(true), int("1")), (pat_wild(), unit())],
        )),
        stmt_expr(expr_match(
            expr_bool(false),
            [(pat_bool(true), int("2")), (pat_wild(), unit())],
        )),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_if_else0() {
    let a = Expr::parse("if true { 1 } else { 2 }");
    let b = expr_match(
        expr_bool(true),
        [(pat_bool(true), int("1")), (pat_wild(), int("2"))],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_if_else1() {
    let a = Expr::parse("if true { 1; 2 } else { 3; 4 }");
    let b = expr_match(
        expr_bool(true),
        [
            (pat_bool(true), block([stmt_expr(int("1"))], int("2"))),
            (pat_wild(), block([stmt_expr(int("3"))], int("4"))),
        ],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_match0() {
    let a = Expr::parse("match 1 { 1 => 2, _ => 3 }");
    let b = expr_match(int("1"), [(pat_int("1"), int("2")), (pat_wild(), int("3"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_match1() {
    let a = Expr::parse("match x { 1 => 2, _ => 3 }");
    let b = expr_match(var("x"), [(pat_int("1"), int("2")), (pat_wild(), int("3"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_method_call0() {
    let a = Expr::parse("1.foo()");
    let b = dcall("foo", [], [int("1")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2)");
    let b = dcall("foo", [], [int("1"), int("2")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2,)");
    let b = dcall("foo", [], [int("1"), int("2")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2, 3)");
    let b = dcall("foo", [], [int("1"), int("2"), int("3")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_method_call1() {
    let a = Expr::parse("1.foo::[i32]()");
    let b = dcall("foo", [t("i32")], [int("1")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt0() {
    let a = Stmt::parse("def id(x: i32): i32 = x;");
    let b = stmt_def("id", [], [("x", t("i32"))], [], t("i32"), var("x"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt1() {
    let a = Stmt::parse("def id(x: i32): i32 { x }");
    let b = stmt_def(
        "id",
        [],
        [("x", t("i32"))],
        [],
        t("i32"),
        block([], var("x")),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt2() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x;");
    let b = stmt_def(
        "id",
        [],
        [("x", t("i32")), ("y", t("i32"))],
        [],
        t("i32"),
        var("x"),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt3() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x + y;");
    let b = stmt_def(
        "id",
        [],
        [("x", t("i32")), ("y", t("i32"))],
        [],
        t("i32"),
        add(var("x"), var("y")),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt4() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x + y * 2;");
    let b = stmt_def(
        "id",
        [],
        [("x", t("i32")), ("y", t("i32"))],
        [],
        t("i32"),
        add(var("x"), mul(var("y"), int("2"))),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt5() {
    let a = Stmt::parse("def debug(x: i32): i32 { print(x); x }");
    let b = stmt_def(
        "debug",
        [],
        [("x", t("i32"))],
        [],
        t("i32"),
        block([stmt_expr(dcall("print", [], [var("x")]))], var("x")),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_program0() {
    let a = Program::parse(
        "def id(x: i32): i32 = x;
         def main(): i32 = id(42);",
    );
    let b = program([
        stmt_def("id", [], [("x", t("i32"))], [], t("i32"), var("x")),
        stmt_def("main", [], [], [], t("i32"), dcall("id", [], [int("42")])),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_impl_stmt0() {
    let a = Stmt::parse(
        "impl Eq[bool] {
             def eq(x: bool, y: bool): bool = true;
         }",
    );
    let b = stmt_impl(
        [],
        tr("Eq", [t("bool")], []),
        [],
        [stmt_def(
            "eq",
            [],
            [("x", t("bool")), ("y", t("bool"))],
            [],
            t("bool"),
            expr_bool(true),
        )],
        [],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_impl_stmt1() {
    StmtImpl::parse(
        "impl[T, R] Add[Vec[T], R] where Add[T, R] {
             type Output = Vec[Add[T, R]::Output];
         }",
    );
}

#[test]
fn test_parser_var_stmt0() {
    let a = Stmt::parse("var x = 1;");
    let b = stmt_var("x", hole(), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_var_stmt1() {
    let a = Stmt::parse("var x: i32 = 1;");
    let b = stmt_var("x", t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_var_stmt2() {
    let a = Program::parse("var x = 1; var y = x;");
    let b = program([
        stmt_var("x", hole(), int("1")),
        stmt_var("y", hole(), var("x")),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_var_assign0() {
    let a = Expr::parse("x = 1");
    let b = expr_assign(var("x"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_type_stmt0() {
    let a = Stmt::parse("type T = i32;");
    let b = stmt_type("T", [], t("i32"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_type_stmt1() {
    let a = Stmt::parse("type T[U] = U;");
    let b = stmt_type("T", ["U"], t("U"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_type_stmt2() {
    let a = Stmt::parse("type T[U] = (U, U);");
    let b = stmt_type("T", ["U"], ttuple([t("U"), t("U")]));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt0() {
    let a = Stmt::parse("struct S;");
    let b = stmt_struct("S", [], []);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt_err0() {
    let a = Stmt::diagnose("struct S");
    assert_eq!(
        a,
        indoc::indoc! {"
        Error: Unexpected end of file
           ╭─[test:1:9]
           │
         1 │ struct S
           │         │
           │         ╰─ Expected `;`
        ───╯"}
    );
}

#[test]
fn test_parser_struct_stmt1() {
    let a = Stmt::parse("struct S { }");
    let b = stmt_struct("S", [], []);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt2() {
    let a = Stmt::parse("struct S { x: i32 }");
    let b = stmt_struct("S", [], [("x", t("i32"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt3() {
    let a = Stmt::parse("struct S { x: i32, }");
    let b = stmt_struct("S", [], [("x", t("i32"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt4() {
    let a = Stmt::parse("struct S[T] { x: T }");
    let b = stmt_struct("S", ["T"], [("x", t("T"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr0() {
    let a = Expr::parse("S { x=1 }");
    let b = expr_struct("S", [], [("x", int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr1() {
    let a = Expr::parse("S { x }");
    let b = expr_struct("S", [], [("x", var("x"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr2() {
    let a = Expr::parse("s.x.y");
    let b = field(field(var("s"), "x"), "y");
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr3() {
    let a = Expr::parse("S { s.x }");
    let b = expr_struct("S", [], [("x", field(var("s"), "x"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr5() {
    let a = Expr::parse("S::[i32] { x=1 }");
    let b = expr_struct("S", [t("i32")], [("x", int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_stmt0() {
    let a = Stmt::parse("enum E { A(i32), B(i32) }");
    let b = stmt_enum("E", [], [("A", t("i32")), ("B", t("i32"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr0() {
    let a = Expr::parse("E::A");
    let b = variant("E", [], "A", unit());
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr1() {
    let a = Expr::parse("E::A()");
    let b = variant("E", [], "A", unit());
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr2() {
    let a = Expr::parse("E::A(1,)");
    let b = variant("E", [], "A", int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr3() {
    let a = Expr::parse("E::A(1, 2)");
    let b = variant("E", [], "A", etuple([int("1"), int("2")]));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr4() {
    let a = Expr::parse("E::[i32]::A(1, 2)");
    let b = variant("E", [t("i32")], "A", etuple([int("1"), int("2")]));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_array_expr0() {
    let a = Expr::parse("[1, 2, 3]");
    let b = expr_array([int("1"), int("2"), int("3")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_array_expr1() {
    let a = Expr::parse("a[1]");
    let b = get(var("a"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_tuple_expr0() {
    let a = Expr::parse("()");
    let b = etuple([]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_tuple_expr1() {
    let a = Expr::parse("(1,)");
    let b = etuple([int("1")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_tuple_expr2() {
    let a = Expr::parse("(1, 2)");
    let b = etuple([int("1"), int("2")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_tuple_expr3() {
    let a = Expr::parse("a.0");
    let b = expr_index(var("a"), index("0"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace0() {
    let a = Program::parse("{1}");
    let b = program([stmt_expr(block([], int("1")))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace1() {
    let a = Program::parse("{1} {2}");
    let b = program([
        stmt_expr(block([], int("1"))),
        stmt_expr(block([], int("2"))),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace2() {
    let a = Program::parse("{{1}}");
    let b = program([stmt_expr(block([], block([], int("1"))))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace3() {
    let a = Program::parse("{{1} {2}}");
    let b = program([stmt_expr(block(
        [stmt_expr(block([], int("1")))],
        block([], int("2")),
    ))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace4() {
    let a = Program::parse("{{1};{2}}");
    let b = program([stmt_expr(block(
        [stmt_expr(block([], int("1")))],
        block([], int("2")),
    ))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace5() {
    let a = Program::parse("{{1};{2};}");
    let b = program([stmt_expr(block(
        [
            stmt_expr(block([], int("1"))),
            stmt_expr(block([], int("2"))),
        ],
        unit(),
    ))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace6() {
    let a = Program::parse("{;}");
    let b = program([stmt_expr(block([], unit()))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace7() {
    let a = Program::parse("{;;;;;;;;}");
    let b = program([stmt_expr(block([], unit()))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_brace8() {
    let a = Expr::parse("{1;2}");
    let b = block([stmt_expr(int("1"))], int("2"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_paren0() {
    let a = Program::parse("();");
    let b = program([stmt_expr(unit())]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_paren1() {
    let a = Program::parse("(());");
    let b = program([stmt_expr(unit())]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_paren2() {
    let a = Program::parse("({});");
    let b = program([stmt_expr(block([], unit()))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_assoc_expr0() {
    let a = Expr::parse("@Iterator[Vec[i32]]::next");
    let b = expr_assoc(tr("Iterator", [tc("Vec", [t("i32")])], []), "next", []);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_assoc_type0() {
    let a = Type::parse("Iterator[Vec[i32]]::Item");
    let b = type_assoc(tr("Iterator", [tc("Vec", [t("i32")])], []), "Item");
    assert_eq!(a, b);
}

#[test]
fn test_parser_query_expr0() {
    let a = Expr::parse("from x in [1, 2, 3] ");
    let b = query([query_from([(
        "x",
        expr_array([int("1"), int("2"), int("3")]),
    )])]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr1() {
    let a = Expr::parse(
        "from x in source()
         select {x=f(), y=g()}
         into sink()",
    );
    let b = query([
        query_from([("x", dcall("source", [], []))]),
        query_select([("x", dcall("f", [], [])), ("y", dcall("g", [], []))]),
        query_into([dcall("sink", [], [])]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr2() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         select {x=1, y=2}
         where x > 1",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_select([("x", int("1")), ("y", int("2"))]),
        query_where(gt(var("x"), int("1"))),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr3() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         select {x=1, y=2}
         where x > 1
         select {x=1, y=2}",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_select([("x", int("1")), ("y", int("2"))]),
        query_where(gt(var("x"), int("1"))),
        query_select([("x", int("1")), ("y", int("2"))]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr4() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         with y = f(x)",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_with([("y", dcall("f", [], [var("x")]))]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr5() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         group x {
             select {x=1, y=2}
             with z = f(x)
             where x > 1
         }",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_group(
            ["x"],
            [
                query_select([("x", int("1")), ("y", int("2"))]),
                query_with([("z", dcall("f", [], [var("x")]))]),
                query_where(gt(var("x"), int("1"))),
            ],
        ),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr6() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         group x {
             compute total = sum of x
         }",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_group(["x"], [query_compute([("total", var("sum"), var("x"))])]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr7() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         group x {
             compute {
                 total = sum of x,
                 lowest = min of x,
                 highest = max of x
             }
         }",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_group(
            ["x"],
            [query_compute([
                ("total", var("sum"), var("x")),
                ("lowest", var("min"), var("x")),
                ("highest", var("max"), var("x")),
            ])],
        ),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr8() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         over tumbling(1) {
             compute {
                 total = sum of x,
                 lowest = min of x,
                 highest = max of x
             }
             select {x=1, y=2}
             where x > 1
         }",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_over(
            dcall("tumbling", [], [int("1")]),
            [
                query_compute([
                    ("total", var("sum"), var("x")),
                    ("lowest", var("min"), var("x")),
                    ("highest", var("max"), var("x")),
                ]),
                query_select([("x", int("1")), ("y", int("2"))]),
                query_where(gt(var("x"), int("1"))),
            ],
        ),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr9() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         select x=1, y=2, z=3
         where x > 1",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_select([("x", int("1")), ("y", int("2")), ("z", int("3"))]),
        query_where(gt(var("x"), int("1"))),
    ]);
    assert!(a == b, "{a:?}\n{b:?}");
}

#[test]
fn test_parser_query_expr10() {
    let a = Expr::parse(
        "from x in [1, 2, 3]
         select x=1, y=2, z=3
         where x > 1",
    );
    let b = query([
        query_from([("x", expr_array([int("1"), int("2"), int("3")]))]),
        query_select([("x", int("1")), ("y", int("2")), ("z", int("3"))]),
        query_where(gt(var("x"), int("1"))),
    ]);
    assert!(a == b, "{a:?}\n{b:?}");
}

#[test]
fn test_parser_query_expr11() {
    let a = Expr::parse(
        "from x in [1, 2, 3],
              y in [1, 2, 3]
         compute
           highest = max of x,
           lowest = min of x
         select x, y, z=3
         with
           x = f(x),
           y = g(x)
         where x > 1
         into sink()",
    );
    let b = query([
        query_from([
            ("x", expr_array([int("1"), int("2"), int("3")])),
            ("y", expr_array([int("1"), int("2"), int("3")])),
        ]),
        query_compute([
            ("highest", var("max"), var("x")),
            ("lowest", var("min"), var("x")),
        ]),
        query_select([("x", var("x")), ("y", var("y")), ("z", int("3"))]),
        query_with([
            ("x", dcall("f", [], [var("x")])),
            ("y", dcall("g", [], [var("x")])),
        ]),
        query_where(gt(var("x"), int("1"))),
        query_into([dcall("sink", [], [])]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_expr12() {
    let a = Expr::parse(
        "from x in [1, 2, 3],
              y in [1, 2, 3]
         group x, y {
             select {x=1, y=2}
         }
         into sink()",
    );
    let b = query([
        query_from([
            ("x", expr_array([int("1"), int("2"), int("3")])),
            ("y", expr_array([int("1"), int("2"), int("3")])),
        ]),
        query_group(
            ["x", "y"],
            [query_select([("x", int("1")), ("y", int("2"))])],
        ),
        query_into([dcall("sink", [], [])]),
    ]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_query_stmt() {
    let a = Stmt::parse(
        "from x in [1, 2, 3],
              y in [1, 2, 3]
         compute
           highest = max of x,
           lowest = min of x
         select x, y, z=3
         with
           x = f(x),
           y = g(x)
         where x > 1
         into sink(),
              sink();",
    );
    let b = stmt_expr(query([
        query_from([
            ("x", expr_array([int("1"), int("2"), int("3")])),
            ("y", expr_array([int("1"), int("2"), int("3")])),
        ]),
        query_compute([
            ("highest", var("max"), var("x")),
            ("lowest", var("min"), var("x")),
        ]),
        query_select([("x", var("x")), ("y", var("y")), ("z", int("3"))]),
        query_with([
            ("x", dcall("f", [], [var("x")])),
            ("y", dcall("g", [], [var("x")])),
        ]),
        query_where(gt(var("x"), int("1"))),
        query_into([dcall("sink", [], []), dcall("sink", [], [])]),
    ]));
    assert_eq!(a, b, "{a}\n{b}");
}

#[test]
fn test_parser_params0() {
    let s = "def f(): i32 = 1;";
    let mut sources = Sources::new();
    let file = sources.add("0", s);
    let mut parser = Parser::new(Lexer::new(file, "def f(): i32 = 1;"));
    let a = parser.parse();
    let b = program([stmt_def("f", [], [], [], t("i32"), int("1"))]);
    parser.report.print(&mut sources).unwrap();
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_params1() {
    let a = Stmt::parse("def f(x: i32): i32 = x;");
    let b = stmt_def("f", [], [("x", t("i32"))], [], t("i32"), var("x"));
    assert_eq!(a, b, "{a}\n{b}");
}

#[test]
fn test_parser_params2() {
    let a = Stmt::parse("def f(x: i32,): i32 = x;");
    let b = stmt_def("f", [], [("x", t("i32"))], [], t("i32"), var("x"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_params3() {
    let a = Stmt::parse("def f(x: i32, y: i32): i32 = x;");
    let b = stmt_def(
        "f",
        [],
        [("x", t("i32")), ("y", t("i32"))],
        [],
        t("i32"),
        var("x"),
    );
    assert!(a == b, "{a}\n{b}");
}

// TODO: Handle errors more gracefully.
#[test]
fn test_parser_generics0() {
    let a = Stmt::parse("def f[](): i32 = 1;");
    let b = stmt_def("f", [], [], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics1() {
    let a = Stmt::parse("def f[T](): i32 = 1;");
    let b = stmt_def("f", ["T"], [], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics2() {
    let a = Stmt::parse("def f[T,](): i32 = 1;");
    let b = stmt_def("f", ["T"], [], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics3() {
    let a = Stmt::parse("def f[T, U](): i32 = 1;");
    let b = stmt_def("f", ["T", "U"], [], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where0() {
    let a = Stmt::parse("def x(): i32 where = 1;");
    let b = stmt_def("x", [], [], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where1() {
    let a = Stmt::parse("def x(): i32 where Clone[i32] = 1;");
    let b = stmt_def(
        "x",
        [],
        [],
        [tr("Clone", [t("i32")], [])],
        t("i32"),
        int("1"),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where2() {
    let a = Stmt::parse("def x(): i32 where Clone[i32], Copy[i32] = 1;");
    let b = stmt_def(
        "x",
        [],
        [],
        [tr("Clone", [t("i32")], []), tr("Copy", [t("i32")], [])],
        t("i32"),
        int("1"),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where3() {
    let a = Stmt::parse("def x(): i32 where Clone[i32], Copy[i32], = 1;");
    let b = stmt_def(
        "x",
        [],
        [],
        [tr("Clone", [t("i32")], []), tr("Copy", [t("i32")], [])],
        t("i32"),
        int("1"),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where4() {
    let a = Stmt::parse("impl Copy[i32] where Clone[i32] {}");
    let b = stmt_impl(
        [],
        tr("Copy", [t("i32")], []),
        [tr("Clone", [t("i32")], [])],
        [],
        [],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_fun_expr0() {
    let a = Expr::parse("fun() = 1");
    let b = expr_fun([], int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_fun_expr1() {
    let a = Expr::parse("fun(x: i32): i32 = 1");
    let b = typed_expr_fun([("x", t("i32"))], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_fun_expr2() {
    let a = Expr::parse("fun(x) = fun(y) = 1");
    let b = expr_fun(["x"], expr_fun(["y"], int("1")));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_fun_type0() {
    let a = Type::parse("fun(i32, i32): i32");
    let b = tfun([t("i32"), t("i32")], t("i32"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_fun_type1() {
    let a = Type::parse("fun(i32): fun(i32): i32");
    let b = tfun([t("i32")], tfun([t("i32")], t("i32")));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_return0() {
    let a = Expr::parse("return 1");
    let b = expr_return(int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_return1() {
    let a = Expr::parse("return");
    let b = expr_return(unit());
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_continue0() {
    let a = Expr::parse("continue");
    let b = expr_continue();
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_break0() {
    let a = Expr::parse("break");
    let b = expr_break();
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_unexpected_eof0() {
    let a = Stmt::diagnose("def f(x: i32): i32 = 1");
    assert_eq!(
        a,
        indoc::indoc! {"
            Error: Unexpected end of file
               ╭─[test:1:23]
               │
             1 │ def f(x: i32): i32 = 1
               │                       │
               │                       ╰─ Expected `;`
            ───╯"}
    );
}

#[test]
fn test_parser_unexpected_token0() {
    let a = Stmt::diagnose("def f(x: i32): i32 = 1 2");
    assert_eq!(
        a,
        indoc::indoc! {"
            Error: Unexpected token `2`
               ╭─[test:1:24]
               │
             1 │ def f(x: i32): i32 = 1 2
               │                        ┬
               │                        ╰── Expected `;`
            ───╯"}
    );
}

#[test]
fn test_parser_recover0() {
    let a = Stmt::parse("def f(x: i32): i32 = +;");
    let b = stmt_def(
        "f",
        [],
        [("x", t("i32"))],
        [],
        t("i32"),
        add(expr_err(), expr_err()),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_recover1() {
    let (a, msg) = Stmt::try_parse("def f(x: +): i32 = 1;").unwrap_err();
    let a = a.unwrap();
    let b = stmt_def("f", [], [("x", type_err())], [], t("i32"), int("1"));
    assert!(a == b, "{a}\n{b}");
    assert_eq!(
        msg,
        indoc::indoc! {"
            Error: Unexpected token `+`
               ╭─[test:1:10]
               │
             1 │ def f(x: +): i32 = 1;
               │          ┬
               │          ╰── Expected `)`
            ───╯"},
    )
}

#[test]
fn test_parser_pat_annotate0() {
    let a = Pat::try_parse("x:i32").unwrap();
    let b = pv("x").with(t("i32"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_expr_annotate0() {
    let a = Expr::try_parse("1:i32").unwrap();
    let b = int("1").with(t("i32"));
    assert!(a == b, "{a}\n{b}");
}
