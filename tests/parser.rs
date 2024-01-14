mod util;

use crate::util::add;
use crate::util::and;
use crate::util::div;
use crate::util::eq;
use crate::util::expr_array;
use crate::util::expr_index;
use crate::util::float;
use crate::util::ge;
use crate::util::get;
use crate::util::gt;
use crate::util::impl_def;
use crate::util::index;
use crate::util::le;
use crate::util::lt;
use crate::util::mul;
use crate::util::ne;
use crate::util::or;
use crate::util::sub;
use aqua::ast::Expr;
use aqua::ast::Program;
use aqua::ast::Stmt;
use aqua::ast::StmtImpl;
use aqua::ast::Type;
use aqua::diag::Sources;
use aqua::lexer::Lexer;
use aqua::parser::Parser;
use util::block;
use util::bool;
use util::call;
use util::def;
use util::expr_assoc;
use util::expr_struct;
use util::expr_tuple;
use util::field;
use util::hole;
use util::int;
use util::program;
use util::stmt_def;
use util::stmt_enum;
use util::stmt_expr;
use util::stmt_impl;
use util::stmt_struct;
use util::stmt_var;
use util::t;
use util::tc;
use util::tr;
use util::type_assoc;
use util::unit;
use util::var;
use util::variant;

#[test]
fn test_parser_int0() {
    let a = Expr::parse("1");
    let b = int("1");
    assert_eq!(a, b);
}

#[test]
fn test_parser_int1() {
    let a = Expr::parse("123");
    let b = int("123");
    assert_eq!(a, b);
}

#[test]
fn test_parser_float0() {
    let a = Expr::parse("1.0");
    let b = float("1.0");
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
    let diags = Stmt::parse_diags("1 == 2 == 3;");
    assert!(!diags.is_empty());
}

#[test]
fn test_parser_call0() {
    let a = Expr::parse("x()");
    let b = call(var("x"), []);
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
fn test_parser_method_call0() {
    let a = Expr::parse("1.foo()");
    let b = call(var("foo"), [int("1")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2)");
    let b = call(var("foo"), [int("1"), int("2")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2,)");
    let b = call(var("foo"), [int("1"), int("2")]);
    assert!(a == b, "{a}\n{b}");
    let a = Expr::parse("1.foo(2, 3)");
    let b = call(var("foo"), [int("1"), int("2"), int("3")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_method_call1() {
    let a = Expr::parse("1.foo::[i32]()");
    let b = call(def("foo", [t("i32")]), [int("1")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt0() {
    let a = Stmt::parse("def id(x: i32): i32 = x;");
    let b = stmt_def("id", [], [], [("x", t("i32"))], t("i32"), var("x"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt1() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x;");
    let b = stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        var("x"),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt2() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x + y;");
    let b = stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        add(var("x"), var("y")),
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_def_stmt3() {
    let a = Stmt::parse("def id(x: i32, y: i32): i32 = x + y * 2;");
    let b = stmt_def(
        "id",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        add(var("x"), mul(var("y"), int("2"))),
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
        stmt_def("id", [], [], [("x", t("i32"))], t("i32"), var("x")),
        stmt_def("main", [], [], [], t("i32"), call(var("id"), [int("42")])),
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
        [impl_def(
            "eq",
            [],
            [],
            [("x", t("bool")), ("y", t("bool"))],
            t("bool"),
            bool(true),
        )],
    );
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_impl_stmt1() {
    StmtImpl::parse(
        "impl[T, R] Add[Vec[T], R] where Add[T, R] {
             type Output = Vec[Add[T, R].Output];
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
fn test_parser_struct_stmt0() {
    let a = Stmt::parse("struct S;");
    let b = stmt_struct("S", [], []);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_stmt_err0() {
    let a = Stmt::try_parse("struct S");
    assert_eq!(a, None);
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
fn test_parser_struct_expr0() {
    let a = Expr::parse("S { x: 1 }");
    let b = expr_struct("S", [("x", int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_struct_expr1() {
    let a = Expr::parse("S { x }");
    let b = expr_struct("S", [("x", var("x"))]);
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
    let b = expr_struct("S", [("x", field(var("s"), "x"))]);
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
    let b = variant("E", "A", unit());
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr1() {
    let a = Expr::parse("E::A()");
    let b = variant("E", "A", unit());
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr2() {
    let a = Expr::parse("E::A(1,)");
    let b = variant("E", "A", int("1"));
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_enum_expr3() {
    let a = Expr::parse("E::A(1, 2)");
    let b = variant("E", "A", expr_tuple([int("1"), int("2")]));
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
    let a = Expr::parse("(1, 2, 3)");
    let b = expr_tuple([int("1"), int("2"), int("3")]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_tuple_expr1() {
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
    let a = Expr::parse("@Iterator[Vec[i32]].next");
    let b = expr_assoc(tr("Iterator", [tc("Vec", [t("i32")])], []), "next", []);
    println!("{a}");
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_assoc_type0() {
    let a = Type::parse("Iterator[Vec[i32]].Item");
    let b = type_assoc(tr("Iterator", [tc("Vec", [t("i32")])], []), "Item");
    assert_eq!(a, b);
}

#[test]
fn test_parser_query_expr0() {
    let _p0 = Program::parse(
        "from x in [1, 2, 3]
         select {x:1, y:2}
         where x > 1;",
    );
}

#[test]
fn test_parser_params0() {
    let s = "def f(): i32 = 1;";
    let mut sources = Sources::new();
    let file = sources.add("0", s);
    let mut parser = Parser::new(Lexer::new(file, "def f(): i32 = 1;"));
    let a = parser.parse();
    let b = program([stmt_def("f", [], [], [], t("i32"), int("1"))]);
    parser.diags.print(&mut sources).unwrap();
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_params1() {
    let a = Program::parse("def f(x: i32): i32 = x;");
    let b = program([stmt_def("f", [], [], [("x", t("i32"))], t("i32"), var("x"))]);
    assert_eq!(a, b, "{a}\n{b}");
}

#[test]
fn test_parser_params2() {
    let a = Program::parse("def f(x: i32,): i32 = x;");
    let b = program([stmt_def("f", [], [], [("x", t("i32"))], t("i32"), var("x"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_params3() {
    let a = Program::parse("def f(x: i32, y: i32): i32 = x;");
    let b = program([stmt_def(
        "f",
        [],
        [],
        [("x", t("i32")), ("y", t("i32"))],
        t("i32"),
        var("x"),
    )]);
    assert!(a == b, "{a}\n{b}");
}

// TODO: Handle errors more gracefully.
#[test]
fn test_parser_generics0() {
    let a = Program::parse("def f[](): i32 = 1;");
    let b = program([stmt_def("f", [], [], [], t("i32"), int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics1() {
    let a = Program::parse("def f[T](): i32 = 1;");
    let b = program([stmt_def("f", ["T"], [], [], t("i32"), int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics2() {
    let a = Program::parse("def f[T,](): i32 = 1;");
    let b = program([stmt_def("f", ["T"], [], [], t("i32"), int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_generics3() {
    let a = Program::parse("def f[T, U](): i32 = 1;");
    let b = program([stmt_def("f", ["T", "U"], [], [], t("i32"), int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where0() {
    let a = Program::parse("def x(): i32 where = 1;");
    let b = program([stmt_def("x", [], [], [], t("i32"), int("1"))]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where1() {
    let a = Program::parse("def x(): i32 where Clone[i32] = 1;");
    let b = program([stmt_def(
        "x",
        [],
        [tr("Clone", [t("i32")], [])],
        [],
        t("i32"),
        int("1"),
    )]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where2() {
    let a = Program::parse("def x(): i32 where Clone[i32], Copy[i32] = 1;");
    let b = program([stmt_def(
        "x",
        [],
        [tr("Clone", [t("i32")], []), tr("Copy", [t("i32")], [])],
        [],
        t("i32"),
        int("1"),
    )]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where3() {
    let a = Program::parse("def x(): i32 where Clone[i32], Copy[i32], = 1;");
    let b = program([stmt_def(
        "x",
        [],
        [tr("Clone", [t("i32")], []), tr("Copy", [t("i32")], [])],
        [],
        t("i32"),
        int("1"),
    )]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_parser_where4() {
    let a = Program::parse("impl Copy[i32] where Clone[i32] {}");
    let b = program([stmt_impl(
        [],
        tr("Copy", [t("i32")], []),
        [tr("Clone", [t("i32")], [])],
        [],
    )]);
    assert!(a == b, "{a}\n{b}");
}

// #[test]
// fn test_parser_err0() {
//     let s = "1 + ;";
//     let mut sources = Sources::new();
//     let file = sources.next_file();
//     let lexer = Lexer::new(file, s);
//     let mut parser = Parser::new(lexer);
//     let a = parser.parse();
//     let b = program([stmt_expr(binop(int("1"), "__add__", expr_err()))]);
//
//     assert!(a == b, "{a}\n{b}");
//
//     assert_eq!(
//         parser.diags.string(&mut sources).unwrap(),
//         "1:4: expected expression"
//     );
// }
