use aqua::ast::Program;
use aqua::util::call;
use aqua::util::expr_err;
use aqua::util::expr_struct;
use aqua::util::etuple;
use aqua::util::hole;
use aqua::util::int;
use aqua::util::program;
use aqua::util::stmt_def;
use aqua::util::stmt_def_decl;
use aqua::util::stmt_enum;
use aqua::util::stmt_expr;
use aqua::util::stmt_impl;
use aqua::util::stmt_struct;
use aqua::util::stmt_trait;
use aqua::util::stmt_var;
use aqua::util::t;
use aqua::util::tc;
use aqua::util::tg;
use aqua::util::tr;
use aqua::util::ttuple;
use aqua::util::type_err;
use aqua::util::var;
use aqua::util::variant;

macro_rules! ok {
    {$s:tt} => { { Program::try_resolve(indoc::indoc! { $s }).unwrap_or_else(|(_, s)| panic!("{}", s)) } }
}

macro_rules! err {
    {$s:tt} => { { Program::try_resolve(indoc::indoc! { $s }).unwrap_err() } }
}

#[test]
fn test_resolve_var0() {
    let a = ok!("var x = 0; x;");
    let b = program([stmt_var("x", hole(), int("0")), stmt_expr(var("x"))]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_var_err0() {
    let (a, msg) = err!("var x = 0; y;");
    let b = program([stmt_var("x", hole(), int("0")), stmt_expr(expr_err())]);
    assert!(a == b, "{a}\n\n{b}");
    assert_eq!(
        msg,
        indoc::indoc! {"
        Error: Name `y` not found.
           ╭─[test:1:12]
           │
         1 │ var x = 0; y;
           │            ┬
           │            ╰── Expected expression.
        ───╯"},
        "{msg}"
    )
}

#[test]
fn test_resolve_def0() {
    let a = ok!("def f(): i32 = 0; f();");
    let b = program([
        stmt_def("f", [], [], [], t("i32"), int("0")),
        stmt_expr(call(var("f"), [])),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_def1() {
    let a = ok!("def f(x: i32): i32 = x;");
    let b = program([stmt_def("f", [], [("x", t("i32"))], [], t("i32"), var("x"))]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_def2() {
    let a = ok!("def f(x:i32): i32 = f(x);");
    let b = program([stmt_def(
        "f",
        [],
        [("x", t("i32"))],
        [],
        t("i32"),
        call(var("f"), [var("x")]),
    )]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_def_param1() {
    let (a, msg) = err!("def f(): i32 = x;");
    let b = program([stmt_def("f", [], [], [], t("i32"), expr_err())]);
    assert!(a == b, "{a}\n\n{b}");
    assert_eq!(
        msg,
        indoc::indoc! {"
            Error: Name `x` not found.
               ╭─[test:1:16]
               │
             1 │ def f(): i32 = x;
               │                ┬
               │                ╰── Expected expression.
            ───╯"}
    )
}

#[test]
fn test_resolve_def_generic() {
    let a = ok!("def f[T](x: T): T = x;");
    let b = program([stmt_def(
        "f",
        ["T"],
        [("x", tg("T"))],
        [],
        tg("T"),
        var("x"),
    )]);
    assert!(a == b, "{a}\n{b}");
}

#[test]
fn test_resolve_type0() {
    let a = ok!("type T = i32;
                 var x: T = 0;");
    let b = program([stmt_var("x", t("i32"), int("0"))]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_type1() {
    let a = ok!("type T[U] = (i32, U);
                var x: T[i32] = (0, 0);");
    let b = program([stmt_var(
        "x",
        ttuple([t("i32"), t("i32")]),
        etuple([int("0"), int("0")]),
    )]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_type2() {
    let (a, msg) = err!(
        "type T[U] = (i32, U);
         var x: T[i32, i32] = (0, 0);"
    );
    let b = program([stmt_var("x", type_err(), etuple([int("0"), int("0")]))]);
    assert!(a == b, "{a}\n\n{b}");
    assert_eq!(
        msg,
        indoc::indoc! {"
            Error: Wrong number of arguments. Found 2, expected 1
               ╭─[test:2:8]
               │
             2 │ var x: T[i32, i32] = (0, 0);
               │        ┬
               │        ╰── Expected 1 arguments.
            ───╯"}
    );
}

#[test]
fn test_resolve_trait0() {
    let a = ok!("trait Trait[T] {
                     def f(x:T): T;
                 }");
    let b = program([stmt_trait(
        "Trait",
        ["T"],
        [],
        [stmt_def_decl("f", [], [("x", tg("T"))], [], tg("T"))],
        [],
    )]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_trait1() {
    let a = ok!("trait Trait[T] {
                     def f(x:T): T;
                 }
                 def g[T](x:T): T where Trait[T] = f(x);");
    let b = program([
        stmt_trait(
            "Trait",
            ["T"],
            [],
            [stmt_def_decl("f", [], [("x", tg("T"))], [], tg("T"))],
            [],
        ),
        stmt_def(
            "g",
            ["T"],
            [("x", tg("T"))],
            [tr("Trait", [tg("T")], [])],
            tg("T"),
            call(var("f"), [var("x")]),
        ),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_impl0() {
    let a = ok!("trait Trait[T] {
                     def f(x:T): T;
                 }
                 impl Trait[i32] {
                     def f(x:i32): i32 = x;
                 }");
    let b = program([
        stmt_trait(
            "Trait",
            ["T"],
            [],
            [stmt_def_decl("f", [], [("x", tg("T"))], [], tg("T"))],
            [],
        ),
        stmt_impl(
            [],
            tr("Trait", [t("i32")], []),
            [],
            [stmt_def("f", [], [("x", t("i32"))], [], t("i32"), var("x"))],
            [],
        ),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_trait_impl1() {
    let (a, msg) = err!(
        "trait Trait[T] {
             def f(x: T): T;
         }
         impl Trait[i32] {
             def g(x: i32): i32 = x;
         }"
    );
    let b = program([
        stmt_trait(
            "Trait",
            ["T"],
            [],
            [stmt_def_decl("f", [], [("x", tg("T"))], [], tg("T"))],
            [],
        ),
        stmt_impl(
            [],
            tr("Trait", [t("i32")], []),
            [],
            [stmt_def("f", [], [("x", t("i32"))], [], t("i32"), var("x"))],
            [],
        ),
    ]);
    assert!(a == b, "{a}\n\n{b}");
    assert_eq!(msg, "");
}

#[test]
fn test_resolve_struct0() {
    let a = ok!("struct S[T] { x: T }");
    let b = program([stmt_struct("S", ["T"], [("x", tg("T"))])]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_struct1() {
    let a = ok!("struct S[T] { x: T }
                 var s: S[i32] = S::[i32] { x: 0 };");
    let b = program([
        stmt_struct("S", ["T"], [("x", tg("T"))]),
        stmt_var(
            "s",
            tc("S", [t("i32")]),
            expr_struct("S", [t("i32")], [("x", int("0"))]),
        ),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_struct2() {
    let (a, msg) = err!(
        "struct S { x: i32 }
         S { y: 0 };"
    );
    let b = program([
        stmt_struct("S", [], [("x", t("i32"))]),
        stmt_expr(expr_err()),
    ]);
    assert!(a == b, "{a}\n\n{b}");
    assert_eq!(
        msg,
        indoc::indoc! {"
            Error: Wrong fields provided. Found { y }, expected { x }
               ╭─[test:2:1]
               │
             2 │ S { y: 0 };
               │ ┬
               │ ╰── Expected { x } fields.
            ───╯"}
    );
}

#[test]
fn test_resolve_struct3() {
    let a = ok!("struct S;
                 var s: S = S;");
    let b = program([
        stmt_struct("S", [], []),
        stmt_var("s", t("S"), expr_struct("S", [], [])),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_enum0() {
    let a = ok!("enum E[T] { A(T) }");
    let b = program([stmt_enum("E", ["T"], [("A", tg("T"))])]);
    assert!(a == b, "{a}\n\n{b}");
}

#[test]
fn test_resolve_enum1() {
    let a = ok!("enum E[T] { A(T) }
                 var e: E[i32] = E::[i32]::A(0);");
    let b = program([
        stmt_enum("E", ["T"], [("A", tg("T"))]),
        stmt_var(
            "e",
            tc("E", [t("i32")]),
            variant("E", [t("i32")], "A", int("0")),
        ),
    ]);
    assert!(a == b, "{a}\n\n{b}");
}
