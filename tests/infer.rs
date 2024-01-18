use aqua::ast::Program;
use aqua::util::call;
use aqua::util::int;
use aqua::util::program;
use aqua::util::stmt_def;
use aqua::util::stmt_expr;
use aqua::util::t;
use aqua::util::tfun;
use aqua::util::tg;
use aqua::util::var;

macro_rules! ok {
    {$s:tt} => { { Program::try_infer(indoc::indoc! { $s }).unwrap_or_else(|(_, s)| panic!("{}", s)) } }
}

macro_rules! err {
    {$s:tt} => { { Program::try_infer(indoc::indoc! { $s }).unwrap_err() } }
}

macro_rules! ty {
    {$s:tt} => { { Program::try_resolve(indoc::indoc! { $s }).unwrap_or_else(|(_, s)| panic!("{}", s)) } }
}

#[test]
fn test_literal0() {
    let a = ok!("1;");
    let b = ty!("1:i32;");
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
}

#[test]
fn test_function0() {
    let a = ok!("def f(): i32 = 0; f();");
    let b = ty!("def f(): i32 = 0:i32; (f:(fun(i32):i32))():i32;");
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
}

#[test]
fn test_function1() {
    let a = ok!("def f(x: i32): i32 = 0;");
    let b = program([stmt_def(
        "f",
        [],
        [("x", t("i32"))],
        [],
        t("i32"),
        int("0").with(t("i32")),
    )]);
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
}

#[test]
fn test_function2() {
    let a = ok!("def f(x: i32): i32 = x; f(0);");
    let b = program([
        stmt_def(
            "f",
            [],
            [("x", t("i32"))],
            [],
            t("i32"),
            var("x").with(t("i32")),
        ),
        stmt_expr(
            call(
                var("f").with(tfun([t("i32")], t("i32"))),
                [int("0").with(t("i32"))],
            )
            .with(t("i32")),
        ),
    ]);
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
}

#[test]
fn test_function3() {
    let (a, msg) = err!("def f(x: i32): f32 = x;");
    let b = program([stmt_def(
        "f",
        [],
        [("x", t("i32"))],
        [],
        t("f32"),
        var("x").with(t("i32")),
    )]);
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
    assert_eq!(
        msg,
        indoc::indoc! {"
            Error: Type mismatch
               ╭─[test:1:1]
               │
             1 │ def f(x: i32): f32 = x;
               │ ───────────┬─────────┬
               │            ╰──────────── Expected f32
               │                      │
               │                      ╰── Found i32
            ───╯"}
    )
}

#[test]
fn test_function4() {
    let a = ok!("def f[T](x: T): T = x;");
    let b = program([stmt_def(
        "f",
        ["T"],
        [("x", tg("T"))],
        [],
        tg("T"),
        var("x").with(tg("T")),
    )]);
    assert!(a == b, "{}\n\n{}", a.display_types(), b.display_types());
}

// #[test]
// fn test_function5() {
//     let a = ok!("def f[T](x: T): T = x; f(0); f(0.0);");
//     let b = program([
//         stmt_def(
//             "f",
//             ["T"],
//             [("x", tv("T"))],
//             [],
//             tv("T"),
//             var("x").with(tv("T")),
//         ),
//         stmt_expr(
//             call(
//                 var("f").with(tfun([t("i32")], t("i32"))),
//                 [int("0").with(t("i32"))],
//             )
//             .with(t("i32")),
//         ),
//         stmt_expr(
//             call(
//                 var("f").with(tfun([t("f32")], t("f32"))),
//                 [float("0.0").with(t("f32"))],
//             )
//             .with(t("f32")),
//         ),
//     ]);
//     assert!(a == b, "{}\n\n{}", a.type_info(), b.type_info());
// }

// impl Clone[i32] {}
// def f(x: i32): i32 where Clone[i32] = x;
// f(0);
// #[test]
// fn test_function2() {
//     let prog = Program::parse(
//         "impl Clone[i32] {}
//          def f(x: i32): i32 where Clone[i32] = x;
//          f(0);",
//     );
//     program([
//         // fact([], p("Clone", [t("i32")], [])).into(),
//         def("f", [], [], [("x", t("i32"))], t("i32"), var("x")).into(),
//         call(var("f"), [int("0")]).into(),
//     ]);
//     assert!(prog.infer().is_ok())
// }
//
// // def f(x: i32): i32 = x;
// // trait Foo<T> {
// //     def f(x: T): T;
// // }
// //
// // f(1); # Should fail
