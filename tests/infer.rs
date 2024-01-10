use trait_solver::data::Program;
use trait_solver::helper::call;
use trait_solver::helper::stmt_def;
use trait_solver::helper::ev;
use trait_solver::helper::int;
use trait_solver::helper::program;
use trait_solver::helper::t;
use trait_solver::helper::tc;

#[test]
fn test_function0() {
    let prog = Program::parse(
        "def f(): i32 = 0;
         f();",
    );
    let result = prog.infer();
    assert!(result.is_ok());

    let prog2 = result.unwrap();

    let tprog = program([
        stmt_def("f", [], [], [], t("i32"), int("0").with(t("i32"))).into(),
        call(ev("f").with(tc("fun", [t("i32")])), [])
            .with(t("i32"))
            .into(),
    ]);
    assert_eq!(prog2, tprog);
}

#[test]
fn test_function1() {
    let prog = Program::parse(
        "def f(x: i32): i32 = x;
         f(0);",
    );
    assert!(prog.infer().is_ok())
}

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
//         def("f", [], [], [("x", t("i32"))], t("i32"), ev("x")).into(),
//         call(ev("f"), [int("0")]).into(),
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