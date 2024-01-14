use aqua::ast::Program;
use aqua::resolve::Resolver;

mod util;

#[test]
fn test_resolve0() {
    let p = Program::parse("var x = 0; x;");
    let mut ctx = Resolver::new();
    let _p = p.resolve(&mut ctx);
    assert!(ctx.diags.is_empty());
}
