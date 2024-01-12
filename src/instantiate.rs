use crate::ast::StmtDef;
use crate::ast::StmtImpl;
use crate::infer::Context;

pub fn instantiate_impl(imp: &StmtImpl, ctx: &mut Context) -> StmtImpl {
    let sub = imp
        .generics
        .iter()
        .map(|q| (q.clone(), ctx.new_tyvar()))
        .collect::<Vec<_>>();

    let head = imp.head.apply(&sub);
    let body = imp.body.iter().map(|i| i.apply(&sub)).collect::<Vec<_>>();
    let defs = imp.defs.iter().map(|d| d.apply(&sub)).collect::<Vec<_>>();

    StmtImpl::new(imp.span, vec![], head, body, defs)
}

pub fn instantiate_def(def: &StmtDef, ctx: &mut Context) -> StmtDef {
    let sub = def
        .generics
        .iter()
        .map(|q| (q.clone(), ctx.new_tyvar()))
        .collect::<Vec<_>>();

    let body = def.preds.iter().map(|p| p.apply(&sub)).collect::<Vec<_>>();
    let params = def.params.iter().map(|p| p.apply(&sub)).collect::<Vec<_>>();
    let ty = def.ty.apply(&sub);
    let expr = def.expr.apply(&sub);

    StmtDef::new(def.span, def.name.clone(), vec![], body, params, ty, expr)
}
