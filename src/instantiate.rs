use crate::data::Context;
use crate::data::Def;
use crate::data::Impl;

pub fn instantiate_impl(imp: &Impl, ctx: &mut Context) -> Impl {
    let sub = imp
        .generics
        .iter()
        .map(|q| (q.clone(), ctx.new_tyvar()))
        .collect::<Vec<_>>();

    let head = imp.head.apply(&sub);
    let body = imp.body.iter().map(|i| i.apply(&sub)).collect::<Vec<_>>();
    let defs = imp.defs.iter().map(|d| d.apply(&sub)).collect::<Vec<_>>();

    Impl::new(vec![], head, body, defs)
}

pub fn instantiate_def(def: &mut Def, ctx: &mut Context) -> Def {
    let sub = def
        .generics
        .iter()
        .map(|q| (q.clone(), ctx.new_tyvar()))
        .collect::<Vec<_>>();

    let body = def.preds.iter().map(|p| p.apply(&sub)).collect::<Vec<_>>();

    let params = def
        .params
        .iter_mut()
        .map(|p| p.apply(&sub))
        .collect::<Vec<_>>();

    let ty = def.ty.apply(&sub);

    let expr = def.expr.apply(&sub);

    Def::new(def.name.clone(), vec![], body, params, ty, expr)
}
