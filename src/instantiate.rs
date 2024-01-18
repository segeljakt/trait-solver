use crate::ast::StmtImpl;
use crate::infer::Context;

pub fn instantiate_impl(s: &StmtImpl, ctx: &mut Context) -> StmtImpl {
    let sub = s
        .generics
        .iter()
        .map(|q| (q.clone(), ctx.new_tyvar()))
        .collect::<Vec<_>>();

    let head = s.head.apply(&sub);
    let body = s
        .where_clause
        .iter()
        .map(|i| i.apply(&sub))
        .collect::<Vec<_>>();
    let defs = s.defs.iter().map(|d| d.apply(&sub)).collect::<Vec<_>>();
    let types = s.types.iter().map(|t| t.apply(&sub)).collect::<Vec<_>>();

    StmtImpl::new(s.span, vec![], head, body, defs, types)
}

// pub fn instantiate_def(s: &StmtDef, ctx: &mut Context) -> StmtDef {
//     let sub = s
//         .generics
//         .iter()
//         .map(|q| (q.clone(), ctx.new_tyvar()))
//         .collect::<Vec<_>>();
//
//     let body = s
//         .where_clause
//         .iter()
//         .map(|p| p.apply(&sub))
//         .collect::<Vec<_>>();
//     let params = s.params.iter().map(|p| p.apply(&sub)).collect::<Vec<_>>();
//     let ty = s.ty.apply(&sub);
//     let expr = s.expr.apply(&sub);
//
//     StmtDef::new(s.span, s.name.clone(), vec![], body, params, ty, expr)
// }
