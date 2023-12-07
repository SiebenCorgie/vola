use ahash::{AHashMap, AHashSet};
use rvsdg::{NodeRef, Rvsdg};
use tinyvec::ArrayVec;
use vola_ast::{common::Identifier, Ast};

use crate::{err::PassResult, VolaHir};

mod alge;

pub type TinyCollection<T> = ArrayVec<[T; 3]>;

///Tries to build a basic VolaHir representation from the AST.
pub fn tranform_into_ast(ast: Ast) -> PassResult<VolaHir> {
    //The transformer will create a lambda for each function-like
    // expression in our AST. Fields are exported, all other expressions not
    // (for now, TODO: maybe allow export for vola libraries?)
    //
    // While building those initial λ-Nodes, we create a lookup table that maps the names to the nodes.
    // This allows us to connect all context variables in a second pass.
    //
    // At that point the basic un-typed, un-validated RVSDG of the AST should be build.

    let Ast {
        fields,
        ops,
        prims,
        alges,
    } = ast;

    let mut graph = VolaHir::new();
    //tracks how lambda identifier map to a defined node.
    let mut lambda_map: AHashMap<Identifier, NodeRef> = AHashMap::default();

    graph.rvsdg.on_omega_node(|omega| {
        for (ident, alge) in alges {
            let lambda_decl = alge::build_alge_lambda(&mut graph.type_states, omega, alge);

            //let _ = lambda_map.insert(ident, lambda_decl);
            todo!("continue")
        }
    });

    PassResult::Ok(graph)
}
