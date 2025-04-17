/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::StructuralNode,
    region::RegionLocation,
    smallvec::smallvec,
    util::copy::StructuralClone,
    NodeRef, SmallColl,
};
use vola_common::{ariadne::Label, report, warning_reporter, Span, VolaError};

use crate::{
    alge::EvalNode, common::Ty, csg::CsgOp, graph::auxiliary::ImplKey, DialectNode, OptEdge,
    OptError, Optimizer,
};

struct SpecCtx {
    tree_access_span: Span,
    csg_tree: OutportLocation,
    eval_node: NodeRef,
}

impl Optimizer {
    /// Shortcut to call [Self::specialize_export] for all exported λs
    pub fn specialize_all_exports(&mut self) -> Result<(), Vec<VolaError<OptError>>> {
        let mut errors = Vec::with_capacity(0);

        //specialize all functions:
        //Iterate over all functions in the TopLevel region, topological.
        //IFF they exist in the _functions_ map, call the specializer

        let life_toplevel_nodes = self
            .graph
            .live_nodes_in_region(self.graph.toplevel_region());
        let topord_functions =
            self.graph
                .topological_order_nodes(self.functions.values().filter_map(|f| {
                    if life_toplevel_nodes.contains(&f.lambda) {
                        Some(f.lambda)
                    } else {
                        None
                    }
                }));

        //NOTE: we don't specialize implementations, since those are
        //      _pulled-in_ in this process, so whenever a impl-block with eval-nodes is
        //      inlined, the "restart" step will discover that, and ... restart the process.
        for function in topord_functions {
            let (function_region, function_name, head_span) =
                if let Some(function) = self.functions.values().find(|v| v.lambda == function) {
                    (
                        function.region(),
                        function.name.clone(),
                        function.def_span.clone(),
                    )
                } else {
                    errors.push(VolaError::new(OptError::Internal(format!(
                    "function node {function:?} was discovered, but not present in function map."
                ))));
                    continue;
                };
            if std::env::var("VOLA_DUMP_ALL").is_ok()
                || std::env::var("DUMP_BEFORE_SPECIALIZE").is_ok()
            {
                self.push_debug_state(&format!("before specialize {}", function_name));
            }
            //NOTE: defer breaking to _after_ all exports are specialized (or not^^).
            if let Err(e) = self.specialize_region(function_region) {
                errors.push(e.with_label(head_span, "on this function"));
            }

            if std::env::var("VOLA_DUMP_ALL").is_ok()
                || std::env::var("DUMP_POST_SPECIALIZE").is_ok()
            {
                self.push_debug_state(&format!("post specialize {}", function_name));
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_POST_SPECIALIZE").is_ok() {
            self.push_debug_state(&format!("post specialize"));
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    ///The specializer pass for one region. Transforms a tree of CSG nodes into a
    /// call tree of specialized _alge_-dialect based λ-nodes.
    ///
    /// Contrary to the _old_ specializer this one doesn't have to inline all calls.
    ///
    /// Instead a inliner _can_ be used if needed. It also doesn't import values via context variables, but
    /// uses _normal_ arguments to the specific λ-nodes / apply-nodes.
    pub fn specialize_region(&mut self, region: RegionLocation) -> Result<(), VolaError<OptError>> {
        //NOTE: The new specializer follows the learning of the first one
        //(see https://gitlab.com/tendsinmende/vola/-/blob/cc9624a2c159bac35cc818130f3041409b296419/crates/vola-opt/src/passes/field_dispatch.rs)
        //
        // However, this time more _structured_ for better maintainability.
        //
        // The main job of this pass is to specialize ImplBlocks of nodes, based on a specific
        // CSG-Node tree.
        //
        // So let say you have a tree of
        //
        //          csg t =
        //            |
        //          Union
        //         /     \
        //  Translate   Sphere
        //     |
        //   Box
        //
        //
        // If an exports states `eval t.SomeConcept(p0, p1)`
        //
        // The specializer takes care of pulling in the implementation of SomeConcept for Union, then
        // specializing any eval _within_ that implementation based on the remaining two sub-trees etc.
        //
        // In the end the goal is having replaced `eval t.SomeConcept(p0, p1)`
        // with a call to
        // SomeConceptForUnion(p0, p1,
        //    SomeConceptForTranslate(p0, p1,
        //       SomeConceptForBox(p0, p1)
        //    )
        //    SomeConceptForSphere(p0, p1)
        // )
        //
        // Now the probleme here is that
        // 1. Any impl block is free to call any other concept, or several for a subtree
        // 2. The impl block if free to use other parameters to any call, or mutate parameters before calling.
        //
        // So the specialization becomes a lot more involved in practice.
        //
        // To handle that, we still keep any implementation of a concept in seperate λ-Nodes.
        // We then expose any parameter _to-the-concept_ and _to-the-entity/op_ as a simple λ-argument,
        // which in turn is supplied by the caller.
        //
        // We express the CSG-Tree by importing the further specialized sub-trees as context variables.
        // So if for instance a operation has a single sub-tree "t" that is once evaluated with concept A
        // and once with concept B, then, two CVs are connected to the specialized λ-node, t.A & t.B.
        // Each (again) supplied with the apropriate arguments
        //
        // A detail is, that we do all this within the ExportFn's region. So any parameter that is taken
        // from that region (for instance parameters to the export, or calculation done within the tree definition)
        // can be imported as a context variable. Same goes for any parameters from a used field-def. This
        // is, because we first inline all field-defs before specializing.

        //NOTE inline takes care of bringing all CSG nodes into the same region
        //TODO: Don't inline all, use heuristic instead.
        let region_span = self.find_span(region.into()).unwrap_or(Span::empty());
        self.inline_all_region(region)
            .map_err(|e| VolaError::error_here(e, region_span.clone(), "in this region"))?;

        //Try specializing reverse-topo-ord eval nodes, till we don't have any left
        'restart: loop {
            let topoord = self.graph.topological_order_region(region);
            for node in topoord.into_iter().rev() {
                for subreg in 0..self.graph[node].regions().len() {
                    //if there are sub-regions to this node, try to specialize those first
                    let region = RegionLocation {
                        node,
                        region_index: subreg,
                    };
                    self.specialize_region(region)?;
                }
                //if this is a eval node, specialize its value, then restart
                if self.is_node_type::<EvalNode>(node) {
                    //node seems good, start specializer
                    let span = self.find_span(node.into()).unwrap_or(region_span.clone());
                    self.specialize_eval_entry(node)
                        .map_err(|e| e.with_label(span, "while specializing this"))?;
                    continue 'restart;
                }
            }

            //if we came till here, there is no eval-node left in the graph, so we finished specializing
            break;
        }

        Ok(())
    }

    ///Specializes the whole tree-access tree.
    pub fn specialize_eval_entry(&mut self, eval: NodeRef) -> Result<(), VolaError<OptError>> {
        //build the frist eval_spec_ctx
        //for the first connected subtree, then substitude the tree-access node with a eval node
        let src_span = self
            .graph
            .node(eval)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();

        for spec in self.eval_node_to_spec_ctx(src_span, eval)? {
            //now start the actual replacement descent
            self.specialize_eval_node(spec)?
        }
        Ok(())
    }

    ///Specializes the `eval` node based on `spec_ctx`. Assumes that the eval-node is in the same region
    /// as the csg-value it is connected to.
    fn specialize_eval_node(&mut self, spec_ctx: SpecCtx) -> Result<(), VolaError<OptError>> {
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_SPECIALIZE").is_ok()
        {
            self.push_debug_state(&format!("before specialize {}", spec_ctx.eval_node));
        }
        //The first step is to find the correct specialization impl block based on the called
        // csg-tree root node, and the called concept of the root node.
        let concept_name = self.graph[spec_ctx.eval_node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<EvalNode>()
            .unwrap()
            .called_concept
            .clone();

        let csg_name = self.graph[spec_ctx.csg_tree.node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<CsgOp>()
            .unwrap()
            .op
            .clone();

        let implkey = ImplKey {
            concept: concept_name.clone(),
            node: csg_name.clone(),
        };

        let subtree_count = self.graph[spec_ctx.csg_tree.node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<CsgOp>()
            .unwrap()
            .subtree_count;

        //The region in which the specialized CSG node is placed. Either the eval-nodes's region, or a parent.
        let csg_region = self.graph[spec_ctx.csg_tree.node].parent.unwrap();

        //NOTE: we prepare the identity implementation at this point, if the subtree-count is 1 and there
        //      is no such implblock yet.
        if !self.concept_impl.contains_key(&implkey) && subtree_count == 1 {
            if let Err(e) = self.implement_identity_for_concept(implkey.clone()) {
                return Err(VolaError::error_here(
                    e,
                    spec_ctx.tree_access_span,
                    "While specializing this tree-access",
                ));
            }
        }

        //Try to get such an impl block, if successful, test that the arguments match, the types match etc.
        let created_spec_node = if let Some(concept_impl) = self.concept_impl.get(&implkey) {
            //If we found a concept impl for that tree, import it into the _host_region_
            // and hook it up to the eval node's first input.
            let impl_subtree_count = concept_impl.subtrees.len();
            let impl_span = concept_impl.def_span.clone();

            //We always build the specialized λ in the region, in which the CSG-node resides.
            //This might be the region of the calling eval-node, or a parent region
            let csg_impl_lambda = self
                .deep_copy_lmd_into_region(concept_impl.lambda, csg_region)
                .map_err(|e| {
                    VolaError::error_here(e, impl_span.clone(), "could not inline this impl-block")
                        .with_label(spec_ctx.tree_access_span.clone(), "for this tree-access")
                })?;

            //NOTE: Reconnect the context variables that are

            //tag with debug info
            self.names.set(
                csg_impl_lambda.into(),
                format!(
                    "impl {} for {} specialized for {}",
                    concept_name, csg_name, spec_ctx.eval_node
                ),
            );

            //Make sure that the actual subtree-count matches the sub-tree-count of the implementation.
            if impl_subtree_count != subtree_count {
                let errspan = self
                    .graph
                    .node(spec_ctx.eval_node)
                    .node_type
                    .unwrap_simple_ref()
                    .span
                    .clone();
                let err = OptError::DispatchAnyError {
                    concept: concept_name.clone(),
                    opname: csg_name.clone(),
                    errstring: "Concept implementation CSG-Operand-count does not match usage"
                        .to_owned(),
                };

                return Err(VolaError::error_here(
                    err,
                    errspan,
                    format!(
                        "Trying to use the implementation of\
                        {csg_name} for {concept_name}: Implementation is for\
                        {impl_subtree_count} sub-trees, but using it for {subtree_count} sub-trees"
                    ),
                )
                .with_label(impl_span.clone(), "Trying to use this implementation")
                .with_label(
                    spec_ctx.tree_access_span.clone(),
                    "Specialization for this eval.",
                ));
            }

            //now hook up the subtree(s) of the eval-connected csg node to the CVs
            //of the just specialized/imported impl block.
            //then do the same with the
            // arguments.
            let arg_count = self.graph.node(spec_ctx.csg_tree.node).inputs().len() - subtree_count;
            for subtree_idx in 0..subtree_count {
                //disconnect from csg-node and connect to the just created λ
                let edg = self
                    .graph
                    .node(spec_ctx.csg_tree.node)
                    .inport(&InputType::Input(subtree_idx))
                    .unwrap()
                    .edge
                    .expect("Expected subtree to be connected!")
                    .clone();
                let subtree_src = self.graph.edge(edg).src().clone();
                let opt_edge = self.graph.edge(edg).ty.clone();
                assert!(opt_edge.get_type() == Some(&Ty::CSG));

                self.graph
                    .connect(
                        subtree_src,
                        InportLocation {
                            node: csg_impl_lambda,
                            input: InputType::ContextVariableInput(subtree_idx),
                        },
                        opt_edge,
                    )
                    .map_err(|e| {
                        VolaError::error_here(
                            OptError::InternalGraphError(e),
                            impl_span.clone(),
                            "while working on this",
                        )
                    })?;
            }

            //For the arguments we have to respect the call-convention
            //which is, that the impl-block first pulls in the csg-nodes's args,
            //and then the concept's args.
            // This needs us to _insert_ the csg-node connected none-csg args _before_ the currently
            // connected args of the eval node.
            //
            // Since we are replacing the eval with an Apply node anyways, this is pretty
            // simple. We just dissassamble the Eval node,
            // insert the OutportLocations of the csg node's args and then build the apply node

            let eval_region = self.graph.node(spec_ctx.eval_node).parent.unwrap();
            let eval_dsts = self
                .graph
                .node(spec_ctx.eval_node)
                .output_dsts(&self.graph, 0)
                .clone()
                .unwrap();
            let eval_ty = self
                .find_type(&spec_ctx.eval_node.output(0).into())
                .expect("expected eval to be typed!");
            let call_src = if eval_region != csg_region {
                self.import_context(
                    OutportLocation {
                        node: csg_impl_lambda,
                        output: OutputType::LambdaDeclaration,
                    },
                    eval_region,
                )
                .unwrap()
            } else {
                //If the eval is in the host region, we can use the call-source as is
                OutportLocation {
                    node: csg_impl_lambda,
                    output: OutputType::LambdaDeclaration,
                }
            };
            let mut args = SmallColl::new();
            for csg_arg_idx in 0..arg_count {
                if let Some(connected_edge) = self
                    .graph
                    .node(spec_ctx.csg_tree.node)
                    .inport(&InputType::Input(subtree_count + csg_arg_idx))
                    .unwrap()
                    .edge
                {
                    let src = self.graph.edge(connected_edge).src().clone();
                    let ty = self.graph.edge(connected_edge).ty.clone();
                    //NOTE: We possibly need to import the `src` for the csg-args,
                    //      since they might be defined in the _outer_ region of this λ
                    let src = if self.graph.node(spec_ctx.csg_tree.node).parent
                        != self.graph.node(spec_ctx.eval_node).parent
                    {
                        self.import_context(
                            src,
                            self.graph.node(spec_ctx.eval_node).parent.unwrap(),
                        )
                        .unwrap()
                    } else {
                        src
                    };

                    args.push((src, ty));
                } else {
                    panic!("Unconnected argument!")
                }
            }

            //now append the eval args as well
            for input in self.graph.node(spec_ctx.eval_node).inputs().iter().skip(1) {
                if let Some(connected_edge) = input.edge {
                    let src = self.graph.edge(connected_edge).src().clone();
                    let ty = self.graph.edge(connected_edge).ty.clone();

                    args.push((src, ty));
                } else {
                    panic!("Unconnected argument!")
                }
            }

            //finally delete the eval node, and connect the applynode instead

            self.graph.remove_node(spec_ctx.eval_node).map_err(|e| {
                VolaError::error_here(
                    e.into(),
                    spec_ctx.tree_access_span.clone(),
                    "could not delete this",
                )
            })?;
            self.graph.on_region(&eval_region, |r| {
                let (args, argty): (SmallColl<_>, SmallColl<_>) = args.into_iter().fold(
                    (SmallColl::new(), SmallColl::new()),
                    |(mut argcoll, mut tycoll), (a, t)| {
                        argcoll.push(a);
                        tycoll.push(t);
                        (argcoll, tycoll)
                    },
                );
                let (call, edg) = r.call(call_src, &args).unwrap();
                assert!(edg.len() == argty.len() + 1);
                for (edg, ty) in edg.into_iter().skip(1).zip(argty.into_iter()) {
                    r.ctx_mut().edge_mut(edg).ty = ty;
                }
                //now connect call to the eval's result
                for dst in eval_dsts {
                    r.ctx_mut()
                        .connect(
                            call.output(0),
                            dst,
                            OptEdge::value_edge().with_type(eval_ty.clone()),
                        )
                        .unwrap();
                }
            });

            csg_impl_lambda
        } else {
            let span = self
                .graph
                .node(spec_ctx.eval_node)
                .node_type
                .unwrap_simple_ref()
                .span
                .clone();
            let csgspan = self
                .graph
                .node(spec_ctx.csg_tree.node)
                .node_type
                .unwrap_simple_ref()
                .span
                .clone();
            let err = OptError::Any {
                text: format!(
                    "Could not find implementation of \"{concept_name}\" for \"{csg_name}\""
                ),
            };
            return Err(
                VolaError::error_here(err, span, "While trying to specialize this eval")
                    .with_label(csgspan, "For this CSG-Node")
                    .with_label(spec_ctx.tree_access_span.clone(), "For this tree-access"),
            );
        };

        //At this point we successfuly specialized the eval node for this context.
        //The only thing thats left is recursing the
        // tree, by exploring all evals in the
        // just created region

        let new_lambda_region = RegionLocation {
            node: created_spec_node,
            region_index: 0,
        };
        for eval in self.find_all_evals(new_lambda_region) {
            for spectx in self.eval_node_to_spec_ctx(spec_ctx.tree_access_span.clone(), eval)? {
                self.specialize_eval_node(spectx)?;
            }
        }

        //once we are done, check if we need to route the result out of our region.

        //for good measures, remove unused CVs on the λ we just build
        self.graph
            .remove_unused_context_variables(created_spec_node);

        Ok(())
    }

    fn find_all_evals(&self, host_region: RegionLocation) -> SmallColl<NodeRef> {
        let mut evals = SmallColl::new();
        for node in &self.graph.region(&host_region).unwrap().nodes {
            if self.is_node_type::<EvalNode>(*node) {
                evals.push(*node);
            } else {
                //If not eval, check if there are sub_regions. if so, try to find evals in there as well
                let subregcount = self.graph.node(*node).regions().len();
                for regidx in 0..subregcount {
                    let mut inner_evals = self.find_all_evals(RegionLocation {
                        node: *node,
                        region_index: regidx,
                    });
                    evals.append(&mut inner_evals);
                }
            }
        }
        evals
    }

    fn deep_copy_lmd_into_region(
        &mut self,
        lmd: NodeRef,
        region: RegionLocation,
    ) -> Result<NodeRef, OptError> {
        //NOTE: This is similar to an inlining, but easier. We just copy over the lmd and fix
        //      up the context variables
        let in_host_region_impl = self.graph.deep_copy_node(lmd, region);
        self.copy_node_attributes(lmd, in_host_region_impl);
        let cvcount = self
            .graph
            .node(in_host_region_impl)
            .node_type
            .unwrap_lambda_ref()
            .context_variable_count();
        for cvidx in 0..cvcount {
            let old_dst = InportLocation {
                node: lmd,
                input: InputType::ContextVariableInput(cvidx),
            };
            if let Some(producer) = self.graph.find_producer_inp(old_dst) {
                let in_context_port = self.import_context(producer, region)?;
                //Set type for import path
                let ty = if let Some(ty) = self.find_type(&in_context_port.clone().into()) {
                    ty
                } else {
                    log::warn!("Could not find type for imported prototype, using Callable");
                    Ty::Callable
                };
                //now hook up to the
                let _edg = self.graph.connect(
                    in_context_port,
                    InportLocation {
                        node: in_host_region_impl,
                        input: InputType::ContextVariableInput(cvidx),
                    },
                    OptEdge::value_edge().with_type(ty),
                )?;
            } else {
                //For sanity, make sure that it shouldn't be connected indeed
                if self
                    .graph
                    .node(lmd)
                    .inport(&InputType::ContextVariableInput(cvidx))
                    .map(|p| p.edge.is_some())
                    .unwrap_or(false)
                {
                    //Throw an error if the cv was conncted
                    return Err(OptError::Internal(format!(
                        "Could not inline λ-template while specializing {lmd}"
                    )));
                }
            }
        }

        Ok(in_host_region_impl)
    }

    ///Takes care of preparing a eval-node, in order to be _specializeable_. As
    ///specially when handling control-flow, a single eval-node might become multiple.
    fn eval_node_to_spec_ctx(
        &mut self,
        tree_access_span: Span,
        eval: NodeRef,
    ) -> Result<SmallColl<SpecCtx>, VolaError<OptError>> {
        assert!(self
            .graph
            .node(eval)
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<EvalNode>()
            .is_some());
        let src_span = self
            .graph
            .node(eval)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();

        //Find all specialization contexts.
        //if `eval` is connceted to `just` a CSG-node, this is easy, since there is only
        //the context to that node.
        //
        //However, if it is connected to a control-flow node, or an apply-node, then we have to _step-into_ the referenced
        //region (i.e. both branches, loop-body, or function-body), and specialize in there.
        //once that specialization is done, the value has to be exported _correctly_
        //
        //# Methode for Gamma
        //1. Check if the eval-node is connected to CF node.
        //2. If so, find producers of the value in the CF-Node
        //3. Create eval node there, import all needed context to _make it work_
        //4. Remove original eval node, instead, route the final value out of the CF-Node, and connect it to the original _result_ port of
        //   the just deleted eval node.
        //
        //# Methode for Loop
        //
        //If we cross a theta boundary for a eval-node, we just _unroll_ the loop, and possibly emit a warning if the loop is _big_.
        //The reason is, that we'd have to come up with a way to assemble the unrolled CSG anyways. The cool thing about _just_ unrolling the loop is,
        //that the we can also eval partial _in-iteration_ trees _while_ assembling a new one (see PR !55).

        if let Some(edg) = self.graph.node(eval).inputs()[0].edge {
            let edge = self.graph.edge(edg);
            match edge.ty.get_type() {
                Some(&Ty::CSG) => {
                    //is some kind of csg node, call the canonicalized
                    //and then wrapp all canonicalizede evals into a context
                    let mut collected = SmallColl::new();
                    for (canon_eval, csg_src) in self
                        .spec_canonicalize_eval_node(eval)
                        .map_err(|e| e.with_label(src_span, "on this eval"))?
                    {
                        collected.push(SpecCtx {
                            tree_access_span: tree_access_span.clone(),
                            csg_tree: csg_src,
                            eval_node: canon_eval,
                        })
                    }

                    Ok(collected)
                }
                Some(other) => {
                    let err = OptError::CsgStructureIssue(format!(
                        "Eval wrongly typed first argument. Expected CSGTree, was {other:?}!"
                    ));
                    Err(VolaError::error_here(err, src_span, "here"))
                }
                None => {
                    let err =
                        OptError::CsgStructureIssue("Eval untyped first argument!".to_owned());
                    Err(VolaError::error_here(err, src_span, "here"))
                }
            }
        } else {
            let err =
                OptError::CsgStructureIssue("Eval node had no csg tree connected!".to_owned());
            Err(VolaError::error_here(err, src_span, "here"))
        }
    }

    ///Recursion wrapper that turns the `eval` node in the canonical-form that can be handeled by the specializer.
    ///Returns all (eval-node, csg-producer) pairs that need to be specialized for this _canonicalized_ node.
    fn spec_canonicalize_eval_node(
        &mut self,
        eval: NodeRef,
    ) -> Result<SmallColl<(NodeRef, OutportLocation)>, VolaError<OptError>> {
        //find the CSG-Producer of the eval node

        let mut prod = self
            .graph
            .find_producer_out(self.graph[eval].input_src(&self.graph, 0).unwrap())
            .expect("Expected a producer for the eval's CSG-Tree");

        //if the producer is a apply-node, inline the apply node, since it _must_ (according to typing)
        //be some kind of CSG-Tree
        //
        //NOTE: we'd have to build a specialized λ-regardless, so we can also just inline it at this point.
        if self.graph[prod.node].node_type.is_apply() {
            self.graph.inline_apply_node(prod.node).unwrap();
            //update the producer afterwards
            prod = self
                .graph
                .find_producer_out(self.graph[eval].input_src(&self.graph, 0).unwrap())
                .unwrap();
        }

        //try to find out which type of node is connected to the eval:

        //Is just simply a CsgOp, can return a single csg-value
        //NOTE: this is also the _standard_ case. Most _normal_ trees will just
        //      return here. The whole rest if for the _functions or CF in CSG-Trees_ case.
        if self.is_node_type::<CsgOp>(prod.node) {
            return Ok(smallvec![(eval, prod)]);
        }

        //is the gamma case, use our preparation helper to yield all _new_ eval nodes, and return a context for each.
        if self.graph[prod.node].node_type.is_gamma() {
            if std::env::var("VOLA_DUMP_ALL").is_ok()
                || std::env::var("DUMP_BEFORE_GAMMA_PREP").is_ok()
            {
                self.push_debug_state(&format!("before gamma-preparation {}", eval));
            }
            return self.prepare_gamma_eval(eval, prod);
        }

        //Is a theta node. Try to find the loop-bound, and iff successful, unroll
        if self.graph[prod.node].node_type.is_theta() {
            //NOTE: loops should always be annotated.
            let loop_span = self.find_span(prod.node.into());
            let count = self.loop_count(prod.node).map_err(|e| {
                let err = VolaError::new(e.into());
                if let Some(span) = loop_span.clone() {
                    err.with_label(
                        span,
                        "Tried unrolling loop in order to\
                                use as CSG value, but loop is not statically bound"
                            .to_owned(),
                    )
                } else {
                    err
                }
            })?;

            if count > 2 {
                let mut warner = warning_reporter(
                    format!(
                        "Unrolling loop {count}-times for CSG Node. \
                                Consider using implicit repetition instead. See https://mercury.sexy/hg_sdf/ 'domain operators'"
                    ),
                    loop_span.clone().unwrap_or(Span::empty()),
                );
                if let Some(ls) = &loop_span {
                    warner = warner.with_label(Label::new(ls.clone()).with_message("For this loop"))
                }
                report(warner.finish());
            }

            self.graph
                .unroll_replace_theta(prod.node, count)
                .map_err(|e| {
                    let err = VolaError::new(e.into());
                    if let Some(ls) = loop_span {
                        err.with_label(ls, "Failed to unroll this loop for the CSG value!")
                    } else {
                        err
                    }
                })?;

            //now retrace the CSG-producer and return
            prod = self
                .graph
                .find_producer_out(self.graph[eval].input_src(&self.graph, 0).unwrap())
                .unwrap();

            return Ok(smallvec![(eval, prod)]);
        }

        //Was not a CsgNode nor a CF-Node, so is some kind of _wrong_ tree.
        let err = OptError::CsgStructureIssue("Non-CSG value used in CSG tree!".to_owned());
        let error = if let Some(span) = self.find_span(prod.node.into()) {
            VolaError::error_here(err, span.clone(), "this should be a CSG value")
        } else {
            VolaError::new(err)
        };

        Err(error)
    }

    ///Splits and moves the `eval` node into the connected gamma-node. Then return the new eval
    ///node, and its connected CSG-Node.
    fn prepare_gamma_eval(
        &mut self,
        eval: NodeRef,
        gamma_prod: OutportLocation,
    ) -> Result<SmallColl<(NodeRef, OutportLocation)>, VolaError<OptError>> {
        assert!(self.graph[gamma_prod.node].node_type.is_gamma());

        //we have to take care of two _cases_: Moving _into_ a gamma-node
        //and moving _out_ of a gamma-node.
        let exit_variable = match gamma_prod.output {
            OutputType::ExitVariableOutput(exv) => {
                //this is the hard case, where we have to pull stuff _into_ the branch. We'll continue below.
                exv
            }
            OutputType::EntryVariableArgument {
                branch,
                entry_variable: _,
            } => {
                //just walking outside the branch. This is, structurely, the same case
                //as using a context-value-csg from _within_ a λ. So we can just return the tuple of the acutal producer and the
                //eval node.
                //
                //TODO: There is an edge case, if the _actual-producer_ is a gamma/theta node as well. We currently just test for that. However
                //      at some point we should probably handle that. Maybe by inserting a dummy-node or something...

                let actual_producer = {
                    let entry_ty = gamma_prod.output.map_out_of_region().unwrap();
                    let prod = self
                        .graph
                        .inport_src(entry_ty.to_location(gamma_prod.node))
                        .unwrap();

                    if !self.is_node_type::<CsgOp>(prod.node) {
                        let e = OptError::CsgStructureIssue(format!("Could not specialize branch [{branch}]: The branch uses a CSG-value, that itself comes from a branch. Which is an edge-case we currently don't support. Please file an issue!"));
                        let err = if let Some(span) = self.find_span(prod.node.into()) {
                            VolaError::error_here(e, span, "The CSG-value comes from here. Consider moving that out of a branch for now!")
                        } else {
                            VolaError::new(e)
                        };
                        return Err(err);
                    }
                    prod
                };
                return Ok(smallvec![(eval, actual_producer)]);
            }
            other => {
                return Err(VolaError::new(OptError::CsgStructureIssue(format!(
                    "Encountered none-gamma port while specializing gamma-node: {:?}!",
                    other
                ))));
            }
        };

        //find the actual producers in each branch, build a new eval node that uses that, then build a new gamma-result
        //from which we route the specialized value _out_.
        //
        //NOTE: we don't _replace_ the CSG-result, since another eval might use the branch's produced CSG-value to specialize _something else_.

        //Create the exit-var we'll connect all branches to
        let specialized_exv = self.graph[gamma_prod.node]
            .node_type
            .unwrap_gamma_mut()
            .add_exit_var();
        //collect all context-srcs of the eval node. By definition the first is the csg-src we are currently working on
        //the rest must be imported whenever we build the substitution eval
        let mut needed_context_ports = self.graph[eval].input_srcs(&self.graph);
        //NOTE: this is only the context-variable, at which the csg-var is in, we handel this _specifically_
        //      in the first part of the per-branch loop.
        let _csgsrc = needed_context_ports.remove(0);

        let evalspan = self.graph[eval].node_type.unwrap_simple_ref().span.clone();
        let eval_node_template = self
            .try_unwrap_node::<EvalNode>(eval)
            .unwrap()
            .structural_copy(evalspan.clone());
        let eval_parent = self.graph[eval].parent.unwrap();

        let mut collected = SmallColl::new();
        //Tracks the
        let mut most_outer_region = eval_parent.clone();
        for branch in 0..self.graph[gamma_prod.node].regions().len() {
            //find the actual source of the exit_variables's result
            let src = self
                .graph
                .inport_src(
                    InputType::ExitVariableResult {
                        branch,
                        exit_variable,
                    }
                    .to_location(gamma_prod.node),
                )
                .unwrap();

            let branch_region = RegionLocation {
                node: gamma_prod.node,
                region_index: branch,
            };

            // add the src (csg-typed, but not necessarly csg-node) and import the context (import dedups :) )
            // needed to create the _same_ semantics
            let mut local_context_ports = SmallColl::new();
            //first, push the _by-definition_ csg-arg;
            local_context_ports.push(src);
            //now append all context values needed, by importing them
            for ctx_var in needed_context_ports.iter() {
                let in_region_port = if let Some(src) = ctx_var {
                    match src.output {
                        OutputType::ContextVariableArgument(_cv) => {
                            //CVs can just be traced and imported. Find the actual producer...
                            let producer = self.graph.find_producer_out(*src).unwrap();
                            //... and import it into our branch
                            let (import_cv, _) = self
                                .graph
                                .import_argument(producer, branch_region)
                                .map_err(|e| {
                                    VolaError::error_here(
                                        OptError::InternalGraphError(e),
                                        evalspan.clone(),
                                        "while trying to specialize this eval for branch",
                                    )
                                })?;
                            import_cv
                        }
                        OutputType::Argument(argidx) => {
                            //for arguments its a little harder, we have to find the argument
                            //that is being supplied at the call-site.
                            //We do this by trying to find the (already) specialized callsite, and then try to find the producer of the argument.
                            //if that succeeds, we _should_ be able to import it as context into the gamma-node, where we'll evaluate the CSG.
                            if let Some(parent_lmd) = self.graph.find_parent_lambda_or_phi(eval) {
                                if let Some(mut callsites) = self.graph.find_caller(parent_lmd) {
                                    assert_eq!(
                                        callsites.len(),
                                        1,
                                        "specialized callsites should only be called once!"
                                    );

                                    let callsite = callsites.remove(0);
                                    assert!(self.graph[callsite].node_type.is_apply());

                                    let argument_edge = self.graph[callsite]
                                        .node_type
                                        .unwrap_apply_ref()
                                        .argument_input(argidx)
                                        .unwrap()
                                        .edge
                                        .unwrap();
                                    let argument_src = self
                                        .graph
                                        .find_producer_out(*self.graph[argument_edge].src())
                                        .unwrap();
                                    //update _most_outer_region_
                                    if let Some(parent) = self.graph[argument_src.node].parent {
                                        //if is not in parent, it is _more_ outside, so update
                                        if !self
                                            .graph
                                            .is_in_parent(parent.node, most_outer_region.node)
                                        {
                                            most_outer_region = parent;
                                        }
                                    } else {
                                        //fallback to omega region.
                                        most_outer_region = self.graph.toplevel_region();
                                    }

                                    let (import_cv, _) = self
                                        .graph
                                        .import_context(argument_src, branch_region)
                                        .map_err(|e| {
                                            VolaError::error_here(
                                                OptError::InternalGraphError(e),
                                                evalspan.clone(),
                                                "while trying to specialize this eval for branch",
                                            )
                                        })?;

                                    import_cv
                                } else {
                                    return Err(VolaError::error_here(
                                        OptError::Internal("Could not find callsites".to_owned()),
                                        evalspan.clone(),
                                        "for this eval",
                                    ));
                                }
                            } else {
                                return Err(VolaError::error_here(
                                    OptError::Internal(
                                        "Could not find specialized function".to_owned(),
                                    ),
                                    evalspan.clone(),
                                    "for this eval",
                                ));
                            }
                        }
                        _other => {
                            return Err(VolaError::error_here(
                                OptError::CsgStructureIssue(
                                    "could not resolve context for evaluation".to_owned(),
                                ),
                                evalspan.clone(),
                                "for this eval",
                            ));
                        }
                    }
                } else {
                    return Err(VolaError::error_here(
                        OptError::CsgStructureIssue(
                            "Eval node's argument was unconnected".to_owned(),
                        ),
                        evalspan.clone(),
                        "for this eval",
                    ));
                };

                local_context_ports.push(in_region_port);
            }

            //create a eval-node, and connect all (imported) args and the CSG node
            let created_eval = self
                .graph
                .on_region(&branch_region, |reg| {
                    let (node, _edges) = reg
                        .connect_node(
                            eval_node_template.structural_copy().into(),
                            local_context_ports,
                        )
                        .unwrap();
                    //hookup the new _produced_ value to the prepared exit variable
                    reg.ctx_mut()
                        .connect(
                            node.output(0),
                            gamma_prod
                                .node
                                .as_inport_location(InputType::ExitVariableResult {
                                    branch,
                                    exit_variable: specialized_exv,
                                }),
                            OptEdge::value_edge_unset(),
                        )
                        .unwrap();
                    node
                })
                .unwrap();

            //now again, two cases:
            //1. Is _just_ a CsgOp. In that case, we can just return the created (in-branch) eval-node and csg pair.
            //2. Is some kind of CF-Node. This is where we recurse. We insert a dummy eval
            //   We just _pretend_ that the there is a eval-node, and call the spec-ctx-builder
            if self.is_node_type::<CsgOp>(src.node) {
                //finally push the new eval-producer combination
                collected.push((created_eval, src));
            } else {
                //must be some kind of argument. There are now two ways (yet again :D)
                //1. This is a argument, in that case we'd continue by placing a eval here
                //2. This is a result, in that case we have to do the _move eval into region_ trick from before.
                //
                // So what we actually did is place a eval-node here (pretending that works), connect the CSG source, from which we don't really know whats going on,
                // and recurse the canonicalization. This will handle both cases. Either it'll just return the eval-node and the actual _outside_ CSG (case 1)
                // or it handels moving the eval even further into the CF, and replacing everything that comes with it.
                let mut new_pairs = self.spec_canonicalize_eval_node(created_eval)?;
                //now append all new pairs we found in the recursion
                collected.append(&mut new_pairs);
            }
        }

        //before returning, route the values out of their gamma-node and replace the original
        //eval consumer's source with the routed value.
        let producer_port = gamma_prod
            .node
            .as_outport_location(OutputType::ExitVariableOutput(specialized_exv));
        for consumer in self.graph[eval].output_dsts(&self.graph, 0).unwrap() {
            let edg = self.graph[consumer].edge.unwrap();
            let edge = self.graph.disconnect(edg).unwrap();
            let imported_producer = self.import_context(producer_port, eval_parent).unwrap();
            self.graph
                .connect(imported_producer, consumer, edge)
                .unwrap();
        }
        //now delete the old eval-node, so it won't be re-discovered
        let _ = self.graph.remove_node(eval).unwrap();
        //and re-type the parent region
        let type_region_span = self
            .find_span(most_outer_region.into())
            .unwrap_or(Span::empty());
        self.derive_region(most_outer_region, type_region_span)?;

        Ok(collected)
    }
}
