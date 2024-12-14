/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::{NodeType, StructuralNode},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::EvalNode, common::Ty, csg::CsgOp, graph::auxiliary::ImplKey, OptEdge, OptError, Optimizer,
};

struct SpecCtx {
    tree_access_span: Span,
    host_region: RegionLocation,
    csg_tree: OutportLocation,
    eval_node: NodeRef,
}

impl Optimizer {
    /// Shortcut to call [Self::specialize_export] for all exported λs
    pub fn specialize_all_exports(&mut self) -> Result<(), OptError> {
        let mut errors = SmallColl::new();
        //specialize all functions.
        //NOTE: we don't specialize implementations, since those are
        //      _pulled-in_ in this process, so whenever a impl-block with eval-nodes is
        //      inlined, the "restart" step will discover that, and ... restart the process.
        for (name, fnregion) in self
            .functions
            .values()
            .map(|f| (f.name.clone(), f.region()))
            .collect::<Vec<_>>()
        {
            if std::env::var("VOLA_DUMP_ALL").is_ok()
                || std::env::var("DUMP_BEFORE_SPECIALIZE").is_ok()
            {
                self.push_debug_state(&format!("before specialize {name}"));
            }
            //NOTE: defer breaking to _after_ all exports are specialized (or not^^).
            if let Err(e) = self.specialize_region(fnregion) {
                errors.push(e);
            }

            if std::env::var("VOLA_DUMP_ALL").is_ok()
                || std::env::var("DUMP_POST_SPECIALIZE").is_ok()
            {
                self.push_debug_state(&format!("post specialize {name}"));
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_POST_SPECIALIZE").is_ok() {
            self.push_debug_state(&format!("post specialize"));
        }
        if errors.len() > 0 {
            Err(errors.remove(0))
        } else {
            Ok(())
        }
    }

    ///The _new_ specializer pass. Transforms a tree of CSG nodes into a
    /// call tree of specialized _alge_-dialect based λ-nodes.
    ///
    /// Contrary to the _old_ specializer this one doesn't have to inline all calls.
    ///
    /// Instead a inliner _can_ be used if needed. It also doesn't import values via context variables, but
    /// uses _normal_ arguments to the specific λ-nodes / apply-nodes.
    pub fn specialize_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
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
        self.inline_all_region(region)?;

        //Try specializing reverse-topo-ord eval nodes, till we don't have any left
        //TODO: handle sub-regions first? aka. control-flow csg nodes?
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
                    self.specialize_eval_entry(region, node)?;
                    continue 'restart;
                }
            }

            //if we came till here, there is no eval-node left in the graph, so we finished specializing
            break;
        }

        Ok(())
    }

    ///Specializes the whole tree-access tree.
    pub fn specialize_eval_entry(
        &mut self,
        host_region: RegionLocation,
        eval: NodeRef,
    ) -> Result<(), OptError> {
        //build the frist eval_spec_ctx
        //for the first connected subtree, then substitude the tree-access node with a eval node
        let src_span = self
            .graph
            .node(eval)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();

        let spec = self.eval_node_to_spec_ctx(src_span, host_region, eval)?;
        //now start the actual replacement descent
        self.specialize_eval_node(spec)
    }

    ///Specializes the `eval` node based
    fn specialize_eval_node(&mut self, spec_ctx: SpecCtx) -> Result<(), OptError> {
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_SPECIALIZE").is_ok()
        {
            self.push_debug_state(&format!("before specialize {}", spec_ctx.eval_node));
        }
        //The first step is to find the correct specialization impl block based on the called
        // csg-tree root node, and the called concept of the root node.
        let concept_name = self
            .graph
            .node(spec_ctx.eval_node)
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<EvalNode>()
            .unwrap()
            .called_concept
            .clone();

        let csg_name = self
            .graph
            .node(spec_ctx.csg_tree.node)
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

        let subtree_count = self
            .graph
            .node(spec_ctx.csg_tree.node)
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<CsgOp>()
            .unwrap()
            .subtree_count;

        //NOTE: we prepare the identity implementation at this point, if the subtree-count is 0 and there
        //      is no such implblock yet.
        if !self.concept_impl.contains_key(&implkey) && subtree_count == 1 {
            if let Err(e) = self.implement_identity_for_concept(implkey.clone()) {
                report(
                    error_reporter(e.clone(), spec_ctx.tree_access_span.clone())
                        .with_label(
                            Label::new(spec_ctx.tree_access_span)
                                .with_message("While specializing this tree-access"),
                        )
                        .finish(),
                );

                return Err(e);
            }
        }

        //Try to get such an impl block, if successful, test that the arguments match, the types match etc.
        let created_spec_node = if let Some(concept_impl) = self.concept_impl.get(&implkey) {
            //If we found a concept impl for that tree, import it into the _host_region_
            // and hook it up to the eval node's first input.

            let in_host_region_impl =
                self.deep_copy_lmd_into_region(concept_impl.lambda, spec_ctx.host_region)?;

            //NOTE: Reconnect the context variables that are

            //tag with debug info
            self.names.set(
                in_host_region_impl.into(),
                format!(
                    "impl {} for {} specialized for {}",
                    concept_name, csg_name, spec_ctx.eval_node
                ),
            );

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
                            node: in_host_region_impl,
                            input: InputType::ContextVariableInput(subtree_idx),
                        },
                        opt_edge,
                    )
                    .unwrap();
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
            let call_src =
                if self.graph.node(spec_ctx.eval_node).parent != Some(spec_ctx.host_region) {
                    self.import_context(
                        OutportLocation {
                            node: in_host_region_impl,
                            output: OutputType::LambdaDeclaration,
                        },
                        eval_region,
                    )
                    .unwrap()
                } else {
                    //If the eval is in the host region, we can use the call-source as is
                    OutportLocation {
                        node: in_host_region_impl,
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

            self.graph.remove_node(spec_ctx.eval_node)?;
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

            in_host_region_impl
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
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(span)
                            .with_message("While trying to specialize this eval")
                            .with_color(vola_common::ariadne::Color::Green),
                    )
                    .with_label(
                        Label::new(csgspan)
                            .with_message("For this CSG-Node")
                            .with_color(vola_common::ariadne::Color::Green),
                    )
                    .with_label(
                        Label::new(spec_ctx.tree_access_span.clone())
                            .with_message("For this tree-access")
                            .with_color(vola_common::ariadne::Color::Green),
                    )
                    .finish(),
            );

            return Err(err);
        };

        //At this point we successfuly specialized the eval node for this context.
        //The only thing thats left is recursing the
        // tree, by exploring all evals in the
        // just created region

        for eval in self.find_all_evals(RegionLocation {
            node: created_spec_node,
            region_index: 0,
        }) {
            let new_spec_ctx = self.eval_node_to_spec_ctx(
                spec_ctx.tree_access_span.clone(),
                spec_ctx.host_region,
                eval,
            )?;
            self.specialize_eval_node(new_spec_ctx)?;
        }

        //for good measures, remove unused CVs on the λ we just build
        self.graph
            .remove_unused_context_variables(created_spec_node);

        Ok(())
    }

    fn find_all_evals(&self, host_region: RegionLocation) -> SmallColl<NodeRef> {
        let mut evals = SmallColl::new();
        for node in &self.graph.region(&host_region).unwrap().nodes {
            let is_eval = if let NodeType::Simple(s) = &self.graph.node(*node).node_type {
                s.try_downcast_ref::<EvalNode>().is_some()
            } else {
                false
            };

            if is_eval {
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

    fn eval_node_to_spec_ctx(
        &self,
        tree_access_span: Span,
        host_region: RegionLocation,
        eval: NodeRef,
    ) -> Result<SpecCtx, OptError> {
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

        let csg_tree = if let Some(edg) = self.graph.node(eval).inputs()[0].edge {
            let edge = self.graph.edge(edg);
            match edge.ty.get_type() {
                Some(&Ty::CSG) => {
                    let prod = self
                        .graph
                        .find_producer_out(edge.src().clone())
                        .expect("Expected a producer for the eval's CSG-Tree");

                    if !self.is_node_type::<CsgOp>(prod.node) {
                        let err =
                            OptError::CsgStructureIssue(format!("Non-CSG value used in CSG tree!"));
                        if let Some(span) = self.find_span(prod.node.into()) {
                            report(
                                error_reporter(err.clone(), src_span.clone())
                                    .with_label(
                                        Label::new(span).with_message("this should be a CSG value"),
                                    )
                                    .finish(),
                            );
                        } else {
                            report(
                                error_reporter(err.clone(), src_span.clone())
                                    .with_label(
                                        Label::new(tree_access_span).with_message("for this eval"),
                                    )
                                    .finish(),
                            );
                        }
                        return Err(err);
                    }
                    prod
                }
                Some(other) => {
                    let err = OptError::Internal(format!(
                        "Eval wrongly typed first argument. Expected CSGTree, was {other:?}!"
                    ));
                    report(
                        error_reporter(err.clone(), src_span.clone())
                            .with_label(Label::new(src_span.clone()).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
                }
                None => {
                    let err = OptError::Internal("Eval untyped first argument!".to_owned());
                    report(
                        error_reporter(err.clone(), src_span.clone())
                            .with_label(Label::new(src_span.clone()).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
                }
            }
        } else {
            let err = OptError::Internal("Eval node had no csg tree connected!".to_owned());
            report(
                error_reporter(err.clone(), src_span.clone())
                    .with_label(Label::new(src_span.clone()).with_message("here"))
                    .finish(),
            );
            return Err(err);
        };
        Ok(SpecCtx {
            tree_access_span,
            host_region,
            csg_tree,
            eval_node: eval,
        })
    }
}
