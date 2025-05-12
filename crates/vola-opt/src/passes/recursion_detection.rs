/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::{
    edge::{InputType, OutportLocation, OutputType},
    nodes::StructuralNode,
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};
use vola_common::VolaError;

use crate::{
    alge::EvalNode,
    csg::CsgOp,
    graph::auxiliary::{Impl, ImplKey},
    OptError, Optimizer,
};

impl Optimizer {
    ///Simple pass that finds recursive calls and reports them as errors.
    pub fn detect_recursive_calls(&self) -> Result<(), Vec<VolaError<OptError>>> {
        let mut errors = Vec::with_capacity(0);
        //Iterate all _simple_ function, and try to find self-calling.
        for node in self.functions.values().map(|v| v.lambda) {
            if self.graph[node].node_type.is_lambda() {
                if let Err(e) = self.detect_self_call(node) {
                    if let Some(span) = self.find_span(node.into()) {
                        errors.push(VolaError::error_here(e, span, "in this function"))
                    } else {
                        errors.push(VolaError::new(e));
                    }
                }
            }
        }

        //early return if we already found something
        if errors.len() > 0 {
            return Err(errors);
        }

        //if not, iterate all impl-blocks and search for self-references.
        //those can happen for _hyper-trees_, so if a impl-block creates a internal tree, that by extension
        //calls the impl-block itself.
        for (ident, impl_block) in self.concept_impl.iter() {
            match self.detect_impl_cycle(ident, impl_block) {
                Ok(_) => {}
                Err(e) => {
                    //Eh, maybe use small-vec for all? But that grows the stack
                    //    for each function call :(
                    for e in e {
                        errors.push(e);
                    }
                }
            }
        }

        if errors.len() > 0 {
            return Err(errors);
        } else {
            Ok(())
        }
    }

    //we perform this _simply_ testing each node's CV-dependencies (and transitions, if they contain the node)
    //itself.
    //
    //luckily this is simple, we just need to find all λ-declerations that are connected to a node, and their
    //connected CV-λs etc.
    fn detect_self_call(&self, lambda: NodeRef) -> Result<(), OptError> {
        let mut waiting = VecDeque::default();
        let mut seen = AHashSet::default();
        let get_cvlambdas = |node: NodeRef| {
            let cv_count = self.graph[node]
                .node_type
                .unwrap_lambda_ref()
                .context_variable_count();
            let mut cv_lambdas = SmallColl::default();
            for cv in 0..cv_count {
                let port = node.as_inport_location(InputType::ContextVariableInput(cv));
                if let Some(src) = self.graph.inport_src(port) {
                    if src.output == OutputType::LambdaDeclaration {
                        cv_lambdas.push(src);
                    }
                }
            }

            cv_lambdas
        };

        //init the waiting list with all λ-connected λs
        for initlmd in get_cvlambdas(lambda) {
            if initlmd.node == lambda {
                let name = self
                    .names
                    .get(&lambda.into())
                    .cloned()
                    .unwrap_or("unamed function".to_owned());
                return Err(OptError::Any {
                    text: format!("detected recursive call of \"{name}\""),
                });
            }
            waiting.push_back(initlmd);
        }
        //mark self as seen
        seen.insert(lambda);

        //now work on the waiting list, if we see our-self twice, we can bail
        while let Some(next) = waiting.pop_front() {
            //break loop if needed
            if seen.contains(&next.node) {
                continue;
            }
            //not yet seen, find all its connected λs and push them back
            for connected_lambda in get_cvlambdas(next.node) {
                //If a imported called λ imports `self`, this is recursion, which is not allowed
                if connected_lambda == lambda.as_outport_location(OutputType::LambdaDeclaration) {
                    let node_name = self
                        .names
                        .get(&next.node.into())
                        .cloned()
                        .unwrap_or("unknown-function".to_owned());
                    return Err(OptError::Any {
                        text: format!("detected recursive call of \"{node_name}\""),
                    });
                }

                waiting.push_back(connected_lambda);
            }

            seen.insert(next.node);
        }

        Ok(())
    }

    fn detect_impl_cycle(
        &self,
        key: &ImplKey,
        block: &Impl,
    ) -> Result<(), SmallColl<VolaError<OptError>>> {
        //NOTE: we do this in a depth-first-search style.
        //      That is not realy the the most effective way, but works for the moment.
        let mut errors = SmallColl::new();
        for node in self.graph[block.region()].nodes.iter() {
            if self.is_node_type::<EvalNode>(*node) {
                //seed the recursive search with the connected csg node
                let span = self.find_span(node.into()).unwrap();
                let initial_src = self
                    .graph
                    .inport_src(node.as_inport_location(InputType::Input(0)))
                    .unwrap();

                //if the initila src is already not a CSG-Node, bail already
                match self.recursive_search_impl_cycle(key, block, initial_src) {
                    Ok(_) => {}
                    Err(e) => {
                        errors.push(e.with_label(span, "for this initial eval"));
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn recursive_search_impl_cycle(
        &self,
        key: &ImplKey,
        block: &Impl,
        csg: OutportLocation,
    ) -> Result<(), VolaError<OptError>> {
        //In principle this is the standard _test self, then recurse for all siblings_
        //style DFS.
        //
        //However, we do search for a pattern loops. By treating CSG-Nodes as _function-calls_ (and function call too duh),
        //we find a cycle, if a csg-tree in `key` calls `key.operation`.
        //
        //This is somewhat imperfect, because in principle it should be possible to first call Op.Concept1 followed at some point
        //by Op.Concept2. Howevere, we don't know _yet_ which specialization is used. So we are a little over conservative
        //for the sake of correctness.
        //
        //Luckily we don't have to disptach CSG-operands, because Iff they are used in a CSG, then it is ensured that
        //the tree progresses, so we can safely ignore them. However, that should probably be proofed somewhere :eyes:.

        let span = self.find_span(csg.node.into());
        match self.graph[csg.node].into_abstract() {
            AbstractNodeType::Apply => {
                //go to the λ-producer, and recurse for the producing node.
                //We expect the node to only produce one value atm.
                assert_eq!(self.graph[csg.node].outputs().len(), 1);

                if let Some(prod) = self
                    .graph
                    .find_callabel_def(self.graph[csg.node].input_src(&self.graph, 0).unwrap())
                {
                    //found the producer, go inside and recurse, also tag our location for any error produced
                    let result_src = self
                        .graph
                        .inport_src(prod.node.as_inport_location(InputType::Result(0)))
                        .unwrap();
                    self.recursive_search_impl_cycle(key, block, result_src)
                        .map_err(|e| {
                            if let Some(span) = span {
                                e.with_label(span, "via this call")
                            } else {
                                e
                            }
                        })
                } else {
                    return Err(VolaError::error_here(
                        OptError::CsgStructureIssue(
                            "CSG-Function-call has no definition".to_owned(),
                        ),
                        span.expect("Expected span for call"),
                        "for this call",
                    ));
                }
            }
            AbstractNodeType::Simple => {
                //make sure this is
                //1. csg-node
                //2. Not-our-self
                let span = span.expect("expected span for simple node");
                if let Some(node) = self.try_unwrap_node::<CsgOp>(csg.node) {
                    if node.op == key.csgdef {
                        Err(VolaError::error_here(
                            OptError::CsgStructureIssue(
                                format!(
                                    "Detected CSG-Graph cycle: uses {} from within {}.",
                                    key.csgdef, key.csgdef
                                )
                                .to_owned(),
                            ),
                            span.clone(),
                            "here",
                        ))
                    } else {
                        //try to find the source of all siblings and recurse.
                        for subtree in 0..node.subtree_count {
                            //there must be a source, but it can be an unconnected CV, in that case we can abort.
                            if let Some(child_src) = self
                                .graph
                                .inport_src(csg.node.as_inport_location(InputType::Input(subtree)))
                            {
                                self.recursive_search_impl_cycle(key, block, child_src)?;
                            } else {
                                return Err(VolaError::error_here(
                                    OptError::CsgStructureIssue(
                                        format!("Child[{subtree}] is not defined").to_owned(),
                                    ),
                                    span,
                                    "here",
                                ));
                            }
                        }
                        //If all children checked out, stop
                        Ok(())
                    }
                } else {
                    Err(VolaError::error_here(
                        OptError::CsgStructureIssue("Found non-CSG-node in CSG-Graph".to_owned()),
                        span,
                        "here",
                    ))
                }
            }
            AbstractNodeType::Lambda => {
                //if we are on a unconnected CV, that is also the block's λ, just return
                if let OutputType::ContextVariableArgument(_) = csg.output {
                    if csg.node == block.lambda {
                        return Ok(());
                    }
                }
                //internal λ's shouldn happen
                let err = OptError::CsgStructureIssue("Unexpected function in CSG-Tree".to_owned());
                Err(if let Some(span) = span {
                    VolaError::error_here(err, span, "here")
                } else {
                    VolaError::error_here(
                        err,
                        block.def_span.clone(),
                        "within this impelementation",
                    )
                })
            }
            AbstractNodeType::Gamma => {
                //for gammas, branch _into_ all branches, if we are on a output, and _out_ if we are on a argument
                match csg.output {
                    OutputType::EntryVariableArgument {
                        branch,
                        entry_variable,
                    } => {
                        if let Some(src) = self.graph.inport_src(
                            csg.node
                                .as_inport_location(InputType::EntryVariableInput(entry_variable)),
                        ) {
                            self.recursive_search_impl_cycle(key, block, src)
                                .map_err(|e| {
                                    if let Some(span) = span {
                                        e.with_label(span, format!("coming from branch [{branch}]"))
                                    } else {
                                        e
                                    }
                                })
                        } else {
                            let err = OptError::Internal(
                                "Branch CSG-Tree-source was undefined".to_owned(),
                            );
                            Err(if let Some(span) = span {
                                VolaError::error_here(err, span, "here")
                            } else {
                                VolaError::error_here(
                                    err,
                                    block.def_span.clone(),
                                    "within this impelementation",
                                )
                            })
                        }
                    }
                    OutputType::ExitVariableOutput(_) => {
                        for region_index in 0..self.graph[csg.node].regions().len() {
                            let mapped_inside = csg.output.map_to_in_region(region_index).unwrap();
                            let src = self
                                .graph
                                .inport_src(csg.node.as_inport_location(mapped_inside))
                                .unwrap();
                            self.recursive_search_impl_cycle(key, block, src)?
                        }

                        Ok(())
                    }
                    _ => Err(VolaError::error_here(
                        OptError::Internal("Invalid branch construct in CSG-Tree".to_owned()),
                        span.unwrap_or(block.region_span.clone()),
                        "here",
                    )),
                }
            }
            //for loops, map _into_ the loop for outputs and _out_ of the lopp for arguments.
            //we don't really care for the _looping_ aspect in this case
            AbstractNodeType::Theta => match csg.output {
                OutputType::Output(_) => {
                    let inside = csg
                        .output
                        .map_to_in_region(0)
                        .unwrap()
                        .to_location(csg.node);
                    let src = self.graph.inport_src(inside).unwrap();
                    self.recursive_search_impl_cycle(key, block, src)
                }
                OutputType::Argument(_) => {
                    let outside = csg
                        .output
                        .map_out_of_region()
                        .unwrap()
                        .to_location(csg.node);
                    let src = self.graph.inport_src(outside).unwrap();
                    self.recursive_search_impl_cycle(key, block, src)
                }
                _ => Err(VolaError::error_here(
                    OptError::Internal("Invalid loop construct in CSG-Tree".to_owned()),
                    span.unwrap_or(block.region_span.clone()),
                    "here",
                )),
            },
            other => {
                let err = OptError::CsgStructureIssue(format!("Unexpected node type {other}"));
                if let Some(span) = span {
                    Err(VolaError::error_here(err, span, "here"))
                } else {
                    Err(VolaError::error_here(
                        err,
                        block.region_span.clone(),
                        "within this region",
                    ))
                }
            }
        }
    }
}
