use ahash::{AHashMap, AHashSet};
use thiserror::Error;

use crate::{
    edge::LangEdge,
    nodes::{LangNode, NodeType, StructuralNode, ThetaNode},
    region::RegionLocation,
    NodeRef, Rvsdg,
};

use super::{BasicBlock, Cfg, CfgNode, CfgRef};

#[derive(Debug, Error)]
pub enum ScfrError {
    #[error(
        "The region contained intra-procedural nodes, like a λ-Node. Those are ignored by SCFR. Node: {0}"
    )]
    ContainedInterProceduralNode(String),
    #[error("Failed to resolve all nodes, there where still {0} left when the resolver got stuck. There is probably a dependency cycle in the region.")]
    ResolverStuck(usize),
    #[error("The scfr currently only supports 2-region gamma-nodes (if-then-else). But gamma node had {0} sub-regions")]
    NoneBinaryGamma(usize),
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Naive structured-control-flow reconstruction of a region that only contains intra-procedural nodes (simpel, apply, γ / θ nodes). Note that currently only γ-Nodes with two branches are supported.
    pub fn region_to_cfg_scfr(&self, region: RegionLocation) -> Result<Cfg, ScfrError> {
        let mut cfg = Cfg::new();

        let first_block_id = cfg.nodes.insert(CfgNode::Null);
        let (entry, _exit) = self.scfr_region(region, &mut cfg, first_block_id)?;
        //Update the root node to point to the first of the region's exit nodes
        *cfg.nodes.get_mut(cfg.root).unwrap() = CfgNode::Root(entry);
        Ok(cfg)
    }

    ///Builds `region`. If successful, returns the CfgRef of the entry block of this region as well as the unified
    /// exit node.
    fn scfr_region(
        &self,
        region: RegionLocation,
        cfg: &mut Cfg,
        entry_id: CfgRef,
    ) -> Result<(CfgRef, CfgRef), ScfrError> {
        //Serializing a region works by building the dependency
        //map of a region. By definition all args to that region are
        // assumed _live_.
        //
        // we then produce all node where all dependencies are met
        // iteratively.
        //
        // In the case of scfr, whenever we encounter a γ/θ-node,
        // we emit the corresbonding CFG-pattern.
        //
        // In the case of a θ/Loop node, we just recurse
        // with a new block for the θ-Region. When that recursion finished
        // we recover the loop-continue predicate node, and use that
        // for a followup conditional branch either to the loop-entry
        // block, or the the _post-loop_ block.
        //
        // For gamma nodes we recurse the if/true branch with a new
        // block, and factor out the remaining blocks into a new
        // (n-1)-wide gamma node.
        // This creates a skewed-binary-tree where always the true-branch is
        // taken, or _all other branches_

        let region_ref = self.region(&region).unwrap();
        //All unresolved nodes with their dependencies
        let mut unresolved = AHashMap::with_capacity(region_ref.nodes.len());

        let mut dedub_set = AHashSet::new();
        for node in &region_ref.nodes {
            dedub_set.clear();
            for input_type in self.node(*node).inport_types() {
                if let Some(edg) = self.node(*node).inport(&input_type).unwrap().edge {
                    let src = self.edge(edg).src().node;
                    dedub_set.insert(src);
                }
            }

            unresolved.insert(*node, dedub_set.clone());
        }

        let mut working_bb = BasicBlock::new(cfg);
        //pre-insert the bb and overwrite it later
        let mut bb_id = entry_id;

        while unresolved.len() > 0 {
            let mut resolved_any = false;
            let worklist: Vec<NodeRef> = unresolved.keys().map(|n| *n).collect();
            //Iterate over all _not-yet-resolved_ nodes, and
            for node in worklist {
                //check if all dependencies are NOT in the unresolved map (so are resoleved).
                //if so, mark as _was_resolved_
                let mut was_resolved = true;
                for dep in unresolved.get(&node).unwrap() {
                    //if a dependency is in the unresolved map, flag this one as unresolved as well.
                    if unresolved.contains_key(dep) {
                        was_resolved = false;
                    }
                }

                //Was resolved, so we can remove that node. Depending on the node type
                //possibly start recursion
                if was_resolved {
                    resolved_any = true;
                    assert!(unresolved.remove(&node).is_some());
                    match &self.node(node).node_type {
                        //Nothing to do for both, just push them into our current bb
                        NodeType::Simple(_) | NodeType::Apply(_) => working_bb.nodes.push(node),
                        NodeType::Theta(t) => {
                            //in the case of theta nodes we _know_ that the RVSDG encodes
                            //do-while loops. So what we do now is _end_ the current
                            //bb.
                            //We then add an unconditional branch _into_ the theta node
                            // and finally call the _handle-theta_ routine. We also prepare the merge
                            // block, so the handle-theta can add the branch to that
                            // itself.
                            //
                            // afterwards we continue by setting up the merge block as the new
                            // _normal_ block

                            let loop_entry_id = cfg.nodes.insert(CfgNode::Null);
                            let tail_ctrl_id = cfg.nodes.insert(CfgNode::Null);
                            let merge_block_id = cfg.nodes.insert(CfgNode::Null);
                            let loop_header = cfg.nodes.insert(CfgNode::LoopHeader {
                                pre_loop_bb: bb_id,
                                src_node: node,
                                loop_entry_bb: loop_entry_id,
                                ctrl_tail: tail_ctrl_id,
                            });
                            let mut post_merge_block = BasicBlock::new(cfg);

                            //set this condition-id to the exit reference of the current bb
                            working_bb.exit_node = loop_header;

                            //the condition node is already set to branch into the loop
                            self.scfr_handle_theta(
                                cfg,
                                t,
                                node,
                                loop_entry_id,
                                loop_header,
                                merge_block_id,
                                tail_ctrl_id,
                            )?;

                            //switch the current context to the new (merge)-bb.
                            std::mem::swap(&mut post_merge_block, &mut working_bb);
                            *cfg.nodes.get_mut(bb_id).unwrap() =
                                CfgNode::BasicBlock(post_merge_block);
                            bb_id = merge_block_id;
                        }
                        NodeType::Gamma(g) => {
                            if g.regions().len() != 2 {
                                return Err(ScfrError::NoneBinaryGamma(g.regions().len()));
                            }

                            let condition_srcport = self
                                .edge(g.predicate().edge.expect(
                                    "Expected gamma node to have its predicate-port connected",
                                ))
                                .src()
                                .clone();
                            //setup the conditional branch for both pre allocated branch
                            //entry points
                            let merge_node_id = cfg.nodes.insert(CfgNode::Null);
                            let post_merge_id = cfg.nodes.insert(CfgNode::Null);
                            let entry_true_id = cfg.nodes.insert(CfgNode::Null);
                            let entry_false_id = cfg.nodes.insert(CfgNode::Null);

                            let branch_header = cfg.nodes.insert(CfgNode::BranchHeader {
                                src_node: node,
                                last_bb: bb_id,
                                condition_src: condition_srcport,
                                true_branch: entry_true_id,
                                false_branch: entry_false_id,
                                merge: merge_node_id,
                                post_merge_block: post_merge_id,
                            });

                            //Overwrite the current working_bb's exit node to that
                            working_bb.exit_node = branch_header;

                            let _branch_merge_id = self.scfr_handle_gamma(
                                cfg,
                                node,
                                entry_true_id,
                                entry_false_id,
                                post_merge_id,
                                merge_node_id,
                            )?;

                            //finaly swap out the working bb with the merge id
                            //and push both in_gamma_exit_node_ids to the new bb
                            let mut merge_block = BasicBlock::new(cfg);
                            std::mem::swap(&mut working_bb, &mut merge_block);
                            //swap in the current bb
                            *cfg.nodes.get_mut(bb_id).unwrap() = CfgNode::BasicBlock(merge_block);
                            //now overwrite to the correct id
                            bb_id = post_merge_id;
                        }
                        any => {
                            return Err(ScfrError::ContainedInterProceduralNode(format!("{any}")))
                        }
                    }
                }
            }

            if !resolved_any && unresolved.len() > 0 {
                return Err(ScfrError::ResolverStuck(unresolved.len()));
            }
        }

        //push the current bb into the cfg and return the created id
        *cfg.nodes.get_mut(bb_id).unwrap() = CfgNode::BasicBlock(working_bb);
        Ok((entry_id, bb_id))
    }

    ///Handles the theta body construction. Branches to `merge_id` whenever the loop stops.
    ///Returns the id of the node that branches to the merge_id
    fn scfr_handle_theta(
        &self,
        cfg: &mut Cfg,
        theta: &ThetaNode,
        theta_node: NodeRef,
        entry_id: CfgRef,
        header_id: CfgRef,
        merge_id: CfgRef,
        ctrl_tail_id: CfgRef,
    ) -> Result<(), ScfrError> {
        //We just build the theta-region the same way we build all regions.
        //but then we exit into a conditional branch, that
        // uses the theta-predicate source to either jump to `entry_id` or to `merge_id`
        let (loop_entry, loop_last) = self.scfr_region(
            RegionLocation {
                node: theta_node,
                region_index: 0,
            },
            cfg,
            entry_id,
        )?;

        assert!(loop_entry == entry_id);

        let condition_srcport = self
            .edge(
                theta
                    .loop_predicate()
                    .edge
                    .expect("Expected theta node to have its predicate-port connected"),
            )
            .src()
            .clone();
        //the conditional_branch
        *cfg.nodes.get_mut(ctrl_tail_id).unwrap() = CfgNode::LoopCtrlTail {
            src_node: theta_node,
            condition_src: condition_srcport,
            loop_entry_bb: entry_id,
            post_loop_bb: merge_id,
            last_bb: loop_last,
            header: header_id,
        };

        //set the conditional branch as the exit of the last loop_body_block
        if let CfgNode::BasicBlock(bb) = cfg.nodes.get_mut(loop_last).unwrap() {
            bb.exit_node = ctrl_tail_id;
        } else {
            panic!("Loops last cfg node was not a basic-block!");
        }

        Ok(())
    }

    ///Handles the setup of both branch regions. Returns the id of both
    ///nodes that branch _to_ the merge_id
    fn scfr_handle_gamma(
        &self,
        cfg: &mut Cfg,
        gamma_node: NodeRef,
        entry_branch_true: CfgRef,
        entry_branch_false: CfgRef,
        post_merge_block: CfgRef,
        merge_id: CfgRef,
    ) -> Result<(), ScfrError> {
        //for the gamma nodes, we
        //setup both branches within the given entry ids,
        //then setup the uncond branches
        // to the merge id

        let (true_entry_id, true_exit_id) = self.scfr_region(
            RegionLocation {
                node: gamma_node,
                region_index: 0,
            },
            cfg,
            entry_branch_true,
        )?;
        assert!(entry_branch_true == true_entry_id);

        let (false_entry_id, false_exit_id) = self.scfr_region(
            RegionLocation {
                node: gamma_node,
                region_index: 1,
            },
            cfg,
            entry_branch_false,
        )?;
        assert!(entry_branch_false == false_entry_id);

        //setup the branch merge
        *cfg.nodes.get_mut(merge_id).unwrap() = CfgNode::BranchMerge {
            src_node: gamma_node,
            src_true: true_exit_id,
            src_false: false_exit_id,
            next: post_merge_block,
        };

        //push both as exit nodes to the last basic blocks of both branches
        if let CfgNode::BasicBlock(bb) = cfg.nodes.get_mut(true_exit_id).unwrap() {
            bb.exit_node = merge_id;
        } else {
            panic!("Gamma's last cfg node was not a basic-block!");
        }
        if let CfgNode::BasicBlock(bb) = cfg.nodes.get_mut(false_exit_id).unwrap() {
            bb.exit_node = merge_id;
        } else {
            panic!("Gamma's last cfg node was not a basic-block!");
        }

        Ok(())
    }
}
