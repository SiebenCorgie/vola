//! Implements some verification helper on the rvsdg.

use crate::{edge::LangEdge, err::LegalizationError, nodes::LangNode, Rvsdg};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Returns true, if set parental relations of all [Node]s are correct.
    ///
    /// correct means: If `node.parent = Some(region_location)`, then  `region_location.nodes.contains(node)`
    pub fn verify_parental_relations(&self) -> bool {
        //TODO: at some point we might want to only traverse nodes that are reachabel from the exports of an ω-node.
        let mut found_error = false;
        for (noderef, node) in self.nodes.iter() {
            if let Some(parent) = &node.parent {
                if let Some(parent_region) = self.region(&parent) {
                    if !parent_region.nodes.contains(&noderef) {
                        println!(
                            "Node {} has parent {:?}, but parent does not know node",
                            noderef, parent_region
                        );
                        //parent region does not contain node
                        found_error = true;
                    }
                } else {
                    //parent registered, but region does not exist
                    println!("parent region of node {} does not exist", noderef);
                    found_error = true;
                }
            }
        }

        !found_error
    }

    ///legalizes all nodes for structural correctness described in section 4. of the source paper.
    ///
    /// If anything un-legalizable is found, an error is returned.
    pub fn is_legal_structural(&self) -> Result<(), LegalizationError> {
        //NOTE: Right now, most nodes are defined in a way, that only legal nodes can be represented.
        //
        // This pass mostly takes care of two things
        // - Check for cycles
        // - Check that apply-node callees are λ or ϕ nodes

        for reg in self.walk_region_locations() {
            if self.region_has_cycles(reg) {
                return Err(LegalizationError::CycleDetected);
            }
        }

        Ok(())
    }
}
