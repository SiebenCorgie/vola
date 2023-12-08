//! Implements some verification helper on the rvsdg.

use crate::{edge::LangEdge, nodes::LangNode, Rvsdg};

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
}
