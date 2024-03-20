//! Liveness analysis.

use crate::{
    attrib::FlagStore,
    edge::LangEdge,
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Calculates the liveness of the whole graph. See [liveness_region] for more information.
    pub fn liveness(&self) -> FlagStore<bool> {
        //just execute for the toplevel region.
        self.liveness_region(self.toplevel_region())
    }

    ///Does liveness analysis on `region` and all sub-regions of it. Returns a [FlagStore](crate::attrib::FlagStore)
    ///That flags all known _live_ ports with `true`. All ports that are not flagged, or flagged `false` are dead.
    ///
    ///Since we are seeding with this function, all results of this `region` are considered _live_.
    pub fn liveness_region(&self, region: RegionLocation) -> FlagStore<bool> {
        //setup the initial flags for all results.
        //The _type of result_ depends on the region's node type. So we use that to explore the definitions
        match &self.node(region.node).node_type {
            NodeType::Simple(_) | NodeType::Apply(_) => panic!("node has no region"),
            NodeType::Gamma(_) => {
                //for gamma nodes
            }
            _ => todo!(),
        }

        todo!();
    }

    ///The recursive algorithm, This is more or less the _mark_ phase of the dead-node elimination algorithm described
    /// in [the source paper](http://arxiv.org/abs/1912.05036v2) in _Algorithm VI_.
    fn calc_liveness(&self, region: RegionLocation, flags: FlagStore<bool>) -> FlagStore<bool> {
        todo!()
    }
}
