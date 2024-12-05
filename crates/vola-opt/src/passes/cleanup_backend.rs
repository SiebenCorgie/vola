use rvsdg::{region::RegionLocation, util::cne::CneError};

use crate::{OptError, Optimizer};

impl Optimizer {
    fn remove_unused_toplevel_cvs(&mut self) {
        //then remove all unused cvs from the export λs
        let tl = self.graph.toplevel_region();
        for residx in 0..self.graph.region(&tl).unwrap().results.len() {
            if let Some(srcnode) = self
                .graph
                .region(&tl)
                .unwrap()
                .result_src(&self.graph, residx)
            {
                self.graph.remove_unused_context_variables(srcnode.node);
            } else {
                #[cfg(feature = "log")]
                log::warn!("export port {} was not connected!", residx);
            }
        }
    }

    ///Removes edges from the graph that are unused. This means all edges that
    ///go into a _region-containing_ node, but aren't used within that region
    pub fn remove_unused_edges(&mut self) -> Result<(), OptError> {
        let tl = self.graph.toplevel_region();
        let mut topo_ord = self.graph.topological_order_region(tl);
        topo_ord.reverse();
        for node in topo_ord {
            for region_index in 0..self.graph[node].regions().len() {
                self.graph
                    .remove_unused_edges_in_region(RegionLocation { node, region_index }, true)?;
            }
        }

        Ok(())
    }

    ///Smoll pass that cleans up the export lambda declarations to not reference any _unused_ context.
    /// This should in fact also make the graph _purely_ in the alge dialect.
    pub fn cleanup_export_lmd(&mut self) {
        //first do deadnode elimination, then

        #[cfg(feature = "log")]
        log::info!("cleanup export λ-Nodes");
        self.remove_unused_toplevel_cvs();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_DNE").is_ok() {
            self.push_debug_state("pre dead node elemination");
        }

        let dnecount = self.graph.dead_node_elimination().unwrap().len();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_DNE").is_ok() {
            self.push_debug_state("post dead node elemination");
        }

        #[cfg(feature = "log")]
        log::info!("Deleted {dnecount} nodes in DNE ");
        self.remove_unused_toplevel_cvs();
        self.graph.dead_node_elimination().unwrap();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_CV_CLEANUP").is_ok() {
            self.push_debug_state("post cv cleanup");
        }
    }

    ///Executes CNE on all exported λs
    pub fn cne_exports(&mut self) -> Result<(), CneError> {
        #[cfg(feature = "log")]
        log::info!("CNE Exports");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_CNE").is_ok() {
            self.push_debug_state("before cne");
        }
        //let mut deleted = Vec::new();
        //TODO: actually only do the exports, right now we just do _all_
        let mut deleted = Vec::new();
        for exported in self.exported_functions() {
            self.graph.cne_region(exported, &mut deleted)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_CNE").is_ok() {
            self.push_debug_state("after cne");
        }

        Ok(())
    }
}
