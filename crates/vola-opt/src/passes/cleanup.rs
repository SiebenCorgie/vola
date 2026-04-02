use rvsdg::region::RegionLocation;

use crate::{OptError, Optimizer};

///Collection of general purpose cleanup utilities.
pub struct Cleanup<'opt> {
    opt: &'opt mut Optimizer,
}

impl<'opt> Cleanup<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        Self { opt }
    }

    ///Removes context variables from exported functions, that are not used. This might reduce
    /// code-size, if functions import context, that is ultimately unused.
    pub fn remove_unused_toplevel_cvs(self) -> Self {
        #[cfg(feature = "log")]
        log::info!("Removing unused context-variables on exported functions");

        for result in self
            .opt
            .graph
            .result_ports(self.opt.graph.toplevel_region())
        {
            let Some(connected) = self.opt.graph.inport_src(result) else {
                #[cfg(feature = "log")]
                log::warn!("export port {} was not connected!", result);
                continue;
            };

            if self.opt.graph[connected.node].node_type.is_lambda() {
                self.opt
                    .graph
                    .remove_unused_context_variables(connected.node);
            }
        }

        self
    }

    ///Removes edges from the graph that are unused. This means all edges that
    ///go into a _region-containing_ node, but aren't used within that region
    pub fn remove_unused_edges(self) -> Result<Self, OptError> {
        #[cfg(feature = "log")]
        log::info!("Remove unused edge");

        let tl = self.opt.graph.toplevel_region();
        let mut topo_ord = self.opt.graph.topological_order_region(tl);
        topo_ord.reverse();
        for node in topo_ord {
            for region_index in 0..self.opt.graph[node].regions().len() {
                self.opt
                    .graph
                    .remove_unused_edges_in_region(RegionLocation { node, region_index }, true)?;
            }
        }

        Ok(self)
    }

    /// General-purpose pass that cleans the exported lambdas by:
    ///
    /// 1. remove unused contexd
    /// 2. remove all dead code
    pub fn cleanup_export_lmd(mut self) -> Self {
        //first do deadnode elimination, then
        #[cfg(feature = "log")]
        log::info!("cleanup export λ-Nodes");

        self = self.remove_unused_toplevel_cvs();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_DNE").is_ok() {
            self.opt.push_debug_state("pre dead node elimination");
        }

        let dnecount = self.opt.graph.dead_node_elimination().unwrap().len();
        #[cfg(feature = "log")]
        log::info!("Deleted {dnecount} nodes in DNE ");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_DNE").is_ok() {
            self.opt.push_debug_state("post dead node elimination");
        }

        self = self.remove_unused_toplevel_cvs();
        self.opt.graph.dead_node_elimination().unwrap();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_CV_CLEANUP").is_ok() {
            self.opt.push_debug_state("post cv cleanup");
        }

        self
    }

    ///Executes CNE on all exported λs
    pub fn cne_exports(self) -> Result<Self, OptError> {
        #[cfg(feature = "log")]
        log::info!("CNE Exports");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_CNE").is_ok() {
            self.opt.push_debug_state("before cne");
        }
        //let mut deleted = Vec::new();
        //TODO: actually only do the exports, right now we just do _all_
        let mut deleted = Vec::new();
        for exported in self.opt.exported_functions() {
            self.opt.graph.cne_region(exported, &mut deleted)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_CNE").is_ok() {
            self.opt.push_debug_state("after cne");
        }

        Ok(self)
    }
}
