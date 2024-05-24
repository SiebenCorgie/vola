use crate::Optimizer;

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
    ///Smoll pass that cleans up the export lambda declarations to not reference any _unused_ context.
    /// This should in fact also make the graph _purely_ in the alge dialect.
    pub fn cleanup_export_lmd(&mut self) {
        //first do deadnode elimination, then

        #[cfg(feature = "log")]
        log::info!("cleanup export λ-Nodes");
        self.remove_unused_toplevel_cvs();

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
}
