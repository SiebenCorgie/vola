use crate::Optimizer;

impl Optimizer {
    ///Smoll pass that cleans up the export lambda declerations to not reference any _unused_ context.
    /// This should in fact also make the graph _purely_ in the alge dialect.
    pub fn cleanup_export_lmd(&mut self) {
        //first do deadnode elemintation, then

        let dnecount = self.graph.dead_node_elimination().unwrap().len();

        #[cfg(feature = "log")]
        log::error!("Delted {dnecount} nodes in DNE ");

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
}
