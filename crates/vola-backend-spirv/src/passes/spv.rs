use ahash::{AHashMap, AHashSet};
use rspirv::{dr::Builder, spirv::Word};
use rvsdg::{
    edge::InputType,
    nodes::{NodeType, StructuralNode},
    smallvec::SmallVec,
    NodeRef, SmallColl,
};

use crate::{BackendSpirvError, SpirvBackend, SpirvConfig, SpirvModule};

struct EmitCtx {
    extinst_ids: AHashMap<String, Word>,
}

impl SpirvBackend {
    pub fn into_spv_module(&self, config: &SpirvConfig) -> Result<SpirvModule, BackendSpirvError> {
        //Build the initial _empty_ module with the header specified by config
        let mut b = Builder::new();
        let mut ctx = EmitCtx {
            extinst_ids: AHashMap::default(),
        };
        b.set_version(config.version_major, config.version_minor);
        b.memory_model(
            rspirv::spirv::AddressingModel::Logical,
            rspirv::spirv::MemoryModel::Vulkan,
        );
        //always emitting a linkable shader module
        b.capability(rspirv::spirv::Capability::Linkage);
        b.capability(rspirv::spirv::Capability::Shader);

        for ext in &config.extensions {
            b.extension(ext.clone());
        }

        for ext_inst in &config.ext_inst {
            let id = b.ext_inst_import(ext_inst.clone());
            assert!(ctx.extinst_ids.insert(ext_inst.clone(), id).is_none());
        }

        self.emit_into(&ctx, &mut b)?;

        Ok(b.module())
    }

    fn emit_into(&self, ctx: &EmitCtx, builder: &mut Builder) -> Result<(), BackendSpirvError> {
        //Our strategy is a bottom-up procedure, where we assign each node a
        //id in the module.
        //
        //Since SPIR-V is a SCF-graph, construction _should_ be possible directly.
        //
        // We proceed by first building a dependecy graph of all top-level λ-nodes that are exported.
        // We then process the dependencies of each export first, and finally process the exported λ-nodes.
        // This is possible, because the graph is already in a SPIR-V-like form. So no λ-nesting etc. _should_
        // be present.

        let mut lmd_dependecies = AHashMap::default();
        for tlnode_ref in self
            .graph
            .region(&self.graph.toplevel_region())
            .unwrap()
            .nodes
            .iter()
        {
            let tlnode = self.graph.node(*tlnode_ref);
            let mut deps: SmallColl<_> = SmallColl::new();
            match &tlnode.node_type {
                NodeType::Lambda(lmd) => {
                    //checkout all dependecies.
                    for cvidx in 0..lmd.context_variable_count() {
                        if let Some(cv) = tlnode.inport(&InputType::ContextVariableInput(cvidx)) {
                            if let Some(edg) = cv.edge {
                                deps.push(self.graph.edge(edg).src().node);
                            }
                        }
                    }
                }
                _ => {
                    return Err(BackendSpirvError::Any {
                        text: format!("Unexpected none-λ node in toplevel of SPIR-V graph."),
                    })
                }
            }
            lmd_dependecies.insert(tlnode_ref, deps);
        }

        //NOTE: lets us find out if some processed lmd needs to be exported, and also
        //      deduplicates those by being a Set.
        let exported_nodes: AHashSet<NodeRef> = self
            .graph
            .region(&self.graph.toplevel_region())
            .unwrap()
            .results
            .iter()
            .filter_map(|res| {
                if let Some(edg) = res.edge {
                    Some(self.graph.edge(edg).src().node)
                } else {
                    None
                }
            })
            .collect();

        println!("Exporting {} λs", exported_nodes.len());

        Ok(())
    }
}
