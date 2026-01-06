/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::Optimizer;
use ahash::AHashMap;
use rvsdg::{attrib::AttribLocation, region::RegionLocation, util::inline::InlineError, NodeRef};
use vola_common::{Span, VolaError};

///Takes care of inlining apply-nodes, while also retaining metadata of any inlined node.
pub struct Inliner<'opt> {
    opt: &'opt mut Optimizer,
    waiting_regions: Vec<RegionLocation>,
    attrib_map: AHashMap<AttribLocation, AttribLocation>,
}

impl<'opt> Inliner<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        Self {
            opt: optimizer,
            //don't allocate by default, but allow reuse
            waiting_regions: Vec::with_capacity(0),
            attrib_map: AHashMap::with_capacity(0),
        }
    }

    ///Inlines all `apply` nodes in `region`. If `recursive` is set, first takes care of any live sub-regions, and the function's
    /// dependencies. I.e. totally inlines everything.
    pub fn inline_region(
        mut self,
        region: RegionLocation,
        recursive: bool,
        use_heuristic: bool,
    ) -> Result<Self, VolaError<InlineError>> {
        //init region list
        self.waiting_regions.clear();
        self.waiting_regions.push(region);

        //If recrusively adding, walk all live nodes and ... recursively
        // add all sub regions in a way that popping from the front will start
        // with the _deepest_ node.
        if recursive {
            self.add_sub_regions(region);
        }

        //now pop regions from the back and inline any apply-node
        while let Some(region) = self.waiting_regions.pop() {
            for node in self.opt.graph.live_nodes_in_region(region) {
                if self.opt.graph[node].node_type.is_apply() {
                    self = self.inline_apply_node(node, use_heuristic)?;
                }
            }
        }

        Ok(self)
    }

    ///Inlines `node` returning an error if it failed. Returns Ok(Self) is successful, or if `node`
    /// is no apply-node.
    ///
    /// If `use_heuristic` is true, might not inline, if the heuristic detects that
    /// it wouldn't be beneficial.
    ///
    /// If the called function (by the apply-node) is tagged as _no-inline_, does in fact not inline it.
    pub fn inline_apply_node(
        mut self,
        node: NodeRef,
        use_heuristic: bool,
    ) -> Result<Self, VolaError<InlineError>> {
        if !self.opt.graph[node].node_type.is_apply() {
            return Ok(self);
        }

        if self.opt.is_tagged_no_inline(node) {
            #[cfg(feature = "log")]
            log::warn!("Trying to inline all, but function is tagged as no_inline, not inlining this one...");
            return Ok(self);
        }

        //we shall not inline according to heuristic.
        if use_heuristic && !self.opt.should_inline_apply(node) {
            return Ok(self);
        }

        let span = self.opt.find_span(node).unwrap_or(Span::empty());

        //NOTE: clean the attrib map, then remap all collected nodes
        self.attrib_map.clear();

        let paths = self
            .opt
            .graph
            .inline_apply_node(node, Some(&mut self.attrib_map))
            .map_err(|e| VolaError::error_here(e, span, "while inlining this call"))?;

        for path in paths {
            if let Err(e) = self.opt.type_path(&path) {
                #[cfg(feature = "log")]
                log::warn!("Could not type inlined-path: {e}");
            }
        }

        //Take care of node-attribute inlining
        for (src, dst) in &self.attrib_map {
            if let (AttribLocation::Node(src), AttribLocation::Node(dst)) = (src, dst) {
                self.opt.copy_node_attributes(*src, *dst);
            }
        }

        Ok(self)
    }

    ///Adds all sub-regions to the waiting list
    fn add_sub_regions(&mut self, region: RegionLocation) {
        for node in self.opt.graph.live_nodes_in_region(region) {
            //add all sub regions of the node, if there is any
            for region_index in 0..self.opt.graph[node].regions().len() {
                let location = RegionLocation { node, region_index };
                self.waiting_regions.push(location);
                self.add_sub_regions(location);
            }

            //if this is an apply-node, add the called function to the list of
            // to-be-inlined regions
            if self.opt.graph[node].node_type.is_apply() {
                let apply_node_call_port = node.input(0);
                let src = self.opt.graph.inport_src(apply_node_call_port).unwrap();
                let prod = self.opt.graph.find_callabel_def(src).unwrap();
                assert!(self.opt.graph.node(prod.node).node_type.is_lambda());
                //recursively inline anything in this producer λ
                let body = RegionLocation {
                    node: prod.node,
                    region_index: 0,
                };
                //add the body to the waiting list...
                self.waiting_regions.push(body);
                //... and take care of it
                self.add_sub_regions(body);
            }
        }
    }
}

///Inlines all apply nodes that are currently alive. Good if a backend doesn't implement call-def
pub struct InlineAll<'opt> {
    opt: &'opt mut Optimizer,
}

impl<'opt> InlineAll<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        Self { opt }
    }

    pub fn execute(self) -> Result<(), VolaError<InlineError>> {
        #[cfg(feature = "log")]
        log::info!("Inline all");
        //simply wrap the inliner with a _always-inline_ policy
        let tl = self.opt.graph.toplevel_region();
        Inliner::setup(self.opt).inline_region(tl, true, false)?;
        Ok(())
    }
}

pub struct InlineExports<'opt> {
    opt: &'opt mut Optimizer,
}

impl<'opt> InlineExports<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        Self { opt }
    }

    pub fn execute(self) -> Result<(), VolaError<InlineError>> {
        let exports = self.opt.exported_functions();

        #[cfg(feature = "log")]
        log::info!("Total inline of {} exports", exports.len());

        let mut inliner = Inliner::setup(self.opt);
        for export in exports {
            inliner = inliner.inline_region(export, true, true)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("VOLA_DUMP_POST_INLINE_EXPORT").is_ok()
        {
            self.opt.push_debug_state("post inline fields");
        }
        Ok(())
    }
}
