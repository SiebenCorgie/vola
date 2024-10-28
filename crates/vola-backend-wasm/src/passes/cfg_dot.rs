use std::path::Path;

use graphviz_rust::{
    cmd::Format,
    exec_dot,
    printer::{DotPrinter, PrinterContext},
};
use rvsdg::util::cfg::{Cfg, CfgNode, CfgRef};
use rvsdg_viewer::View;
use vola_common::dot::{DotNode, GraphvizBuilder};

use crate::WasmBackend;

pub struct CfgBundle<'a> {
    r: CfgRef,
    id: u64,
    node: &'a CfgNode,
    backend: &'a WasmBackend,
}

impl<'a> DotNode for CfgBundle<'a> {
    fn id(&self) -> String {
        format!("{}", self.id)
    }
    fn content(&self) -> String {
        match self.node {
            CfgNode::Null => format!("Null"),
            CfgNode::Root(_r) => format!("Root[{:?}]", self.r),
            CfgNode::BasicBlock(bb) => {
                let mut label_buffer = String::new();
                label_buffer += &format!("BasicBlock[{:?}]\n", self.r);
                for node in &bb.nodes {
                    let name = self.backend.graph.node(*node).name();
                    label_buffer += &format!("{} ({})\n", name, node);
                }
                label_buffer += &format!("\nExit {:?}", bb.exit_node);
                label_buffer
            }
            CfgNode::LoopHeader { .. } => format!("LoopHeader[{:?}]", self.r),
            CfgNode::LoopCtrlTail { condition_src, .. } => format!(
                "LoopCtrlTail[{:?}] - on {}",
                self.r,
                self.backend.graph.node(condition_src.node).name(),
            ),
            CfgNode::BranchHeader { condition_src, .. } => {
                format!(
                    "Branch[{:?}] on {}",
                    self.r,
                    self.backend.graph.node(condition_src.node).name(),
                )
            }
            CfgNode::BranchMerge { .. } => format!("BranchMerge[{:?}]", self.r),
        }
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        //No cildren at all, but we can setup the connections from the ids
        match self.node {
            CfgNode::Null => {}
            CfgNode::Root(r) => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", r.ffi()));
            }
            CfgNode::BasicBlock(bb) => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", bb.exit_node.ffi()));
            }
            CfgNode::LoopHeader { loop_entry_bb, .. } => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", loop_entry_bb.ffi()));
            }
            CfgNode::LoopCtrlTail {
                post_loop_bb,
                header,
                ..
            } => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", post_loop_bb.ffi()));
                builder.connect_by_id(format!("{}", self.id), format!("{}", header.ffi()));
            }
            CfgNode::BranchHeader {
                true_branch,
                false_branch,
                ..
            } => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", true_branch.ffi()));
                builder.connect_by_id(format!("{}", self.id), format!("{}", false_branch.ffi()));
            }
            CfgNode::BranchMerge { next, .. } => {
                builder.connect_by_id(format!("{}", self.id), format!("{}", next.ffi()));
            }
        }
        builder
    }
}

///Renders the `cfg` as an SVG file to `svg_path`. If it fails it'll
/// print the error to the console, but won't panic.
#[allow(dead_code)]
pub fn cfg_to_svg(cfg: &Cfg, backend: &WasmBackend, svg_path: impl AsRef<Path>) {
    //Our strategy is, that we have a stable IDing scheme, so we
    //really can just iterate over all (key, cfg) node pairs and
    //emit them into the graphviz builder.

    let mut builder = GraphvizBuilder::new();

    builder.start_graph("cfg");
    for (key, node) in &cfg.nodes {
        let id = key.ffi();
        let bundle = CfgBundle {
            r: key,
            id,
            node,
            backend,
        };
        builder.add_node(&bundle);
        builder = bundle.build_children(builder);
    }
    builder.end_graph();

    let dot = builder.graph.print(&mut PrinterContext::default());
    let format = Format::Svg;
    let graph_svg = exec_dot(dot, vec![format.into()]).unwrap();

    std::fs::write(svg_path, graph_svg).unwrap();
}