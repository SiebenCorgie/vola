//! Models node relation structures

use crate::{
    edge::{InportLocation, LangEdge, OutportLocation},
    region::{Input, Output},
    Rvsdg,
};

use super::{LangNode, Node};

pub struct PredIter<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a Rvsdg<N, E>,
    inputs: &'a [Input],
    idx: usize,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for PredIter<'a, N, E> {
    type Item = OutportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.inputs.len() {
            let this_edg = self.idx;
            self.idx += 1;
            if let Some(edg) = self.inputs[this_edg].edge {
                return Some(self.ctx.edge(edg).src.clone());
            }
        }

        None
    }
}

pub struct SuccIter<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a Rvsdg<N, E>,
    outputs: &'a [Output],
    out_idx: usize,
    sub_idx: usize,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for SuccIter<'a, N, E> {
    type Item = InportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        while self.out_idx < self.outputs.len() {
            if let Some(edg) = self.outputs[self.out_idx].edges.get(self.sub_idx) {
                self.sub_idx += 1;
                return Some(self.ctx.edge(*edg).dst.clone());
            } else {
                //No edge in output collection left, go to the next output.
                self.sub_idx = 0;
                self.out_idx += 1;
            }
        }

        None
    }
}

impl<N: LangNode + 'static> Node<N> {
    ///Iterates over all nodes that are connected as inputs to this node. We call those predecessors.
    pub fn pred<'a, E: LangEdge + 'static>(&'a self, ctx: &'a Rvsdg<N, E>) -> PredIter<'a, N, E> {
        PredIter {
            ctx,
            inputs: self.inputs(),
            idx: 0,
        }
    }

    ///Iterates over all nodes that are connected to outputs of this node. We call those successors. This includes the
    /// parent node, if the connection is to an result port
    pub fn succ<'a, E: LangEdge + 'static>(&'a self, ctx: &'a Rvsdg<N, E>) -> SuccIter<'a, N, E> {
        SuccIter {
            ctx,
            outputs: self.outputs(),
            out_idx: 0,
            sub_idx: 0,
        }
    }
}
