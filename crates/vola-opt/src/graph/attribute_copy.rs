/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::Optimizer;
use rvsdg::NodeRef;

impl Optimizer {
    ///Copies all node (and port) attributes of _src_ to _dst_, if the same attribute location exists.
    pub(crate) fn copy_node_attributes(&mut self, src: NodeRef, dst: NodeRef) {
        for attrib in self.graph.iter_node_attribs(src.into()) {
            let dst_attrib = attrib.clone().change_node(dst);
            if let Some(name) = self.names.get(&attrib).cloned() {
                self.names.set(dst_attrib.clone(), name);
            }

            if let Some(ty) = self.typemap.get(&attrib).cloned() {
                self.typemap.set(dst_attrib.clone(), ty);
            }

            if let Some(span) = self.span_tags.get(&attrib).cloned() {
                self.span_tags.set(dst_attrib.clone(), span);
            }

            if let Some(vp) = self.var_producer.get(&attrib).cloned() {
                self.var_producer.set(dst_attrib, vp);
            }
        }
    }
}
