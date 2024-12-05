/* * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, NodeRef, SmallColl};
use vola_common::Span;

use crate::common::Ty;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ImplKey {
    ///The concept that is implemented
    pub concept: String,
    ///The entity or operation that is implemented
    pub node: String,
}

///Reference to some implementation
pub struct Impl {
    pub region_span: Span,
    pub def_span: Span,
    pub concept: String,
    pub lambda: NodeRef,
    pub subtrees: SmallColl<String>,
    //The argument and its type
    pub args: SmallColl<(String, Ty)>,
    pub return_type: Ty,
}

impl Impl {
    pub fn region(&self) -> RegionLocation {
        RegionLocation {
            node: self.lambda,
            region_index: 0,
        }
    }
}

///An implemented function
pub struct Function {
    pub name: String,
    pub region_span: Span,
    pub def_span: Span,
    pub lambda: NodeRef,
    pub args: SmallColl<(String, Ty)>,
    pub return_type: Ty,
}

impl Function {
    pub fn region(&self) -> RegionLocation {
        RegionLocation {
            node: self.lambda,
            region_index: 0,
        }
    }
}
