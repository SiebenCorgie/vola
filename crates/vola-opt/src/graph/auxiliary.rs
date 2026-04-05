/* * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{NodeRef, SmallColl, region::RegionLocation};
use vola_common::Span;

use crate::common::Ty;

///Identifies an implementation by the concept and csg-operation or entity.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ImplKey {
    ///The concept that is implemented
    pub concept: String,
    ///The entity or operation that is implemented
    pub csgdef: String,
}

///Reference to some implementation
#[derive(Clone)]
pub struct Impl {
    #[allow(unused)]
    pub region_span: Span,
    pub def_span: Span,
    pub concept: String,
    pub lambda: NodeRef,
    #[allow(unused)]
    pub subtrees: SmallColl<String>,
    //The argument and its type
    #[allow(unused)]
    pub args: SmallColl<(String, Ty)>,
    #[allow(unused)]
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
#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub region_span: Span,
    pub def_span: Span,
    pub lambda: NodeRef,
    pub args: SmallColl<(String, Ty, Span)>,
    pub return_type: Ty,
    pub no_inline: bool,
}

impl Function {
    pub fn region(&self) -> RegionLocation {
        RegionLocation {
            node: self.lambda,
            region_index: 0,
        }
    }
}
