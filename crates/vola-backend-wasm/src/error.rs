/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::error::Error;

use rvsdg::{
    edge::InportLocation,
    util::{cfg::scfr::ScfrError, graph_type_transform::GraphTypeTransformerError},
    NodeRef, SmallColl,
};
use vola_common::thiserror::Error;
use vola_opt::common::Ty;

use crate::{graph::WasmTy, wasm::ExternOp};

#[derive(Debug, Error)]
pub enum WasmError {
    #[error(transparent)]
    Any(Box<dyn Error + Sync + Send + 'static>),
    #[error(transparent)]
    WalrusError(walrus::ErrorKind),
    #[error("Failed to lower optimizer state")]
    LoweringFailed,
    #[error(transparent)]
    GraphTransError(#[from] GraphTypeTransformerError),
    #[error("Encountered unsupported optimizer operation \"{0}\"")]
    UnsupportedNode(String),
    #[error("Encountered composite immediate value. Was \"ImmScalarize\" applied?")]
    UnexpectedComposite,
    #[error("Unexpected type {0}")]
    UnexpectedType(Ty),
    #[error("Node {node} had unexpected signature:\nInput: {input:?}\noutput: {output:?}")]
    UnexpectedSignature {
        node: String,
        input: SmallColl<Ty>,
        output: SmallColl<Ty>,
    },
    #[error("Export {0:?} unconnected!")]
    ExportUnconnected(InportLocation),
    #[error("Exported Node {0:?} has no symbol name set.")]
    UnnamedExport(NodeRef),
    #[error("Type was undefined")]
    UndefinedType,
    #[error("Failed to generate CFG for graph: {0}")]
    CfgError(#[from] ScfrError),
    #[error("Extern Runtime Op {0:?} encountered incompatible input count {1}")]
    RuntimeIncompatibleSig(ExternOp, usize),
    #[error("Extern Runtime Op {0:?} encountered incompatible type for argument {1} : {2:?}")]
    RuntimeIncompatibleType(ExternOp, usize, WasmTy),
    #[error("ExternOp not (yet) implemented: {0:?}")]
    ExternOpUnimplemented(ExternOp),
}
