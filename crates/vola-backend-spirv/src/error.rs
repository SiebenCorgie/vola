/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::util::graph_type_transform::GraphTypeTransformerError;
use spirv_grammar_rules::Rule;
use vola_common::thiserror::{self, Error};

#[derive(Debug, Error, Clone)]
pub enum BackendSpirvError {
    #[error("{text}")]
    Any { text: String },
    #[error("Failed to intern optimizer graph: {0}")]
    LoweringError(GraphTypeTransformerError),
    #[error("Could not convert type {0:?} into SPIR-V type!")]
    TypeConversionError(vola_opt::common::Ty),
    #[error("SPIR-V Instruction {inst} does not respect rule {rule:?}")]
    SpvLegalizationRuleFailed { inst: String, rule: Rule },
    #[error("SPIR-V Instruction {inst} was malformed: {text}")]
    SpvLegalizationMalformed { inst: String, text: String },
    #[error("SPIR-V backend error: {0}")]
    SPVError(String),
}

impl BackendSpirvError {
    pub fn set_opname(&mut self, opname: String) {
        match self {
            BackendSpirvError::SpvLegalizationRuleFailed { inst, rule: _ } => *inst = opname,
            BackendSpirvError::SpvLegalizationMalformed { inst, text: _ } => *inst = opname,
            _ => {}
        }
    }
}
