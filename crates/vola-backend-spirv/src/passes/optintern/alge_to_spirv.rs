use rspirv::spirv::Word;
use rvsdg::NodeRef;
use vola_opt::{OptNode, Optimizer};

use crate::BackendSpirvError;

use super::InterningCtx;

//NOTE: just here to not bload the interner with the alge-dialect logic.
impl InterningCtx {
    ///analyzes the `simple_node` and interns it based on the alge->spirv dialect conversion.
    pub(crate) fn intern_alge_node(
        &mut self,
        opt: &Optimizer,
        alge_node: &OptNode,
    ) -> Result<Word, BackendSpirvError> {
        todo!();
    }
}
