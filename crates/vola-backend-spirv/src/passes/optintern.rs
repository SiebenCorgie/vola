use vola_opt::Optimizer;

use crate::{error::BackendSpirvError, SpirvBackend};

impl SpirvBackend {
    ///Interns the `opt` graph, by exploring all exported λ-nodes, and recursively interning all
    ///dependent nodes.
    ///
    /// For any node in the graph we try to transform them into a [SpvNode] equivalent.
    pub(crate) fn intern(&mut self, opt: &Optimizer) -> Result<(), BackendSpirvError> {
        todo!()
    }
}
