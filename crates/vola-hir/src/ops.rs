use rvsdg::nodes::LangNode;

pub struct HirOp;

impl LangNode for HirOp {
    fn inputs(&self) -> &[rvsdg::region::Input] {
        &[]
    }
    fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input] {
        &mut []
    }
    fn outputs(&self) -> &[rvsdg::region::Output] {
        &[]
    }
    fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output] {
        &mut []
    }
}
