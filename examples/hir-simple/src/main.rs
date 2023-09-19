use vola_hir::{AlgeNode, AlgeOp, Module, ModuleBuilder};

fn main() {
    let mut builder = Module::builder();

    builder.new_region(|mut b| {
        b.register_arg(Some("rad"), AlgeNode::new(AlgeOp::None));
        b.register_arg(Some("offset"), AlgeNode::new(AlgeOp::None));
        b
    });
}
