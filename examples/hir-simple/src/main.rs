use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec,
    printer::{DotPrinter, PrinterContext},
};
use vola_hir::{CombNode, CombOp, EntryPointType, Module};

fn main() {
    let mut builder = Module::builder();

    builder.new_entrypoint("testfield", EntryPointType::Field, |mut b| {
        let arg_rad = b.register_arg("rad");
        let arg_offset = b.register_arg("offset");

        let sphere_def = b.register_node(
            Some("sphere_def"),
            CombNode::new(CombOp::PrimCall("Sphere".into()), b.get_at()).with_arg(arg_rad),
        );
        let trans = b.register_node(
            Some("trans"),
            CombNode::new(CombOp::OpCall("Translate".into()), b.get_at())
                .with_arg(b.get_at())
                .with_arg(arg_offset)
                .with_child(sphere_def),
        );
        let union = b.register_node(
            Some("union"),
            CombNode::new(CombOp::OpCall("Union".into()), b.get_at())
                .with_child(trans)
                .with_child(sphere_def),
        );

        b.set_out_node(union);
        b
    });

    let graph = builder.dot_graph();

    let string = graph.print(&mut PrinterContext::default());
    println!("{}", string);

    exec(
        graph,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("simple-hir.svg".to_string()),
        ],
    )
    .unwrap();
}
