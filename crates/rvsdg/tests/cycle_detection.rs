use rvsdg::{common::VSEdge, edge::OutputType, err::LegalizationError, Rvsdg};
use test_rvsdg::LNode;

mod test_rvsdg;

fn build_rvsdg_with_top_level_cycle() -> Rvsdg<LNode, VSEdge> {
    let mut rvsdg = Rvsdg::new();

    //build:
    // Nodes A, B, C
    //
    // Input -> A[0]
    // A[1] -> B[0]
    // A[0] -> Output[0]
    // B[0] -> C[0]
    // C[0] -> A[1]

    rvsdg.on_omega_node(|omg| {
        let import = omg.import();
        let output = omg.export();

        omg.on_region(|reg| {
            let a = reg.insert_node(LNode::new().with_inputs(2).with_outputs(2));
            let b = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));
            let c = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));

            reg.ctx_mut()
                .connect(import, a.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(a.output(1), b.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(a.output(0), output, VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(b.output(0), c.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(c.output(0), a.input(1), VSEdge::Value)
                .unwrap();
        });
    });

    rvsdg
}

#[test]
fn detect_cycle_region_tl() {
    let rvsdg = build_rvsdg_with_top_level_cycle();
    assert!(
        rvsdg.region_has_cycles(rvsdg.toplevel_region()),
        "expected cycle in graph"
    )
}

#[test]
fn detect_cycle_graph_tl() {
    let rvsdg = build_rvsdg_with_top_level_cycle();
    let res = rvsdg.is_legal_structural();
    assert!(
        res == Err(LegalizationError::CycleDetected),
        "expected cycle in graph"
    )
}

fn build_rvsdg_without_top_level_cycle() -> Rvsdg<LNode, VSEdge> {
    let mut rvsdg = Rvsdg::new();

    //build:
    // Nodes A, B, C
    //
    // Input -> A[0]
    // A[1] -> B[0]
    // A[0] -> Output[0]
    // B[0] -> Output[1]
    // C[0] -> A[1]

    rvsdg.on_omega_node(|omg| {
        let import = omg.import();
        let output0 = omg.export();
        let output1 = omg.export();

        omg.on_region(|reg| {
            let a = reg.insert_node(LNode::new().with_inputs(2).with_outputs(2));
            let b = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));
            let c = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));

            reg.ctx_mut()
                .connect(import, a.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(a.output(1), b.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(a.output(0), output0, VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(b.output(0), c.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(c.output(0), output1, VSEdge::Value)
                .unwrap();
        });
    });
    rvsdg
}

///Builds an RVSDG with an internal cycle on the omega node's region
#[test]
fn detect_no_cycle_region_tl() {
    let rvsdg = build_rvsdg_without_top_level_cycle();
    assert!(
        rvsdg.region_has_cycles(rvsdg.toplevel_region()) == false,
        "expected no cycle in simple graph"
    )
}

///Builds an RVSDG with an internal cycle on the omega node's region
#[test]
fn detect_no_cycle_graph_tl() {
    let rvsdg = build_rvsdg_without_top_level_cycle();
    assert!(
        rvsdg.is_legal_structural().is_ok(),
        "expected no cycle in simple graph"
    )
}

#[test]
fn region_walker_single_with_cycle_tl() {
    let rvsdg = build_rvsdg_with_top_level_cycle();
    assert!(rvsdg.walk_regions().count() == 1);
}
#[test]
fn region_walker_single_without_cycle_tl() {
    let rvsdg = build_rvsdg_without_top_level_cycle();
    assert!(rvsdg.walk_regions().count() == 1);
}

//Now test everything on a more complex graph
fn build_rvsdg_with_inner_cycle() -> Rvsdg<LNode, VSEdge> {
    let mut rvsdg = Rvsdg::new();

    rvsdg.on_omega_node(|omg| {
        let import = omg.import();
        let output = omg.export();

        //We bulid a λ with an inner region, that has a cycle. And hook that up to an apply node in the toplevel
        let (lambda, _) = omg.new_function(false, |lmd| {
            let _arg = lmd.add_argument();
            let res = lmd.add_result();
            lmd.on_region(|reg| {
                let a = reg.insert_node(LNode::new().with_inputs(2).with_outputs(2));
                let b = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));
                let c = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));

                reg.connect_arg_to(OutputType::Argument(0), a.input(0))
                    .unwrap();
                reg.ctx_mut()
                    .connect(a.output(1), b.input(0), VSEdge::Value)
                    .unwrap();
                reg.ctx_mut()
                    .connect(a.output(0), res, VSEdge::Value)
                    .unwrap();
                reg.ctx_mut()
                    .connect(b.output(0), c.input(0), VSEdge::Value)
                    .unwrap();
                reg.ctx_mut()
                    .connect(c.output(0), a.input(1), VSEdge::Value)
                    .unwrap();
            });
        });

        omg.on_region(|reg| {
            let (apply, _) = reg
                .call(
                    lambda.as_outport_location(OutputType::LambdaDeclaration),
                    &[import],
                )
                .unwrap();
            reg.connect_to_result(apply.output(0), output.input)
                .unwrap()
        })
    });

    rvsdg
}

fn build_rvsdg_without_inner_cycle() -> Rvsdg<LNode, VSEdge> {
    let mut rvsdg = Rvsdg::new();

    rvsdg.on_omega_node(|omg| {
        let import = omg.import();
        let output = omg.export();

        //We bulid a λ with an inner region, that has a cycle. And hook that up to an apply node in the toplevel
        let (lambda, _) = omg.new_function(false, |lmd| {
            let _arg = lmd.add_argument();
            let res = lmd.add_result();
            lmd.on_region(|reg| {
                let a = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));
                let b = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));
                let c = reg.insert_node(LNode::new().with_inputs(1).with_outputs(1));

                reg.connect_arg_to(OutputType::Argument(0), a.input(0))
                    .unwrap();
                reg.ctx_mut()
                    .connect(a.output(0), b.input(0), VSEdge::Value)
                    .unwrap();
                reg.ctx_mut()
                    .connect(b.output(0), c.input(0), VSEdge::Value)
                    .unwrap();
                reg.ctx_mut()
                    .connect(c.output(0), res, VSEdge::Value)
                    .unwrap();
            });
        });

        omg.on_region(|reg| {
            let (apply, _) = reg
                .call(
                    lambda.as_outport_location(OutputType::LambdaDeclaration),
                    &[import],
                )
                .unwrap();
            reg.connect_to_result(apply.output(0), output.input)
                .unwrap()
        })
    });

    rvsdg
}

//Check everything except for the regions with a bigger graph and inner cycles
#[test]
fn detect_cycle_graph_il() {
    let rvsdg = build_rvsdg_with_inner_cycle();
    let res = rvsdg.is_legal_structural();
    assert!(
        res == Err(LegalizationError::CycleDetected),
        "expected cycle in graph"
    )
}

///Builds an RVSDG with an internal cycle on the omega node's region
#[test]
fn detect_no_cycle_graph_il() {
    let rvsdg = build_rvsdg_without_inner_cycle();
    assert!(
        rvsdg.is_legal_structural().is_ok(),
        "expected no cycle in simple graph"
    )
}

#[test]
fn region_walker_single_with_cycle_il() {
    let rvsdg = build_rvsdg_with_inner_cycle();
    assert!(rvsdg.walk_regions().count() == 2);
}
#[test]
fn region_walker_single_without_cycle_il() {
    let rvsdg = build_rvsdg_without_inner_cycle();
    assert!(rvsdg.walk_regions().count() == 2);
}
