use rvsdg::{
    common::{CommonRvsdg, VSEdge},
    edge::{InputType, OutputType},
    Rvsdg,
};
pub use rvsdg_viewer::macroquad;

use crate::{LNode, MyNodes};

pub fn emit() -> Rvsdg<LNode, VSEdge> {
    let mut graph = CommonRvsdg::<LNode>::new();
    //We build figure 2.d from the source paper.
    // ```C
    //int r = 1, n = 1;
    //do {
    //    r = n*r
    //    n++;
    //} while(n<5);
    // ```

    graph.on_omega_node(|omega| {
        //NOTE: the example does not live in a omega node. In our case not connecting the result to any export would
        // result in an empty omega, since nothing is reachable. So we connect both loop-variables to an artificial output, to get _some_
        // connection.
        let export_lv0 = omega.export("lv0".to_string());
        let export_lv1 = omega.export("lv1".to_string());
        //lit 1, then start theta node
        omega.on_region(|reg| {
            let litone = reg.insert_node(LNode::new(MyNodes::ImmI32(1)));
            let (_loop_node, (lv0_in, lv0_out, lv1_in, lv1_out)) = reg.new_loop(|theta| {
                let (lv0_in, lv0_arg, lv0_res, lv0_out) = theta.add_loop_variable();
                let (lv1_in, lv1_arg, lv1_res, lv1_out) = theta.add_loop_variable();

                theta.on_loop(|lbuilder| {
                    //lit one and five.
                    let litone = lbuilder.insert_node(LNode::new(MyNodes::ImmI32(1)));
                    let litfive = lbuilder.insert_node(LNode::new(MyNodes::ImmI32(5)));
                    let (add_one, _) = lbuilder
                        .connect_node(
                            LNode::new(MyNodes::Add),
                            &[
                                lv0_arg.clone(),
                                litone.as_outport_location(OutputType::Output(0)),
                            ],
                        )
                        .unwrap();
                    let (less_five, _) = lbuilder
                        .connect_node(
                            LNode::new(MyNodes::Lt),
                            &[
                                add_one.as_outport_location(OutputType::Output(0)),
                                litfive.as_outport_location(OutputType::Output(0)),
                            ],
                        )
                        .unwrap();
                    //Connect to theta predicate
                    lbuilder
                        .connect_to_result(
                            less_five.as_outport_location(OutputType::Output(0)),
                            InputType::ThetaPredicate,
                        )
                        .unwrap();
                    //also export the addone to lv0
                    lbuilder
                        .ctx_mut()
                        .connect(
                            add_one.as_outport_location(OutputType::Output(0)),
                            lv0_res,
                            VSEdge::Value,
                        )
                        .unwrap();

                    //now build the multiplication and connect it to lv1
                    let (mul, _) = lbuilder
                        .connect_node(LNode::new(MyNodes::Mul), &[lv0_arg, lv1_arg])
                        .unwrap();
                    lbuilder
                        .ctx_mut()
                        .connect(
                            mul.as_outport_location(OutputType::Output(0)),
                            lv1_res,
                            VSEdge::Value,
                        )
                        .unwrap();
                });
                (lv0_in, lv0_out, lv1_in, lv1_out)
            });

            //connect one to lv inputs
            reg.ctx_mut()
                .connect(
                    litone.as_outport_location(OutputType::Output(0)),
                    lv0_in,
                    VSEdge::Value,
                )
                .unwrap();
            reg.ctx_mut()
                .connect(
                    litone.as_outport_location(OutputType::Output(0)),
                    lv1_in,
                    VSEdge::Value,
                )
                .unwrap();

            //Connect to export
            reg.ctx_mut()
                .connect(lv0_out, export_lv0, VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(lv1_out, export_lv1, VSEdge::Value)
                .unwrap();
        });
    });
    graph
}
