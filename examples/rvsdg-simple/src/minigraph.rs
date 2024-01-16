use rvsdg::{
    common::{CommonRvsdg, VSEdge},
    Rvsdg,
};

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
        let import = omega.import();
        let export = omega.export();

        omega.on_region(|reg| {
            let con = reg.insert_node(LNode::new(MyNodes::ImmI32(1)));
            let node = reg.insert_node(LNode::new(MyNodes::Add));
            reg.ctx_mut()
                .connect(import, node.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(con.output(0), node.input(1), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(node.output(0), export, VSEdge::Value)
                .unwrap();
        })
    });
    graph
}
