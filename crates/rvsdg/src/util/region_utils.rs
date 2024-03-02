use crate::{edge::LangEdge, nodes::LangNode, Rvsdg};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    //NOTE: At first I wanted to implement things like `remove_result` and `remove_argument` here,
    //to make life easier. But this would make it easy to produce invalid graph. For instanec if you'd
    //remove a result of a λ-Node's body, you'd have to update all apply node outputs as well.
}
