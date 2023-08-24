use std::any::Any;

use slotmap::{new_key_type, SlotMap};
use tinyvec::ArrayVec;

pub type ChildVec = ArrayVec<[NodeRef; 3]>;

pub struct Node {
    ///Anonymous node description. Use the [Ctx::get] function to retrieve the actual node.
    node: Box<dyn Any + Send + Sync>,
    ///All children of this node
    children: ChildVec,
}

impl Node {
    pub fn try_deref<T: 'static>(&self) -> Option<&T> {
        self.node.downcast_ref()
    }

    pub fn try_deref_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.node.downcast_mut()
    }

    pub fn children(&self) -> &ChildVec {
        &self.children
    }
}

new_key_type! {
    ///Reference to any node within a [Ctx].
    pub struct NodeRef;
}

///A _program context_. Basically a couple of nodes with one interpretation (an AST), as the
/// entry point for further transformations.
pub struct Ctx {
    nodes: SlotMap<NodeRef, Node>,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            nodes: SlotMap::with_key(),
        }
    }

    pub fn insert<T: Send + Sync + 'static>(&mut self, node: T, children: ChildVec) -> NodeRef {
        self.nodes.insert(Node {
            node: Box::new(node),
            children,
        })
    }

    pub fn get<T: 'static>(&self, key: NodeRef) -> Option<&T> {
        if let Some(node) = self.nodes.get(key) {
            node.try_deref()
        } else {
            None
        }
    }

    pub fn get_mut<T: 'static>(&mut self, key: NodeRef) -> Option<&mut T> {
        if let Some(node) = self.nodes.get_mut(key) {
            node.try_deref_mut()
        } else {
            None
        }
    }

    ///Mutates the node behind `key` to contain `node` and `children`. In that case returns the old node. Does nothing if no node for `key`` exists.
    pub fn mutate_node<T: Send + Sync + 'static>(
        &mut self,
        key: NodeRef,
        node: T,
        children: ChildVec,
    ) -> Option<Node> {
        if let Some(inner_node) = self.nodes.get_mut(key) {
            let mut new_node = Node {
                node: Box::new(node),
                children,
            };
            std::mem::swap(&mut new_node, inner_node);
            Some(new_node)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::{ChildVec, Ctx};

    #[test]
    pub fn access_test() {
        let mut ctx = Ctx::new();
        let key = ctx.insert(String::from("my_value"), ChildVec::new());

        let val: &String = ctx.get(key).unwrap();
        assert!(val == "my_value")
    }

    #[test]
    pub fn access_mut() {
        let mut ctx = Ctx::new();
        let key = ctx.insert(String::from("my_value"), ChildVec::new());

        {
            let val: &mut String = ctx.get_mut(key).unwrap();
            *val = "teddy".to_owned();
        }
        let accessed: &String = ctx.get(key).unwrap();
        assert!(accessed == "teddy")
    }

    #[test]
    pub fn type_mutation() {
        let mut ctx = Ctx::new();
        let key = ctx.insert(String::from("my_value"), ChildVec::new());

        //Mutate String to usize node
        {
            ctx.mutate_node(key, 42usize, ChildVec::new());
        }
        let accessed: &usize = ctx.get(key).unwrap();
        assert!(*accessed == 42)
    }
}
