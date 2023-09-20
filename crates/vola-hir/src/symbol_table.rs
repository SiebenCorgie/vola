use ahash::AHashMap;

use crate::{debug::Span, Ident, NodeRef};

pub struct SymbolDescriptor {
    node_ref: NodeRef,
    definition: Span,
}

///Simple symbol-table implementation that allows the builder to keep track of life variables.
pub struct SymbolTable {
    super_scope: Option<Box<SymbolTable>>,
    scope: AHashMap<Ident, SymbolDescriptor>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            super_scope: None,
            scope: AHashMap::default(),
        }
    }

    pub fn open_scope(&mut self) {
        let mut tmp_scope = Box::new(SymbolTable::new());
        std::mem::swap(tmp_scope.as_mut(), self);
        assert!(self.super_scope.is_none());
        self.super_scope = Some(tmp_scope);
    }

    pub fn close_scope(&mut self) {
        assert!(
            self.super_scope.is_some(),
            "Tried to close the first scope. This is most likely a bug!"
        );

        let mut swap_out = self.super_scope.take().unwrap();
        std::mem::swap(self, &mut swap_out);
    }

    pub fn push_ref(&mut self, ident: impl Into<Ident>, node: NodeRef) {
        let ident = ident.into();
        let x = self.scope.insert(
            ident.clone(),
            SymbolDescriptor {
                node_ref: node,
                definition: Span {},
            },
        );

        assert!(
            x.is_none(),
            "Shadowing (of variable {}) not yet supported!",
            ident.0
        );
    }

    pub fn resolve(&self, ident: &Ident) -> Option<NodeRef> {
        if let Some(r) = self.scope.get(ident) {
            println!("Found node: {}", ident.0);
            Some(r.node_ref.clone())
        } else {
            if let Some(supersc) = &self.super_scope {
                println!("resolve superscope");
                supersc.resolve(ident)
            } else {
                None
            }
        }
    }

    ///Returns true if this is the top most scope
    pub fn is_top_scope(&self) -> bool {
        self.super_scope.is_none()
    }
}

#[cfg(test)]
mod tests {
    use slotmap::SlotMap;

    use crate::{NodeRef, SymbolTable};

    #[test]
    fn ref_easy() {
        let mut fake_nodes = SlotMap::with_key();
        let k0: NodeRef = fake_nodes.insert(4usize);
        let mut scope = SymbolTable::new();
        scope.push_ref("k0", k0);

        assert!(scope.resolve(&"k0".into()).is_some());
        assert!(scope.resolve(&"k0".into()).unwrap() == k0);
    }

    #[test]
    fn ref_scoped() {
        let mut fake_nodes = SlotMap::with_key();
        let k0: NodeRef = fake_nodes.insert(4usize);
        let k1: NodeRef = fake_nodes.insert(42usize);
        let mut scope = SymbolTable::new();
        scope.push_ref("k0", k0);
        scope.open_scope();
        scope.push_ref("k1", k1);

        assert!(scope.resolve(&"k0".into()).is_some());
        assert!(scope.resolve(&"k0".into()).unwrap() == k0);
        assert!(scope.resolve(&"k1".into()).is_some());
        assert!(scope.resolve(&"k1".into()).unwrap() == k1);

        scope.close_scope();

        assert!(scope.resolve(&"k0".into()).is_some());
        assert!(scope.resolve(&"k0".into()).unwrap() == k0);
        assert!(scope.resolve(&"k1".into()).is_none());
    }
}
