use ahash::AHashMap;

use crate::{
    error::ParserError,
    scad_ast::{ScadBlock, ScadStmt},
};

impl ScadBlock {
    ///Recursively normalizes this, and all sub-blocks by moving all _last_ variables assignments
    ///to the top of the block.
    pub fn normalize(&mut self) -> Result<(), ParserError> {
        //filter out all assignments, and put them on the top of the block.
        //we do that in reverse, in order to only keep the _last-write_, which is
        //(more or less) what Scad does internally.

        //now iterate through the assignments, and only keep the last one for each variable
        //NOTE: the indirection lets us keep the initial order, which we need, (later), to find undefined
        //      variables at compile-time (of vola). In OpenScad those would _just_ be undefined.
        let mut assign_map = AHashMap::default();
        let mut assigns = Vec::new();
        //filter all assigns
        self.stmts = std::mem::take(&mut self.stmts)
            .into_iter()
            .filter_map(|x| match x {
                ScadStmt::Assign(a) => {
                    let index = if let Some(known_index) = assign_map.get(&a.var.0) {
                        *known_index
                    } else {
                        let new_index = assigns.len();
                        assign_map.insert(a.var.0.clone(), new_index);
                        //Init slot
                        assigns.push(ScadStmt::None);
                        new_index
                    };
                    //overwrite index of assign
                    assigns[index] = ScadStmt::Assign(a);

                    None
                }
                other => Some(other),
            })
            .collect();

        //now prepend all _kept_ assigns
        assigns.append(&mut self.stmts);
        assert!(self.stmts.len() == 0);
        self.stmts = assigns;

        //finally recurse on any block_carrying stmt
        for stmt in &mut self.stmts {
            stmt.normalize()?;
        }

        //Alright, finished
        Ok(())
    }
}

impl ScadStmt {
    pub fn normalize(&mut self) -> Result<(), ParserError> {
        match self {
            Self::Assert
            | Self::Assert
            | Self::Comment(_)
            | Self::Assign(_)
            | Self::None
            | Self::IncludeStmt(_) => Ok(()),
            Self::Overwrite {
                overwrites: _,
                block,
            } => block.normalize(),
            Self::IfBlock {
                head_span: _,
                condition: _,
                consequence,
                alternative,
            } => {
                consequence.normalize()?;
                if let Some(alt) = alternative {
                    alt.normalize()?;
                }
                Ok(())
            }
            Self::ForBlock { block, .. } => block.normalize(),
            Self::Chain { chain, span: _ } => {
                for element in chain {
                    match element {
                        crate::scad_ast::ChainElement::Block(b) => b.normalize()?,
                        crate::scad_ast::ChainElement::Call(_) => {}
                    }
                }
                Ok(())
            }
        }
    }
}
