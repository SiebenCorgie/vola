use vola_ast::common::{Block, Branch, Loop};

use crate::{FormatTree, Keyword};

impl From<&Loop> for FormatTree {
    fn from(value: &Loop) -> Self {
        let header = FormatTree::Seq(vec![
            FormatTree::Keyword(Keyword::LoopFor),
            FormatTree::from(&value.iteration_variable_ident),
            FormatTree::Keyword(Keyword::LoopIn),
            FormatTree::from(&value.bound_lower),
            FormatTree::Keyword(Keyword::Range),
            FormatTree::from(&value.bound_upper),
        ]);
        FormatTree::Block {
            lines: vec![header, FormatTree::from(value.body.as_ref())],
            braced: true,
        }
    }
}

impl From<&Branch> for FormatTree {
    fn from(value: &Branch) -> Self {
        let header = FormatTree::Seq(vec![
            FormatTree::Keyword(Keyword::IfBranch),
            FormatTree::from(&value.conditional.0),
        ]);
        let mut blk = vec![header, FormatTree::from(value.conditional.1.as_ref())];

        if let Some(uncond) = &value.unconditional {
            blk.push(FormatTree::Keyword(Keyword::ElseBranch));
            blk.push(FormatTree::from(uncond.as_ref()));
        }

        FormatTree::Block {
            lines: blk,
            braced: true,
        }
    }
}

impl From<&Block> for FormatTree {
    fn from(value: &Block) -> Self {
        let mut sub_stmts = Vec::with_capacity(value.stmts.len() + 1);

        for sub in &value.stmts {
            sub_stmts.push(FormatTree::from(sub));
        }

        if let Some(end) = &value.retexpr {
            sub_stmts.push(FormatTree::from(end));
        }
        FormatTree::Block {
            lines: sub_stmts,
            braced: true,
        }
    }
}
