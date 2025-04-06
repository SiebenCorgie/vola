use vola_ast::csg::{CSGConcept, CsgDef, CsgStmt, ScopedCall};

use crate::{FormatTree, Keyword};

impl From<&CsgStmt> for FormatTree {
    fn from(value: &CsgStmt) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Keyword(crate::Keyword::Csg),
            FormatTree::Token(value.decl_name.0.clone()),
            FormatTree::Keyword(crate::Keyword::Equal),
            FormatTree::from(&value.expr),
            FormatTree::StmtEnd,
        ])
    }
}

impl From<&ScopedCall> for FormatTree {
    fn from(value: &ScopedCall) -> Self {
        //scoped call works like a normal call, but appends a couple ouf
        //scopes after it
        let mut seq = vec![FormatTree::from(&value.call)];

        for block in &value.blocks {
            seq.push(FormatTree::from(block));
        }

        FormatTree::Seq(seq)
    }
}

impl From<&CsgDef> for FormatTree {
    fn from(value: &CsgDef) -> Self {
        let starter = match &value.ty {
            vola_ast::csg::CsgTy::Entity => FormatTree::Keyword(Keyword::Entity),
            vola_ast::csg::CsgTy::Operation => FormatTree::Keyword(Keyword::Operation),
        };

        FormatTree::Seq(vec![
            starter,
            FormatTree::from(&value.name),
            FormatTree::Wrapped {
                left: '(',
                right: ')',
                sub: Box::new(FormatTree::seperated_list(",", value.args.iter())),
            },
            FormatTree::StmtEnd,
        ])
    }
}

impl From<&CSGConcept> for FormatTree {
    fn from(value: &CSGConcept) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Keyword(Keyword::Concept),
            FormatTree::from(&value.name),
            FormatTree::TypeEnd,
            FormatTree::from(&value.src_ty),
            FormatTree::Keyword(Keyword::ResultArrow),
            FormatTree::from(&value.dst_ty),
            FormatTree::StmtEnd,
        ])
    }
}
