/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::fmt::Display;

use vola_ast::{AstEntry, TopLevelNode, VolaAst};

mod alge;
mod block;
mod common;
mod csg;

#[derive(Debug, Clone)]
pub enum FormatTree {
    ///List of sub-trees, typically formatted vertically on the same
    /// column.
    Block {
        //true if {} should be round _lines_.
        braced: bool,
        lines: Vec<Self>,
    },
    ///Indentation of something
    Indent(Box<Self>),
    ///Wrapps a sub tree into the given chars
    Wrapped {
        left: char,
        right: char,
        sub: Box<Self>,
    },
    ///Just a simple Token that'll be emitted as-is
    Token(String),
    ///sequence of sub-trees, typically formatted horizontally on the same row,
    ///might be formated over multiple rows, if too long.
    Seq(Vec<Self>),
    UnaryOp {
        op: String,
        operand: Box<Self>,
    },
    BinaryOp {
        left: Box<Self>,
        right: Box<Self>,
        op: String,
    },
    ///Marks the end of a statement, so a `;` usually
    StmtEnd,
    ///Type ending token, so a ':'
    TypeEnd,
    Space,
    Keyword(Keyword),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
    LoopFor,
    LoopIn,
    IfBranch,
    ElseBranch,
    Csg,
    Concept,
    Entity,
    Operation,
    Impl,
    For,
    Range,
    Fn,
    Export,
    Equal,
    Eval,
    AccessDot,
    ResultArrow,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Let => write!(f, "let"),
            Keyword::LoopFor => write!(f, "for"),
            Keyword::LoopIn => write!(f, "in"),
            Keyword::IfBranch => write!(f, "if"),
            Keyword::ElseBranch => write!(f, "else"),
            Keyword::Csg => write!(f, "csg"),
            Keyword::Concept => write!(f, "concept"),
            Keyword::Entity => write!(f, "entity"),
            Keyword::Operation => write!(f, "operation"),
            Keyword::Impl => write!(f, "impl"),
            Keyword::For => write!(f, "for"),
            Keyword::Range => write!(f, ".."),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Export => write!(f, "export"),
            Keyword::Equal => write!(f, "="),
            Keyword::Eval => write!(f, "eval"),
            Keyword::AccessDot => write!(f, "."),
            Keyword::ResultArrow => write!(f, "->"),
        }
    }
}

impl FormatTree {
    pub fn seperated_list<T>(seperator: &str, list: impl Iterator<Item = T>) -> Self
    where
        Self: From<T>,
    {
        let mut seq = Vec::new();
        for item in list {
            seq.push(FormatTree::from(item));
            seq.push(FormatTree::Token(seperator.to_owned()));
        }
        //pop off the last comma
        let _last = seq.pop();
        FormatTree::Seq(seq)
    }
}

#[derive(Debug, Clone)]
pub enum Indentation {
    //Uses a single tab for identation
    Tabs,
    //Uses the given amount of spaces.
    Spaces(usize),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(4)
    }
}

#[derive(Debug, Default, Clone)]
pub struct FormatConfig {
    indentation: Indentation,
}

///A formated [VolaAst].
#[derive(Debug, Clone)]
pub struct Formater {
    tree: FormatTree,
    config: FormatConfig,
}

impl From<&TopLevelNode> for FormatTree {
    fn from(value: &TopLevelNode) -> Self {
        //if there are ct-args, wrap into another block, otherwise just emit the
        // entrypoint
        if value.ct_args.len() > 0 {
            let mut block = Vec::with_capacity(value.ct_args.len() + 1);

            for ct in value.ct_args.iter() {
                block.push(FormatTree::from(ct));
            }
            block.push(FormatTree::from(&value.entry));
            FormatTree::Block {
                lines: block,
                braced: false,
            }
        } else {
            FormatTree::from(&value.entry)
        }
    }
}

impl From<&AstEntry> for FormatTree {
    fn from(value: &AstEntry) -> Self {
        match value {
            AstEntry::Comment(c) => Self::Token(c.content.clone()),
            AstEntry::Module(m) => Self::from(m),
            AstEntry::Concept(c) => Self::from(c),
            AstEntry::Func(f) => Self::from(f),
            AstEntry::CsgDef(csgdef) => Self::from(csgdef),
            AstEntry::ImplBlock(implblock) => Self::from(implblock),
        }
    }
}

impl Formater {
    ///Transform the AST-Tree into a FormatTree
    pub fn format_ast(ast: &VolaAst) -> Self {
        //TODO: Barebone implementation for now.
        //      Main points one could optimize:
        //      - Don't allocate, instead use a SlotMap for references, and capture parts
        //        of `ast` via a 'a lifetime
        //
        //      - Possibly use a framework, if we find a nice one
        let mut root = Vec::with_capacity(ast.entries.len());
        for entry in &ast.entries {
            root.push(FormatTree::from(entry));
        }

        Formater {
            tree: FormatTree::Block {
                lines: root,
                braced: false,
            },
            config: FormatConfig::default(),
        }
    }

    pub fn with_config(mut self, config: FormatConfig) -> Self {
        self.config = config;
        self
    }
}

#[derive(Clone)]
struct FormatingState {
    indentation: Indentation,
    ident_level: usize,
}

impl FormatingState {
    ///Adds the current level of identation to `f`.
    fn ident_on(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //NOTE I'm pretty sure there is a better way :D
        match self.indentation {
            Indentation::Tabs => {
                for _l in 0..self.ident_level {
                    write!(f, "\t")?
                }
                Ok(())
            }
            Indentation::Spaces(count_per_level) => {
                let ident_buf = String::from_iter((0..count_per_level).map(|_| ' '));
                for _l in 0..self.ident_level {
                    write!(f, "{}", ident_buf)?
                }
                Ok(())
            }
        }
    }
}

impl FormatTree {
    //Depth first formating walker on the Formating tree.
    fn format(&self, f: &mut std::fmt::Formatter<'_>, state: &FormatingState) -> std::fmt::Result {
        match self {
            FormatTree::Block { lines, braced } => {
                let mut new_state = state.clone();
                if *braced {
                    let t = "{";
                    write!(f, "{t}")?;
                    new_state.ident_level += 1;
                }
                //blocks are just serialized by setting up a new line for each, and pre-prending
                //the identation
                for item in lines {
                    write!(f, "\n")?;
                    new_state.ident_on(f)?;
                    item.format(f, state)?
                }

                if *braced {
                    write!(f, "\n")?;
                    //ident the closing brace based on the old
                    //level
                    state.ident_on(f)?;
                    let t = "}";
                    write!(f, "{t}")?;
                }

                Ok(())
            }
            FormatTree::UnaryOp { op, operand } => {
                write!(f, "{op}")?;
                operand.format(f, state)
            }
            FormatTree::BinaryOp { left, right, op } => {
                left.format(f, state)?;
                write!(f, " {op} ")?;
                right.format(f, state)
            }
            FormatTree::Indent(inner) => {
                let mut new_state = state.clone();
                new_state.ident_level += 1;
                inner.format(f, &new_state)
            }
            FormatTree::Wrapped { left, right, sub } => {
                write!(f, "{left}")?;
                sub.format(f, state)?;
                write!(f, "{right}")
            }
            FormatTree::Token(token) => write!(f, "{token}"),
            FormatTree::Seq(seq) => {
                for item in seq {
                    if item.has_space_before() {
                        write!(f, " ")?;
                    }
                    item.format(f, state)?;
                    if item.has_space_after() {
                        write!(f, " ")?;
                    }
                }

                Ok(())
            }
            FormatTree::StmtEnd => write!(f, ";"),
            FormatTree::TypeEnd => write!(f, ":"),
            FormatTree::Keyword(keyword) => write!(f, "{keyword}"),
            FormatTree::Space => {
                state.ident_on(f)?;
                write!(f, "\n")
            }
        }
    }

    fn has_space_before(&self) -> bool {
        match self {
            Self::Keyword(k) => match k {
                Keyword::Equal => true,
                Keyword::ResultArrow => true,
                Keyword::For => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn has_space_after(&self) -> bool {
        match self {
            //NOTE: for keywords we blacklist the _after_ thing, since most keywords take a _after_ space
            Self::Keyword(k) => match k {
                Keyword::Range => false,
                Keyword::ElseBranch => false,
                Keyword::AccessDot => false,
                Keyword::Concept => false,

                _ => true,
            },
            Self::TypeEnd => true,
            Self::Token(t) => match t.as_str() {
                "," => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl Display for Formater {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let state = FormatingState {
            indentation: self.config.indentation.clone(),
            ident_level: 0,
        };

        self.tree.format(f, &state)
    }
}
