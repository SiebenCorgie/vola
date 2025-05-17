/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */
use crate::{
    alge::{AssignStmt, Expr, ExprTy, Func, LetStmt},
    common::{Block, Branch, Comment, Loop, Stmt},
    csg::{CsgConcept, CsgDef, CsgStmt, ImplBlock},
    AstEntry, Module, TopLevelNode, VolaAst,
};

///Allows an implementor to traverse the AST, which calls the respective methode on each node.
#[allow(unused_variables)]
pub trait AstTransformer {
    fn root(&mut self, root: &mut VolaAst) {}
    fn toplevel(&mut self, tl: &mut TopLevelNode) {}
    fn entry(&mut self, entry: &mut AstEntry) {}
    fn module(&mut self, tl: &mut Module) {}
    fn comment(&mut self, comment: &mut Comment) {}
    fn concept(&mut self, csgconcept: &mut CsgConcept) {}
    fn csg_def(&mut self, csgdef: &mut CsgDef) {}
    fn implblock(&mut self, implblock: &mut ImplBlock) {}
    fn func(&mut self, func: &mut Func) {}
    fn block(&mut self, block: &mut Block) {}
    fn stmt(&mut self, stmt: &mut Stmt) {}
    fn assign_stmt(&mut self, assign: &mut AssignStmt) {}
    fn let_stmt(&mut self, let_stmt: &mut LetStmt) {}
    fn csg_stmt(&mut self, csg_stmt: &mut CsgStmt) {}
    fn branch(&mut self, branch: &mut Branch) {}
    fn loop_stmt(&mut self, loop_stmt: &mut Loop) {}
    fn expr(&mut self, entry: &mut Expr) {}
}

impl VolaAst {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.root(self);
        for inner in &mut self.entries {
            inner.traverse_trans(transformer);
        }
    }
}

impl TopLevelNode {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.toplevel(self);
        self.entry.traverse_trans(transformer);
    }
}

impl AstEntry {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.entry(self);
        match self {
            Self::Comment(c) => c.traverse_trans(transformer),
            Self::Concept(concept) => concept.traverse_trans(transformer),
            Self::CsgDef(def) => def.traverse_trans(transformer),
            Self::ImplBlock(block) => block.traverse_trans(transformer),
            Self::Func(f) => f.traverse_trans(transformer),
            Self::Module(m) => m.traverse_trans(transformer),
        }
    }
}

impl Module {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.module(self)
    }
}

impl Comment {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.comment(self)
    }
}
impl CsgConcept {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.concept(self)
    }
}
impl CsgDef {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.csg_def(self);
    }
}
impl ImplBlock {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.implblock(self);
        self.block.traverse_trans(transformer)
    }
}
impl Func {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.func(self);
        self.block.traverse_trans(transformer)
    }
}
impl Block {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.block(self);
        for stmt in &mut self.stmts {
            stmt.traverse_trans(transformer);
        }
        if let Some(expr) = &mut self.retexpr {
            expr.traverse_trans(transformer)
        }
    }
}
impl Stmt {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.stmt(self);
        match self {
            Stmt::Assign(assign) => assign.traverse_trans(transformer),
            Stmt::Let(l) => l.traverse_trans(transformer),
            Stmt::Block(blk) => blk.traverse_trans(transformer),
            Stmt::Branch(b) => b.traverse_trans(transformer),
            Stmt::Loop(l) => l.traverse_trans(transformer),
            Stmt::Comment(c) => c.traverse_trans(transformer),
            Stmt::Csg(csg) => csg.traverse_trans(transformer),
        }
    }
}

impl AssignStmt {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.assign_stmt(self);
        self.expr.traverse_trans(transformer)
    }
}
impl LetStmt {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.let_stmt(self);
        self.expr.traverse_trans(transformer)
    }
}
impl CsgStmt {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.csg_stmt(self);
        self.expr.traverse_trans(transformer)
    }
}

impl Branch {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.branch(self);
        self.conditional.0.traverse_trans(transformer);
        self.conditional.1.traverse_trans(transformer);
        if let Some(uncond) = &mut self.unconditional {
            uncond.traverse_trans(transformer);
        }
    }
}

impl Loop {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.loop_stmt(self);
        self.bound_lower.traverse_trans(transformer);
        self.bound_upper.traverse_trans(transformer);
        self.body.traverse_trans(transformer);
    }
}

impl Expr {
    pub fn traverse_trans<T: AstTransformer>(&mut self, transformer: &mut T) {
        transformer.expr(self);
        match &mut self.expr_ty {
            ExprTy::Unary { operand, .. } => operand.traverse_trans(transformer),
            ExprTy::Binary { left, right, .. } => {
                left.traverse_trans(transformer);
                right.traverse_trans(transformer);
            }
            ExprTy::Call(c) => {
                for arg in &mut c.args {
                    arg.traverse_trans(transformer);
                }
            }
            ExprTy::Branch(b) => b.traverse_trans(transformer),
            ExprTy::Eval(ev) => {
                for p in &mut ev.params {
                    p.traverse_trans(transformer);
                }
            }
            ExprTy::List(l) => {
                for le in l {
                    le.traverse_trans(transformer);
                }
            }
            ExprTy::ScopedCall(sc) => {
                for callarg in &mut sc.call.args {
                    callarg.traverse_trans(transformer);
                }
                for block in &mut sc.blocks {
                    block.traverse_trans(transformer);
                }
            }
            ExprTy::Splat { expr, .. } => expr.traverse_trans(transformer),
            ExprTy::Tuple(elements) => {
                for e in elements {
                    e.traverse_trans(transformer);
                }
            }
            ExprTy::FieldAccess { .. } | ExprTy::Ident(_) | ExprTy::Literal(_) => {}
            ExprTy::Cast { expr, .. } => expr.traverse_trans(transformer),
        }
    }
}
