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
///If you want to mutate the AST, see [AstTransformer].
#[allow(unused_variables)]
pub trait AstVisitor {
    fn root(&mut self, root: &VolaAst) {}
    fn toplevel(&mut self, tl: &TopLevelNode) {}
    fn entry(&mut self, entry: &AstEntry) {}
    fn module(&mut self, tl: &Module) {}
    fn comment(&mut self, comment: &Comment) {}
    fn concept(&mut self, csgconcept: &CsgConcept) {}
    fn csg_def(&mut self, csgdef: &CsgDef) {}
    fn implblock(&mut self, implblock: &ImplBlock) {}
    fn func(&mut self, func: &Func) {}
    fn block(&mut self, block: &Block) {}
    fn stmt(&mut self, stmt: &Stmt) {}
    fn assign_stmt(&mut self, assign: &AssignStmt) {}
    fn let_stmt(&mut self, let_stmt: &LetStmt) {}
    fn csg_stmt(&mut self, csg_stmt: &CsgStmt) {}
    fn branch(&mut self, branch: &Branch) {}
    fn loop_stmt(&mut self, loop_stmt: &Loop) {}
    fn expr(&mut self, entry: &Expr) {}
}

impl VolaAst {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.root(&self);
        for inner in &self.entries {
            inner.traverse_visit(visitor);
        }
    }
}

impl TopLevelNode {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.toplevel(&self);
        self.entry.traverse_visit(visitor);
    }
}

impl AstEntry {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.entry(&self);
        match self {
            Self::Comment(c) => c.traverse_visit(visitor),
            Self::Concept(concept) => concept.traverse_visit(visitor),
            Self::CsgDef(def) => def.traverse_visit(visitor),
            Self::ImplBlock(block) => block.traverse_visit(visitor),
            Self::Func(f) => f.traverse_visit(visitor),
            Self::Module(m) => m.traverse_visit(visitor),
        }
    }
}

impl Module {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.module(&self)
    }
}

impl Comment {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.comment(&self)
    }
}
impl CsgConcept {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.concept(&self)
    }
}
impl CsgDef {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.csg_def(&self);
    }
}
impl ImplBlock {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.implblock(&self);
        self.block.traverse_visit(visitor)
    }
}
impl Func {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.func(&self);
        self.block.traverse_visit(visitor)
    }
}
impl Block {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.block(&self);
        for stmt in &self.stmts {
            stmt.traverse_visit(visitor);
        }
        if let Some(expr) = &self.retexpr {
            expr.traverse_visit(visitor)
        }
    }
}
impl Stmt {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.stmt(&self);
        match self {
            Stmt::Assign(assign) => assign.traverse_visit(visitor),
            Stmt::Let(l) => l.traverse_visit(visitor),
            Stmt::Block(blk) => blk.traverse_visit(visitor),
            Stmt::Branch(b) => b.traverse_visit(visitor),
            Stmt::Loop(l) => l.traverse_visit(visitor),
            Stmt::Comment(c) => c.traverse_visit(visitor),
            Stmt::Csg(csg) => csg.traverse_visit(visitor),
        }
    }
}

impl AssignStmt {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.assign_stmt(&self);
        self.expr.traverse_visit(visitor)
    }
}
impl LetStmt {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.let_stmt(&self);
        self.expr.traverse_visit(visitor)
    }
}
impl CsgStmt {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.csg_stmt(&self);
        self.expr.traverse_visit(visitor)
    }
}

impl Branch {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.branch(&self);
        self.conditional.0.traverse_visit(visitor);
        self.conditional.1.traverse_visit(visitor);
        if let Some(uncond) = &self.unconditional {
            uncond.traverse_visit(visitor);
        }
    }
}

impl Loop {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.loop_stmt(&self);
        self.bound_lower.traverse_visit(visitor);
        self.bound_upper.traverse_visit(visitor);
        self.body.traverse_visit(visitor);
    }
}

impl Expr {
    pub fn traverse_visit<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.expr(&self);
        match &self.expr_ty {
            ExprTy::Unary { operand, .. } => operand.traverse_visit(visitor),
            ExprTy::Binary { left, right, .. } => {
                left.traverse_visit(visitor);
                right.traverse_visit(visitor);
            }
            ExprTy::Call(c) => {
                for arg in &c.args {
                    arg.traverse_visit(visitor);
                }
            }
            ExprTy::Branch(b) => b.traverse_visit(visitor),
            ExprTy::Eval(ev) => {
                for p in &ev.params {
                    p.traverse_visit(visitor);
                }
            }
            ExprTy::List(l) => {
                for le in l {
                    le.traverse_visit(visitor);
                }
            }
            ExprTy::ScopedCall(sc) => {
                for callarg in &sc.call.args {
                    callarg.traverse_visit(visitor);
                }
                for block in &sc.blocks {
                    block.traverse_visit(visitor);
                }
            }
            ExprTy::Splat { expr, .. } => expr.traverse_visit(visitor),
            ExprTy::Tuple(elements) => {
                for e in elements {
                    e.traverse_visit(visitor);
                }
            }
            ExprTy::FieldAccess { .. } | ExprTy::Ident(_) | ExprTy::Literal(_) => {}
        }
    }
}
