/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Module that definse the OptNode and OptEdge related structures.

use core::panic;
use std::{any::Any, fmt::Debug};

use crate::error::OptError;
use crate::{common::Ty, Optimizer};
use ahash::AHashMap;
use rvsdg::attrib::AttribLocation;
use rvsdg::edge::OutportLocation;
use rvsdg::nodes::NodeType;
use rvsdg::region::RegionLocation;
use rvsdg::util::node_equality::NodeTypeEq;
use rvsdg::util::Path;
use rvsdg::NodeRef;
use rvsdg::{
    attrib::FlagStore, edge::LangEdge, nodes::LangNode, rvsdg_derive_lang::LangNode,
    util::copy::StructuralClone,
};

use rvsdg_viewer::{Color, View};
use vola_ast::csg::{CSGConcept, CSGNodeDef};
use vola_common::Span;

use crate::OptGraph;

pub(crate) mod impl_utils;

///A node of some dialect
pub trait DialectNode: LangNode + Any + View {
    ///Dialect identifier of this node.
    fn dialect(&self) -> &'static str;

    ///When presented with the given type map and graph, lets the implementation choose a type, if possible.
    ///
    /// Can return an error if an invalid configuration is detected. If the configuration is just incomplete, should return
    /// Ok(None). In that case the type resolution will try again later.
    fn try_derive_type(
        &self,
        _typemap: &FlagStore<Ty>,
        _graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        Err(OptError::Any {
            text: format!("Type resolution not implemented for {}", self.name()),
        })
    }

    ///Used to check if two nodes implement the same operation.
    fn is_operation_equal(&self, other: &OptNode) -> bool;

    ///Builds a structural copy of this node, where no inputs/outputs are connected.
    /// Needed to break up the `dyn` indirection in OptNode.
    fn structural_copy(&self, span: Span) -> OptNode;
}

///Single optimizer node of some dialect.4
#[derive(LangNode)]
pub struct OptNode {
    ///The source span this node originated from
    pub span: Span,
    ///The inner node that is being represented
    #[expose]
    pub node: Box<dyn DialectNode + Send + Sync + 'static>,
}

impl OptNode {
    pub fn new(node: impl DialectNode + Send + Sync + 'static, span: Span) -> Self {
        OptNode {
            span,
            node: Box::new(node),
        }
    }

    ///Tries to downcast the inner `node` to `T`.
    pub fn try_downcast_ref<'a, T: DialectNode + Send + Sync + 'static>(&'a self) -> Option<&'a T> {
        let upcast: &'a dyn Any = self.node.as_ref();
        upcast.downcast_ref()
    }
    ///Tries to downcast the inner `node` to `T`.
    pub fn try_downcast_mut<'a, T: DialectNode + Send + Sync + 'static>(
        &'a mut self,
    ) -> Option<&'a mut T> {
        let upcast: &'a mut dyn Any = self.node.as_mut();
        upcast.downcast_mut()
    }
}

impl StructuralClone for OptNode {
    fn structural_copy(&self) -> Self {
        self.node.structural_copy(self.span.clone())
    }
}

impl StructuralClone for OptEdge {
    fn structural_copy(&self) -> Self {
        self.clone()
    }
}

impl NodeTypeEq for OptNode {
    fn type_equal(&self, other: &OptNode) -> bool {
        //escape hatch
        if self.node.dialect() != other.node.dialect() {
            return false;
        }

        self.node.is_operation_equal(other)
    }
}

impl View for OptNode {
    fn color(&self) -> Color {
        self.node.color()
    }
    fn name(&self) -> String {
        self.node.name()
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        self.node.stroke()
    }
}

impl Debug for OptNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} . {}", self.node.dialect(), self.node.name())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeState {
    Set(Ty),
    Derived(Ty),
    Unset,
}

impl TypeState {
    pub fn get_type(&self) -> Option<Ty> {
        match self {
            Self::Set(t) | Self::Derived(t) => Some(t.clone()),
            Self::Unset => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum OptEdge {
    State,
    Value { ty: TypeState },
}

impl OptEdge {
    ///Sets an unset edge to an derived edge. Does nothing if the state is already
    /// `Set`.
    ///
    /// Panics if the state is already derived.
    //TODO: Make that nicer. Shouldn't happen thought. However, better panic then creating invalid state.
    pub fn set_derived_state(&mut self, ts: Ty) -> Result<(), OptError> {
        match self {
            OptEdge::State => panic!("Cannot set type on state edge"),
            OptEdge::Value { ty } => match ty {
                TypeState::Unset => {
                    *ty = TypeState::Derived(ts);
                    Ok(())
                }
                TypeState::Derived(t) => {
                    if t != &ts {
                        Err(OptError::TypeResolutionErrorDerive {
                            a: t.clone(),
                            b: ts,
                        })
                    } else {
                        Ok(())
                    }
                }
                TypeState::Set(t) => {
                    if t != &ts {
                        Err(OptError::TypeResolutionErrorSet {
                            set: t.clone(),
                            derive: ts,
                        })
                    } else {
                        Ok(())
                    }
                }
            },
        }
    }

    ///Returns the type of this edge, if possible. Is none for State edges and untyped value edges.
    pub fn get_type(&self) -> Option<&Ty> {
        match &self {
            OptEdge::State
            | OptEdge::Value {
                ty: TypeState::Unset,
            } => None,
            OptEdge::Value {
                ty: TypeState::Derived(t) | TypeState::Set(t),
            } => Some(t),
        }
    }

    ///Sets the type of this edge, if it is a Value edge. Does nothing if its a state
    /// edge.
    ///
    /// Returns the old type state
    pub fn set_type(&mut self, set_ty: Ty) -> Option<TypeState> {
        match self {
            Self::Value { ty } => {
                let mut new_type = TypeState::Set(set_ty);
                std::mem::swap(ty, &mut new_type);
                Some(new_type)
            }
            Self::State => None,
        }
    }

    pub fn with_type(mut self, ty: Ty) -> Self {
        self.set_type(ty);
        self
    }
}

impl LangEdge for OptEdge {
    fn state_edge() -> Self {
        Self::State
    }
    fn value_edge() -> Self {
        Self::Value {
            ty: TypeState::Unset,
        }
    }
    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value { .. } = self {
            true
        } else {
            false
        }
    }
}

impl View for OptEdge {
    fn color(&self) -> Color {
        match self {
            Self::State => Color::from_rgba(255, 0, 0, 255),
            Self::Value { ty } => match ty {
                TypeState::Unset => Color::from_rgba(255, 0, 255, 255),
                TypeState::Set(_) => Color::from_rgba(0, 255, 0, 255),
                TypeState::Derived(_) => Color::from_rgba(0, 0, 255, 255),
            },
        }
    }
    fn name(&self) -> String {
        match self {
            Self::State => "State".to_owned(),
            Self::Value { ty } => match ty {
                TypeState::Derived(t) => format!("Value(derived {:?})", t),
                TypeState::Set(t) => format!("Value(set {:?})", t),
                TypeState::Unset => format!("Value(unset)"),
            },
        }
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        match self {
            Self::State => rvsdg_viewer::Stroke::Line,
            Self::Value { .. } => rvsdg_viewer::Stroke::Line,
        }
    }
}

impl Optimizer {
    //Tries to find a span for this node or port.
    //This'll first check if a span tag is active, if not,
    //tries to use the simple-node's span, or tries one of the porst
    pub fn find_span(&self, loc: AttribLocation) -> Option<Span> {
        if let Some(s) = self.span_tags.get(&loc) {
            return Some(s.clone());
        }

        match loc {
            AttribLocation::Node(n) => {
                if let NodeType::Simple(sn) = &self.graph.node(n).node_type {
                    return Some(sn.span.clone());
                }

                for output in self.graph.node(n).outport_types() {
                    if let Some(s) = self.find_span(OutportLocation { node: n, output }.into()) {
                        return Some(s);
                    }
                }

                None
            }
            _ => None,
        }
    }

    ///Utility that tries to find type information in the vicinity of `loc`
    pub fn find_type(&self, loc: &AttribLocation) -> Option<Ty> {
        if let Some(t) = self.typemap.get(loc) {
            return Some(t.clone());
        }

        //try the _vicinity_ This means the ports of an edge, or all edges that are connected to a port.
        match loc {
            AttribLocation::InPort(portloc) => {
                if let Some(edg) = self
                    .graph
                    .node(portloc.node)
                    .inport(&portloc.input)
                    .unwrap()
                    .edge
                {
                    self.graph.edge(edg).ty.get_type().cloned()
                } else {
                    None
                }
            }
            AttribLocation::OutPort(portloc) => {
                //check if any of the edges has a type for us. We actually unify all edges and
                //panic if we encounter inconsistensies.
                let mut unified_type = None;
                for edg in &self
                    .graph
                    .node(portloc.node)
                    .outport(&portloc.output)
                    .unwrap()
                    .edges
                {
                    match self.graph.edge(*edg).ty.get_type() {
                        Some(new_ty) => {
                            if let Some(set_ty) = &unified_type {
                                if new_ty != set_ty {
                                    panic!("Found edge inconsistensy on edge {edg}. There are two edge types on the same port: {new_ty:?} & {set_ty:?}" )
                                }
                            } else {
                                unified_type = Some(new_ty.clone());
                            }
                        }
                        None => {}
                    }
                }
                unified_type
            }
            AttribLocation::Region(_) => None,
            AttribLocation::Node(_) => None,
            AttribLocation::Edge(edg) => {
                if let Some(t) = self.find_type(&self.graph.edge(*edg).src().into()) {
                    return Some(t);
                }
                self.find_type(&self.graph.edge(*edg).dst().into())
            }
        }
    }

    pub fn node_name(&self, node: NodeRef) -> String {
        if let Some(name) = self.names.get(&node.into()) {
            return name.clone();
        }

        match &self.graph.node(node).node_type {
            rvsdg::nodes::NodeType::Simple(s) => s.name(),
            other => format!("{other:?}"),
        }
    }

    pub fn outport_in_region(&self, region: RegionLocation, port: OutportLocation) -> bool {
        //If the port is argument-like, and part of the region's node, this is true
        if region.node == port.node && port.output.is_argument() {
            return true;
        }

        //Otherwise the node needs to be in the region, and be result like
        self.graph
            .region(&region)
            .unwrap()
            .nodes
            .contains(&port.node)
            && !port.output.is_argument()
    }

    pub fn find_path_type(&self, path: &Path) -> Result<Ty, OptError> {
        if let Some(start_type) = self.find_type(&path.start.into()) {
            return Ok(start_type);
        }
        if let Some(end_type) = self.find_type(&path.end.into()) {
            return Ok(end_type);
        }

        for edg in &path.edges {
            if let Some(ty) = self.find_type(&edg.into()) {
                return Ok(ty);
            }
        }

        //if we didn't find anything yet, try all ports instead
        for edg in &path.edges {
            let src = self.graph.edge(*edg).src().clone();
            if let Some(t) = self.find_type(&src.into()) {
                return Ok(t);
            }
            let dst = self.graph.edge(*edg).dst().clone();
            if let Some(t) = self.find_type(&dst.into()) {
                return Ok(t);
            }
        }
        Err(OptError::NotTypeOnPath)
    }

    ///types the `path` of edges based on any found type information it can find on that path.
    ///
    /// If successful, return the type that was set on the path.
    pub fn type_path(&mut self, path: &Path) -> Result<Ty, OptError> {
        //We first try the start and end port. If we can't find anything on there,
        //we walk the path and try to find anything.
        //if that ain't working as well, we gotta return with an error

        let found_type = self.find_path_type(&path)?;

        for edg in &path.edges {
            self.graph.edge_mut(*edg).ty.set_type(found_type.clone());
        }

        Ok(found_type)
    }

    ///Imports the `src` into `target_region`. Also takes care of type-setting any created paths
    pub fn import_context(
        &mut self,
        src: OutportLocation,
        target_region: RegionLocation,
    ) -> Result<OutportLocation, OptError> {
        let (out, path) = self.graph.import_context(src, target_region)?;
        if let Some(p) = path {
            match self.type_path(&p) {
                Ok(ty) => {
                    for edg in p.edges {
                        self.graph.edge_mut(edg).ty.set_type(ty.clone());
                    }
                }
                //Just report that one, but that can happen
                Err(OptError::NotTypeOnPath) => {
                    log::error!("{}", OptError::NotTypeOnPath);
                }
                Err(other) => return Err(other),
            }
        }
        Ok(out)
    }

    ///Imports the `src` into `target_region`. Also takes care of type-setting any created paths
    pub fn import_argument(
        &mut self,
        src: OutportLocation,
        target_region: RegionLocation,
    ) -> Result<OutportLocation, OptError> {
        let (out, path) = self.graph.import_argument(src, target_region)?;
        if let Some(p) = path {
            match self.type_path(&p) {
                Ok(_) => {}
                //Just report that one, but that can happen
                Err(OptError::NotTypeOnPath) => {
                    log::warn!("{}", OptError::NotTypeOnPath);
                }
                Err(other) => return Err(other),
            }
        }
        Ok(out)
    }
}
