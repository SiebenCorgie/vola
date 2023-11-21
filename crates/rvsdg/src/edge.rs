use crate::{
    nodes::{LangNode, Node},
    NodeRef,
};

///Port index references a certain type of port on a node. Note that
/// [Node](crate::nodes::Node) implements `Index`/`IndexMut` for that type, as well as `get()`, `get_mut`, `try_get()`, `try_get_mut()`.
///
/// On a lower level we copy the exact addressing scheme of the source paper. Some port indices can overlab, for instance
/// on a loop node `Input(1)` might also be `EntryVar(var_index = 0, tuple_index=None)`.
#[derive(Debug, Clone)]
pub enum PortIndex {
    Input(usize),
    Output(usize),
    ///Represents the context_variable `var_index`, and the `tuple_index`-th element of the ContexVar tuple defined in _Definition 4, 6, 7_
    /// of the source paper. The tuple is (INPUT=0, ARGUMENT=1).
    ///
    /// # Understanding context variables
    ///
    ///ContextVariables basically represent functions and arguments that are used _internally_ in this λ-node/function.
    /// For instance consider this code:
    /// ```rust
    /// fn max(a, b) -> int{...}
    ///
    /// fn f(a, b) -> int {
    ///     println("max");
    ///     return max(a, b);
    /// }
    /// ```
    ///
    /// In this case we need to _know_ that we import `max` from our _translation unit_, but we need to import `println`
    /// from an external source. We also need to known that we depend on the global value `"max"`.
    ///
    /// For a better/more visual explanation, see Figure 3.a in the source paper.
    ContextVar {
        var_index: usize,
        ///Must be either 0 or 1
        tuple_index: usize,
    },

    ///Represents the recursion_variable `var_index`, and the `tuple_index`-th element of the RV tuple defined in _Definition 8_
    /// of the source paper.
    ///
    ///
    /// # Understanding recursion variables
    ///
    ///Represent the argument stream to a recursion, as well as the output of the recursion.
    ///
    /// Consider the following:
    /// ```rust
    /// fn f(a: int) -> int{
    ///     if a <= 1{ return; }
    ///     return f(a - 1) + f(a - 2);
    /// }
    /// ```
    ///
    /// Then the recursion variable is made out of `a` as well as the result of `f()`.
    RecursionVar {
        var_index: usize,
        ///Must be $<=2$
        tuple_index: usize,
    },
    ///Represents the entry-variable of a [γ-node](crate::nodes::GammaNode) as defined in _Definition 1_ of the source paper.
    EntryVar {
        ///Denotes the n-th entry variable
        var_index: usize,
        ///is the input port of EV (tuple element 0 in the definition), or the ev-argument of the `region-index`-th inner-region of the node.
        ///
        /// Must be None, or < region count of the GammaNode.
        region_index: Option<usize>,
    },

    ///Represents the exit-variable of a [γ-node](crate::nodes::GammaNode) as defined in _Definition 2_ of the source paper.
    ExitVar {
        ///Denotes the n-th exit variable
        var_index: usize,
        ///is the output port of EV (last tuple element in the definition), or the ex-result of the `region-index`-th inner-region of the node.
        ///
        /// Must be None, or < region count of the GammaNode.
        region_index: Option<usize>,
    },

    /// The predicate of a [γ-Node](crate::nodes::GammaNode) or an [θ-Node][crate::nodes::ThetaNode].
    Predicate,

    ///Represents the LoopVariable of a [θ-Node](crate::nodes::ThetaNode) as described in _Definition 3_ of the source paper.
    ///
    /// We address the `var_index`-th loop variable, where `tuple_index` corresponds to the tuple
    /// (input=0, argument=1, result=2, output=3) described in the paper.
    LoopVar {
        var_index: usize,
        ///Must be $< 4$.
        tuple_index: usize,
    },

    ///This port type is not directly described in the paper, but means any _normal_ input, that is not part of a variable. So in the case of GammaNode for instance everything
    /// after EntryVars and the predicate.
    StdInput(usize),
    ///This port type is not directly described in the paper, but means any _normal_ argument to some nodes body/inner-region, that is not part of a variable. So in the case of a [λ-Node](crate::nodes::LambdaNode), any argument to
    /// that function's body that is not an context variable.
    StdArg {
        subregion: usize,
        arg_idx: usize,
    },
    ///This port type is not directly described in the paper, but means any _normal_ result of some nodes body/inner-region, that is not part of a variable. So in the case of a [λ-Node](crate::nodes::LambdaNode), any result of the node's body.
    StdResult {
        subregion: usize,
        arg_idx: usize,
    },
    ///This port type is not directly described in the paper, but means any _normal_ output of some node, that is not part of a variable. There are only two cases where this can happen: Any output of a _SimpleNode_, _ApplyNode_ or _OmegaNode_.
    /// All other nodes use their output as part of some kind of variable.
    StdOutput(usize),
}

//TODO: there are multiple invariants we could check here. bound checking and legalisation (lambda can only have one output, apply has always at least one input etc.)
impl PortIndex {
    ///Calculates the addressed port based on the node type and the input or output array length. The calculation results
    /// from the papers definitions 1-8.
    ///
    /// Returns None if the port (or port type) does not exist. This can happen for instance if you try to index a SimpleNode with an
    /// ContextVar.
    pub fn into_location<N: LangNode + 'static>(&self, node: &Node<N>) -> Option<PortLocation> {
        match &self {
            PortIndex::Input(i) => Some(PortLocation::Inputs(*i)),
            PortIndex::Output(o) => Some(PortLocation::Outputs(*o)),
            PortIndex::ContextVar {
                var_index,
                tuple_index,
            } => {
                assert!(
                    *tuple_index <= 1,
                    "ContextVaribale index l must be <= 1, but was {}.",
                    tuple_index
                );

                //Only those nodes have a CV. CVs are generally the first n (n == |CV|) inputs and arguments of a architectural node and its inner body.
                match node {
                    Node::Lambda(_l) => {
                        if *tuple_index == 0 {
                            Some(PortLocation::Inputs(*var_index))
                        } else {
                            //Try to get the inner region's argument
                            Some(PortLocation::Arguments {
                                subregion: 0,
                                arg_idx: *var_index,
                            })
                        }
                    }
                    Node::Delta(_d) => {
                        if *tuple_index == 0 {
                            Some(PortLocation::Inputs(*var_index))
                        } else {
                            //Try to get the inner region's argument
                            Some(PortLocation::Arguments {
                                subregion: 0,
                                arg_idx: *var_index,
                            })
                        }
                    }
                    Node::Phi(_p) => {
                        if *tuple_index == 0 {
                            Some(PortLocation::Inputs(*var_index))
                        } else {
                            //Try to get the inner region's argument
                            Some(PortLocation::Arguments {
                                subregion: 0,
                                arg_idx: *var_index,
                            })
                        }
                    }
                    _ => None, //TODO: emit a warning or error?
                }
            }
            PortIndex::RecursionVar {
                var_index,
                tuple_index,
            } => {
                if let Node::Phi(p) = node {
                    assert!(
                        *tuple_index < 3,
                        "RecursionVariable has only 3 elements, tried to access {}",
                        *tuple_index
                    );
                    match tuple_index {
                        0 => Some(PortLocation::Results {
                            subregion: 0,
                            arg_idx: *var_index,
                        }),
                        1 => Some(PortLocation::Arguments {
                            subregion: 0,
                            arg_idx: p.cv_count + var_index,
                        }),
                        2 => Some(PortLocation::Outputs(*var_index)),
                        _ => panic!("indexed RVargs out of bounds"),
                    }
                } else {
                    None
                }
            }
            PortIndex::EntryVar {
                var_index,
                region_index,
            } => {
                if let Node::Gamma(g) = node {
                    if let Some(regidx) = region_index {
                        //Try accessing argument of `regindex` region
                        assert!(
                            *regidx < g.regions.len(),
                            "Tried to access region {} of Gamma/Branch node, but has only {} regions",
                            regidx,
                            g.regions.len()
                        );
                        Some(PortLocation::Arguments {
                            subregion: *regidx,
                            arg_idx: *var_index,
                        })
                    } else {
                        //accessing input
                        Some(PortLocation::Inputs(1 + *var_index))
                    }
                } else {
                    None
                }
            }
            PortIndex::ExitVar {
                var_index,
                region_index,
            } => {
                if let Node::Gamma(g) = node {
                    if let Some(regidx) = region_index {
                        //Try accessing argument of `regindex` region
                        assert!(
                            *regidx < g.regions.len(),
                            "Tried to access region {} of Gamma/Branch node, but has only {} regions",
                            regidx,
                            g.regions.len()
                        );

                        Some(PortLocation::Results {
                            subregion: *regidx,
                            arg_idx: *var_index,
                        })
                    } else {
                        //accessing input
                        Some(PortLocation::Outputs(1 + *var_index))
                    }
                } else {
                    None
                }
            }
            PortIndex::LoopVar {
                var_index,
                tuple_index,
            } => {
                if let Node::Theta(_t) = node {
                    assert!(
                        *tuple_index < 4,
                        "Tried to index LoopVar tuple >=4, but has only 4 elements"
                    );

                    match tuple_index {
                        0 => Some(PortLocation::Inputs(*var_index)),
                        1 => Some(PortLocation::Arguments {
                            subregion: 0,
                            arg_idx: *var_index,
                        }),
                        2 => Some(PortLocation::Results {
                            subregion: 0,
                            arg_idx: *var_index + 1,
                        }),
                        3 => Some(PortLocation::Outputs(*var_index)),
                        _ => panic!("LoopVar out of bounds"),
                    }
                } else {
                    None
                }
            }
            PortIndex::Predicate => match &node {
                Node::Theta(_t) => Some(PortLocation::Inputs(0)),
                Node::Gamma(_t) => Some(PortLocation::Results {
                    subregion: 0,
                    arg_idx: 0,
                }),
                _ => None,
            },
            PortIndex::StdInput(inp) => match &node {
                Node::Simple(_node) => Some(PortLocation::Inputs(*inp)),
                Node::Gamma(g) => Some(PortLocation::Inputs(g.entry_var_count + inp)),
                Node::Theta(g) => Some(PortLocation::Inputs(g.lv_count + inp)),
                Node::Lambda(g) => Some(PortLocation::Inputs(g.cv_count + inp)),
                //NOTE: first is the to-be-applied function
                Node::Apply(_g) => Some(PortLocation::Inputs(*inp + 1)),
                Node::Delta(g) => Some(PortLocation::Inputs(g.cv_count + inp)),
                Node::Phi(g) => Some(PortLocation::Inputs(g.cv_count + inp)),
                Node::Omega(_g) => Some(PortLocation::Inputs(*inp)),
                Node::Invalid => None,
            },
            PortIndex::StdArg { subregion, arg_idx } => {
                //Check that non-gamma nodes do not access > 0 blocks.
                if !(*subregion > 0
                    && (if let Node::Gamma(_) = node {
                        true
                    } else {
                        false
                    }))
                {
                    return None;
                }

                match node {
                    Node::Simple(_node) => None,
                    Node::Gamma(g) => Some(PortLocation::Arguments {
                        subregion: *subregion,
                        arg_idx: g.entry_var_count + arg_idx,
                    }),
                    Node::Theta(g) => Some(PortLocation::Arguments {
                        subregion: 0,
                        arg_idx: *arg_idx + g.lv_count,
                    }),
                    Node::Lambda(g) => Some(PortLocation::Arguments {
                        subregion: 0,
                        arg_idx: *arg_idx + g.cv_count,
                    }),
                    Node::Apply(_g) => None,
                    Node::Delta(g) => Some(PortLocation::Arguments {
                        subregion: 0,
                        arg_idx: arg_idx + g.cv_count,
                    }),
                    Node::Phi(g) => Some(PortLocation::Arguments {
                        subregion: 0,
                        arg_idx: g.cv_count + g.rv_count + arg_idx,
                    }),
                    Node::Omega(_g) => Some(PortLocation::Arguments {
                        subregion: 0,
                        arg_idx: *arg_idx,
                    }),
                    Node::Invalid => None,
                }
            }
            PortIndex::StdResult { subregion, arg_idx } => {
                //Check that non-gamma nodes do not access > 0 blocks.
                if !(*subregion > 0
                    && (if let Node::Gamma(_) = node {
                        true
                    } else {
                        false
                    }))
                {
                    return None;
                }

                match node {
                    Node::Simple(_node) => None,
                    Node::Gamma(g) => Some(PortLocation::Results {
                        subregion: *subregion,
                        arg_idx: g.exit_var_count + *arg_idx,
                    }),
                    Node::Theta(g) => Some(PortLocation::Results {
                        subregion: 0,
                        arg_idx: *arg_idx + g.lv_count + 1,
                    }),
                    Node::Lambda(_g) => Some(PortLocation::Results {
                        subregion: 0,
                        arg_idx: *arg_idx,
                    }),
                    Node::Apply(_g) => None,
                    Node::Delta(_g) => Some(PortLocation::Results {
                        subregion: 0,
                        arg_idx: *arg_idx, //TODO could already return None for result > 0
                    }),
                    Node::Phi(g) => Some(PortLocation::Results {
                        subregion: 0,
                        arg_idx: g.rv_count + arg_idx,
                    }),
                    Node::Omega(_g) => Some(PortLocation::Results {
                        subregion: 0,
                        arg_idx: *arg_idx,
                    }),
                    Node::Invalid => None,
                }
            }
            PortIndex::StdOutput(out) => match &node {
                Node::Simple(_node) => Some(PortLocation::Outputs(*out)),
                Node::Gamma(g) => Some(PortLocation::Outputs(g.exit_var_count + *out)),
                Node::Theta(_g) => Some(PortLocation::Outputs(*out)),
                Node::Lambda(_g) => Some(PortLocation::Outputs(*out)),
                Node::Apply(_g) => Some(PortLocation::Outputs(*out)),
                Node::Delta(_g) => Some(PortLocation::Outputs(*out)),
                Node::Phi(_g) => Some(PortLocation::Outputs(*out)),
                Node::Omega(_g) => Some(PortLocation::Outputs(*out)),
                Node::Invalid => None,
            },
        }
    }
}

///Lowerlevel routing location of some [PortIndex]. You should usually not have to build that yourself.
//TODO: Encapsulate and make crate private
#[derive(Debug, Clone)]
pub enum PortLocation {
    Inputs(usize),
    Outputs(usize),
    Arguments { subregion: usize, arg_idx: usize },
    Results { subregion: usize, arg_idx: usize },
}

impl PortLocation {
    ///Assumes that this is an input location, unwrapping into the index of the input array.
    pub fn unwrap_input(&self) -> usize {
        if let PortLocation::Inputs(i) = self {
            *i
        } else {
            panic!("Was not PortLocation::Input");
        }
    }
    ///Assumes that this is an output location, unwrapping into the index of the output array.
    pub fn unwrap_output(&self) -> usize {
        if let PortLocation::Outputs(o) = self {
            *o
        } else {
            panic!("Was not PortLocation::Output");
        }
    }

    ///Assumes that this is the argument to some sub region of a node. Unwraps into `(region_index, region's argument index)`.
    pub fn unwrap_region_argument(&self) -> (usize, usize) {
        if let PortLocation::Arguments { subregion, arg_idx } = self {
            (*subregion, *arg_idx)
        } else {
            panic!("Was not PortLocation::Arguments")
        }
    }

    ///Assumes that this is the result to some sub region of a node. Unwraps into `(region_index, region's result index)`.
    pub fn unwrap_region_result(&self) -> (usize, usize) {
        if let PortLocation::Results { subregion, arg_idx } = self {
            (*subregion, *arg_idx)
        } else {
            panic!("Was not PortLocation::Results")
        }
    }
}

///An edge in a RVSDG is always a directed edge from `src` to `dst`. It contains a language specific
/// edge-type `E`, that represent the dependency type. In general there are at least `State` and `Value` edges, but a
/// language is free to represent more sophisticated dependencies through those types.
///
/// # Rational behind the src-index / dst-index.
///
/// On a surface level it's kinda shaky to store some _port-index_ on a node that can, potentially change its output configuration while being
/// build.
/// However, in practice the only type of port that frequently changes are context-, loop-, and recursion-variables, those variables are, however only ever
/// appended, never inserted. The input/output configuration is stable.
/// For instance a loop only has a single _real_ input, the loop-criteria. Or a hypothetical _simple_ `LoadOp` will always have
/// one input (the load address), and two outputs (load_value, and load_state). So when we reference the loaded value via index 0, this will
/// stay stable. Similarly, if we use input 0 of a loop and assume that it is the loop-criteria, this ought to be right.
pub struct Edge<E: LangEdge + 'static> {
    pub src: NodeRef,
    ///Index into the `src`-nodes output array.
    pub src_index: PortIndex,
    pub dst: NodeRef,
    ///Index into the `dst`-node input array.
    pub dst_index: PortIndex,
    pub ty: E,
}

pub trait LangEdge {
    ///Should create a
    fn value_edge() -> Self;
    fn state_edge() -> Self;
    fn is_value_edge(&self) -> bool;
    fn is_state_edge(&self) -> bool;
}
