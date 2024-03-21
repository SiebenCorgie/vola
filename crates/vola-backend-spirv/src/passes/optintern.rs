use ahash::AHashMap;
use rspirv::{
    dr::Builder,
    spirv::{FunctionControl, Word},
};
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{NodeType, StructuralNode},
    region::RegionLocation,
    smallvec::SmallVec,
    NodeRef,
};
use vola_opt::{common::Ty, Optimizer};

use crate::{error::BackendSpirvError, SpirvBackend};

mod alge_to_spirv;

pub(crate) struct InterningCtx {
    ///Maps a λ-Node to a function Id, which we can use to call the function once its defined
    node_fn_mapping: AHashMap<NodeRef, Word>,
    builder: Builder,
}

impl InterningCtx {
    fn type_void(&mut self) -> Word {
        self.builder.type_void()
    }
    fn type_scalar(&mut self) -> Word {
        self.builder.type_float(32)
    }

    fn type_vector(&mut self, width: usize) -> Word {
        let ty_scalar = self.type_scalar();
        self.builder
            .type_vector(ty_scalar, width.try_into().unwrap())
    }

    ///Builds a matrix of `width` with the height/line-count of the `vector_type`.
    fn type_matrix(&mut self, vector_type: Word, width: usize) -> Word {
        self.builder
            .type_matrix(vector_type, width.try_into().unwrap())
    }
    fn optty_to_spirvty(&mut self, optty: Ty) -> Word {
        match optty {
            Ty::Void => self.type_void(),
            //NOTE: always 32bit float atm
            Ty::Scalar => self.type_scalar(),
            Ty::Vector { width } => self.type_vector(width),
            Ty::Matrix { width, height } => {
                let ty_vec = self.type_vector(height);
                self.type_matrix(ty_vec, width)
            }
            Ty::Tensor { dim } => {
                panic!("Tensors not implemented in SPIR-V :(")
            }
            t => panic!("encountered unexpected type {t:?}"),
        }
    }
}

impl SpirvBackend {
    ///Interns the `opt` graph, by exploring all exported λ-nodes, and recursively interning all
    ///dependent nodes.
    ///
    pub(crate) fn intern(&mut self, opt: &Optimizer) -> Result<(), BackendSpirvError> {
        opt.dump_svg("ayay.svg", false);

        //take the internal module
        let builder = Builder::new_from_module(self.module.take().unwrap());

        let mut ctx = InterningCtx {
            node_fn_mapping: AHashMap::default(),
            builder,
        };

        //So for our interning to work, we basically checkout all exports, and their dependencies. We then build all dependent λ-nodes before building
        //their siblings. This makes sure they can call anything they need.

        //Collects all dependecies to a λ-node
        let mut lmd_dependencies: AHashMap<NodeRef, SmallVec<[NodeRef; 3]>> = AHashMap::default();

        //alright, seed all exports. NOTE that we build the export graph first, and then resolve it,
        //this lets us find cycles in the graph compared to _already adding to the module while traversing_.
        for expidx in 0..opt
            .graph
            .region(&opt.graph.toplevel_region())
            .unwrap()
            .results
            .len()
        {
            let src = if let Some(sr) = opt
                .graph
                .region(&opt.graph.toplevel_region())
                .unwrap()
                .result_src(&opt.graph, expidx)
            {
                if !lmd_dependencies.contains_key(&sr.node) {
                    sr
                } else {
                    continue;
                }
            } else {
                #[cfg(feature = "log")]
                log::warn!("export {} was not connected", expidx);
                continue;
            };

            Self::build_dependencies(&mut lmd_dependencies, opt, src.node);
        }

        //now do the boogie-wookie iterative dependency resolve thingy
        'resolver: loop {
            let mut changed_any = false;
            let mut waiting = AHashMap::with_capacity(lmd_dependencies.len());
            std::mem::swap(&mut lmd_dependencies, &mut waiting);

            //we define a λ-node as _resolveable_, if none of its dependecies are in the
            // waiting queue.
            'worker: for (w, deps) in &waiting {
                //TODO: Aww ... currently too dump to untangle this. But I hope the compiler
                //      does it for me :D
                let cloned_deps = deps.clone();
                for dep in deps {
                    //skip if any dependency is waiting by adding back to the lmd_dependencies
                    if waiting.contains_key(&dep) {
                        lmd_dependencies.insert(*w, cloned_deps);
                        continue 'worker;
                    }
                }

                //all dependecies are met, so lets build the λ-node as a function
                let callable = Self::intern_lmd(&mut ctx, opt, *w)?;
                //add it to the ctx
                let old = ctx.node_fn_mapping.insert(*w, callable);
                assert!(old.is_none());
                changed_any = true;
            }

            if !changed_any {
                //So if we are here, but there are nodes in the dependency graph, still,
                //then there is a dependency loop somewhere, so we bail.
                if lmd_dependencies.len() > 0 {
                    #[cfg(feature = "log")]
                    log::error!(
                        "Failed to resolve all λ-dependencies, there where still {} waiting",
                        lmd_dependencies.len()
                    );

                    #[cfg(debug_assertions)]
                    {
                        #[cfg(feature = "log")]
                        log::error!("dumping as \"LmdCycle.svg\"");

                        opt.dump_svg("LmdCycle.svg", true);
                    }

                    return Err(BackendSpirvError::Any {
                        text: format!("Failed to resolve all λ-Node dependencies!"),
                    });
                }

                break 'resolver;
            }
        }

        //put back the builder
        self.module = Some(ctx.builder.module());
        Ok(())
    }

    //Recursively builds all λ-node dependencies
    fn build_dependencies(
        lmd_dependencies: &mut AHashMap<NodeRef, SmallVec<[NodeRef; 3]>>,
        opt: &Optimizer,
        node: NodeRef,
    ) {
        assert!(opt.graph.node(node).node_type.is_lambda());

        //early return, since we passed by this node already
        if lmd_dependencies.contains_key(&node) {
            return;
        }

        let mut local_deps = SmallVec::default();
        let cvcount = opt
            .graph
            .node(node)
            .node_type
            .unwrap_lambda_ref()
            .context_variable_count();
        for cvidx in 0..cvcount {
            let cvsrc = if let Some(sr) = opt
                .graph
                .node(node)
                .inport(&InputType::ContextVariableInput(cvidx))
            {
                if let Some(edg) = sr.edge {
                    opt.graph.edge(edg).src().node
                } else {
                    #[cfg(featue = "log")]
                    log::warn!("cv input {} was not hooked up!", cvidx);
                    continue;
                }
            } else {
                #[cfg(feature = "log")]
                log::warn!("λ-Node context variable {} does not exist!", cvidx);
                continue;
            };

            //dispatch all cv-srcs then add them to the dependencies
            Self::build_dependencies(lmd_dependencies, opt, cvsrc);
            local_deps.push(cvsrc);
        }

        //now add the to table
        lmd_dependencies.insert(node, local_deps);
    }

    fn intern_lmd(
        ctx: &mut InterningCtx,
        opt: &Optimizer,
        node: NodeRef,
    ) -> Result<Word, BackendSpirvError> {
        //Change the builder into the _function_-mode, find all the arguments and CVs for that λ-Ctx
        //then start adding all the nodes we can discover
        let regloc = RegionLocation {
            node,
            region_index: 0,
        };
        let (return_type, return_src_node) = {
            assert!(opt.graph.region(&regloc).unwrap().results.len() == 1);
            let ty = opt
                .get_type_for_inport(InportLocation {
                    node: node,
                    input: InputType::Result(0),
                })
                .expect("Could not get λ-output type");

            let src_node = opt
                .graph
                .region(&regloc)
                .unwrap()
                .result_src(&opt.graph, 0)
                .unwrap();
            //Make sure its the first.
            assert!(src_node.output == OutputType::Output(0));
            (ty, src_node.node)
        };

        let mut arg_types: SmallVec<[Ty; 3]> = SmallVec::default();
        let mut argidx = 0;
        while let Some(arg) = opt
            .graph
            .node(node)
            .node_type
            .unwrap_lambda_ref()
            .argument(argidx)
        {
            //NOTE: It can happen that no edge is connected. In that case we can't derive a type
            //      so we use void in that case.

            let ty = if let Some(edg) = arg.edges.get(0) {
                opt.graph
                    .edge(*edg)
                    .ty
                    .get_type()
                    .cloned()
                    .expect("Expected edge to have type!")
            } else {
                #[cfg(feature = "log")]
                log::warn!("λ-Node arg({}) had no edge, using void as type", argidx);

                Ty::Void
            };
            arg_types.push(ty.clone());
            argidx += 1;
        }

        println!("{:?} = {:?}", return_type, arg_types);

        //convert the args and results to SPIR-V types, so we can build the function type
        let resty_spirv = ctx.optty_to_spirvty(return_type);
        let argtys: SmallVec<[Word; 3]> = arg_types
            .into_iter()
            .map(|t| ctx.optty_to_spirvty(t))
            .collect();

        //setup the function type.
        let fty = ctx.builder.type_function(resty_spirv, argtys.clone());
        //now build the function, by first setting up the parameters, and then building an
        //LmdCtx accordingly. Finaly start the first basic-block and change into the recursive builder.
        //once it returns, hookup the result and end the function.
        let start_id = ctx
            .builder
            .begin_function(
                resty_spirv,
                None,
                //Thats kinda cool right now. Since we do not read/write at all, we can tag them
                FunctionControl::PURE | FunctionControl::CONST,
                fty,
            )
            .unwrap();

        let mut lmdctx = LmdCtx {
            known_nodes: AHashMap::default(),
        };
        //now build the arguments, and record them in the context
        for (argidx, argty) in argtys.into_iter().enumerate() {
            let argid = ctx.builder.function_parameter(argty).unwrap();
            //now record
            let old = lmdctx.known_nodes.insert(
                OutportLocation {
                    node,
                    output: OutputType::Argument(argidx),
                },
                argid,
            );
            assert!(old.is_none());
        }
        //now begin the bb0
        ctx.builder.begin_block(None).unwrap();
        //and start recursion with our src node
        let return_id = Self::intern_node(ctx, &mut lmdctx, opt, return_src_node)?;
        //end the block by returning the value and ending the bb
        ctx.builder.ret_value(return_id).unwrap();
        ctx.builder.end_function().unwrap();
        //finally return the id of the function, so we can call it later on
        Ok(start_id)
    }

    ///Interns the λ-internal `node`. Returns the id it was interned under.
    /// Before interning itself, interns all dependecies
    fn intern_node(
        ctx: &mut InterningCtx,
        lmdctx: &mut LmdCtx,
        opt: &Optimizer,
        node: NodeRef,
    ) -> Result<Word, BackendSpirvError> {
        let outport = OutportLocation {
            node,
            output: OutputType::Output(0),
        };
        if let Some(known) = lmdctx.known_nodes.get(&outport) {
            return Ok(*known);
        }

        //dang, don't known ourselfs yet, so explore all dependecies, then add ourself
        //first step is to match the node type. Depending on it, we shall _do the right_
        // stuff. Right now we just expect calls and simple nodes.
        //
        // however in the future we might support loops and branches. So that would be handled here as well.
        // TODO: To myself: In that case it might make sense to operate on regions instead of λ-nodes.

        //dependecy exploration
        let dep_ids: SmallVec<[Option<Word>; 3]> = opt
            .graph
            .node(node)
            .input_edges()
            .into_iter()
            .map(|edg| {
                if let Some(edg) = edg {
                    let src = opt.graph.edge(edg).src();
                    assert!(
                        src.output == OutputType::Output(0),
                        "{:?} != Output(0)",
                        src.output
                    );
                    Some(Self::intern_node(ctx, lmdctx, opt, src.node).unwrap())
                } else {
                    None
                }
            })
            .collect();

        let id = match &opt.graph.node(node).node_type {
            NodeType::Simple(s) => {
                assert!(
                    s.node.dialect() == "alge",
                    "expected alge node, found {} node",
                    s.node.dialect()
                );

                ctx.intern_alge_node(opt, s)?
            }
            NodeType::Apply(a) => todo!(),
            other => panic!("unexpected node type {other:?}"),
        };

        //add to the mapping and return
        let old = lmdctx.known_nodes.insert(
            OutportLocation {
                node,
                output: OutputType::Output(0),
            },
            id,
        );
        assert!(old.is_none());

        todo!()
    }
}

struct LmdCtx {
    ///The ids of all output ports. Which are either λ-declerations (for CVs) or arguments (for Args duh).
    /// All _normal nodes_ are collected here as well, so that we don't duplicate anything.
    known_nodes: AHashMap<OutportLocation, Word>,
}
