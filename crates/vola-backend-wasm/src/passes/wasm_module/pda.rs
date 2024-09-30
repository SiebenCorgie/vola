/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Strategies to transform a CFG-Graph to a Stack-Machine based execution model.
//!
//! The idea is to wrap each instruction with loads for each argument, and a result-store.
//! The builder pre-allocates _enough_ local variables.

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::OutportLocation,
    nodes::StructuralNode,
    util::cfg::{Cfg, CfgNode},
    NodeRef, Rvsdg, SmallColl,
};
use walrus::{FunctionId, LocalId, Module, ModuleFunctions, ModuleLocals};

use crate::{
    graph::{WasmEdge, WasmNode, WasmTy},
    WasmBackend, WasmError,
};

use super::FunctionBuilderCtx;

struct HeapElement {
    //Possibly shaped wasm type this heap element refers to
    ty: WasmTy,
    //the actual indexes into the heap table for each element of the `ty`
    indices: SmallColl<(usize, LocalId)>,
}

///The helper that tracks the virtual heap for building a λ-node.
pub struct WasmLambdaBuilder {
    ///Mapps each entry to a wasm type
    heap_table: Vec<HeapElement>,
    ///Mapps each outport in the orginal graph to a entry in the heap table. Note that each element
    ///in the heap table can contain multiple actual local variables. This lets us map a Vec3
    /// to 3 element in the heap table.
    heap_map: AHashMap<OutportLocation, usize>,
    ///Tracks current heap size
    heap_size: usize,

    ctx: FunctionBuilderCtx,
}

impl WasmBackend {
    pub(super) fn init_sm_for_ctx(
        &self,
        ctx: FunctionBuilderCtx,
        module: &mut Module,
    ) -> WasmLambdaBuilder {
        let mut sm = WasmLambdaBuilder {
            heap_map: AHashMap::new(),
            heap_table: Vec::new(),
            heap_size: 0,
            ctx,
        };

        //To init, we need to offset the heap size for each input element and output element. We take
        //this to also init the heap-table for all argument-ports and output ports
        for arg in &sm.ctx.arguments {
            let heap_element = HeapElement {
                ty: arg.ty.clone(),
                //Offset each of the args's elements this is based on the WASM function convention.
                indices: (0..arg.len)
                    .map(|idx| {
                        let index = sm.heap_size + idx;
                        let id = module.locals.add(arg.ty.unwarp_walrus_ty());
                        (index, id)
                    })
                    .collect(),
            };

            //Insert the heap element
            let idx = sm.heap_table.len();
            sm.heap_table.push(heap_element);
            //now update the lookup table
            sm.heap_map.insert(arg.port, idx);
            //finally update the heap size
            sm.heap_size += arg.len;
        }

        //NOTE: we don't push the result indices _yet_ since those
        //      are only known once we allocated the locales for the CFG.

        sm
    }
}

impl WasmLambdaBuilder {
    ///Returns the index into the heap table for the allocated element.
    fn alloc_heap_element(&mut self, ty: WasmTy, module: &mut walrus::Module) -> usize {
        let element_count = ty.element_count();
        let element_indices = (0..element_count)
            .map(|idx| {
                let idx = self.heap_size + idx;
                let id = module.locals.add(ty.unwarp_walrus_ty());
                (idx, id)
            })
            .collect();
        self.heap_size += element_count;

        let element = HeapElement {
            ty,
            indices: element_indices,
        };

        let table_index = self.heap_table.len();
        self.heap_table.push(element);
        table_index
    }

    ///Get or set-up a port in the graph for the heap
    fn get_or_set_port(
        &mut self,
        port: OutportLocation,
        backend: &WasmBackend,
        module: &mut Module,
    ) -> usize {
        if let Some(cached) = self.heap_map.get(&port) {
            *cached
        } else {
            let ty = backend.outport_type(port).unwrap();
            //setup the heap element
            let lookup_index = self.alloc_heap_element(ty, module);
            self.heap_map.insert(port, lookup_index);
            lookup_index
        }
    }

    fn get_port(&self, port: OutportLocation) -> usize {
        if let Some(cached) = self.heap_map.get(&port) {
            *cached
        } else {
            panic!("Port had no assigned heap element")
        }
    }

    pub fn serialize_cfg(
        mut self,
        cfg: Cfg,
        backend: &WasmBackend,
        lambda: NodeRef,
        module: &mut Module,
    ) -> Result<FunctionId, WasmError> {
        //Iterate all nodes in order of the cfg and serialize them into the flat sequence of nodes.
        //while doing so, record the heap growth.
        let topoord = cfg.topological_order()?;

        //This pass just records all _simple_ nodes in order of occurence and allocates the correct
        //heap elements.
        /*
                println!("TOPORD:");
                for n in &topoord {
                    println!("    {:?}", cfg.nodes[*n]);
                }
        */
        for node in &topoord {
            match &cfg.nodes[*node] {
                CfgNode::BasicBlock(bb) => {
                    for node in &bb.nodes {
                        assert!(backend.graph[*node].node_type.is_simple());

                        for inp in backend.graph[*node].inport_types() {
                            //get all args. NOTE: those _have to have_ allocated elements already.
                            let producer = backend
                                .graph
                                .find_producer_inp(node.as_inport_location(inp))
                                .unwrap();
                            //println!("Producer: {:?}", producer);
                            assert!(self.heap_map.contains_key(&producer));
                        }

                        //alloc the node's result element
                        assert!(backend.graph[*node].outputs().len() == 1);
                        let result_port = node.output(0);
                        let _result_element = self.get_or_set_port(result_port, backend, module);
                    }
                }
                //All other nodes are ignored in this pass
                _ => {}
            }
        }
        /*
                println!("HeapTable:");
                for ele in &self.heap_table {
                    println!(
                        "    {:?} @ <{}>:{:?}",
                        ele.ty,
                        ele.indices.len(),
                        ele.indices
                    )
                }
        */
        //start the function builder
        let mut params = SmallColl::new();
        for arg in self.ctx.arguments.iter() {
            arg.ty.append_elements_to_signature(&mut params);
        }
        let mut results = SmallColl::new();
        for res in self.ctx.results.iter() {
            res.ty.append_elements_to_signature(&mut results);
        }

        //setup the export's type
        //let export_type_id = module.types.add(&params, &results);
        let mut fnbuilder = walrus::FunctionBuilder::new(&mut module.types, &params, &results);
        let mut builder = fnbuilder.func_body();

        //actually serialize all cfg nodes, including the loop / branch nodes via the walrus builder
        for cfgnode in &topoord {
            match &cfg.nodes[*cfgnode] {
                CfgNode::Null => {}
                CfgNode::Root(_) => {}
                CfgNode::BasicBlock(bb) => {
                    for node in &bb.nodes {
                        self.serialize_simple_node(
                            *node,
                            backend,
                            &mut builder,
                            &module.funcs,
                            &module.locals,
                        )?;
                    }
                }
                _ => todo!("CFG not implemented (yet)"),
            }
        }

        //Now setup the ordered load instructions for the result(s).
        for result in backend.graph[lambda].result_types(0) {
            let resultport = lambda.as_inport_location(result);
            let result_src = backend.graph.find_producer_inp(resultport).unwrap();
            //produce the loads for each result element
            self.emit_load(result_src, &mut builder);
        }

        //Now finish the walrus builder, and yeet the function into the actual module
        let mut args = Vec::new();
        for arg in self.ctx.arguments.iter() {
            let heap_element = self.get_port(arg.port);
            for (_, id) in self.heap_table[heap_element].indices.iter() {
                args.push(id.clone());
            }
        }
        let fnid = fnbuilder.finish(args, &mut module.funcs);

        Ok(fnid)
    }

    fn emit_load(&self, port: OutportLocation, builder: &mut walrus::InstrSeqBuilder) {
        //Based on the port's type, emit n loads
        // _in orde_
        let element = &self.heap_table[self.get_port(port)];
        for (_heap_index, id) in element.indices.iter() {
            let _ = builder.local_get(*id);
        }
    }

    fn emit_store(&self, port: OutportLocation, builder: &mut walrus::InstrSeqBuilder) {
        let element = &self.heap_table[self.get_port(port)];
        //NOTE: reverse the index elements, since this is a stack
        for (_hidx, id) in element.indices.iter().rev() {
            let _ = builder.local_set(*id);
        }
    }

    fn serialize_simple_node(
        &mut self,
        node: NodeRef,
        backend: &WasmBackend,
        builder: &mut walrus::InstrSeqBuilder,
        functions: &ModuleFunctions,
        locals: &ModuleLocals,
    ) -> Result<(), WasmError> {
        //Simple nodes, contrary to the name, are really the hardest here.
        //We have a couple of native ops, like ADD/ MUL etc. We _assume_ that those are already the correct
        //ones
        //
        //For all others we dispatch a runtime-call that calls the runtime's implementation of those ops.
        //
        //regardless, we first emit the op loads from heap

        assert!(backend.graph[node].node_type.is_simple());

        for inp in backend.graph[node].inport_types() {
            let src = backend
                .graph
                .find_producer_inp(node.as_inport_location(inp))
                .unwrap();
            //emit the loads
            self.emit_load(src, builder);
        }

        //now match the operation and emit it.
        match backend.graph[node].node_type.unwrap_simple_ref() {
            WasmNode::Unary(u) => {
                builder.unop(u.op.clone());
            }
            WasmNode::Binary(b) => {
                builder.binop(b.op.clone());
            }
            WasmNode::Value(v) => {
                builder.const_(v.op.clone());
            }
            WasmNode::Runtime(r) => {
                let input_types = backend.graph[node]
                    .inport_types()
                    .iter()
                    .map(|inp| {
                        let port = node.as_inport_location(*inp);
                        let edg = backend.graph[port].edge.unwrap();
                        backend.graph[edg].ty.type_or_undefined()
                    })
                    .collect::<SmallColl<_>>();

                let mut flattened_valty = SmallColl::new();
                for inty in input_types.iter() {
                    inty.append_elements_to_signature(&mut flattened_valty);
                }

                let extern_symbol = r.op.get_static_symbol_name(&input_types)?;
                let function_symbol =
                    crate::runtime::lookup_function_symbol(locals, functions, &extern_symbol);
                //While at it, verify that the input types match

                assert!(function_symbol.arguments.len() == flattened_valty.len());
                for (a, b) in function_symbol
                    .arguments
                    .iter()
                    .zip(flattened_valty.into_iter())
                {
                    assert!(
                        a.1 == b,
                        "Runtime function argument type missmatch on {extern_symbol}"
                    );
                }

                //emit the call, assuming that the signature matches
                let _ = builder.call(function_symbol.id);
            }
            WasmNode::Error { .. } => panic!("encountered error node!"),
        }

        //finally emit the store(s) for the value(s) that where just put on the stack.
        self.emit_store(node.output(0), builder);

        Ok(())
    }
}
