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

use rvsdg::{
    edge::OutportLocation,
    util::cfg::{scfr::ScfrError, Cfg, CfgNode},
    NodeRef, SmallColl,
};
use walrus::{
    ir::{MemArg, Value},
    FunctionId, GlobalId, LocalId, Module, ModuleFunctions, ModuleLocals, ValType,
};

use crate::{
    graph::{WasmNode, WasmTy},
    WasmBackend, WasmError,
};

use super::{memory::MemoryHandler, FunctionBuilderCtx};

///The helper that tracks the virtual heap for building a λ-node.
pub struct WasmLambdaBuilder {
    mem: MemoryHandler,
    ctx: FunctionBuilderCtx,

    ///The function local stack pointer set at the start of the function
    fn_local_stack_pointer: LocalId,
    global_stack_pointer: GlobalId,
}

impl WasmBackend {
    pub(super) fn init_sm_for_ctx(
        &self,
        ctx: FunctionBuilderCtx,
        module: &mut Module,
    ) -> WasmLambdaBuilder {
        //Allocate ourself some empty memory
        //let memid = module.memories.add_local(false, false, 0, None, None);

        let memid = module.get_memory_id().unwrap();
        let fn_local_stack_pointer = module.locals.add(ValType::I32);
        module.locals.get_mut(fn_local_stack_pointer).name =
            Some("__vola_stack_pointer".to_string());

        let stackptr = module
            .globals
            .iter()
            .find_map(|g| {
                if g.name == Some("__stack_pointer".to_string()) {
                    Some(g.id())
                } else {
                    None
                }
            })
            .expect("Failed to find stack pointer in module!");

        let sm = WasmLambdaBuilder {
            mem: MemoryHandler::empty(memid, &mut module.locals),
            ctx,
            fn_local_stack_pointer,
            global_stack_pointer: stackptr,
        };
        sm
    }
}

impl WasmLambdaBuilder {
    ///Emits a λ's argument preample. This bascially makes sure, that the _by-value_ elements
    ///of each argument are stored in its respective memory element.
    pub fn emit_argument_preample(
        &mut self,
        builder: &mut walrus::InstrSeqBuilder,
        locals: &mut ModuleLocals,
    ) {
        for arg in self.ctx.arguments.iter_mut() {
            assert!(arg.local_id.is_none());
            let mut local_ids = SmallColl::new();
            //allocate such a memory element
            self.mem.alloc_port(arg.port, arg.ty.clone());
            let addr = self.mem.get_port_base_address(arg.port);
            //now emit a store for each local element of the

            let per_element_offset = arg.ty.base_type_size();
            for element_index in 0..arg.ty.element_count() {
                //Emit a load for the local element, followed by a store to the correct
                //to the correct offset of the `addr`

                //NOTE: Since we are allocating _in order_ we can just use add
                let localid = locals.add(arg.ty.unwarp_walrus_ty());
                local_ids.push(localid);
                //store it to the offset
                builder.const_(addr);
                builder.local_get(self.fn_local_stack_pointer);
                builder.binop(walrus::ir::BinaryOp::I32Add);

                builder.local_get(localid);
                builder.store(
                    self.mem.memid,
                    arg.ty.store_kind(),
                    MemArg {
                        align: 0,
                        //calculate local offset for this address
                        offset: (per_element_offset * element_index).try_into().unwrap(),
                    },
                );
            }

            //set the used local ids for the argument
            arg.local_id = Some(local_ids);
        }
    }

    ///Prepends the stackpointer preample _before_ the start of this function
    pub fn emit_stackpointer_preample(&mut self, builder: &mut walrus::InstrSeqBuilder) {
        //get the stack pointer
        builder.global_get_at(0, self.global_stack_pointer);
        //lit our local stack size we calculated while collecting all the ~stuff~
        builder.const_at(1, Value::I32(self.mem.memory_size as i32));
        builder.binop_at(2, walrus::ir::BinaryOp::I32Sub);
        //sub to arrive at the new global stack pointer location
        //store locally...
        builder.local_tee_at(3, self.fn_local_stack_pointer);
        builder.global_set_at(4, self.global_stack_pointer);
    }

    ///Emits the _post-function_ global stack reconstruction.
    ///Note that the result valuse (if any) are pushed to the stack _before that_.
    ///So this really are the last instructions on the tape.
    pub fn emit_stackpointer_post_build(&mut self, builder: &mut walrus::InstrSeqBuilder) {
        builder.local_get(self.fn_local_stack_pointer);
        builder.i32_const(self.mem.memory_size as i32);
        builder.binop(walrus::ir::BinaryOp::I32Add);
        builder.global_set(self.global_stack_pointer);
    }

    ///Puts the `port`'s address on stack.
    ///
    ///If the port's memory location is a local value, adds a
    ///new memory object and stores it to that location.
    fn load_port_addr(&self, port: OutportLocation, builder: &mut walrus::InstrSeqBuilder) {
        //NOTE: in practice we get the base address, and add it to the current stack address
        builder.const_(self.mem.get_port_base_address(port));
        builder.local_get(self.fn_local_stack_pointer);
        builder.binop(walrus::ir::BinaryOp::I32Add);
    }

    fn load_addr_offset(
        &self,
        base_addr: i32,
        offset: u32,
        ty: &WasmTy,
        builder: &mut walrus::InstrSeqBuilder,
    ) {
        //push and calculate the base addr
        builder.i32_const(base_addr);
        builder.local_get(self.fn_local_stack_pointer);
        builder.binop(walrus::ir::BinaryOp::I32Add);
        //now load the element at index
        builder.load(self.mem.memid, ty.load_kind(), MemArg { align: 0, offset });
    }

    ///Puts the elements of `port` on stack, regardless of its location
    fn load_port_elements(&self, port: OutportLocation, builder: &mut walrus::InstrSeqBuilder) {
        //what we do is emit a series of _in-order_ loads,
        //so that all elements end up on the stack
        let memele = self.mem.get_port_element(port);

        let pe_offset = memele.ty.base_type_size();
        for ele_idx in 0..memele.ty.element_count() {
            let offset = (pe_offset * ele_idx).try_into().unwrap();

            self.load_addr_offset(memele.base_addr, offset, &memele.ty, builder);
        }
    }

    ///Stores the top elements of the stack to the port's memory location.
    fn store_values_to_port(&self, port: OutportLocation, builder: &mut walrus::InstrSeqBuilder) {
        //NOTE: because this is pain, we have to do the following:
        //      The stack currently is expected to look like this for a vec3:
        //      x,y,z
        //          ^stack_ptr
        //
        //      a store expects:
        //      addr, value
        //             ^stack_ptr.
        //
        //In order to store in z,y,x order, we somehow have to get the
        //address _in between_ each element.
        //
        // the way we do this by first pushing the address, to arrive at
        // x,y,z,addr
        //        ^stack_ptr
        //
        // then we use a local value to swap z and addr
        // local.set(0)
        // local.set(1)
        // logal.get(0)
        //
        // In practice, we have to store the add value only once
        // to a local, and then, for each element, set a different local value,
        // and pop both in order again. But that is still the main _idea_ behind it.
        // Also we do this on the informal stack, so we have to add that value as well.

        let memele = self.mem.get_port_element(port);
        let pe_offset = memele.ty.base_type_size();

        //push the addr value into the local element
        builder.const_(Value::I32(memele.base_addr));
        builder.local_get(self.fn_local_stack_pointer);
        builder.binop(walrus::ir::BinaryOp::I32Add);
        let addr_id = self.mem.swap_i32[0];
        builder.local_set(addr_id);

        //we always use the _second_ swap value
        let payload_swap_id = match memele.ty.unwarp_walrus_ty() {
            ValType::F32 => self.mem.swap_f32[1],
            ValType::I32 => self.mem.swap_i32[1],
            _ => panic!("{:?} cannot swap atm.", memele.ty),
        };

        //NOTE: Store has to be in reverse...
        for ele_idx in (0..memele.ty.element_count()).rev() {
            //now set the top stack value into _another_ local
            //value, and pop both in reverse
            builder.local_set(payload_swap_id);

            builder.local_get(addr_id);
            builder.local_get(payload_swap_id);

            //now store the element at index
            builder.store(
                self.mem.memid,
                memele.ty.store_kind(),
                MemArg {
                    align: 0,
                    offset: (pe_offset * ele_idx).try_into().unwrap(),
                },
            );
        }
    }

    pub fn serialize_cfg(
        mut self,
        cfg: Cfg,
        backend: &WasmBackend,
        lambda: NodeRef,
        module: &mut Module,
        symbol_name: &str,
    ) -> Result<FunctionId, WasmError> {
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
        fnbuilder.name(symbol_name.to_string());
        let mut builder = fnbuilder.func_body();
        //Before starting anything, we emit a preample that
        //moves all arguments to the memory
        //object
        self.emit_argument_preample(&mut builder, &mut module.locals);

        //Iterate all nodes in order of the cfg and serialize them into the flat sequence of nodes.
        //while doing so, record the heap growth.
        let topoord = match cfg.topological_order() {
            Ok(l) => l,
            Err(ScfrError::TopoOrdNotAllNodesTraversed(l)) => l,
            Err(e) => return Err(e.into()),
        };

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
                            assert!(self.mem.mem_map.contains_key(&producer));
                        }

                        //alloc the node's result element
                        assert!(backend.graph[*node].outputs().len() == 1);
                        let result_port = node.output(0);
                        let result_ty = backend.outport_type(result_port).unwrap();
                        self.mem.alloc_port(result_port, result_ty);
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
                CfgNode::BranchHeader {
                    src_node,
                    condition_src,
                    last_bb,
                    true_branch,
                    false_branch,
                    merge,
                    post_merge_block,
                } => {
                    //
                }
                CfgNode::BranchMerge {
                    src_node,
                    src_true,
                    src_false,
                    next,
                } => {}
                CfgNode::LoopHeader {
                    src_node,
                    pre_loop_bb,
                    loop_entry_bb,
                    ctrl_tail,
                } => {}
                CfgNode::LoopCtrlTail {
                    last_bb,
                    loop_entry_bb,
                    post_loop_bb,
                    condition_src,
                    src_node,
                    header,
                } => {}
            }
        }

        //Now setup the ordered load instructions for the result(s).
        for result in backend.graph[lambda].result_types(0) {
            let resultport = lambda.as_inport_location(result);
            let result_src = backend.graph.find_producer_inp(resultport).unwrap();
            //produce the loads for each result element
            self.load_port_elements(result_src, &mut builder);
        }

        //The function _part_ is emitted at this point. We now wrap it in a stack_pointer construction _preample_
        //and a stack_pointer destruction routine.
        //This'll effectively grow the stack (downwards) for the elements we are using while working
        //and'll shrink it back once we ended.
        self.emit_stackpointer_preample(&mut builder);
        self.emit_stackpointer_post_build(&mut builder);

        //Now finish the walrus builder, and yeet the function into the actual module
        let mut args = Vec::new();
        for arg in self.ctx.arguments.iter() {
            args.extend_from_slice(&arg.local_id.as_ref().unwrap().as_slice());
        }
        let fnid = fnbuilder.finish(args, &mut module.funcs);

        //Once we finished serializing, we read the memory size, and fit our
        //memory to that size
        let desired_page_count = (self.mem.memory_size % MemoryHandler::WASM_PAGE_SIZE) + 1;
        module.memories.get_mut(self.mem.memid).initial = desired_page_count.try_into().unwrap();

        Ok(fnid)
    }

    fn handle_branch_header(
        &mut self,
        node: NodeRef,
        backend: &WasmBackend,
        builder: &mut walrus::InstrSeqBuilder,
        functions: &ModuleFunctions,
        locals: &ModuleLocals,
    ) -> Result<(), WasmError> {
        todo!()
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

        let mut input_src_ports = SmallColl::new();
        let mut output_ports = SmallColl::new();

        for inp in backend.graph[node].inport_types() {
            let src = backend
                .graph
                .find_producer_inp(node.as_inport_location(inp))
                .unwrap();
            input_src_ports.push(src);
        }

        for outp in backend.graph[node].outport_types() {
            output_ports.push(node.as_outport_location(outp));
        }

        //now match the operation and emit it.
        //
        //This makes sure that the stack is configured on a _per-operation_ basis.
        //
        //Generally speaking tho, anything that is not a runtime value
        //is call-by-value, so we push the actual elements on the stack
        //
        //The runtime follows a call-by-reference configuration that
        //reads first all argument adresses, followed by all result adresses.
        match backend.graph[node].node_type.unwrap_simple_ref() {
            WasmNode::Unary(u) => {
                assert!(input_src_ports.len() == 1);
                self.load_port_elements(input_src_ports[0], builder);
                builder.unop(u.op.clone());
                assert!(output_ports.len() == 1);
                self.store_values_to_port(output_ports[0], builder);
            }
            WasmNode::Binary(b) => {
                assert!(input_src_ports.len() == 2);
                self.load_port_elements(input_src_ports[0], builder);
                self.load_port_elements(input_src_ports[1], builder);
                builder.binop(b.op.clone());
                assert!(output_ports.len() == 1);
                self.store_values_to_port(output_ports[0], builder);
            }
            WasmNode::Value(v) => {
                builder.const_(v.op.clone());
                assert!(output_ports.len() == 1);
                self.store_values_to_port(output_ports[0], builder);
            }
            WasmNode::Index(i) => {
                //Index basically just mean copy the _nth_ input to our output

                assert!(input_src_ports.len() == 1);
                assert!(output_ports.len() == 1);

                let input_ty = backend.outport_type(input_src_ports[0]).unwrap();
                let addr = self.mem.get_port_element(input_src_ports[0]).base_addr;
                for element_offset in input_ty.index_to_offset_elements(i.index) {
                    self.load_addr_offset(addr, element_offset, &input_ty, builder);
                }

                self.store_values_to_port(output_ports[0], builder);
            }
            WasmNode::Construct(_) => {
                //for construct we just load all inputs on the stack, and then
                //store them.
                for input in &input_src_ports {
                    self.load_port_elements(*input, builder);
                }
                assert!(output_ports.len() == 1);
                self.store_values_to_port(output_ports[0], builder);
            }
            WasmNode::Runtime(r) => {
                //NOTE: The convention is, that there is an 32bit adress for each
                //      argument, followed by an address for each result.
                let mut flattened_valty = SmallColl::new();
                for _inty in &input_src_ports {
                    flattened_valty.push(ValType::I32);
                }
                for _out in &output_ports {
                    flattened_valty.push(ValType::I32);
                }

                let input_types = input_src_ports
                    .iter()
                    .map(|port| backend.outport_type(*port).unwrap())
                    .collect::<SmallColl<_>>();

                let extern_symbol = r.op.get_static_symbol_name(&input_types)?;
                let function_symbol =
                    crate::runtime::lookup_function_symbol(locals, functions, &extern_symbol);
                //While at it, verify that the input types match

                assert!(
                    function_symbol.arguments.len() == flattened_valty.len(),
                    "symbol_args.len()={}, valty.args.len()={} for {}",
                    function_symbol.arguments.len(),
                    flattened_valty.len(),
                    extern_symbol
                );

                for (a, b) in function_symbol
                    .arguments
                    .iter()
                    .zip(flattened_valty.into_iter())
                {
                    assert!(
                        a.1 == b,
                        "Runtime function argument type missmatch on {extern_symbol}: {} != {}",
                        a.1,
                        b
                    );
                }

                //At this point we are pretty sure that the signature is given,
                //therfore load all adresses in order and _just_ execute,
                //without any write back
                for inp in &input_src_ports {
                    self.load_port_addr(*inp, builder);
                }

                for res in &output_ports {
                    self.load_port_addr(*res, builder);
                }

                //emit the call, assuming that the signature matches
                let _ = builder.call(function_symbol.id);
            }
            WasmNode::Error { .. } => panic!("encountered error node!"),
        }

        Ok(())
    }
}
