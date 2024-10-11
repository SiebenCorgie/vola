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
    attrib::FlagStore,
    edge::{InputType, OutportLocation, OutputType},
    region::RegionLocation,
    util::cfg::{scfr::ScfrError, Cfg, CfgNode, CfgRef},
    NodeRef, SmallColl,
};
use walrus::{
    ir::{Block, IfElse, InstrSeqType, Loop, MemArg, Value},
    FunctionBuilder, Module, ModuleFunctions, ModuleLocals, ValType,
};

use crate::{
    graph::{WasmNode, WasmTy},
    WasmBackend, WasmError,
};

use super::{memory::MemoryHandler, WasmLambdaBuilder, WasmLmdBuildReport};

impl WasmLambdaBuilder {
    ///Emits a λ's argument preamble. This bascially makes sure, that the _by-value_ elements
    ///of each argument are stored in its respective memory element.
    pub fn emit_argument_preamble(
        &mut self,
        builder: &mut walrus::InstrSeqBuilder,
        locals: &mut ModuleLocals,
        backend: &WasmBackend,
    ) {
        for arg in self.ctx.arguments.iter_mut() {
            assert!(arg.local_id.is_none());

            //As an optimization for code size, do not allocat a port (and therfore memory)
            //for unused args. However, still keep the local-id, since those are needed to build the
            //function signature.
            let is_dead_arg = if backend.graph[arg.port].edges.len() == 0 {
                true
            } else {
                false
            };

            let mut local_ids = SmallColl::new();
            //allocate such a memory element
            let addr = if !is_dead_arg {
                self.mem.alloc_port(arg.port, arg.ty.clone());
                let addr = self.mem.get_port_base_address(arg.port);
                Some(addr)
            } else {
                None
            };
            //now emit a store for each local element of the
            let per_element_offset = arg.ty.base_type_size();
            for element_index in 0..arg.ty.element_count() {
                //Emit a load for the local element, followed by a store to the correct
                //to the correct offset of the `addr`

                //NOTE: Since we are allocating _in order_ we can just use add
                let localid = locals.add(arg.ty.unwarp_walrus_ty());
                local_ids.push(localid);
                //only store, if there is an actual address
                if let Some(addr) = addr {
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
            }

            //set the used local ids for the argument
            arg.local_id = Some(local_ids);
        }
    }

    ///Prepends the stackpointer preamble _before_ the start of this function
    pub fn emit_stackpointer_preamble(&mut self, builder: &mut walrus::InstrSeqBuilder) {
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

    fn allocate_instr_sequences(
        &mut self,
        cfg: &Cfg,
        list: &[CfgRef],
        backend: &WasmBackend,
        fn_builder: &mut FunctionBuilder,
    ) {
        for node in list {
            match &cfg.nodes[*node] {
                CfgNode::BasicBlock(bb) => {
                    if bb.nodes.len() == 0 {
                        continue;
                    }

                    //allocate a seq-instr builder if not yet done
                    let region = backend.graph[bb.nodes[0]].parent.unwrap();
                    if !self.label.flags.contains_key(&region.into()) {
                        //NOTE: the λ id should be pushed allready, so we should only encounter _simple_
                        //      regions for gamma or theta nodes
                        assert!(
                            backend.graph[region.node].node_type.is_intra_procedural(),
                            "Exepected Gamma or Theta, was: {}",
                            backend.graph[region.node].node_type
                        );
                        let builder = fn_builder.dangling_instr_seq(InstrSeqType::Simple(None));
                        let builder_id = builder.id();
                        let _ = self.label.set(region.into(), builder_id);
                    }
                }
                CfgNode::LoopHeader { src_node, .. } => {
                    //allocate a sequence id for the loop. This is the one used for the
                    //_break-out-of-block_ section
                    let builder = fn_builder.dangling_instr_seq(InstrSeqType::Simple(None));
                    let id = builder.id();
                    self.label.set(src_node.into(), id);
                }
                _ => {}
            }
        }
    }

    pub(crate) fn serialize_cfg(
        mut self,
        cfg: Cfg,
        backend: &mut WasmBackend,
        module: &mut Module,
        mut fn_builder: FunctionBuilder,
    ) -> Result<WasmLmdBuildReport, WasmError> {
        //Before starting to work on the actual graph, we have to setup a preamble
        // The first action is to load all arguments onto the implicit stack
        // We that interate all nodes in the CFG, and allocate space in the implicit stack for them.
        // Afterwards we start the actual serialzing process.

        //First annotate the λ's region as the function body
        let lmd_region = RegionLocation {
            node: self.ctx.lamda,
            region_index: 0,
        };
        self.label
            .set(lmd_region.clone().into(), fn_builder.func_body_id());

        {
            let mut builder = fn_builder.func_body();
            self.emit_argument_preamble(&mut builder, &mut module.locals, backend);
        }

        //NOTE: Since we don't necessarly clean up all nodes before creating the CFG
        //      there might be untouched nodes.
        let topoord = match cfg.topological_order() {
            Ok(l) => l,
            Err(ScfrError::TopoOrdNotAllNodesTraversed(l)) => l,
            Err(e) => return Err(e.into()),
        };

        //allocates a instruction sequence for each region that is touched by the
        //cfg. Note that instruction-sequences in walrus are NOT the same as basic-blocks.
        self.allocate_instr_sequences(&cfg, &topoord, backend, &mut fn_builder);
        //Dataflow based memory allocation. Have a look at the implementation for more info.
        self.mem.allocate_for_cfg(&cfg, &topoord, backend);

        //If wanted, dump the graph with the assigned memory indices

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_WASM_MEMORY").is_ok()
        {
            backend.push_debug_state_with(&format!("WASM memory for {}", self.name), |b| {
                let mut flagstore = FlagStore::new();
                for (port, index) in self.mem.mem_map.iter() {
                    flagstore.set(port.into(), *index);
                }
                b.with_flags("MemoryId", &flagstore)
            });
        }

        //actually serialize all cfg nodes, including the loop / branch nodes via the walrus builder
        for cfgnode in &topoord {
            match &cfg.nodes[*cfgnode] {
                CfgNode::Null => {}
                CfgNode::Root(_) => {}
                CfgNode::BasicBlock(bb) => {
                    //ignore empty blocks
                    if bb.nodes.len() == 0 {
                        continue;
                    }

                    //Get the parent region for the nodes of this block
                    let parent_region = backend.graph[bb.nodes[0]].parent.unwrap();

                    //switch to the region's builder, and serialize all simple nodes
                    {
                        let seqid = self.label.get(&parent_region.into()).unwrap();
                        let mut seq = fn_builder.instr_seq(*seqid);
                        for node in &bb.nodes {
                            self.serialize_simple_node(
                                *node,
                                backend,
                                &mut seq,
                                &module.funcs,
                                &module.locals,
                            )?;
                        }
                    }
                }
                CfgNode::BranchHeader {
                    src_node,
                    condition_src,
                    ..
                } => {
                    //Insert the if-else instruction on the
                    //out-region's sequence builder

                    let parent_region = backend.graph[*src_node].parent.unwrap();
                    {
                        let seqid = self.label.get(&parent_region.into()).unwrap();
                        let mut seq = fn_builder.instr_seq(*seqid);
                        let consequence_id = self
                            .label
                            .get(
                                &RegionLocation {
                                    node: *src_node,
                                    region_index: 0,
                                }
                                .into(),
                            )
                            .unwrap();
                        let alternative_id = self
                            .label
                            .get(
                                &RegionLocation {
                                    node: *src_node,
                                    region_index: 1,
                                }
                                .into(),
                            )
                            .unwrap();
                        //now load the switch criterion and add the if-else instruction
                        self.load_port_elements(*condition_src, &mut seq);
                        seq.instr(IfElse {
                            consequent: *consequence_id,
                            alternative: *alternative_id,
                        });
                    }
                }
                CfgNode::BranchMerge { .. } => {
                    //NOTE: right now we don't need to do anything here, since this is
                    // a) handeled by walrus
                    // b) everything is done on the implicit stack, so all CF sequences are _simple_.
                }
                CfgNode::LoopHeader { src_node, .. } => {
                    //emit the loop

                    let parent_region = backend.graph[*src_node].parent.unwrap();
                    let loop_body = RegionLocation {
                        node: *src_node,
                        region_index: 0,
                    };

                    {
                        let loop_block_id = self.label.get(&src_node.into()).unwrap();
                        let loop_body_id = self.label.get(&loop_body.into()).unwrap();

                        //Now add the loop block to the outer sequence
                        {
                            let seqid = self.label.get(&parent_region.into()).unwrap();
                            let mut seq = fn_builder.instr_seq(*seqid);
                            //start the loop block
                            seq.instr(Block {
                                seq: *loop_block_id,
                            });
                        }
                        //After that, on the loop-block, emit the _loop_ directive that'll
                        //pull in the actual loop's sequence
                        {
                            let mut seq = fn_builder.instr_seq(*loop_block_id);
                            seq.instr(Loop { seq: *loop_body_id });
                        }
                    }
                }
                CfgNode::LoopCtrlTail {
                    condition_src,
                    src_node,
                    ..
                } => {
                    //At the end of the loop, first load the loop criterion, and depending on that
                    //either branch to the block-id (which will abort the loop), or branch to the loop-body-id,
                    //which will start another iteration
                    let loop_body = RegionLocation {
                        node: *src_node,
                        region_index: 0,
                    };
                    let loop_block_id = *self.label.get(&src_node.into()).unwrap();
                    let loop_body_id = *self.label.get(&loop_body.into()).unwrap();
                    {
                        let mut seq = fn_builder.instr_seq(loop_body_id);
                        self.load_port_elements(*condition_src, &mut seq);
                        seq.if_else(
                            None,
                            |if_seq| {
                                //criterion is true, branch to the loop-start for the next iteration.
                                //before doint that, load all _intermediate_ results
                                //and write them back to the argument port

                                for result in backend.graph[*src_node].result_types(0) {
                                    if result == InputType::ThetaPredicate {
                                        continue;
                                    }
                                    let result_port = src_node.as_inport_location(result);
                                    if backend.graph[result_port].edge.is_none() {
                                        continue;
                                    }
                                    let result_src = backend.graph.inport_src(result_port).unwrap();
                                    let argument = if let InputType::Result(x) = result {
                                        OutputType::Argument(x)
                                    } else {
                                        panic!("invalid result!")
                                    };
                                    let argument_port = src_node.as_outport_location(argument);

                                    //if both ports are the same, we can safe that
                                    if result_src == argument_port {
                                        continue;
                                    }

                                    self.load_port_elements(result_src, if_seq);
                                    self.store_values_to_port(argument_port, if_seq)
                                }

                                if_seq.br(loop_body_id);
                            },
                            |then_seq| {
                                //is false, end the loop
                                //while doing so, store the result to the port's outputs
                                for output in backend.graph[*src_node].outport_types() {
                                    let outport = src_node.as_outport_location(output);
                                    if backend.graph[outport].edges.len() == 0 {
                                        continue;
                                    }

                                    let result_src = backend
                                        .graph
                                        .find_producer_inp(src_node.as_inport_location(
                                            output.map_to_in_region(0).unwrap(),
                                        ))
                                        .unwrap();
                                    self.load_port_elements(result_src, then_seq);
                                    self.store_values_to_port(outport, then_seq);
                                }
                                then_seq.br(loop_block_id);
                            },
                        );
                    }
                }
            }
        }

        //After serialzing everything, build the postamble.
        //This'll frist load all result values onto the stack, then
        //emit the implicit-stack cleanup (mostly resetting to the pre-λ-call state).

        {
            let seqid = self.label.get(&lmd_region.into()).unwrap();
            let mut seq = fn_builder.instr_seq(*seqid);

            for result in backend.graph[self.ctx.lamda].result_types(0) {
                let resultport = self.ctx.lamda.as_inport_location(result);
                let result_src = backend.graph.find_producer_inp(resultport).unwrap();
                //produce the loads for each result element
                self.load_port_elements(result_src, &mut seq);
            }

            //NOTE: we only emit the preamble here, since we only know the needed stack size at this point
            //
            //The function _part_ is emitted at this point. We now wrap it in a stack_pointer construction _preamble_
            //and a stack_pointer destruction routine.
            //This'll effectively grow the stack (downwards) for the elements we are using while working
            //and'll shrink it back once we ended.
            self.emit_stackpointer_preamble(&mut seq);
            self.emit_stackpointer_post_build(&mut seq);
        }

        //Now finish the walrus builder, and put the function into the actual module
        let mut args = Vec::new();
        for arg in self.ctx.arguments.iter() {
            args.extend_from_slice(&arg.local_id.as_ref().unwrap().as_slice());
        }
        let fnid = fn_builder.finish(args, &mut module.funcs);

        //Once we finished serializing, we read the memory size, and fit our
        //memory to that size
        let desired_page_count = (self.mem.memory_size % MemoryHandler::WASM_PAGE_SIZE) + 1;
        module.memories.get_mut(self.mem.memid).initial = desired_page_count.try_into().unwrap();

        let build_report = WasmLmdBuildReport {
            additional_memory: self.mem.memory_size,
            function_id: fnid,
            touched_functions: self.touched_export_functions,
        };
        Ok(build_report)
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
                //mark as touched
                self.touched_export_functions.insert(extern_symbol.clone());
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
