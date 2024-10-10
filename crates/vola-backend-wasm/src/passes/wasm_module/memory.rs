/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//!Abstraction over the Memory characteristics of the WASM-VM.

use ahash::AHashMap;
use rvsdg::{
    edge::{InputType, OutportLocation, OutputType},
    region::RegionLocation,
    util::cfg::{Cfg, CfgNode, CfgRef},
};
use walrus::{ir::Value, LocalId, MemoryId, ModuleLocals};

use crate::{graph::WasmTy, WasmBackend};

#[derive(Clone)]
pub struct MemElement {
    pub ty: WasmTy,
    ///The base address of the element.
    pub base_addr: i32,
}

///We have a local "port to memory-element" mapping for each function.
///
///This lets us emit load / store instructions regardless of the actual port's value location.
///
///In practice there are two main memory locations:
/// 1. (function) locals (args, intermediate values etc.)
/// 2. (function) memory: Elements that are part of the WASM linear memory.
pub struct MemoryHandler {
    //The memory we are using.
    pub memid: MemoryId,
    pub mem_table: Vec<MemElement>,
    ///Map that points into `mem-table` allows us to use several ports on the same memory element.
    pub mem_map: AHashMap<OutportLocation, usize>,
    ///Tracks the element count in the local array.
    #[allow(dead_code)]
    pub local_size: usize,
    ///Tracks last knows size of the used memory by this function.
    pub memory_size: usize,

    //we allocate two locals for each type, to be able to swap stack elements.
    pub swap_i32: [LocalId; 2],
    pub swap_f32: [LocalId; 2],
}

impl MemoryHandler {
    pub const WASM_PAGE_SIZE: usize = 65_536;

    pub fn empty(memid: MemoryId, locals: &mut ModuleLocals) -> Self {
        let swap_i32 = [
            locals.add(walrus::ValType::I32),
            locals.add(walrus::ValType::I32),
        ];
        locals.get_mut(swap_i32[0]).name = Some("swap_i32_0".to_string());
        locals.get_mut(swap_i32[1]).name = Some("swap_i32_1".to_string());

        let swap_f32 = [
            locals.add(walrus::ValType::F32),
            locals.add(walrus::ValType::F32),
        ];
        locals.get_mut(swap_f32[0]).name = Some("swap_f32_0".to_string());
        locals.get_mut(swap_f32[1]).name = Some("swap_f32_1".to_string());

        MemoryHandler {
            memid,
            local_size: 0,
            mem_table: Vec::new(),
            mem_map: AHashMap::default(),
            memory_size: 0,
            swap_i32,
            swap_f32,
        }
    }

    pub fn get_port_base_address(&self, port: OutportLocation) -> Value {
        Value::I32(self.get_port_element(port).base_addr)
    }

    pub fn get_port_element(&self, port: OutportLocation) -> MemElement {
        if let Some(index) = self.mem_map.get(&port) {
            self.mem_table[*index].clone()
        } else {
            panic!("Un-mapped port: {:?}", port);
        }
    }

    ///Allocates appropriate memory for the given memory type.
    ///
    /// By definition (taken from whatever is emitted by Rust for the runtime), we
    /// push scalar values into locals, and anything _bigger_ (Vec2, Vec3,..., Mat3, ..., Tensor) into memory.
    pub fn alloc_port(&mut self, port: OutportLocation, ty: WasmTy) {
        let element_size = ty.wasm_size();
        let base_addr = self.memory_size.try_into().expect("Run out of memory");
        self.memory_size += element_size;
        let element = MemElement {
            ty: ty.clone(),
            base_addr,
        };
        /*
        println!(
            "Alloc<{ty:?}> {port:?} @ {} .. {} ({}byte)",
            element.base_addr, self.memory_size, element_size
        );*/

        let index = self.mem_table.len();
        self.mem_table.push(element);
        self.mem_map.insert(port, index);
    }

    pub fn allocate_for_cfg(&mut self, cfg: &Cfg, list: &Vec<CfgRef>, backend: &WasmBackend) {
        //Allocate implicit-stack locations for all outports (that are used).
        //
        //This basically uses the data-flow information of the RVSDG together with the CFG topological
        //trace to build the _correct_ data-flow on the implicit stack in WASM.
        //
        //We associate each Outport of the RVSDG with _some_ MemoryElement. This lets us do things like mapping the
        //results of branches and loops to the right (shared) location.
        //
        //The serializer then assumes that those locations are _valid_ and does not change them.
        for node in list {
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
                            assert!(self.mem_map.contains_key(&producer));
                        }

                        //alloc the node's result element
                        assert!(backend.graph[*node].outputs().len() == 1);
                        let result_port = node.output(0);
                        let result_ty = backend.outport_type(result_port).unwrap();
                        self.alloc_port(result_port, result_ty);
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
                    //For the branches we just map the source-port memory element to the in-region port. So a simple pass through
                    for input in backend.graph[*src_node].inport_types() {
                        let input_port = src_node.as_inport_location(input);
                        //Ignore unused input ports.
                        //NOTE: If they are used _in-region_, this will panic. However, that is an invalid state anyways.
                        if backend.graph[input_port].edge.is_none() {
                            continue;
                        }
                        let src = backend
                            .graph
                            .find_producer_inp(input_port)
                            .expect("Exepcted allocated src!");

                        let src_index = *self.mem_map.get(&src).expect("Expected src to be valid!");
                        //ignore un-mappable input ports
                        for idx in 0..backend.graph[*src_node].regions().len() {
                            if let Some(in_region_ty) = input.map_to_in_region(idx) {
                                self.mem_map
                                    .insert(src_node.as_outport_location(in_region_ty), src_index);
                            }
                        }
                    }
                }
                CfgNode::BranchMerge {
                    src_node,
                    src_true,
                    src_false,
                    next,
                } => {
                    //NOTE: For the branch we do the following: We unify the output targets of both branches to the same memory location
                    //      So if branch_true[0] writes to a, then branch_false[0] also writes to a.
                    //      We then, again, just tell the output-port[0] to _be_ a.

                    for output_ty in backend.graph[*src_node].outport_types() {
                        let unified_src_index = {
                            let in_first_branch = output_ty.map_to_in_region(0).unwrap();
                            if let Some(src) = backend
                                .graph
                                .find_producer_inp(src_node.as_inport_location(in_first_branch))
                            {
                                *self.mem_map.get(&src).expect("Expected port to be set!")
                            } else {
                                //make sure the other branch is also unused
                                let in_second_branch = output_ty.map_to_in_region(1).unwrap();
                                assert!(backend.graph[src_node.as_inport_location(in_second_branch)].edge.is_none(), "If first branch doesn't produces a value, the second shouldn't produce one as well");
                                continue;
                            }
                        };

                        //set the location of the src of all none-first branches (which is usally just one).
                        //At this point we assume that the types match, since the whole compiler should have aborted way
                        //before WASM code-generation otherwise.
                        for region_index in 1..backend.graph[*src_node].regions().len() {
                            let in_nth_branch = output_ty.map_to_in_region(region_index).unwrap();
                            let src = backend
                                .graph
                                .find_producer_inp(src_node.as_inport_location(in_nth_branch))
                                .unwrap();
                            //check if ther is already an element allocated (should be the case)
                            //TODO: right now we are not removing it. But a _better_ implementation would mark that as dead, and free up space.
                            assert!(self.mem_map.contains_key(&src));
                            //Now unify to the same index
                            self.mem_map.insert(src, unified_src_index);
                        }

                        //now set the same element for the
                        //finally tag the outport of the gamma node with (yet again) the same index
                        self.mem_map
                            .insert(src_node.as_outport_location(output_ty), unified_src_index);
                    }
                }
                CfgNode::LoopHeader {
                    src_node,
                    pre_loop_bb,
                    loop_entry_bb,
                    ctrl_tail,
                } => {
                    //For the header, just _reuse_ the input-src as the argument_src and the result_destination.

                    for input in backend.graph[*src_node].inport_types() {
                        let in_region = input.map_to_in_region(0).unwrap();
                        let mem_index = if let Some(src) = backend
                            .graph
                            .find_producer_inp(src_node.as_inport_location(input))
                        {
                            *self.mem_map.get(&src).unwrap()
                        } else {
                            //make sure the inner-region doesn't use that
                            assert!(
                                backend.graph[src_node.as_outport_location(in_region)]
                                    .edges
                                    .len()
                                    == 0
                            );
                            continue;
                        };

                        //map to in-region port
                        self.mem_map
                            .insert(src_node.as_outport_location(in_region), mem_index);
                    }
                }
                CfgNode::LoopCtrlTail {
                    last_bb,
                    loop_entry_bb,
                    post_loop_bb,
                    condition_src,
                    src_node,
                    header,
                } => {
                    //For the control-tail, map the result srcs to the input memory index, and the
                    // output-port to the same as well. This basically makes sure, that even if the loop doesn't execute at all, the right memory location is used, and that the intermediate value
                    // is mapped back to the start of the loop again.

                    for result in backend.graph[*src_node].result_types(0) {
                        //don't touch the predicate port though
                        if result == InputType::ThetaPredicate {
                            continue;
                        }

                        let output = result.map_out_of_region().unwrap();
                        let result_src = if let Some(src) = backend
                            .graph
                            .find_producer_inp(src_node.as_inport_location(result))
                        {
                            src
                        } else {
                            //make sure the output is not in use as well
                            assert!(
                                backend.graph[src_node.as_outport_location(output)]
                                    .edges
                                    .len()
                                    == 0
                            );
                            continue;
                        };

                        let argument_ty = match result {
                            InputType::Result(idx) => OutputType::Argument(idx),
                            _ => panic!("unexpected result type: {:?}", result),
                        };

                        let argument_index = *self
                            .mem_map
                            .get(&src_node.as_outport_location(argument_ty))
                            .expect("Expected input-port to have argument already mapped");

                        //now overwrite the value_src port to that index, then set the output-port as that as well
                        self.mem_map.insert(result_src, argument_index);
                        self.mem_map
                            .insert(src_node.as_outport_location(output), argument_index);
                    }
                }
                _ => {}
            }
        }
    }
}
