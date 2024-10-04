/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//!Abstraction over the Memory characteristics of the WASM-VM.

use ahash::AHashMap;
use rvsdg::{edge::OutportLocation, SmallColl};
use walrus::{
    ir::{MemArg, Value},
    LocalId, MemoryId, ModuleLocals,
};

use crate::graph::WasmTy;

use super::ArgumentCtx;

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
    pub mem_map: AHashMap<OutportLocation, MemElement>,
    ///Tracks the element count in the local array.
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
        MemoryHandler {
            memid,
            local_size: 0,
            mem_map: AHashMap::default(),
            memory_size: 0,
            swap_i32: [
                locals.add(walrus::ValType::I32),
                locals.add(walrus::ValType::I32),
            ],
            swap_f32: [
                locals.add(walrus::ValType::F32),
                locals.add(walrus::ValType::F32),
            ],
        }
    }

    pub fn get_port_address(&self, port: OutportLocation) -> Value {
        Value::I32(self.mem_map.get(&port).unwrap().base_addr)
    }

    pub fn get_port_element(&self, port: OutportLocation) -> MemElement {
        self.mem_map.get(&port).unwrap().clone()
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
        println!(
            "Alloc<{ty:?}> {port:?} @ {} .. {} ({}byte)",
            element.base_addr, self.memory_size, element_size
        );
        self.mem_map.insert(port, element);
    }
}
