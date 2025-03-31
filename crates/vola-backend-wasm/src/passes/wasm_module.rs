/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Transforms the current graph into a walrus WASM module.

use ahash::AHashSet;
use memory::MemoryHandler;
use rvsdg::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_common::{error_reporter, report, Span};
use walrus::{
    ir::{InstrSeqId, Value},
    ConstExpr, ExportItem, FunctionId, GlobalId, GlobalKind, LocalId, Module, ValType,
};

use crate::{graph::WasmTy, runtime, WasmBackend, WasmError};

mod memory;
mod pda;

pub(crate) struct WasmLmdBuildReport {
    //How many bytes the generated code needs
    pub additional_memory: usize,
    pub function_id: FunctionId,
    pub touched_functions: AHashSet<String>,
}

struct ArgumentCtx {
    port: OutportLocation,
    local_id: Option<SmallColl<LocalId>>,
    ty: WasmTy,
}

struct ResultCtx {
    #[allow(dead_code)]
    port: InportLocation,
    ty: WasmTy,
}

struct FunctionBuilderCtx {
    pub arguments: SmallColl<ArgumentCtx>,
    pub results: SmallColl<ResultCtx>,
    pub lamda: NodeRef,
}

///The helper that tracks the virtual heap for building a λ-node.
pub struct WasmLambdaBuilder {
    mem: MemoryHandler,
    ctx: FunctionBuilderCtx,

    //Maps regions to instruction sequences of `fn_builder`
    label: FlagStore<InstrSeqId>,

    ///The function local stack pointer set at the start of the function
    fn_local_stack_pointer: LocalId,
    ///The global stack pointer id we are reusing from the rust code
    global_stack_pointer: GlobalId,
    name: String,

    touched_export_functions: AHashSet<String>,
}

impl WasmLambdaBuilder {
    pub fn find_stack_pointer(module: &Module) -> GlobalId {
        module
            .globals
            .iter()
            .find_map(|g| {
                if g.name == Some("__stack_pointer".to_string()) {
                    Some(g.id())
                } else {
                    None
                }
            })
            .expect("Failed to find stack pointer in module!")
    }

    fn init_for_ctx(
        ctx: FunctionBuilderCtx,
        module: &mut Module,
        symbol_name: String,
    ) -> WasmLambdaBuilder {
        //Allocate ourself some empty memory
        //let memid = module.memories.add_local(false, false, 0, None, None);

        let memid = module.get_memory_id().unwrap();
        let fn_local_stack_pointer = module.locals.add(ValType::I32);
        module.locals.get_mut(fn_local_stack_pointer).name =
            Some("__vola_stack_pointer".to_string());

        let stackptr = Self::find_stack_pointer(module);

        let sm = WasmLambdaBuilder {
            mem: MemoryHandler::empty(memid, &mut module.locals),
            ctx,
            fn_local_stack_pointer,
            label: FlagStore::new(),
            global_stack_pointer: stackptr,
            name: symbol_name,
            touched_export_functions: AHashSet::default(),
        };
        sm
    }
}

impl WasmBackend {
    ///Standard WASM page size in bytes.
    const WASM_PAGE_SIZE: u64 = 65536;

    pub fn into_wasm_module(&mut self) -> Result<walrus::Module, WasmError> {
        //First step is to load the runtime as the base module.
        let mut module = runtime::load_runtime_module()?;
        //let mut module = walrus::Module::with_config(ModuleConfig::new());
        let mut last_fail = None;

        //search for all exports, read the name,
        //and start emitting them
        let tlreg = self.graph.toplevel_region();

        let mut touched_functions = AHashSet::default();
        let mut max_additional_pages = 0u64;

        for resixd in 0..self.graph[tlreg].results.len() {
            let result_port = tlreg.node.as_inport_location(InputType::Result(resixd));
            if let Some(res_source) = self.graph.inport_src(result_port) {
                let export_symbol = if let Some(symbol) = self.names.get(&res_source.node.into()) {
                    symbol.clone()
                } else {
                    last_fail = Some(WasmError::UnnamedExport(res_source.node));

                    report(
                        error_reporter(WasmError::UnnamedExport(res_source.node), Span::empty())
                            .finish(),
                    );

                    continue;
                };

                touched_functions.insert(export_symbol.clone());

                match self.emit_export(res_source.node, export_symbol, &mut module) {
                    Err(err) => {
                        let span = self.span_or_empty(res_source.node);
                        let errstr = err.to_string();
                        report(error_reporter(Box::new(errstr), span).finish());
                        last_fail = Some(err);
                    }
                    Ok(WasmLmdBuildReport {
                        additional_memory,
                        function_id: _,
                        touched_functions: tf,
                    }) => {
                        touched_functions.extend(tf.into_iter());
                        //NOTE this is in 64kbit pages, so we have to divide. However, take at least one
                        max_additional_pages = max_additional_pages
                            .max((additional_memory as u64 / Self::WASM_PAGE_SIZE).max(1))
                    }
                }
            } else {
                let error = WasmError::ExportUnconnected(result_port);
                report(
                    error_reporter(WasmError::ExportUnconnected(result_port), Span::empty())
                        .finish(),
                );
                last_fail = Some(error);
            }
        }

        //delete unused functions
        Self::delete_unused_exports(touched_functions, &mut module);

        //For _reasons_ it can happen, that rust comes up with a default stack pointer that points _outside_
        //of our memory. For that reason, read the default stack pointer, fit that to the last page it points to, and then add
        //our additional pages

        let default_stack_ptr = WasmLambdaBuilder::find_stack_pointer(&module);
        let default_stack_ptr_value = match &module.globals.get(default_stack_ptr).kind {
            GlobalKind::Local(ConstExpr::Value(Value::I32(i))) => *i as u64,
            GlobalKind::Local(ConstExpr::Value(Value::I64(i))) => *i as u64,
            e => panic!("unknows default stack pointer type: {e:?}"),
        };

        #[cfg(feature = "log")]
        log::info!(
            "Default stack ptr: {}byte / {}pages",
            default_stack_ptr_value,
            default_stack_ptr_value / Self::WASM_PAGE_SIZE + 1
        );
        let minimal_initial_size = (default_stack_ptr_value / Self::WASM_PAGE_SIZE).max(1);
        let initial_size = minimal_initial_size + max_additional_pages;

        #[cfg(feature = "log")]
        log::info!(
            "Growing WASM inital memory form {} pages {}kbit to: {} pages / {}kbit",
            default_stack_ptr_value / Self::WASM_PAGE_SIZE,
            default_stack_ptr_value / 1024,
            initial_size,
            (initial_size * Self::WASM_PAGE_SIZE) / 1024
        );

        //Now reconfigure
        module
            .memories
            .get_mut(module.get_memory_id().unwrap())
            .initial = initial_size;

        if let Some(err) = last_fail {
            Err(err)
        } else {
            Ok(module)
        }
    }

    //takes care of deleting all unused export functions
    fn delete_unused_exports(touched_functions: AHashSet<String>, module: &mut walrus::Module) {
        let mut to_be_deleted = Vec::new();
        let mut fexp_count = 0;
        for f in module.exports.iter() {
            if let ExportItem::Function(_) = &f.item {
                fexp_count += 1;
                if !touched_functions.contains(&f.name) {
                    to_be_deleted.push(f.id());
                }
            }
        }

        #[cfg(feature = "log")]
        log::info!(
            "Could delete {}/{} Wasm function exports",
            to_be_deleted.len(),
            fexp_count
        );

        for del in to_be_deleted {
            module.exports.delete(del);
        }
    }

    fn emit_export(
        &mut self,
        export: NodeRef,
        symbol_name: String,
        module: &mut walrus::Module,
    ) -> Result<WasmLmdBuildReport, WasmError> {
        //Use the structurized to get a CFG of our graph. Then
        //walk the CFG in parallel with the walrus builder to emit the code into the module.

        let input_signature = self.input_signature(export)?;
        let output_signature = self.output_signature(export)?;

        let arguments = input_signature
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| {
                if let Some(ty) = ty {
                    ArgumentCtx {
                        port: export.as_outport_location(OutputType::Argument(idx)),
                        local_id: None,
                        ty,
                    }
                } else {
                    ArgumentCtx {
                        port: export.as_outport_location(OutputType::Argument(idx)),
                        local_id: None,
                        ty: WasmTy::Undefined,
                    }
                }
            })
            .collect();

        let results = output_signature
            .into_iter()
            .enumerate()
            .map(|(idx, res)| {
                if let Some(r) = res {
                    ResultCtx {
                        port: export.as_inport_location(InputType::Result(idx)),
                        ty: r,
                    }
                } else {
                    ResultCtx {
                        port: export.as_inport_location(InputType::Result(idx)),
                        ty: WasmTy::Undefined,
                    }
                }
            })
            .collect();

        let fnctx = FunctionBuilderCtx {
            arguments,
            results,
            lamda: export,
        };

        //Build the CFG for this region
        let cfg = self.graph.region_to_cfg_scfr(RegionLocation {
            node: export,
            region_index: 0,
        })?;

        //Now init the Wasm-λ-builder for the module and node
        let lmd_builder = WasmLambdaBuilder::init_for_ctx(fnctx, module, symbol_name.clone());

        //start the function builder
        let mut params = SmallColl::new();
        for arg in lmd_builder.ctx.arguments.iter() {
            arg.ty.append_elements_to_signature(&mut params);
        }

        let mut results = SmallColl::new();
        for res in lmd_builder.ctx.results.iter() {
            res.ty.append_elements_to_signature(&mut results);
        }
        //setup the export's type
        //let export_type_id = module.types.add(&params, &results);
        let mut fn_builder = walrus::FunctionBuilder::new(&mut module.types, &params, &results);
        fn_builder.name(symbol_name.to_string());

        //Serialize the λ into the `module`
        let build_report = lmd_builder.serialize_cfg(cfg, self, module, fn_builder)?;
        //and finish by exporting the symbol
        module.exports.add(&symbol_name, build_report.function_id);

        Ok(build_report)
    }
}
