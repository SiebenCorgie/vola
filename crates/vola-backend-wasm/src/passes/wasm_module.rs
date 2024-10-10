/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Transforms the current graph into a walrus WASM module.

use ahash::AHashMap;
use memory::MemoryHandler;
use rvsdg::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_common::{error::error_reporter, report, Span};
use walrus::{ir::InstrSeqId, FunctionBuilder, GlobalId, LocalId, Module, ValType};

use crate::{graph::WasmTy, runtime, WasmBackend, WasmError};

mod memory;
mod pda;

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
}

impl WasmLambdaBuilder {
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
            label: FlagStore::new(),
            global_stack_pointer: stackptr,
            name: symbol_name,
        };
        sm
    }
}

impl WasmBackend {
    pub fn into_wasm_module(&mut self) -> Result<walrus::Module, WasmError> {
        //First step is to load the runtime as the base module.
        let mut module = runtime::load_runtime_module()?;
        //let mut module = walrus::Module::with_config(ModuleConfig::new());
        let mut last_fail = None;

        //search for all exports, read the name,
        //and start emitting them
        let tlreg = self.graph.toplevel_region();
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

                if let Err(err) = self.emit_export(res_source.node, export_symbol, &mut module) {
                    let span = self.span_or_empty(res_source.node);
                    let errstr = err.to_string();
                    report(error_reporter(Box::new(errstr), span).finish());
                    last_fail = Some(err);
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

        if let Some(err) = last_fail {
            Err(err)
        } else {
            Ok(module)
        }
    }

    pub fn emit_export(
        &mut self,
        export: NodeRef,
        symbol_name: String,
        module: &mut walrus::Module,
    ) -> Result<(), WasmError> {
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
        let function_id = lmd_builder.serialize_cfg(cfg, self, module, fn_builder)?;
        //and finish by exporting the symbol
        module.exports.add(&symbol_name, function_id);

        Ok(())
    }
}
