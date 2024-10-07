/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Transforms the current graph into a walrus WASM module.

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_common::{error::error_reporter, report, Span};
use walrus::LocalId;

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
}

impl WasmBackend {
    pub fn into_wasm_module(&self) -> Result<walrus::Module, WasmError> {
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
        &self,
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

        let fnctx = FunctionBuilderCtx { arguments, results };

        //let (export_fn_id, _import) =
        //    module.add_import_func("wasm_runtime.wasm", &symbol_name, export_type_id);

        let cfg = self.graph.region_to_cfg_scfr(RegionLocation {
            node: export,
            region_index: 0,
        })?;

        let sm_sequence = self.init_sm_for_ctx(fnctx, module);
        let function_id = sm_sequence.serialize_cfg(cfg, self, export, module, &symbol_name)?;

        module.exports.add(&symbol_name, function_id);

        Ok(())
    }
}
