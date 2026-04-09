//! # Vola-Bridge
//!
//! Bridges the Vola-As-A-Library use-case. Provides a thin interface to recompile and call Vola functions from Rust.
//!
//!
//! ## Usage
//!
//! The main entrypoint is [load] / [watch]. It lets you compile a vola file (or source-code) and build the call-information.
//! The resulting module can be querried for exported functions, as well as calling those functions.
//!
//! ## Example
//!
//! ```rust,ignore
//! //Load the file + function. Also saves the _ffi_ information.
//! let module = bridge::load("myfile.vola", [])?;
//! let result = module
//!     .call("my_function")?
//!     //Checks that "arg" is indeed a vec3
//!    .with("arg", [1.0, 0.0, 1.0])?
//!    //Returns the result of the call. We might add eval_simd
//!    //or other _special_ versions later.
//!    .eval()?;
//! ```

use std::path::{Path, PathBuf};

use ahash::AHashMap;
use thiserror::Error;
use vola_lib::{
    FunctionSignature, InterfaceDescriptor, OptModule,
    passes::PassError,
    vola_opt::{Optimizer, common::Ty},
    wasm::vola_backend_wasm::WasmError,
};

use crate::types::TypedValue;

mod types;

#[derive(Debug, Error)]
pub enum BridgeError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("{0}")]
    WasmError(String),
    #[error("Had {0} compilation errors, see stdout")]
    CompileError(usize),

    #[error("Expected {0}")]
    TypeError(Ty),
    #[error("{0}")]
    Runtime(String),
}

impl From<PassError> for BridgeError {
    fn from(value: PassError) -> Self {
        let count = value.errors.len();
        for err in value.errors {
            err.report();
        }

        BridgeError::CompileError(count)
    }
}

impl From<WasmError> for BridgeError {
    fn from(value: WasmError) -> Self {
        BridgeError::WasmError(value.to_string())
    }
}

pub fn load_file(
    file: &dyn AsRef<Path>,
    external: impl IntoIterator<Item = (String, PathBuf)>,
) -> Result<Module, BridgeError> {
    Module::from_file(file, external)
}

///Loads the `code`, assuming that the workspace this code comes from is the current execution
/// directiory.
///
/// Consider using [Module::from_code] if you need finer control.
pub fn load_code(
    code: &str,
    external: impl IntoIterator<Item = (String, PathBuf)>,
) -> Result<Module, BridgeError> {
    Module::from_code(code.as_bytes(), Path::new("./"), external)
}

pub struct Module {
    ///The execution-engine we use, i.e. the wasmtime engine atm.
    #[allow(unused)]
    engine: wasmtime::Engine,
    #[allow(unused)]
    linker: wasmtime::Linker<()>,
    #[allow(unused)]
    module: wasmtime::Module,
    store: wasmtime::Store<()>,
    instance: wasmtime::Instance,
    interface: InterfaceDescriptor,
}

impl Module {
    pub fn from_optimizer(optimizer: Optimizer) -> Result<Self, BridgeError> {
        let mut module = vola_lib::OptModule::new();
        module.opt = optimizer;
        Self::from_module(module)
    }

    ///Loads the module from a byte string that is interpreted as an UTF8 string.
    ///
    /// Uses `external` to resolve external dependencies to the given path.
    pub fn from_code(
        code: &[u8],
        workspace: impl AsRef<Path>,
        external: impl IntoIterator<Item = (String, PathBuf)>,
    ) -> Result<Self, BridgeError> {
        let ast =
            vola_lib::passes::LowerAst::from_bytes(code, workspace, external).map_err(|e| {
                let count = e.len();
                for e in e {
                    e.report();
                }
                BridgeError::CompileError(count)
            })?;

        //Lower the ast into the optimizer
        let mut module = vola_lib::OptModule::new();
        module.apply_pass(ast)?;
        Self::from_module(module)
    }

    pub fn from_file(
        file: &dyn AsRef<Path>,
        external: impl IntoIterator<Item = (String, PathBuf)>,
    ) -> Result<Self, BridgeError> {
        let ast = vola_lib::passes::LowerAst::from_file(file, external).map_err(|e| {
            let count = e.len();
            for e in e {
                e.report();
            }
            BridgeError::CompileError(count)
        })?;

        //Lower the ast into the optimizer
        let mut module = vola_lib::OptModule::new();
        module.apply_pass(ast)?;
        Self::from_module(module)
    }

    ///Creates the compiled module from some _pre-prepared_ module. Always executes the standandard pipeline.
    fn from_module(mut module: OptModule) -> Result<Self, BridgeError> {
        //apply the standard pipeline
        module.standard_pipeline()?;

        let interface = module.build_interface_descriptor().map_err(|e| {
            e.report();
            BridgeError::Runtime("Could not build interface descriptor!".to_string())
        })?;

        let mut wasm = vola_lib::wasm::WasmModule::new();
        wasm.lower_opt(module)?;

        let bytes = wasm.build(false)?;

        let mut wasm_config = wasmtime::Config::new();
        wasm_config.wasm_backtrace(true);
        wasm_config.wasm_backtrace_details(wasmtime::WasmBacktraceDetails::Enable);
        let engine = wasmtime::Engine::new(&wasm_config).map_err(|e| {
            BridgeError::WasmError(format!("Failed to start wasmtime Engine: {e:?}"))
        })?;

        let module = wasmtime::Module::new(&engine, bytes).map_err(|e| {
            BridgeError::WasmError(format!("Failed to load buffer as WASM module: {e:?}"))
        })?;

        let linker = wasmtime::Linker::new(&engine);
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = linker.instantiate(&mut store, &module).map_err(|e| {
            if let Some(trap) = e.downcast_ref::<wasmtime::Trap>() {
                BridgeError::WasmError(format!(
                    "Failed to instatiate WASM-Time: {trap:?}\n\nBacktrace:\n{}",
                    e.backtrace()
                ))
            } else {
                BridgeError::WasmError(format!("Failed to instatiate WASM-Time: {e:?}"))
            }
        })?;

        Ok(Module {
            engine,
            linker,
            module,
            store,
            instance,
            interface,
        })
    }

    ///Lists the name of each callable function
    pub fn list_functions(&self) -> impl Iterator<Item = &String> {
        self.interface.functions.iter().map(|i| &i.symbol_name)
    }

    ///Tries to create a [CallBuilder] for a function with the given `function_name`. Reutrns None, if
    /// such a function does not exist
    pub fn call<'a>(&'a mut self, function_name: &str) -> Result<CallBuilder<'a>, BridgeError> {
        //map the called function name to the wasm symbol, then build the expected signature with default values
        // and return the actual builder.

        let Some(wasm_symbol) = self.instance.get_func(&mut self.store, function_name) else {
            return Err(BridgeError::Runtime(format!(
                "Function '{}' does not exist in the WASM module!",
                function_name
            )));
        };

        let Some(signature) = self
            .interface
            .functions
            .iter()
            .find(|f| f.symbol_name.as_str() == function_name)
            .cloned()
        else {
            return Err(BridgeError::Runtime(format!(
                "Function '{}' does not exist in interface description!",
                function_name
            )));
        };

        //build the default initialized values for result and arg
        let args = signature
            .args
            .iter()
            .map(|arg| (arg.name.clone(), TypedValue::initialize(&arg.ty)))
            .collect();

        let results = signature
            .results
            .iter()
            .map(|i| TypedValue::initialize(i))
            .collect();

        Ok(CallBuilder {
            module: self,
            function: wasm_symbol,
            signature,
            args,
            results,
        })
    }
}

pub struct CallBuilder<'a> {
    module: &'a mut Module,
    function: wasmtime::Func,
    signature: FunctionSignature,
    ///Pre-Initialized call-args
    args: AHashMap<String, TypedValue>,
    ///Pre-Initialized results, typically unnamed
    results: Vec<TypedValue>,
}

impl<'a> CallBuilder<'a> {
    ///Returns the argument type of `arg`, if it exists.
    pub fn arg_type(&self, arg: &str) -> Option<&Ty> {
        self.args.get(arg).map(|arg| &arg.ty)
    }

    pub fn with_arg(
        mut self,
        arg: &str,
        value: impl Into<TypedValue>,
    ) -> Result<Self, BridgeError> {
        let Some(arg) = self.args.get_mut(arg) else {
            return Err(BridgeError::Runtime(format!(
                "Argument '{arg}' does not exist!"
            )));
        };

        let parsed = value.into();
        //If the parsed and expected type match, overwrite
        if parsed.ty == arg.ty {
            arg.values = parsed.values;
        } else {
            return Err(BridgeError::TypeError(arg.ty.clone()));
        }

        Ok(self)
    }

    ///Calls the function with the current set of arguments.
    ///
    /// Returns the result of the function after execution, or the builder, and an error that occured while executing.
    ///
    /// If you want to call the same function repeatedly with different arguments, consider
    /// [call_then](Self::call_then).
    pub fn call_once(mut self) -> Result<Vec<TypedValue>, BridgeError> {
        self.call()
    }

    ///Same as [call_once](Self::call_once), but does not destroy the call-builder in the successful case.
    ///
    /// Lets you call the same function multiple times with different arguments.
    pub fn call(&mut self) -> Result<Vec<TypedValue>, BridgeError> {
        //Prepare the results store by flattening structured data
        let mut results_flat = self
            .results
            .iter()
            .map(|res| res.values.iter().cloned())
            .flatten()
            .collect::<Vec<_>>();
        //Flatten all arguments _in order_ of the signature
        let arguments_flat = self
            .signature
            .args
            .iter()
            .map(|arg| self.args.get(&arg.name).unwrap().values.iter().cloned())
            .flatten()
            .collect::<Vec<_>>();

        match self
            .function
            .call(&mut self.module.store, &arguments_flat, &mut results_flat)
        {
            Ok(_) => {
                //build back the structured result.

                let (last_offset, structured_result) = self.results.iter().fold(
                    (0, Vec::with_capacity(self.results.len())),
                    |(offset, mut results), template_value| {
                        let mut local_result = template_value.clone();
                        for (value_offset, local_result_value) in
                            local_result.values.iter_mut().enumerate()
                        {
                            //Index into the overall string of values
                            let result_index = offset + value_offset;
                            let candidate = results_flat[result_index];

                            //Make sure we don't overwrite unknown stuff...
                            assert!(
                                candidate
                                    .matches_ty(
                                        &self.module.store,
                                        &local_result_value.ty(&self.module.store).unwrap()
                                    )
                                    .unwrap()
                            );
                            //types match, overwrite result
                            *local_result_value = candidate;
                        }

                        //Push back the local result
                        results.push(local_result);
                        //now update the offset to the next set of values
                        (offset + template_value.values.len(), results)
                    },
                );

                //Those must match, since we must build-back all of them.
                assert_eq!(last_offset, results_flat.len());

                Ok(structured_result)
            }
            Err(e) => Err(BridgeError::Runtime(format!("Execution failed: {e:?}"))),
        }
    }
}
