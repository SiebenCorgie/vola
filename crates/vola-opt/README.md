<div align="center">

# Vola-Opt

The _optimizer_ or _middle-end_ of the vola compiler infrastructure.

There are multiple levels within opt, that get progressively lowered from an AST-like representation to an SPIR-V like representation using the 
[RVSDG](https://gitlab.com/tendsinmende/vola/-/tree/main/crates/rvsdg) crate.

[![dependency status](https://deps.rs/repo/gitlab/tendsinmende/vola/status.svg)](https://deps.rs/repo/gitlab/tendsinmende/vola)
[![MPL2.0](https://img.shields.io/badge/License-MPL_2.0-blue)](LICENSE)

</div>


## Hints

- export `VOLA_BACKTRACE=1` to enable backtraces whenever the optimizer panics.
- export `VOLA_DUMP_ALL=1` to dump intermediate states of the optimizers to svg
  -> Have a look at the code base / grep for `VOLA_DUMP_*` to dump specific pass stages
- export `VOLA_EXPORT_ALL=1` to export all λ-Regions, which in turn makes them visible to the debugger in all stages.

## Todo 📔

- [ ] Right now we'll have a lot of dead nodes in the λ-Regions after some transformations. That is not _really_ a problem, but implementing the generic dead-node-elimination would probably be a good idea anyways
- [ ] We currently don't catch recursive field-calls. Instead the compiler panics when trying to inline those atm.
- [ ] Right now we do _semantic_ checks whenever they matter. It would be nice to do that _in-one-place_ instead. ATM. there are two times where we can check semantic. Once when doing AST->RVSDG transformation, and once when doing the field-dispatch.
- [ ] We do somtimes depend on a pass being run _before_ some other pass. In a perfect world we might be able to define pass dependecies. However I feel thats more like a a _compiler-framework_ property, which vola isn't right now. But might be interesting further down the (development)-road
