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
