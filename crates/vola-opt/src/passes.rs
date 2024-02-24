//! Optimizer passes module.
//!
//! Right now we focus on _construction_ passes. Which means passes, that make it possible
//! to go from the RVSDG to a SPIR-V representation _at all_.
//!
//! Later on we'll probably start introducing optimizations and compiler-based inference of properties.

mod type_derive;
