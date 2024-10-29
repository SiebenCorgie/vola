/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::NodeRef;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum InputType {
    ///Any input to the node. Could also be a exit variable etc.
    Input(usize),

    ///Any result to the node's body that is not a special argument like _recusion_variable_ or _predicate_.
    Result(usize),
    ///Predicate to a gamma node, which is always the first input to the gamma node.
    GammaPredicate,
    ///Predicate to a theta node, which is always the first result of the theta nodes body.
    ThetaPredicate,
    ExitVariableResult {
        branch: usize,
        exit_variable: usize,
    },
    EntryVariableInput(usize),
    RecursionVariableResult(usize),
    ContextVariableInput(usize),
}

impl Display for InputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Input(i) => write!(f, "In({i})"),
            Self::Result(i) => write!(f, "Res({i})"),
            Self::GammaPredicate => write!(f, "GammaPred"),
            Self::ThetaPredicate => write!(f, "ThetaPred"),
            Self::ExitVariableResult {
                branch,
                exit_variable,
            } => write!(f, "EVRes[{branch}]({exit_variable})"),

            Self::EntryVariableInput(i) => write!(f, "EVIn({i})"),
            Self::RecursionVariableResult(i) => write!(f, "RVRes({i})"),
            Self::ContextVariableInput(i) => write!(f, "CVIn({i})"),
        }
    }
}

impl InputType {
    ///If this is some kind of result, returns the region index.
    /// This mostly will be 0, except for ExitVariableResult.
    pub fn result_region_index(&self) -> Option<usize> {
        match self {
            Self::Result(_) | Self::RecursionVariableResult(_) | Self::ThetaPredicate => Some(0),
            Self::ExitVariableResult {
                branch,
                exit_variable: _,
            } => Some(*branch),
            _ => None,
        }
    }

    ///Returs true if `self` is some kind of result to a region. Is true for
    /// - Result
    /// - ExitVariableResult
    /// - ThetaPredicate
    /// - RecursionVariableResult
    /// - LoopVariableResult
    pub fn is_result(&self) -> bool {
        match self {
            InputType::Result(_)
            | InputType::ThetaPredicate
            | InputType::RecursionVariableResult(_)
            | InputType::ExitVariableResult { .. } => true,
            _ => false,
        }
    }

    ///Returns true for GammaPredicate and ThetaPredicate
    pub fn is_predicate(&self) -> bool {
        if let Self::GammaPredicate | Self::ThetaPredicate = self {
            true
        } else {
            false
        }
    }

    //If possible, tries to map this input to its equivalent port within a region.
    //This only works for ContextVariableInput, EntryVariableInput and Input
    pub fn map_to_in_region(&self, region_index: usize) -> Option<OutputType> {
        match self {
            Self::ContextVariableInput(i) => {
                assert!(region_index == 0, "region index needs to be 0, since context-variables can only map to single internal region");
                Some(OutputType::ContextVariableArgument(*i))
            }
            Self::EntryVariableInput(i) => Some(OutputType::EntryVariableArgument {
                branch: region_index,
                entry_variable: *i,
            }),
            Self::Input(i) => Some(OutputType::Argument(*i)),
            _ => None,
        }
    }

    //If possible, maps the result-like input to an out-of-region equivalent output.
    //works for recursion-variable-result, loop-variable-result, exit-variable-result and result.
    pub fn map_out_of_region(&self) -> Option<OutputType> {
        match self {
            Self::RecursionVariableResult(i) => Some(OutputType::RecursionVariableOutput(*i)),
            Self::ExitVariableResult {
                branch: _,
                exit_variable,
            } => Some(OutputType::ExitVariableOutput(*exit_variable)),
            Self::Result(i) => Some(OutputType::Output(*i)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InportLocation {
    pub node: NodeRef,
    pub input: InputType,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum OutputType {
    ///Any output of the node. Could be a lambda definition, or a exit-variable output.
    Output(usize),
    ///Any argument to the node's body that is not a special argument like _recusion_variable_ or _context_variable_.
    Argument(usize),
    LambdaDeclaration,
    DeltaDeclaration,
    RecursionVariableOutput(usize),
    RecursionVariableArgument(usize),
    EntryVariableArgument {
        branch: usize,
        entry_variable: usize,
    },
    ExitVariableOutput(usize),
    ContextVariableArgument(usize),
}

impl Display for OutputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Output(o) => write!(f, "Out({o})"),
            Self::Argument(o) => write!(f, "Arg({o})"),
            Self::LambdaDeclaration => write!(f, "LambdaDecl"),
            Self::DeltaDeclaration => write!(f, "DeltaDecl"),
            Self::RecursionVariableOutput(o) => write!(f, "RVOut({o})"),
            Self::RecursionVariableArgument(o) => write!(f, "RVArg({o})"),
            Self::EntryVariableArgument {
                branch,
                entry_variable,
            } => write!(f, "EVArg[{branch}]({entry_variable})"),
            Self::ExitVariableOutput(o) => write!(f, "EVOut({o})"),
            Self::ContextVariableArgument(o) => write!(f, "CVArg({o})"),
        }
    }
}

impl OutputType {
    ///If this is a argument, returns the region index this argument resides in, within its
    /// parent node.
    /// This mostly will be 0, except for EntryVariableArguments
    pub fn argument_region_index(&self) -> Option<usize> {
        match self {
            OutputType::Argument(_)
            | OutputType::ContextVariableArgument(_)
            | OutputType::RecursionVariableArgument(_) => Some(0),
            OutputType::EntryVariableArgument {
                branch,
                entry_variable: _,
            } => Some(*branch),
            _ => None,
        }
    }

    ///Returns true if `self` is some kind of argument to a region. This is true for
    /// - Argument
    /// - ContextVariableArgument
    /// - RecursionVariableArgument
    /// - EntryVariableArgument
    pub fn is_argument(&self) -> bool {
        match self {
            OutputType::Argument(_)
            | OutputType::ContextVariableArgument(_)
            | OutputType::RecursionVariableArgument(_)
            | OutputType::EntryVariableArgument { .. } => true,
            _ => false,
        }
    }

    //If possible, maps the output-like type to an region-local result.
    //
    // This is basically the inverse to [InputType::map_out_of_region].
    pub fn map_to_in_region(&self, region_index: usize) -> Option<InputType> {
        match self {
            Self::Output(i) => Some(InputType::Result(*i)),
            Self::RecursionVariableOutput(i) => Some(InputType::RecursionVariableResult(*i)),
            Self::ExitVariableOutput(i) => Some(InputType::ExitVariableResult {
                branch: region_index,
                exit_variable: *i,
            }),
            _ => None,
        }
    }

    ///If possible, maps this argument-like output type to an equivalent input type.
    ///
    /// Is the inverse of [InputType::map_to_in_region].
    pub fn map_out_of_region(&self) -> Option<InputType> {
        match self {
            Self::ContextVariableArgument(i) => Some(InputType::ContextVariableInput(*i)),
            Self::EntryVariableArgument {
                branch: _,
                entry_variable,
            } => Some(InputType::EntryVariableInput(*entry_variable)),
            Self::Argument(i) => Some(InputType::Input(*i)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutportLocation {
    pub node: NodeRef,
    pub output: OutputType,
}

///An edge in a RVSDG is always a directed edge from `src` to `dst`. It contains a language specific
/// edge-type `E`, that represent the dependency type. In general there are at least `State` and `Value` edges, but a
/// language is free to represent more sophisticated dependencies through those types.
///
/// `dst` and `src` are exposed read only, because _by hand_ change of those can lead to an invalid RVSDG.
/// If you want to change those, consider using [connect](crate::Rvsdg::connect) / [disconnect](crate::Rvsdg::disconnect).
pub struct Edge<E: LangEdge + 'static> {
    pub(crate) src: OutportLocation,
    pub(crate) dst: InportLocation,
    pub ty: E,
}

impl<E: LangEdge + 'static> Edge<E> {
    pub fn dst(&self) -> &InportLocation {
        &self.dst
    }
    pub fn src(&self) -> &OutportLocation {
        &self.src
    }
}

pub trait LangEdge {
    ///Should create a
    fn value_edge() -> Self;
    fn state_edge() -> Self;
    fn is_value_edge(&self) -> bool;
    fn is_state_edge(&self) -> bool;
}
