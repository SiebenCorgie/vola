use chumsky::{error::Simple, Parser};

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::NodeRef;
use std::{fmt::Display, str::FromStr};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

#[cfg(feature = "parse")]
impl InputType {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        use crate::util::parse::{enum_with_usize, usize_parser};
        use chumsky::prelude::*;
        //Our parser just accepts exactly what we produce via display
        let parser = choice((
            just("GammaPred").map(|_| Self::GammaPredicate),
            just("ThetaPred").map(|_| Self::ThetaPredicate),
            just("EVRes")
                .then(usize_parser().delimited_by(just('['), just(']')))
                .then(usize_parser().delimited_by(just('('), just(')')))
                .map(|((_, branch), exit_variable)| Self::ExitVariableResult {
                    branch,
                    exit_variable,
                }),
            enum_with_usize("In", Self::Input),
            enum_with_usize("Res", Self::Result),
            enum_with_usize("EVIn", Self::EntryVariableInput),
            enum_with_usize("RVRes", Self::RecursionVariableResult),
            enum_with_usize("CVIn", Self::ContextVariableInput),
        ))
        .then_ignore(end());
        parser
    }
}

#[cfg(feature = "parse")]
impl FromStr for InputType {
    type Err = Vec<Simple<char>>;
    ///Tries to parse the input type from a string. This is bijektive with the [Display] implementation.
    ///
    ///For any `a: InputType` the following holds true:
    ///```
    /// use rvsdg::edge::InputType;
    /// let a = InputType::Input(1);
    /// assert_eq!(a, format!("{}", a).parse().unwrap());
    ///```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        InputType::parser().parse(s)
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

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InportLocation {
    pub node: NodeRef,
    pub input: InputType,
}

impl Display for InportLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Inport[{}, {}]", self.node, self.input)
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

#[cfg(feature = "parse")]
impl OutputType {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        use crate::util::parse::{enum_with_usize, usize_parser};
        use chumsky::prelude::*;
        //Our parser just accepts exactly what we produce via display
        let parser = choice((
            just("LambdaDecl").map(|_| Self::LambdaDeclaration),
            just("DeltaDecl").map(|_| Self::DeltaDeclaration),
            just("EVArg")
                .then(usize_parser().delimited_by(just('['), just(']')))
                .then(usize_parser().delimited_by(just('('), just(')')))
                .map(
                    |((_, branch), entry_variable)| Self::EntryVariableArgument {
                        branch,
                        entry_variable,
                    },
                ),
            enum_with_usize("Out", Self::Output),
            enum_with_usize("Arg", Self::Argument),
            enum_with_usize("RVOut", Self::RecursionVariableOutput),
            enum_with_usize("RVArg", Self::RecursionVariableArgument),
            enum_with_usize("EVOut", Self::ExitVariableOutput),
            enum_with_usize("CVArg", Self::ContextVariableArgument),
        ))
        .then_ignore(end());
        parser
    }
}

#[cfg(feature = "parse")]
impl FromStr for OutputType {
    type Err = Vec<Simple<char>>;
    ///Tries to parse the output type from a string. This is bijektive with the [Display] implementation.
    ///
    ///For any `a: OutputType` the following holds true:
    ///```
    /// use rvsdg::edge::OutputType;
    /// let a = OutputType::Output(1);
    /// assert_eq!(a, format!("{}",a).parse().unwrap());
    ///```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        OutputType::parser().parse(s)
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

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutportLocation {
    pub node: NodeRef,
    pub output: OutputType,
}

impl Display for OutportLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Outport[{}, {}]", self.node, self.output)
    }
}

///An edge in a RVSDG is always a directed edge from `src` to `dst`. It contains a language specific
/// edge-type `E`, that represent the dependency type. In general there are at least `State` and `Value` edges, but a
/// language is free to represent more sophisticated dependencies through those types.
///
/// `dst` and `src` are exposed read only, because _by hand_ change of those can lead to an invalid RVSDG.
/// If you want to change those, consider using [connect](crate::Rvsdg::connect) / [disconnect](crate::Rvsdg::disconnect).
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

#[cfg(test)]
mod test {
    use crate::edge::OutputType;

    use super::InputType;

    #[test]
    fn input_type_parse_gamma_predicate() {
        let a = InputType::GammaPredicate;
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_theta_predicate() {
        let a = InputType::ThetaPredicate;
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_input() {
        let a = InputType::Input(42);
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_result() {
        let a = InputType::Result(420);
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_exit_var_result() {
        let a = InputType::ExitVariableResult {
            branch: 65,
            exit_variable: 87,
        };
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_entry_variable_input() {
        let a = InputType::EntryVariableInput(56321);
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_recursion_variable_result() {
        let a = InputType::RecursionVariableResult(654);
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }
    #[test]
    fn input_type_parse_context_variable_input() {
        let a = InputType::ContextVariableInput(6548);
        assert_eq!(a, format!("{a}").parse::<InputType>().unwrap());
    }

    #[test]
    fn input_type_parse_fails() {
        let a = InputType::ContextVariableInput(6548);
        assert!(format!("{a}something").parse::<InputType>().is_err());
    }

    #[test]
    fn output_type_parse_output() {
        let a = OutputType::Output(645);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_argument() {
        let a = OutputType::Argument(545);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_lambda_decl() {
        let a = OutputType::LambdaDeclaration;
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_delta_decl() {
        let a = OutputType::DeltaDeclaration;
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_recursion_output() {
        let a = OutputType::RecursionVariableOutput(32198);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_recursion_arg() {
        let a = OutputType::RecursionVariableArgument(9324);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_ev_arg() {
        let a = OutputType::EntryVariableArgument {
            branch: 64,
            entry_variable: 128,
        };
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_ev_out() {
        let a = OutputType::ExitVariableOutput(654);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_cv_arg() {
        let a = OutputType::ContextVariableArgument(654);
        assert_eq!(a, format!("{a}").parse::<OutputType>().unwrap());
    }
    #[test]
    fn output_type_parse_wrong() {
        let a = OutputType::Output(645);
        assert!(format!("berg{a}").parse::<OutputType>().is_err());
    }
}
