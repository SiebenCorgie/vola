use crate::NodeRef;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    LoopVariableResult(usize),
    EntryVariableInput(usize),
    RecursionVariableResult(usize),
    ContextVariableInput(usize),
}

impl InputType {
    ///If this is some kind of result, returns the region index.
    /// This mostly will be 0, except for ExitVariableResult.
    pub fn result_region_index(&self) -> Option<usize> {
        match self {
            Self::Result(_)
            | Self::RecursionVariableResult(_)
            | Self::LoopVariableResult(_)
            | Self::ThetaPredicate => Some(0),
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
            | InputType::LoopVariableResult(_)
            | InputType::ExitVariableResult { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct InportLocation {
    pub node: NodeRef,
    pub input: InputType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct OutportLocation {
    pub node: NodeRef,
    pub output: OutputType,
}

///An edge in a RVSDG is always a directed edge from `src` to `dst`. It contains a language specific
/// edge-type `E`, that represent the dependency type. In general there are at least `State` and `Value` edges, but a
/// language is free to represent more sophisticated dependencies through those types.
pub struct Edge<E: LangEdge + 'static> {
    pub src: OutportLocation,
    pub dst: InportLocation,
    pub ty: E,
}

pub trait LangEdge {
    ///Should create a
    fn value_edge() -> Self;
    fn state_edge() -> Self;
    fn is_value_edge(&self) -> bool;
    fn is_state_edge(&self) -> bool;
}
