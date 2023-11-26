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
    EntryVariableInput(usize),
    RecursionVariableResult(usize),
    ContextVariableInput(usize),
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
    LambdaDecleration,
    DeltaDecleration,
    RecursionVariableOutput(usize),
    RecursionVariableArgument(usize),
    EntryVariableArgument {
        branch: usize,
        entry_variable: usize,
    },
    ExitVariableOutput(usize),
    ContextVariableArgument(usize),
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
