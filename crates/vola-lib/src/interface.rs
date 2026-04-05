use rvsdg::NodeRef;
use vola_opt::common::Ty;

pub struct NamedValue {
    pub name: String,
    pub ty: Ty,
}

pub struct FunctionSignature {
    ///The function's ffi-name, i.e. the name
    /// that is used at link-time
    pub symbol_name: String,
    ///The lambda node in the source graph this signature is based on.
    pub lambda: NodeRef,
    ///All arguments an their type, ordered in occurence.
    pub args: Vec<NamedValue>,
    pub results: Vec<Ty>,
}

pub struct InterfaceDescriptor {
    pub functions: Vec<FunctionSignature>,
    //TODO: collect foreighn-data signature.
}
