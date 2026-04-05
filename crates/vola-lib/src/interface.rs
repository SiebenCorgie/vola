use rvsdg::NodeRef;
use vola_opt::common::Ty;

pub struct NamedValue {
    name: String,
    ty: Ty,
}

pub struct FunctionSignature {
    ///The function's ffi-name, i.e. the name
    /// that is used at link-time
    symbol_name: String,
    ///The lambda node in the source graph this signature is based on.
    lambda: NodeRef,
    ///All arguments an their type, ordered in occurence.
    args: Vec<NamedValue>,
    return_type: Ty,
}

pub struct InterfaceDescriptor {
    functions: Vec<FunctionSignature>,
    //TODO: collect foreighn-data signature.
}
