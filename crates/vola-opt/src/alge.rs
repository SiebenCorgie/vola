//! # Alge dialect
//!

use ahash::AHashMap;
use rvsdg::{
    attrib::AttribStore,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use vola_ast::{
    alge::FieldAccessor,
    csg::{CSGConcept, CSGNodeDef},
};

use crate::{common::Ty, error::OptError, DialectNode, OptEdge, OptGraph, TypeState};

pub(crate) mod implblock;

///Well known ops for the optimizer. Includes all _BinaryOp_ of the Ast, as well as
/// some well known _function_like_ ops in the SPIRV spec. For instance
#[derive(Debug, Clone, Copy)]
pub enum WkOp {
    //WK unary ops
    //TODO: do we really want to include NOT? I mean flipping floats is fun I guess,
    // but also error prone.
    Not,
    Neg,

    //WK _standard_ binary ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    //Call like ops we _know_
    Dot,
    Cross,
    Length,
    SquareRoot,
    Exp,
    Min,
    Max,
    Mix,
    Clamp,

    Abs,
    Frac,
}

impl WkOp {
    ///Returns the input-count for the op
    fn in_count(&self) -> usize {
        match self {
            WkOp::Not => 1,
            WkOp::Neg => 1,

            WkOp::Add => 2,
            WkOp::Sub => 2,
            WkOp::Mul => 2,
            WkOp::Div => 2,
            WkOp::Mod => 2,

            WkOp::Dot => 2,
            WkOp::Cross => 2,
            WkOp::Length => 1,
            WkOp::SquareRoot => 1,
            WkOp::Exp => 2,
            WkOp::Min => 2,
            WkOp::Max => 2,
            WkOp::Mix => 3,
            WkOp::Clamp => 3,

            WkOp::Abs => 1,
            WkOp::Frac => 1,
        }
    }

    pub fn try_parse(s: &str) -> Option<Self> {
        match s {
            "dot" => Some(Self::Dot),
            "cross" => Some(Self::Cross),
            "length" => Some(Self::Length),
            "sqrt" => Some(Self::SquareRoot),
            "exp" => Some(Self::Exp),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "mix" | "lerp" => Some(Self::Mix),
            "clamp" => Some(Self::Clamp),
            "frac" => Some(Self::Frac),
            "abs" => Some(Self::Abs),
            _ => None,
        }
    }

    fn try_derive_type(&self, mut sig: SmallVec<[Ty; 2]>) -> Result<Option<Ty>, OptError> {
        //NOTE: sig.len() is somewhat redundant, since the caller wouldn't try to derive if any input is
        // empty. However, its a good place to unify this check even if the type resolution changes at some point.

        match self {
            WkOp::Not | WkOp::Neg => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects one operand, got {:?}", self, sig.len()),
                    });
                }
                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operand (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[0]
                        ),
                    });
                }
                //seem allright, for neg, we return the _same_ datatype as we get
                Ok(Some(sig.remove(0)))
            }

            WkOp::Add | WkOp::Sub | WkOp::Mul | WkOp::Div | WkOp::Min | WkOp::Max => {
                //For those we need _the same algebraic_ for both inputs.
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands, to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Is okay, for these we return the _same_ type that we got
                Ok(Some(sig.remove(0)))
            }
            WkOp::Dot => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_vector() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects operands to be vectors, got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                Ok(Some(Ty::Scalar))
            }
            WkOp::Cross => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_vector() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects operands to be vectors, got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Cross returns the same vector type as supplied
                Ok(Some(sig[0].clone()))
            }
            WkOp::Length => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("Length expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Vector { .. } => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "Length expects operand of type vector, got {:?}",
                                sig[0]
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(Ty::Scalar))
            }

            WkOp::SquareRoot => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("SquareRoot expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Scalar => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "SquareRoot expects operand of type scalar, got {:?}",
                                sig[0]
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(Ty::Scalar))
            }
            WkOp::Exp => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("Exp expects two operand, got {:?}", sig.len()),
                    });
                }
                match (&sig[0], &sig[1]) {
                    (Ty::Scalar, Ty::Scalar) => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!("Exp expects operands of type scalar, got {:?}", sig),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(Ty::Scalar))
            }
            WkOp::Mix | WkOp::Clamp => {
                if sig.len() != 3 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects three operand, got {:?}", self, sig.len()),
                    });
                }
                match (&sig[0], &sig[1], &sig[2]) {
                    (Ty::Scalar, Ty::Scalar, Ty::Scalar) => {},
                    (Ty::Vector{width: w0}, Ty::Vector{width: w1}, Ty::Vector{width: w2}) => {
                        if w0 != w1 || w0 != w2{
                            return Err(OptError::Any {
                                text: format!("{:?} expects operands of type scalar or vector (of equal width), got {:?}", self, sig),
                            });
                        }
                    },
                    _ => {
                        return Err(OptError::Any {
                            text: format!("{:?} expects operands of type scalar or vector (of equal width), got {:?}", self, sig),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(sig[0].clone()))
            }

            WkOp::Abs | WkOp::Frac => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects one operand, got {:?}", self, sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Scalar => {}
                    Ty::Vector { .. } => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "{:?} expects operands of type scalar or vector, got {:?}",
                                self, sig
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(sig[0].clone()))
            }

            wk => Err(OptError::Any {
                text: format!("derive not implemented for {:?}", wk),
            }),
        }
    }
}

impl From<vola_ast::alge::UnaryOp> for WkOp {
    fn from(value: vola_ast::alge::UnaryOp) -> Self {
        match value {
            vola_ast::alge::UnaryOp::Neg => Self::Neg,
            vola_ast::alge::UnaryOp::Not => Self::Not,
        }
    }
}

impl From<vola_ast::alge::BinaryOp> for WkOp {
    fn from(value: vola_ast::alge::BinaryOp) -> Self {
        match value {
            vola_ast::alge::BinaryOp::Add => Self::Add,
            vola_ast::alge::BinaryOp::Sub => Self::Sub,
            vola_ast::alge::BinaryOp::Mul => Self::Mul,
            vola_ast::alge::BinaryOp::Div => Self::Div,
            vola_ast::alge::BinaryOp::Mod => Self::Mod,
        }
    }
}

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewAlgeOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
            }

            fn name(&self) -> String {
                format!($str, $(self.$arg)*,)
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    };
    ($opname:ident, $str:expr) =>{
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
            }

            fn name(&self) -> String {
                $str.to_owned()
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    }
}

///In the algebraic dialect all operations are unified into a Call-Like op.
///
/// Think of it like prefix notation. So the expression `a + b` becomes `add(a, b)`, `-a` becomes `neg(a)` etc.
///
/// this'll make optimizing easier later on.
///
/// Note that always only one result is returned. So only site-effect free ops can be modeled by this node.
#[derive(LangNode)]
pub struct CallOp {
    #[inputs]
    inputs: SmallVec<[Input; 2]>,
    #[output]
    output: Output,

    op: WkOp,
}

impl CallOp {
    pub fn new(op: WkOp) -> Self {
        let mut op = CallOp {
            inputs: SmallVec::new(),
            output: Output::default(),
            op,
        };
        //configure input count
        for _ in 0..op.op.in_count() {
            op.inputs.push(Input::default())
        }
        op
    }
}

implViewAlgeOp!(CallOp, "{:?}", op);
impl DialectNode for CallOp {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //For all WKOps we first collect all inputs, then let the op check itself.
        // For now we already bail if any type is unset, since we currently don't have any ops that
        // _don't_ care about any input.
        let mut signature: SmallVec<[Ty; 2]> = SmallVec::new();
        for (idx, inp) in self.inputs.iter().enumerate() {
            if let Some(edg) = inp.edge {
                //resolve if there is a type set
                if let Some(t) = graph.edge(edg).ty.get_type() {
                    signature.insert(idx, t.clone());
                } else {
                    return Ok(None);
                }
            } else {
                //input not set atm. so return None as well
                return Ok(None);
            }
        }

        self.op.try_derive_type(signature)
    }
}

///Dummy sink node for unimplemented stuff
#[derive(LangNode)]
pub struct DummyNode {
    #[output]
    out: Output,
}

impl DummyNode {
    #[allow(unused)]
    pub fn new() -> Self {
        DummyNode {
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(DummyNode, "Dummynode");
impl DialectNode for DummyNode {
    fn dialect(&self) -> &'static str {
        "dummy"
    }
}

///The Eval node in itself is a call-site to some connected λ-node, when specialised.
///
/// By definition the first argument is the callable that will be called, and all following ports are arguments
/// to that call.
#[derive(LangNode, Debug)]
pub struct EvalNode {
    #[inputs]
    inputs: SmallVec<[Input; 3]>,
    ///The eval node itsel has only one output, the state that is produced by the called concept.
    #[output]
    out: Output,
}

impl EvalNode {
    pub fn new(argount: usize) -> Self {
        EvalNode {
            inputs: smallvec![Input::default(); argount + 1],
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(EvalNode, "Eval");
impl DialectNode for EvalNode {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        graph: &OptGraph,
        concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //For eval nodes, the first type must be a callable, and all following must addher to the called concept's definition
        let mut signature: SmallVec<[Ty; 2]> = SmallVec::new();
        for (idx, inp) in self.inputs.iter().enumerate() {
            if let Some(edg) = inp.edge {
                //resolve if there is a type set
                if let Some(t) = graph.edge(edg).ty.get_type() {
                    signature.insert(idx, t.clone());
                } else {
                    return Ok(None);
                }
            } else {
                //input not set atm. so return None as well
                return Ok(None);
            }
        }

        if signature.len() == 0 {
            return Err(OptError::Any {
                text: format!("Eval must have at least a callable input, had none at all."),
            });
        }

        //Build the expected signature type
        let (expected_signature, output_ty, concept_name): (SmallVec<[Ty; 3]>, Ty, &String) =
            if let Ty::Callable { concept } = &signature[0] {
                if let Some(conc) = concepts.get(concept) {
                    (
                        conc.src_ty
                            .iter()
                            .map(|t| t.clone().try_into().expect("failed to convert type"))
                            .collect(),
                        conc.dst_ty
                            .clone()
                            .try_into()
                            .expect("failed to convert type"),
                        concept,
                    )
                } else {
                    return Err(OptError::Any {
                        text: format!(
                            "the concept {} which is called here is not defined!",
                            concept
                        ),
                    });
                }
            } else {
                return Err(OptError::Any {
                    text: format!("Expected a Callable as first input, got {:?}", signature[0]),
                });
            };

        //Now check that the signature (except for the callabel, is the one we expect. If so, return the output_ty)
        let argcount = signature.len() - 1;
        if argcount != expected_signature.len() {
            return Err(OptError::Any {
                text: format!(
                    "Concept {} expected {} arguments, but got {}",
                    concept_name,
                    expected_signature.len(),
                    argcount
                ),
            });
        }

        for i in 0..argcount {
            //NOTE shift, since the first one is the concept we are calling (by definition).
            if expected_signature[i] != signature[i + 1] {
                return Err(OptError::Any {
                    text: format!(
                        "Concept {} expected {}-th argument to be {:?}, but was {:?}",
                        concept_name,
                        i,
                        expected_signature[i],
                        signature[i + 1]
                    ),
                });
            }
        }

        //If we made it till here, we actually passed, therfore return the right type
        Ok(Some(output_ty))
    }
}

#[derive(LangNode, Debug)]
pub struct Imm {
    ///The immediate value
    pub lit: vola_ast::common::Literal,
    ///the output port the `lit` value is passed down to.
    #[output]
    out: Output,
}

impl Imm {
    pub fn new(lit: vola_ast::common::Literal) -> Self {
        Imm {
            lit,
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(Imm, "{}", lit);
impl DialectNode for Imm {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        _graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Some(Ty::Scalar))
    }
}

///Highlevel "field access" node. Can only be legalized when all types are resolved and the field is specialized.
#[derive(LangNode, Debug)]
pub struct FieldAccess {
    #[input]
    access_src: Input,
    access_list: SmallVec<[FieldAccessor; 1]>,
    #[output]
    output: Output,
}

impl FieldAccess {
    pub fn new(access_list: SmallVec<[FieldAccessor; 1]>) -> Self {
        FieldAccess {
            access_src: Input::default(),
            access_list,
            output: Output::default(),
        }
    }
}

//NOTE hand implementing for nicer field_acces_list
impl rvsdg_viewer::View for FieldAccess {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
    }

    fn name(&self) -> String {
        use std::fmt::Write;
        let mut string = format!("FieldAccess: ");
        for acc in &self.access_list {
            match acc {
                FieldAccessor::Digit { digit, .. } => write!(string, ".{}", digit).unwrap(),
                FieldAccessor::Ident { ident, .. } => write!(string, ".{}", ident.0).unwrap(),
            }
        }

        string
    }

    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
}

impl DialectNode for FieldAccess {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        if let Some(edg) = self.access_src.edge {
            //List access can only be done on algebraic types. The type can decide itself if it fits the access pattern.
            if let OptEdge::Value {
                ty: TypeState::Derived(t) | TypeState::Set(t),
            } = &graph.edge(edg).ty
            {
                t.try_access_pattern(&self.access_list)
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

#[derive(LangNode, Debug)]
pub struct ListConst {
    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    #[output]
    pub output: Output,
}

impl ListConst {
    pub fn new() -> Self {
        ListConst {
            inputs: SmallVec::new(),
            output: Output::default(),
        }
    }
}

implViewAlgeOp!(ListConst, "ListConst");
impl DialectNode for ListConst {
    fn dialect(&self) -> &'static str {
        "alge"
    }
    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //For the list constructor, we check that all inputs are of the same (algebraic) type, and then derive the
        // algebraic super(?)-type. So a list of scalars becomes a vector, a list of vectors a matrix, and a list of
        // matrices a tensor. Tensors then just grow by pushing the next dimension size.
        let mut signature: SmallVec<[Ty; 2]> = SmallVec::new();
        for (idx, inp) in self.inputs.iter().enumerate() {
            if let Some(edg) = inp.edge {
                //resolve if there is a type set
                if let Some(t) = graph.edge(edg).ty.get_type() {
                    signature.insert(idx, t.clone());
                } else {
                    return Ok(None);
                }
            } else {
                //input not set atm. so return None as well
                return Ok(None);
            }
        }

        if signature.len() == 0 {
            return Err(OptError::Any {
                text: format!("Cannot create an empty list (there is no void type in Vola)."),
            });
        }

        if !signature[0].is_algebraic() {
            return Err(OptError::Any { text: format!("List can only be created from algebraic types (scalar, vector, matrix, tensor), but first element was of type {:?}", signature[0]) });
        }

        //Check that all have the same type
        for s in 1..signature.len() {
            if signature[s] != signature[0] {
                return Err(OptError::Any { text: format!("List must be created from equal types. But first element is of type {:?} and {}-th element is of type {:?}", signature[0], s, signature[s]) });
            }
        }

        //Passed the equality check, therefore derive next list->Algebraic type
        let listlen = signature.len();
        match &signature[0] {
            Ty::Scalar => Ok(Some(Ty::Vector { width: listlen })),
            //NOTE this makes us effectively create line first matrices... doc that somewhere...
            Ty::Vector { width } => Ok(Some(Ty::Matrix {
                width: *width,
                height: listlen,
            })),
            Ty::Matrix { width, height } => {
                let mut tensor_sizes = SmallVec::new();
                tensor_sizes.push(*width);
                tensor_sizes.push(*height);
                tensor_sizes.push(listlen);
                Ok(Some(Ty::Tensor { dim: tensor_sizes }))
            }
            Ty::Tensor { dim } => {
                let mut dim = dim.clone();
                dim.push(listlen);
                Ok(Some(Ty::Tensor { dim }))
            }
            _ => panic!("enountered non-algebraic type"),
        }
    }
}
