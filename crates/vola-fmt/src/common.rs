use vola_ast::{
    Module,
    common::{CTArg, Call, Comment, DataTy, Digit, Ident, Literal, Shape, Ty, TypedIdent},
};

use crate::FormatTree;

impl From<&Comment> for FormatTree {
    fn from(value: &Comment) -> Self {
        //NOTE: we just yeet the whole comment (including the // part)
        //      into the content field, so we can also just copy that out
        FormatTree::Token(value.content.clone())
    }
}

impl From<&Ident> for FormatTree {
    fn from(value: &Ident) -> Self {
        FormatTree::Token(value.0.clone())
    }
}

impl From<&Digit> for FormatTree {
    fn from(value: &Digit) -> Self {
        FormatTree::Token(format!("{}", value.0))
    }
}

impl From<&DataTy> for FormatTree {
    fn from(value: &DataTy) -> Self {
        match value {
            DataTy::Void => FormatTree::Token("none".to_owned()),
            DataTy::Real => FormatTree::Token("real".to_owned()),
            DataTy::Integer => FormatTree::Token("int".to_owned()),
            DataTy::Complex => FormatTree::Token("complex".to_owned()),
            DataTy::Quaternion => FormatTree::Token("quat".to_owned()),
            DataTy::Bool => FormatTree::Token("bool".to_owned()),
            DataTy::Csg => FormatTree::Token("csg".to_owned()),
        }
    }
}

impl From<&usize> for FormatTree {
    fn from(value: &usize) -> Self {
        FormatTree::Token(format!("{value}"))
    }
}

impl From<&Shape> for FormatTree {
    fn from(value: &Shape) -> Self {
        match value {
            Shape::Interval => FormatTree::Token("interval)".to_owned()),
            Shape::Vec { width } => FormatTree::Token(format!("vec{}", width)),
            Shape::Matrix { width, height } => {
                if width == height {
                    FormatTree::Token(format!("mat{width}"))
                } else {
                    FormatTree::Token(format!("mat{width}x{height}"))
                }
            }
            Shape::Tensor { sizes } => FormatTree::Seq(vec![
                FormatTree::Token("tensor".to_owned()),
                FormatTree::Wrapped {
                    left: '<',
                    right: '>',
                    sub: Box::new(FormatTree::seperated_list(",", sizes.iter())),
                },
            ]),
        }
    }
}

impl From<&Ty> for FormatTree {
    fn from(value: &Ty) -> Self {
        match value {
            Ty::Shaped { ty, shape } => {
                //We have the convention, that a shape, without a type is
                //the real version of that shape
                if ty == &DataTy::Real {
                    FormatTree::from(shape)
                } else {
                    FormatTree::Seq(vec![FormatTree::from(shape), FormatTree::Wrapped {
                        left: '<',
                        right: '>',
                        sub: Box::new(FormatTree::from(ty)),
                    }])
                }
            }
            Ty::Simple(simple) => FormatTree::from(simple),
            Ty::Tuple(tuple) => FormatTree::Wrapped {
                left: '(',
                right: ')',
                sub: Box::new(FormatTree::seperated_list(",", tuple.iter())),
            },
        }
    }
}

impl From<&Literal> for FormatTree {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::BoolLiteral(b) => FormatTree::Token(format!("{b:?}")),
            Literal::IntegerLiteral(i) => FormatTree::Token(format!("{i:?}")),
            Literal::FloatLiteral(f) => FormatTree::Token(format!("{f:?}")),
        }
    }
}

impl From<&TypedIdent> for FormatTree {
    fn from(value: &TypedIdent) -> Self {
        Self::Seq(vec![
            FormatTree::from(&value.ident),
            FormatTree::TypeEnd,
            FormatTree::from(&value.ty),
        ])
    }
}

impl From<&Call> for FormatTree {
    fn from(value: &Call) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Token(value.ident.0.clone()),
            FormatTree::Wrapped {
                left: '(',
                right: ')',
                sub: Box::new(FormatTree::seperated_list(",", value.args.iter())),
            },
        ])
    }
}

impl From<&Module> for FormatTree {
    fn from(value: &Module) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Token("module".to_owned()),
            FormatTree::seperated_list("::", value.path.iter()),
            FormatTree::StmtEnd,
        ])
    }
}

//NOTE: This basically just steals the Call serialization.
impl From<&CTArg> for FormatTree {
    fn from(value: &CTArg) -> Self {
        let as_call = Call {
            span: value.span.clone(),
            ident: value.ident.clone(),
            args: value.args.clone(),
        };
        FormatTree::Seq(vec![Self::Token("#".to_owned()), Self::Wrapped {
            left: '[',
            right: ']',
            sub: Box::new(FormatTree::from(&as_call)),
        }])
    }
}
