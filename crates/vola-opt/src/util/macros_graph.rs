/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Some macros that make it easier construction _more_complex_ parts of a graph.

///Connects to a new `node` using `region`-builder.
///
/// # exampls
///
/// ```rust ignore
/// //creates a Add node within the region-builder with the given span and routes ports a/b into the inputs.
/// route_new!(region, BinaryArithOp::Add, span, [a, b])
/// ```
#[macro_export]
macro_rules! route_new {
    //Catch Binary operations with exactly two inputs
    ($builder:expr, BinaryArithOp::$binarith:tt, $span:expr, [$a:expr, $b:expr]) =>{
        route_new!($builder, $crate::alge::arithmetic::BinaryArith::new($crate::alge::arithmetic::BinaryArithOp::$binarith), $span, [$a, $b])
    };
    //Unary ops with exactly one input
    ($builder:expr, UnaryArithOp::$unarith:tt, $span:expr, $a:expr) =>{
        route_new!($builder, $crate::alge::arithmetic::UnaryArith::new(crate::alge::arithmetic::UnaryArithOp::$unarith), $span, [$a])
    };

    ($builder:expr, TrigOp::$unarith:tt, $span:expr, [$a:expr, $b:expr]) =>{
        route_new!($builder, crate::alge::trigonometric::Trig::new(crate::alge::trigonometric::TrigOp::$unarith), $span, [$a, $b])
    };

    //Building can have any amount
    ($builder:expr, BuildinOp::$buildin:tt, $span:expr, [$($port:expr),*]) =>{
        route_new!($builder, crate::alge::buildin::Buildin::new(crate::alge::buildin::BuildinOp::$buildin), $span, [$($port),*])
    };

    //This is the lower shell generic over the opt-node
    ($builder:expr, $opt:expr, $span:expr, [$($port:expr),*]) => {
        {
            let (node, _) = $builder
                .connect_node(
                    crate::OptNode::new($opt, $span),
                    [$($port),*],
                )
                .unwrap();
                node
        }
    };
}

///Creates a new constant in `region`-builder without any span.
///
/// works with Real (scalar or vector) and integers numbers.
///
/// # Examples
///
/// ```rust ignore
/// let one = imm!(reg, Real, 1.0);
/// let vec = imm!(reg, Real, [1.0, 2.0, 3.0]);
/// let nat = imm!(reg, Nat, 1)
/// ```
///
/// You may dictate a span, otherwise an empty span is used:
///
/// ```rust ignore
/// let one = imm!(reg, Real, 1.0, my_span);
/// ```
///
#[macro_export]
macro_rules! imm {
    ($builder:expr, Real, $scalar:expr, $span:expr) => {
        imm!($builder, $crate::imm::ImmScalar::new($scalar), $span)
    };
    ($builder:expr, Real, $scalar:expr) => {
        imm!($builder, $crate::imm::ImmScalar::new($scalar))
    };

    ($builder:expr, Real, [$($scalar:expr),*], $span:expr) => {
        imm!($builder, $crate::imm::ImmVector::new(&[$($scalar),*], $span))
    };
    ($builder:expr, Real, [$($scalar:expr),*]) => {
        imm!($builder, crate::imm::ImmVector::new(&[$($scalar),*]))
    };

    ($builder:expr, Nat, $nat:expr, $span:expr) => {
        imm!($builder, crate::imm::ImmNat::new($nat), $span)
    };
    ($builder:expr, Nat, $nat:expr) => {
        imm!($builder, crate::imm::ImmNat::new($nat))
    };

    ($builder:expr, $opt:expr) => {
        $builder.insert_node(crate::OptNode::new($opt, Span::empty()))
    };

    ($builder:expr, $opt:expr, $span:expr) => {
        $builder.insert_node(crate::OptNode::new($opt, $span))
    };

}
