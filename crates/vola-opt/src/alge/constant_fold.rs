use rvsdg::{smallvec::smallvec, SmallColl};
use vola_common::Span;

use crate::{
    imm::{ImmMatrix, ImmScalar, ImmVector},
    OptNode,
};

use super::arithmetic::BinaryArithOp;

pub(crate) fn handle_piece_wise(op: &BinaryArithOp, a: &OptNode, b: &OptNode) -> Option<OptNode> {
    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmScalar>(),
        b.try_downcast_ref::<ImmScalar>(),
    ) {
        let new = op.apply_on_scalar(a.lit, b.lit);

        return Some(OptNode::new(ImmScalar::new(new), Span::empty()));
    }

    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmVector>(),
        b.try_downcast_ref::<ImmVector>(),
    ) {
        assert!(a.lit.len() == b.lit.len());
        let mut new_vec = ImmVector::new(&[]);

        for (el_a, el_b) in a.lit.iter().zip(b.lit.iter()) {
            new_vec.lit.push(op.apply_on_scalar(*el_a, *el_b));
        }

        return Some(OptNode::new(new_vec, Span::empty()));
    }

    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmMatrix>(),
        b.try_downcast_ref::<ImmMatrix>(),
    ) {
        assert!(a.lit.len() == b.lit.len());
        let mut new_columns = SmallColl::new();

        for (col_a, col_b) in a.lit.iter().zip(b.lit.iter()) {
            assert!(col_a.len() == col_b.len());
            let mut new_col = SmallColl::new();
            for (ele_a, ele_b) in col_a.iter().zip(col_b.iter()) {
                new_col.push(op.apply_on_scalar(*ele_a, *ele_b));
            }
            new_columns.push(new_col);
        }

        return Some(OptNode::new(ImmMatrix::new(new_columns), Span::empty()));
    }

    //TODO: implement for tensors?

    None
}

pub(crate) fn handle_matrix_matrix_multiplication(
    a: &SmallColl<SmallColl<f64>>,
    b: &SmallColl<SmallColl<f64>>,
) -> Option<OptNode> {
    //NOTE we implement the _standard_ matrix multiplication.
    //     So mata must be as _wide_ as matb is _deep_

    if a.len() == 0 || b.len() == 0 {
        return None;
    }

    if a.len() != b[0].len() {
        #[cfg(feature = "log")]
        log::error!("Matrix A is {} wide, but matrix B is {} deep. Cannot constant-fold matrix multiplication!", a.len(), b[0].len());
        return None;
    }

    let m: usize = a.len();
    let i: usize = a[0].len();
    let n: usize = b.len();

    let mut res: SmallColl<SmallColl<f64>> = smallvec![smallvec![0.0f64; i]; n];
    for n in 0..n {
        for i in 0..i {
            //now iterate a offset
            for k in 0..m {
                res[n][i] += a[k][i] * b[n][k];
            }
        }
    }

    Some(OptNode::new(ImmMatrix::new(res), Span::empty()))
}

pub(crate) fn handle_constant_mul(a: &OptNode, b: &OptNode) -> Option<OptNode> {
    //NOTE: I chose to implement all the operations _by hand_, since there is no nice, light weight
    //      crate that could handle that. In fact I only found `nalgebra` which is a _heavy_ dependency,
    //      if you only want to use the vector/matrix multiplication in there ^^.

    //If both are of thame shape, use the standard piece wise multiplication.
    if a.try_downcast_ref::<ImmScalar>().is_some() && b.try_downcast_ref::<ImmScalar>().is_some() {
        return handle_piece_wise(&BinaryArithOp::Mul, a, b);
    }
    if a.try_downcast_ref::<ImmVector>().is_some() && b.try_downcast_ref::<ImmVector>().is_some() {
        return handle_piece_wise(&BinaryArithOp::Mul, a, b);
    }

    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmVector>(),
        b.try_downcast_ref::<ImmScalar>(),
    ) {
        //scale each element of a with scalar b
        let scalar = b.lit;
        let mut new = a.lit.clone();
        for ele in new.iter_mut() {
            *ele = *ele * scalar;
        }

        return Some(OptNode::new(ImmVector::new(&new), Span::empty()));
    }

    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmMatrix>(),
        b.try_downcast_ref::<ImmScalar>(),
    ) {
        //scale each element of a with scalar b
        let scalar = b.lit;
        let mut new = a.lit.clone();

        for col in new.iter_mut() {
            for ele in col {
                *ele = *ele * scalar;
            }
        }

        return Some(OptNode::new(ImmMatrix::new(new), Span::empty()));
    }

    //Matrix multiplication
    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmMatrix>(),
        b.try_downcast_ref::<ImmMatrix>(),
    ) {
        return handle_matrix_matrix_multiplication(&a.lit, &b.lit);
    }

    //We just use a _single-column-matrix_ for the vector here.
    //We also implicitly do the transposing of the vector
    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmVector>(),
        b.try_downcast_ref::<ImmMatrix>(),
    ) {
        let n = a.lit.len();
        let m = b.lit.len();

        if n == 0 || m == 0 {
            return None;
        }

        if b.lit[0].len() == 0 {
            return None;
        }

        let i = b.lit[0].len();
        //Does not work, if the _width_ of the vector does not match the _width_ of the matrix
        if n != i {
            #[cfg(feature = "log")]
            log::error!("Vector-Matrix multiplication: Vector-width and Matrix-height don't match: {n} != {i}");
            return None;
        }
        //NOTE: this implicitly transposes the vector `a`, does the multiplication
        //      and transposes it back.
        let mut res: SmallColl<f64> = smallvec![0.0; m];

        //b: column
        for m in 0..m {
            //a: column
            //b: row
            for n in 0..n {
                println!("{} * {}", a.lit[n], b.lit[m][n]);
                res[m] += a.lit[n] * b.lit[m][n];
            }
            println!("= {}", res[m]);
        }

        return Some(OptNode::new(ImmVector::new(&res), Span::empty()));
    }

    //For that case we just fake the vector to be a
    // 1-width matrix, and use the matrix-matrix multiplication
    if let (Some(a), Some(b)) = (
        a.try_downcast_ref::<ImmMatrix>(),
        b.try_downcast_ref::<ImmVector>(),
    ) {
        let b = smallvec![b.lit.clone()];
        let res = handle_matrix_matrix_multiplication(&a.lit, &b).unwrap();
        let payload = &res.try_downcast_ref::<ImmMatrix>().unwrap().lit[0];
        return Some(OptNode::new(
            ImmVector::new(payload.as_ref()),
            Span::empty(),
        ));
    }

    None
}

#[cfg(test)]
mod test {
    use rvsdg::{smallvec::smallvec, SmallColl};
    use vola_common::Span;

    use crate::{
        imm::{ImmMatrix, ImmVector},
        OptNode,
    };

    use super::handle_constant_mul;

    #[test]
    fn matrix_mul_identity() {
        let mat = smallvec![
            smallvec![1.0, 0.0, 0.0],
            smallvec![0.0, 1.0, 0.0],
            smallvec![0.0, 0.0, 1.0]
        ];
        let a = OptNode::new(ImmMatrix::new(mat.clone()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(mat.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        assert!(res.unwrap().try_downcast_ref::<ImmMatrix>().unwrap().lit == mat);
    }

    #[test]
    fn matrix_mul() {
        //NOTE: the vectors are the collumns, so this is written exactly _the wrong way_
        let mata = smallvec![
            smallvec![46.0, 12.0, 18.0],
            smallvec![132.0, 96.0, 92.0],
            smallvec![86.0, 84.0, 80.0]
        ];

        let matb = smallvec![
            smallvec![1.0, 7.0, 6.0],
            smallvec![3.0, 8.0, 7.0],
            smallvec![5.0, 42.0, 6.0]
        ];

        let expected: SmallColl<SmallColl<f64>> = smallvec![
            smallvec![1486.0, 1188.0, 1142.0],
            smallvec![1796.0, 1392.0, 1350.0],
            smallvec![6290.0, 4596.0, 4434.0]
        ];
        let a = OptNode::new(ImmMatrix::new(mata.clone()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(matb.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmMatrix>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }

    #[test]
    fn reject_malformed() {
        let mata = smallvec![
            smallvec![1.0, 0.0, 0.0],
            smallvec![0.0, 1.0, 0.0],
            smallvec![0.0, 0.0, 1.0]
        ];
        let matb = smallvec![smallvec![1.0, 0.0], smallvec![0.0, 1.0]];
        let a = OptNode::new(ImmMatrix::new(mata.clone()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(matb.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_none());
    }

    #[test]
    fn non_symetric_matrix_multiply() {
        //NOTE: the vectors are the collumns, so this is written exactly _the wrong way_
        let mata = smallvec![smallvec![46.0, 12.0, 18.0], smallvec![132.0, 96.0, 92.0],];

        let matb = smallvec![
            smallvec![1.0, 7.0],
            smallvec![3.0, 8.0],
            smallvec![5.0, 42.0]
        ];

        let expected: SmallColl<SmallColl<f64>> = smallvec![
            smallvec![970.0, 684.0, 662.0],
            smallvec![1194.0, 804.0, 790.0],
            smallvec![5774.0, 4092.0, 3954.0]
        ];
        let a = OptNode::new(ImmMatrix::new(mata.clone()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(matb.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmMatrix>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }

    #[test]
    fn vector_matrix_multiply() {
        //NOTE: the vectors are the columns, so this is written exactly _the wrong way_
        let veca: SmallColl<f64> = smallvec![8.0f64, 28.0f64];

        let matb = smallvec![smallvec![8.0, 10.0], smallvec![42.0, 3.0],];

        let expected: SmallColl<f64> = smallvec![344.0, 420.0];
        let a = OptNode::new(ImmVector::new(veca.as_slice()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(matb.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmVector>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }

    #[test]
    fn vector_matrix_multiply_unsymetric() {
        //NOTE: the vectors are the columns, so this is written exactly _the wrong way_
        let veca: SmallColl<f64> = smallvec![46.0f64, 12.0f64];

        let matb = smallvec![
            smallvec![1.0, 3.0],
            smallvec![7.0, 8.0],
            smallvec![3.0, 8.0]
        ];

        let expected: SmallColl<f64> = smallvec![82.0, 418.0, 234.0];
        let a = OptNode::new(ImmVector::new(veca.as_slice()), Span::empty());
        let b = OptNode::new(ImmMatrix::new(matb.clone()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmVector>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }

    #[test]
    fn matrix_vector_multiply() {
        //NOTE: the vectors are the columns, so this is written exactly _the wrong way_
        let mata = smallvec![smallvec![8.0, 10.0], smallvec![42.0, 3.0],];
        let vecb: SmallColl<f64> = smallvec![8.0f64, 28.0f64];

        let expected: SmallColl<f64> = smallvec![1240.0, 164.0];
        let a = OptNode::new(ImmMatrix::new(mata), Span::empty());
        let b = OptNode::new(ImmVector::new(vecb.as_slice()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmVector>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }
    #[test]
    fn matrix_vector_multiply_asym() {
        //NOTE: the vectors are the columns, so this is written exactly _the wrong way_
        let mata = smallvec![smallvec![8.0, 10.0, 11.0], smallvec![42.0, 3.0, 12.0]];
        let vecb: SmallColl<f64> = smallvec![8.0f64, 28.0f64];

        let expected: SmallColl<f64> = smallvec![1240.0, 164.0, 424.0];
        let a = OptNode::new(ImmMatrix::new(mata), Span::empty());
        let b = OptNode::new(ImmVector::new(vecb.as_slice()), Span::empty());

        let res = handle_constant_mul(&a, &b);
        assert!(res.is_some());
        let lit = res
            .unwrap()
            .try_downcast_ref::<ImmVector>()
            .unwrap()
            .lit
            .clone();
        //NOTE should to a _in epsilon_ test. But f64 seems to be okay-ish
        assert!(lit == expected, "expected \n{expected:#?}\ngot\n{lit:#?}");
    }
}
