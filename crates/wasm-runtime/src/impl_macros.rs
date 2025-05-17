//Helper macro that implements a op of the form
// a.op()
macro_rules! impl_op {
    (f32, $op:ident, 1, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &f32, res: &mut f32) {
            *res = a.$op()
        }
    };
    ($t:ty, $op:ident, $count:expr, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &$t, res: &mut $t) {
            for i in 0..$count {
                res[i] = a[i].$op();
            }
        }
    };
}

pub(crate) use impl_op;

//Helper macro that implements a op of the form
// a.op() where a is a vector and the result is a scalar
macro_rules! impl_op_flatten {
    ($t:ty, $op:ident, $count:expr, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &$t, res: &mut f32) {
            *res = a.$op();
        }
    };
}

pub(crate) use impl_op_flatten;

//implements an operation of form
// a.op(b)
macro_rules! impl_op2 {
    (f32, $op:ident, 1, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &f32, b: &f32, res: &mut f32) {
            *res = a.$op(*b);
        }
    };
    ($t:ty, $op:ident, $count:expr, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &$t, b: &$t, res: &mut $t) {
            for i in 0..$count {
                res[i] = a[i].$op(b[i]);
            }
        }
    };
}
pub(crate) use impl_op2;

//implements a vector flattening op of form
// a.op(b), where a&b are vector types, and the result is a scalar (like a dot-operation).
macro_rules! impl_op2_flatten {
    ($t:ty, $op:ident, $count:expr, $name:ident) => {
        #[no_mangle]
        extern "C" fn $name(a: &$t, b: &$t, res: &mut f32) {
            *res = a.$op(*b);
        }
    };
}

pub(crate) use impl_op2_flatten;
