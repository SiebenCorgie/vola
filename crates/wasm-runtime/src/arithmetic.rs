/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use glam::{Mat2, Mat3, Mat4};
use glam::{Vec2, Vec3, Vec4};

use core::ops::Add;
crate::impl_macros::impl_op2!(Vec2, add, 2, add_vec2);
crate::impl_macros::impl_op2!(Vec3, add, 3, add_vec3);
crate::impl_macros::impl_op2!(Vec4, add, 4, add_vec4);

use core::ops::Sub;
crate::impl_macros::impl_op2!(Vec2, sub, 2, sub_vec2);
crate::impl_macros::impl_op2!(Vec3, sub, 3, sub_vec3);
crate::impl_macros::impl_op2!(Vec4, sub, 4, sub_vec4);

use core::ops::Mul;
crate::impl_macros::impl_op2!(Vec2, mul, 2, mul_vec2);
crate::impl_macros::impl_op2!(Vec3, mul, 3, mul_vec3);
crate::impl_macros::impl_op2!(Vec4, mul, 4, mul_vec4);

use core::ops::Div;
crate::impl_macros::impl_op2!(Vec2, div, 2, div_vec2);
crate::impl_macros::impl_op2!(Vec3, div, 3, div_vec3);
crate::impl_macros::impl_op2!(Vec4, div, 4, div_vec4);

//The % operator
use core::ops::Rem;
crate::impl_macros::impl_op2!(f32, rem, 1, mod_scalar);
crate::impl_macros::impl_op2!(Vec2, rem, 2, mod_vec2);
crate::impl_macros::impl_op2!(Vec3, rem, 3, mod_vec3);
crate::impl_macros::impl_op2!(Vec4, rem, 4, mod_vec4);

#[no_mangle]
extern "C" fn inverse_mat2(a: &Mat2, res: &mut Mat2) {
    *res = a.inverse();
}

#[no_mangle]
extern "C" fn inverse_mat3(a: &Mat3, res: &mut Mat3) {
    *res = a.inverse();
}
#[no_mangle]
extern "C" fn inverse_mat4(a: &Mat4, res: &mut Mat4) {
    *res = a.inverse();
}

#[no_mangle]
extern "C" fn mul_vec2_scalar(a: &Vec2, b: &f32, res: &mut Vec2) {
    *res = a * b;
}

#[no_mangle]
extern "C" fn mul_vec3_scalar(a: &Vec3, b: &f32, res: &mut Vec3) {
    *res = a * b;
}

#[no_mangle]
extern "C" fn mul_vec4_scalar(a: &Vec4, b: &f32, res: &mut Vec4) {
    *res = a * b;
}
