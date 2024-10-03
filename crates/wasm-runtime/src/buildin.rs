/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use glam::{Vec2, Vec3, Vec4};

//NOTE can't use our macros, but only in that case, so its fine I guess.
#[no_mangle]
extern "C" fn cross_vec3(a: Vec3, b: Vec3) -> Vec3 {
    glam::Vec3::cross(a.into(), b.into()).into()
}

crate::impl_macros::impl_op_flatten!(Vec2, length, 2, length_vec2);
crate::impl_macros::impl_op_flatten!(Vec3, length, 3, length_vec3);
crate::impl_macros::impl_op_flatten!(Vec4, length, 4, length_vec4);

crate::impl_macros::impl_op2_flatten!(Vec2, dot, 2, dot_vec2);
crate::impl_macros::impl_op2_flatten!(Vec3, dot, 3, dot_vec3);
crate::impl_macros::impl_op2_flatten!(Vec4, dot, 4, dot_vec4);

crate::impl_macros::impl_op!(Vec2, sqrt, 2, sqrt_vec2);
crate::impl_macros::impl_op!(Vec3, sqrt, 3, sqrt_vec3);
crate::impl_macros::impl_op!(Vec4, sqrt, 4, sqrt_vec4);

crate::impl_macros::impl_op!(f32, exp, 1, exp_scalar);
crate::impl_macros::impl_op!(Vec2, exp, 2, exp_vec2);
crate::impl_macros::impl_op!(Vec3, exp, 3, exp_vec3);
crate::impl_macros::impl_op!(Vec4, exp, 4, exp_vec4);

crate::impl_macros::impl_op2!(f32, powf, 1, pow_scalar);
crate::impl_macros::impl_op2!(Vec2, powf, 2, pow_vec2);
crate::impl_macros::impl_op2!(Vec3, powf, 3, pow_vec3);
crate::impl_macros::impl_op2!(Vec4, powf, 4, pow_vec4);

crate::impl_macros::impl_op!(Vec2, abs, 2, abs_vec2);
crate::impl_macros::impl_op!(Vec3, abs, 3, abs_vec3);
crate::impl_macros::impl_op!(Vec4, abs, 4, abs_vec4);

crate::impl_macros::impl_op!(Vec2, ceil, 2, ceil_vec2);
crate::impl_macros::impl_op!(Vec3, ceil, 3, ceil_vec3);
crate::impl_macros::impl_op!(Vec4, ceil, 4, ceil_vec4);

crate::impl_macros::impl_op!(Vec2, floor, 2, floor_vec2);
crate::impl_macros::impl_op!(Vec3, floor, 3, floor_vec3);
crate::impl_macros::impl_op!(Vec4, floor, 4, floor_vec4);

use core::ops::Neg;
crate::impl_macros::impl_op!(Vec2, neg, 2, neg_vec2);
crate::impl_macros::impl_op!(Vec3, neg, 3, neg_vec3);
crate::impl_macros::impl_op!(Vec4, neg, 4, neg_vec4);

crate::impl_macros::impl_op!(Vec2, round, 2, round_vec2);
crate::impl_macros::impl_op!(Vec3, round, 3, round_vec3);
crate::impl_macros::impl_op!(Vec4, round, 4, round_vec4);

//crate::impl_macros::impl_op2!(f32, min, 1, min_scalar);
crate::impl_macros::impl_op2!(Vec2, min, 2, min_vec2);
crate::impl_macros::impl_op2!(Vec3, min, 3, min_vec3);
crate::impl_macros::impl_op2!(Vec4, min, 4, min_vec4);

//crate::impl_macros::impl_op2!(f32, max, 1, max_scalar);
crate::impl_macros::impl_op2!(Vec2, max, 2, max_vec2);
crate::impl_macros::impl_op2!(Vec3, max, 3, max_vec3);
crate::impl_macros::impl_op2!(Vec4, max, 4, max_vec4);

crate::impl_macros::impl_op!(f32, fract, 1, fract_scalar);
crate::impl_macros::impl_op!(Vec2, fract, 2, fract_vec2);
crate::impl_macros::impl_op!(Vec3, fract, 3, fract_vec3);
crate::impl_macros::impl_op!(Vec4, fract, 4, fract_vec4);

#[no_mangle]
extern "C" fn mix_vec2(a: Vec2, b: Vec2, c: f32) -> Vec2 {
    glam::Vec2::lerp(a, b, c)
}

#[no_mangle]
extern "C" fn mix_vec3(a: Vec3, b: Vec3, c: f32) -> Vec3 {
    glam::Vec3::lerp(a, b, c)
}

#[no_mangle]
extern "C" fn mix_vec4(a: Vec4, b: Vec4, c: f32) -> Vec4 {
    glam::Vec4::lerp(a, b, c)
}

#[no_mangle]
extern "C" fn clamp_vec2(a: Vec2, b: Vec2, c: Vec2) -> Vec2 {
    glam::Vec2::clamp(a, b, c)
}

#[no_mangle]
extern "C" fn clamp_vec3(a: Vec3, b: Vec3, c: Vec3) -> Vec3 {
    glam::Vec3::clamp(a, b, c)
}

#[no_mangle]
extern "C" fn clamp_vec4(a: Vec4, b: Vec4, c: Vec4) -> Vec4 {
    glam::Vec4::clamp(a, b, c)
}
