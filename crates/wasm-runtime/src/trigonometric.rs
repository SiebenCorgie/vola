/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use glam::{Vec2, Vec3, Vec4};

crate::impl_macros::impl_op!(f32, sin, 1, sin_scalar);
crate::impl_macros::impl_op!(Vec2, sin, 2, sin_vec2);
crate::impl_macros::impl_op!(Vec3, sin, 3, sin_vec3);
crate::impl_macros::impl_op!(Vec4, sin, 4, sin_vec4);

crate::impl_macros::impl_op!(f32, cos, 1, cos_scalar);
crate::impl_macros::impl_op!(Vec2, cos, 2, cos_vec2);
crate::impl_macros::impl_op!(Vec3, cos, 3, cos_vec3);
crate::impl_macros::impl_op!(Vec4, cos, 4, cos_vec4);

crate::impl_macros::impl_op!(f32, tan, 1, tan_scalar);
crate::impl_macros::impl_op!(Vec2, tan, 2, tan_vec2);
crate::impl_macros::impl_op!(Vec3, tan, 3, tan_vec3);
crate::impl_macros::impl_op!(Vec4, tan, 4, tan_vec4);

crate::impl_macros::impl_op!(f32, asin, 1, asin_scalar);
crate::impl_macros::impl_op!(Vec2, asin, 2, asin_vec2);
crate::impl_macros::impl_op!(Vec3, asin, 3, asin_vec3);
crate::impl_macros::impl_op!(Vec4, asin, 4, asin_vec4);

crate::impl_macros::impl_op!(f32, acos, 1, acos_scalar);
crate::impl_macros::impl_op!(Vec2, acos, 2, acos_vec2);
crate::impl_macros::impl_op!(Vec3, acos, 3, acos_vec3);
crate::impl_macros::impl_op!(Vec4, acos, 4, acos_vec4);

crate::impl_macros::impl_op!(f32, atan, 1, atan_scalar);
crate::impl_macros::impl_op!(Vec2, atan, 2, atan_vec2);
crate::impl_macros::impl_op!(Vec3, atan, 3, atan_vec3);
crate::impl_macros::impl_op!(Vec4, atan, 4, atan_vec4);
