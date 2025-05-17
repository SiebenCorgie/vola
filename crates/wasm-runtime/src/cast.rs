/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use glam::{IVec2, IVec3, IVec4, Vec2, Vec3, Vec4};

#[no_mangle]
extern "C" fn cast_scalar_int(a: &f32, res: &mut i32) {
    *res = *a as i32;
}

#[no_mangle]
extern "C" fn cast_int_scalar(a: &i32, res: &mut f32) {
    *res = *a as f32;
}

#[no_mangle]
extern "C" fn cast_vec2_ivec2(a: &Vec2, res: &mut IVec2) {
    *res = a.as_ivec2();
}

#[no_mangle]
extern "C" fn cast_ivec2_vec2(a: &IVec2, res: &mut Vec2) {
    *res = a.as_vec2();
}

#[no_mangle]
extern "C" fn cast_vec3_ivec3(a: &Vec3, res: &mut IVec3) {
    *res = a.as_ivec3();
}

#[no_mangle]
extern "C" fn cast_ivec3_vec3(a: &IVec3, res: &mut Vec3) {
    *res = a.as_vec3();
}

#[no_mangle]
extern "C" fn cast_vec4_ivec4(a: &Vec4, res: &mut IVec4) {
    *res = a.as_ivec4();
}

#[no_mangle]
extern "C" fn cast_ivec4_vec4(a: &IVec4, res: &mut Vec4) {
    *res = a.as_vec4();
}
