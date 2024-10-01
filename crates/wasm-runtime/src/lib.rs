/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # WASM-Runtime
//!
//! Exports all symbols needed by the wasm backend to fully function.
//!
//! Gets build automatically by the backend's build script.

#![no_std]

use glam::{Vec2, Vec3, Vec4};
/*
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    loop {}
}
*/

#[no_mangle]
extern "C" fn cross_vec3(a: Vec3, b: Vec3) -> Vec3 {
    glam::Vec3::cross(a.into(), b.into()).into()
}

#[no_mangle]
extern "C" fn length_vec2(a: Vec2) -> f32 {
    a.length()
}

#[no_mangle]
extern "C" fn length_vec3(a: Vec3) -> f32 {
    a.length()
}

#[no_mangle]
//NOTE: have to unwrap the vec4, since anything else is not ffi-stable :(
extern "C" fn length_vec4(a: f32, b: f32, c: f32, d: f32) -> f32 {
    Vec4::new(a, b, c, d).length()
}
