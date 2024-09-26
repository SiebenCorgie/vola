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

use glam::Vec3;
/*
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    loop {}
}
*/

#[no_mangle]
extern "C" fn do_stuff(x: i32, f: f32) -> f32 {
    x as f32 * f
}

#[no_mangle]
extern "C" fn cross_vec3(a: Vec3, b: Vec3) -> Vec3 {
    glam::Vec3::cross(a.into(), b.into()).into()
}
