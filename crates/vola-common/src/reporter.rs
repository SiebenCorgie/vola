/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ariadne::Cache;
use backtrace::Backtrace;

use lazy_static::lazy_static;
use std::{error::Error, ops::Deref, sync::Mutex};

use crate::Span;

pub trait Reportable: Error + Send + Sync + 'static {}

lazy_static! {
    static ref FILE_CACHE: Mutex<ariadne::FileCache> = Mutex::new(ariadne::FileCache::default());
}

///Resets the file cache of the reporter. Needs to be called whenever
///The compiler re-compiles _changed_ files without exiting the process first
pub fn reset_file_cache() {
    *FILE_CACHE.lock().unwrap() = ariadne::FileCache::default();
}

pub fn cache_file(path: &std::path::Path) {
    if let Err(e) = FILE_CACHE.lock().unwrap().fetch(path) {
        eprintln!("Couldn't cache {path:?}: {e:?}");
    }
}

///Sends a `err` reportable and reports the error, possibly based on some source-`file`.
pub fn report<'a>(err: ariadne::Report<'a, Span>) {
    match FILE_CACHE.lock() {
        Ok(lck) => {
            if let Err(e) = err.print(lck.deref().clone()) {
                eprintln!("Failed to print error: {e:?}");
            }
        }
        Err(_) => println!("Failed to report error, could not lock source-files!"),
    }
    if std::env::var("VOLA_BACKTRACE").is_ok() {
        println!("Backtrace: {:#?}", Backtrace::new())
    }
}
