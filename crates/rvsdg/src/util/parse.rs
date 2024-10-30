/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! General purpose parsers that can be combined to create parsers for data structures.
//!
//! Those are used to build parsers for structures that need a way back from [Display] into their original representation.

use chumsky::{error::Simple, prelude::just, text::TextParser, Parser};

///Accepts a radix10 usize
pub fn usize_parser() -> impl Parser<char, usize, Error = Simple<char>> {
    chumsky::text::int(10)
        .map(|s: String| s.parse::<usize>().unwrap())
        .padded()
}

///Parses any expression `prefix(654)` where `T` is parsed by `prefix`, and the inner number is parsed to a usize
pub fn enum_with_usize<T: 'static, U: Fn(usize) -> T + 'static>(
    prefix: &'static str,
    t: U,
) -> impl Parser<char, T, Error = Simple<char>> {
    just(prefix)
        .then(usize_parser().delimited_by(just('('), just(')')))
        .map(move |(_, index)| t(index))
}
