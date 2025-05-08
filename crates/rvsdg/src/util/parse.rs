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

use chumsky::{
    error::Simple,
    prelude::{choice, just},
    text::TextParser,
    Parser,
};

use crate::{
    attrib::AttribLocation,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    EdgeRef, NodeRef,
};

///Accepts a radix10 usize
pub fn usize_parser() -> impl Parser<char, usize, Error = Simple<char>> {
    chumsky::text::int(10)
        .map(|s: String| s.parse::<usize>().unwrap())
        .padded()
}

pub fn u64_parser() -> impl Parser<char, u64, Error = Simple<char>> {
    chumsky::text::int(10)
        .map(|s: String| s.parse::<u64>().unwrap())
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

pub fn any_string() -> impl Parser<char, String, Error = Simple<char>> {
    chumsky::prelude::any().repeated().map(|chars| {
        let mut s = String::new();
        s.extend(chars);
        s
    })
}
impl NodeRef {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        chumsky::primitive::just("NodeRef")
            .then(
                u64_parser()
                    .then_ignore(just('v'))
                    .then(u64_parser())
                    .delimited_by(chumsky::primitive::just('('), chumsky::primitive::just(')')),
            )
            .map(|(_, (index, version))| Self::from_ffi(version << 32 | index))
    }
}

impl std::str::FromStr for NodeRef {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        NodeRef::parser().parse(s)
    }
}

impl EdgeRef {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        chumsky::primitive::just("EdgeRef")
            .then(
                u64_parser()
                    .then_ignore(just('v'))
                    .then(u64_parser())
                    .delimited_by(chumsky::primitive::just('('), chumsky::primitive::just(')')),
            )
            .map(|(_, (index, version))| Self::from_ffi(version << 32 | index))
    }
}

impl std::str::FromStr for EdgeRef {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        EdgeRef::parser()
            .then_ignore(chumsky::prelude::end())
            .parse(s)
    }
}

impl InportLocation {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        just("Inport")
            .then(
                NodeRef::parser()
                    .then_ignore(just(',').padded())
                    .then(InputType::parser())
                    .delimited_by(just('['), just(']')),
            )
            .map(|(_start, (node, input))| InportLocation { node, input })
    }
}

impl std::str::FromStr for InportLocation {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        InportLocation::parser()
            .then_ignore(chumsky::prelude::end())
            .parse(s)
    }
}

impl OutportLocation {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        just("Outport")
            .then(
                NodeRef::parser()
                    .then_ignore(just(',').padded())
                    .then(OutputType::parser())
                    .delimited_by(just('['), just(']')),
            )
            .map(|(_start, (node, output))| OutportLocation { node, output })
    }
}

impl std::str::FromStr for OutportLocation {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        OutportLocation::parser()
            .then_ignore(chumsky::prelude::end())
            .parse(s)
    }
}

impl RegionLocation {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        just("Region")
            .then(
                NodeRef::parser()
                    .then_ignore(just(',').padded())
                    .then(usize_parser())
                    .delimited_by(just('['), just(']')),
            )
            .map(|(_, (node, region_index))| RegionLocation { node, region_index })
    }
}
impl std::str::FromStr for RegionLocation {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        RegionLocation::parser()
            .then_ignore(chumsky::prelude::end())
            .parse(s)
    }
}

impl AttribLocation {
    pub fn parser() -> impl Parser<char, Self, Error = chumsky::prelude::Simple<char>> {
        choice((
            InportLocation::parser().map(|i| AttribLocation::from(i)),
            OutportLocation::parser().map(|i| AttribLocation::from(i)),
            NodeRef::parser().map(|i| AttribLocation::from(i)),
            EdgeRef::parser().map(|i| AttribLocation::from(i)),
            RegionLocation::parser().map(|i| AttribLocation::from(i)),
        ))
    }
}

impl std::str::FromStr for AttribLocation {
    type Err = Vec<chumsky::prelude::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        AttribLocation::parser()
            .then_ignore(chumsky::prelude::end())
            .parse(s)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        edge::{InportLocation, InputType, OutportLocation},
        EdgeRef, NodeRef,
    };

    #[test]
    fn noderef_parse() {
        let n = NodeRef::from_ffi(0);
        let s = format!("{}", n);
        assert_eq!(s.parse::<NodeRef>().unwrap(), n);
    }

    #[test]
    fn edgeref_parse() {
        let n = EdgeRef::from_ffi(0);
        let s = format!("{}", n);
        assert_eq!(s.parse::<EdgeRef>().unwrap(), n);
    }

    #[test]
    fn inportlocation_parse() {
        let l = InportLocation {
            node: NodeRef::from_ffi(42),
            input: InputType::GammaPredicate,
        };

        let s = format!("{l}");
        assert_eq!(s.parse::<InportLocation>().unwrap(), l, "{l} failed");
    }

    #[test]
    fn outportlocation_parse() {
        let l = OutportLocation {
            node: NodeRef::from_ffi(42),
            output: crate::edge::OutputType::LambdaDeclaration,
        };

        let s = format!("{l}");
        assert_eq!(s.parse::<OutportLocation>().unwrap(), l);
    }
}
