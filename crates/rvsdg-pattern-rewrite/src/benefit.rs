use std::usize;

pub trait Benefit: PartialOrd + PartialEq + Ord + Default {}

impl<T> Benefit for T where T: PartialOrd + PartialEq + Ord + Default {}

///Codesize reduction benefit. Should generally contain how much is _saved_. I.e.
/// if you save 2 instructions via a pattern application, the CodeSize benefit is 2.
#[derive(PartialEq, PartialOrd, Eq, Ord, Default)]
pub struct CodeSize(pub usize);

///Speedup benefit. 0 means no speedup.
#[derive(PartialEq, PartialOrd, Eq, Ord, Default)]
pub struct Speed(pub usize);
