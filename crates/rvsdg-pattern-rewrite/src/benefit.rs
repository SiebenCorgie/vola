pub trait Benefit: PartialOrd + PartialEq + Ord {}

impl<T> Benefit for T where T: PartialOrd + PartialEq + Ord {}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub struct CodeSize(usize);

#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub struct Speed(usize);
