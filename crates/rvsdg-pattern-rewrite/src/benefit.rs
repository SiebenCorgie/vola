pub trait Benefit: PartialOrd + PartialEq + Ord {}

impl<T> Benefit for T where T: PartialOrd + PartialEq + Ord {}

#[derive(PartialEq, PartialOrd)]
pub struct CodeSize(usize);

#[derive(PartialEq, PartialOrd)]
pub struct Speed(usize);
