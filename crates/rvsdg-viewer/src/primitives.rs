use macroquad::prelude::{Color, Vec2};

pub type Point = Vec2;

pub struct Rect {
    pub from: Point,
    pub to: Point,
    pub color: Color,
}
pub struct Line {
    pub from: Point,
    pub to: Point,
    pub width: f32,
    pub color: Color,
}
pub struct Text {
    pub string: String,
    pub color: Color,
    pub width: f32,
}

///All primitives we can draw, either in macroquad or svg
pub enum Prim {
    Box(Rect),
    Line(Line),
    Text(Text),
}
