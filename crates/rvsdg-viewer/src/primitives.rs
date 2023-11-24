use macroquad::prelude::{Color, Vec2};

pub type Point = Vec2;

pub struct Rect {
    pub from: Point,
    pub to: Point,
    pub color: Color,
}
impl Rect {
    pub fn into_svg(&self) -> svg::node::element::Rectangle {
        let width = self.to.x - self.from.x;
        let height = self.to.y - self.from.y;
        svg::node::element::Rectangle::new()
            .set("width", width)
            .set("height", height)
            .set("x", self.from.x)
            .set("y", self.from.y)
            .set(
                "style",
                format!(
                    "fill:rgb({}, {}, {})",
                    self.color.r, self.color.g, self.color.b
                ),
            )
    }
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
