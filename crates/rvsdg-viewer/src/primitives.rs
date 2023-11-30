use macroquad::prelude::{Color, Vec2};

pub type Point = Vec2;
#[derive(Debug, Clone)]
pub struct Rect {
    pub from: Point,
    pub to: Point,
    pub color: Color,
}

pub fn color_styling(color: &Color) -> String {
    format!(
        "rgb({}, {}, {})",
        ((color.r * 255.0).clamp(0.0, 255.0) as u8),
        ((color.g * 255.0).clamp(0.0, 255.0) as u8),
        ((color.b * 255.0).clamp(0.0, 255.0) as u8),
    )
}

impl Rect {
    pub fn extend(&self) -> Vec2 {
        self.to - self.from
    }
    pub fn empty(color: Color) -> Self {
        Rect {
            from: Vec2::ZERO,
            to: Vec2::ZERO,
            color,
        }
    }

    pub fn emit_svg(&self, id: String) -> String {
        format!(
            "<rect id=\"{}\" x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" style=\"fill:{}\" />",
            id,
            self.extend().x,
            self.extend().y,
            color_styling(&self.color)
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
