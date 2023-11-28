use macroquad::prelude::Vec2;

use crate::primitives::Rect;

///Simple svg builder utility that handles wrapper code
/// and formating our primitives
pub struct SVGWriter {
    ///Our svg lines
    buffer: Vec<String>,

    //Tracks min/max,
    bound: (Vec2, Vec2),
}

impl SVGWriter {
    pub fn start() -> Self {
        SVGWriter {
            buffer: Vec::new(),
            bound: (Vec2::ZERO, Vec2::ZERO),
        }
    }

    pub fn push_rect(&mut self, rect: &Rect) {
        let ext = rect.to - rect.from;

        self.buffer.push(format!(
            "<rect width=\"{}\" height=\"{}\" x=\"{}\" y=\"{}\" style=\"fill:rgb({}, {}, {})\"/>",
            ext.x, ext.y, rect.from.x, rect.from.y, rect.color.r, rect.color.g, rect.color.b
        ));
    }

    ///builds the whole thing as one big string
    pub fn build(self) -> String {
        let mut build = String::with_capacity(100);

        build.push_str("<svg width=\"100%\" height=\"100%\">");

        for line in self.buffer {
            build.push_str("\n");
            build.push_str(&line);
        }

        build.push_str("\n");
        build.push_str("</svg>\n");

        build
    }
}
