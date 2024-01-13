use macroquad::prelude::{Color, Vec2};
use rvsdg::attrib::AttribLocation;
use std::fmt::Write;

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
    pub fn extent(&self) -> Vec2 {
        self.to - self.from
    }
    pub fn center(&self) -> Vec2 {
        self.from + (self.extent() / 2.0)
    }
    pub fn empty(color: Color) -> Self {
        Rect {
            from: Vec2::ZERO,
            to: Vec2::ZERO,
            color,
        }
    }

    pub fn emit_svg(&self, id: &AttribLocation) -> String {
        format!(
            "<rect id=\"{:?}\" x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" style=\"fill:{};stroke:rgb(0,0,0);stroke-width:1\" />",
            id,
            self.from.x,
            self.from.y,
            self.extent().x,
            self.extent().y,
            color_styling(&self.color)
        )
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Line {
    pub from: Point,
    pub to: Point,
    pub width: f32,
    pub color: Color,
}

#[derive(Debug)]
pub struct Path {
    pub points: Vec<Point>,
    pub width: f32,
    pub color: Color,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Text {
    pub at: Point,
    pub string: String,
    pub color: Color,
    pub width: f32,
    pub size: f32,
}

impl Text {
    pub fn width_for_string_size(string: &str, size: usize) -> f32 {
        //assume string with mono-space font
        string.chars().count() as f32 * size as f32
    }

    pub fn emit_svg(&self, id: &AttribLocation) -> String {
        format!(
            "<text id=\"{:?}\" x=\"{}\" y=\"{}\" font-size=\"{}\" font-family=\"monospace\">{}</text>",
            id,
            self.size / 2.0,
            self.size - 1.0,
            self.size,
            self.string
        )
    }
}

///All primitives we can draw, either in macroquad or svg
#[allow(dead_code)]
#[derive(Debug)]
pub enum Prim {
    Box(Rect),
    Line(Line),
    Text(Text),
    Path(Path),
    //Simple offset, without an actual primitive
    Offset(Point),
}

///Primitive based representation of an layout.
///
/// All coordinates are relative to their parent.
///
/// Top left is (0,0), bottom right is (inf, inf)
#[derive(Debug)]
pub struct PrimTree {
    pub id: AttribLocation,
    pub prim: Prim,
    pub children: Vec<PrimTree>,
}

impl PrimTree {
    pub fn to_svg(&self) -> String {
        //1024 because _why not_
        let mut buffer = String::with_capacity(1024);
        write!(buffer, "<svg>").unwrap();

        self.append_svg(&mut buffer);

        write!(buffer, "\n</svg>\n").unwrap();
        buffer
    }
    fn append_svg(&self, buffer: &mut String) {
        match &self.prim {
            Prim::Box(r) => write!(buffer, "\n{}", r.emit_svg(&self.id)).unwrap(),
            Prim::Text(t) => write!(buffer, "\n{}", t.emit_svg(&self.id)).unwrap(),
            Prim::Offset(o) => {
                write!(buffer, "\n<g transform=\"translate({}, {})\">", o.x, o.y).unwrap()
            }
            _ => todo!("got {:?}", self.prim),
        }

        for child in &self.children {
            child.append_svg(buffer)
        }

        if let Prim::Offset(_) = &self.prim {
            write!(buffer, "\n</g>").unwrap()
        }
    }
}
