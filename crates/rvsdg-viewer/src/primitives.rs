/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use glam::Vec2;
use rvsdg::attrib::AttribLocation;
use serde::{Deserialize, Serialize};
use std::fmt::Write;

use crate::Stroke;

pub type Point = Vec2;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Color {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl Color {
    pub const BLACK: Self = Self::new(0.0, 0.0, 0.0, 1.0);
    pub const RED: Self = Self::new(1.0, 0.0, 0.0, 1.0);

    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    pub fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self {
            r: r as f32 / 255.0,
            g: g as f32 / 255.0,
            b: b as f32 / 255.0,
            a: a as f32 / 255.0,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Line {
    pub from: Point,
    pub to: Point,
    pub width: f32,
    pub color: Color,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Path {
    pub points: Vec<Point>,
    pub width: f32,
    pub color: Color,
    pub stroke: Stroke,
}

impl Path {
    pub fn emit_svg(&self, id: &AttribLocation) -> String {
        let start = self.points[0];
        let mut path_command = format!("<path id=\"{:?}\" d=\" M {} {} ", id, start.x, start.y);

        //Write all line-to commads
        for point in &self.points[1..] {
            write!(path_command, "L {} {} ", point.x, point.y).unwrap();
        }
        //finish by setting up stroke and fill
        write!(
            path_command,
            "\" fill=\"none\" {} stroke-width=\"{}\" style=\"stroke:{};\" />",
            self.stroke.into_svg(),
            self.width,
            color_styling(&self.color)
        )
        .unwrap();

        path_command
    }
}

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Clone)]
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
            "
<g transform=\"scale(1, -1)\">
<text id=\"{:?}\" x=\"{}\" y=\"{}\" font-size=\"{}\" font-family=\"monospace\">{}</text>
</g>
",
            id,
            self.size / 2.0,
            -(self.size - 1.0),
            self.size,
            self.string
        )
    }
}

///All primitives we can draw, either in macroquad or svg
#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Clone)]
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
#[derive(Debug, Serialize, Deserialize)]
pub struct PrimTree {
    pub id: AttribLocation,
    pub prim: Prim,
    pub children: Vec<PrimTree>,
}

impl PrimTree {
    pub fn to_svg(&self, height: f32) -> String {
        //1024 because _why not_
        let mut buffer = String::with_capacity(1024);
        write!(buffer, "<svg>").unwrap();
        write!(buffer, "<g transform=\"matrix(1 0 0 -1 0 {height})\">").unwrap();

        self.append_svg(&mut buffer);

        write!(buffer, "\n</g>").unwrap();

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
            Prim::Path(p) => write!(buffer, "\n{}", p.emit_svg(&self.id)).unwrap(),
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
