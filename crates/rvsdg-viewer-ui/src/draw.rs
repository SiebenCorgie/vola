use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::{
    glam::Vec2,
    primitives::{Prim, PrimTree},
};

use crate::ui::UiState;

///The draw state of this app. Somewhat like a _camera_.
///
/// It mostly selects a node that is used as _origin_ to draw and allows
/// zooming _out_ of this node till the root level
pub struct DrawState {
    zoom: f32,
    offset: Vec2,
    is_dragging: bool,
}

impl DrawState {
    pub fn new() -> Self {
        DrawState {
            zoom: 0.001,
            offset: Vec2::ZERO,
            is_dragging: false,
        }
    }
}

impl DrawState {
    const BORDER_THICKNESS: f32 = 1.0;
    const FLIP_Y: bool = true;
    pub fn update_camera(&mut self) {
        //HANDEL SCROLLING
        let mouse_delta = if mouse_wheel().1 < 0.0 {
            0.5
        } else if mouse_wheel().1 > 0.0 {
            2.0
        } else {
            1.0
        };
        self.zoom = (self.zoom * mouse_delta).clamp(0.0001, 100.0);
        let aspect = screen_width() / screen_height();
        //self.cam =
        //    Camera2D::from_display_rect(Rect::new(0.0, 0.0, screen_width(), screen_height()));
        self.cam.zoom = Vec2::new(self.zoom, self.zoom * aspect);

        let delta_poll = mouse_delta_position();
        if is_mouse_button_pressed(MouseButton::Middle) {
            self.is_dragging = true;
        }

        if is_mouse_button_released(MouseButton::Middle)
            || !is_mouse_button_down(MouseButton::Middle)
        {
            self.is_dragging = false;
        }

        //HANDLE TRANSLATION
        if self.is_dragging {
            let offset = if Self::FLIP_Y {
                Vec2::new(delta_poll.x, -delta_poll.y)
            } else {
                delta_poll
            };

            self.offset -= offset * (1.0 / self.zoom);
        }
    }

    pub fn get_relative_cursor_location(&self) -> Vec2 {
        let mouse_pos = mouse_position();
        let screen_ext = Vec2::new(screen_width(), screen_height());
        let offset_perc =
            ((Vec2::new(mouse_pos.0, mouse_pos.1) / screen_ext) - Vec2::splat(0.5)) * 2.0;
        let mut local_offset = offset_perc * (1.0 / self.cam.zoom);

        //finally flip y and add local offset
        if Self::FLIP_Y {
            local_offset.y *= -1.0;
        }
        local_offset -= self.offset;
        local_offset
    }

    pub fn draw_tree(&self, tree: &PrimTree, ui: &UiState) {
        //The recursive draw pattern.
        //Nice thing is, that rvsdg-viewer has already taken care of
        //laying out the tree, so we just have to draw each primitive

        let offset = self.offset;
        set_camera(&self.cam);
        self.draw_tree_inner(tree, offset, ui);
        //let at = self.get_relative_cursor_location();
        //draw_circle(at.x, at.y, 10.0, RED);

        set_default_camera();
    }

    fn draw_tree_inner(&self, subtree: &PrimTree, offset: Vec2, ui: &UiState) {
        let new_offset = self.draw_prim(&subtree.prim, &subtree.id, offset, ui);

        for sub in subtree.children.iter() {
            self.draw_tree_inner(sub, new_offset, ui);
        }
    }

    fn draw_prim(&self, prim: &Prim, attrib: &AttribLocation, offset: Vec2, ui: &UiState) -> Vec2 {
        let is_hovering = ui
            .hover_over
            .as_ref()
            .map(|selected| selected == attrib)
            .unwrap_or(false);

        let is_selected = ui
            .selected_attrib
            .as_ref()
            .map(|selected| selected == attrib)
            .unwrap_or(false);

        match prim {
            Prim::Box(b) => {
                let with_border =
                    if let AttribLocation::Node(_) | AttribLocation::Region(_) = attrib {
                        true
                    } else {
                        false
                    };

                let origin = offset + Vec2::new(b.from.x, b.from.y);
                let ext = b.extent();
                let color = match attrib {
                    AttribLocation::InPort(_) | AttribLocation::OutPort(_) => BLACK,
                    //AttribLocation::Region(_) => WHITE,
                    _ => macroquad::color::Color::new(b.color.r, b.color.g, b.color.b, 1.0),
                };

                if Self::FLIP_Y {
                    draw_rectangle(origin.x, -origin.y, ext.x, -ext.y, color);
                } else {
                    draw_rectangle(origin.x, origin.y, ext.x, ext.y, color);
                }

                if with_border {
                    let thickness = if is_hovering || is_selected {
                        Self::BORDER_THICKNESS + 2.0
                    } else {
                        Self::BORDER_THICKNESS
                    };
                    if Self::FLIP_Y {
                        draw_rectangle_lines(origin.x, -origin.y, ext.x, -ext.y, thickness, BLACK);
                    } else {
                        draw_rectangle_lines(origin.x, origin.y, ext.x, ext.y, thickness, BLACK);
                    }
                } else {
                    //if not bordered, but selected or hovering, draw green line instead
                    if is_selected || is_hovering {
                        if Self::FLIP_Y {
                            draw_rectangle_lines(
                                origin.x,
                                -origin.y,
                                ext.x,
                                -ext.y,
                                Self::BORDER_THICKNESS,
                                GREEN,
                            );
                        } else {
                            draw_rectangle_lines(
                                origin.x,
                                origin.y,
                                ext.x,
                                ext.y,
                                Self::BORDER_THICKNESS,
                                GREEN,
                            );
                        }
                    }
                }
                offset
            }
            Prim::Line(l) => {
                let thickness = if is_hovering || is_selected {
                    Self::BORDER_THICKNESS + 2.0
                } else {
                    Self::BORDER_THICKNESS
                };
                let color =
                    macroquad::color::Color::new(l.color.r, l.color.g, l.color.b, l.color.a);
                let from = offset + Vec2::new(l.from.x, l.from.y);
                let to = offset + Vec2::new(l.to.x, l.to.y);
                if Self::FLIP_Y {
                    draw_line(from.x, -from.y, to.x, -to.y, thickness, color);
                } else {
                    draw_line(from.x, from.y, to.x, to.y, thickness, color);
                }

                offset
            }
            Prim::Text(text) => {
                let color = macroquad::color::Color::new(
                    text.color.r,
                    text.color.g,
                    text.color.b,
                    text.color.a,
                );
                let local_offset = Vec2::new(text.size / 2.0, 0.0);
                let at = Vec2::new(
                    text.at.x + offset.x + local_offset.x,
                    (text.at.y + offset.y) + (text.size / 3.0),
                );

                if Self::FLIP_Y {
                    draw_text_ex(
                        &text.string,
                        at.x,
                        -at.y,
                        TextParams {
                            font: Some(&self.font),
                            font_size: (text.size.ceil() * 4.0) as u16,
                            font_scale: 0.25,
                            color,
                            ..Default::default()
                        },
                    );
                } else {
                    draw_text_ex(
                        &text.string,
                        at.x,
                        at.y,
                        TextParams {
                            font: Some(&self.font),
                            font_size: (text.size.ceil() * 4.0) as u16,
                            font_scale: 0.25,
                            color,
                            ..Default::default()
                        },
                    );
                }

                offset
            }
            Prim::Path(p) => {
                let thickness = if is_hovering || is_selected {
                    Self::BORDER_THICKNESS + 2.0
                } else {
                    Self::BORDER_THICKNESS
                };
                let color =
                    macroquad::color::Color::new(p.color.r, p.color.g, p.color.b, p.color.a);

                for idx in 0..(p.points.len() - 1) {
                    let start = offset + Vec2::new(p.points[idx].x, p.points[idx].y);
                    let end = offset + Vec2::new(p.points[idx + 1].x, p.points[idx + 1].y);

                    if Self::FLIP_Y {
                        draw_line(start.x, -start.y, end.x, -end.y, thickness, color);
                    } else {
                        draw_line(start.x, start.y, end.x, end.y, thickness, color);
                    }
                }

                offset
            }
            Prim::Offset(o) => offset + Vec2::new(o.x, o.y),
        }
    }
}
