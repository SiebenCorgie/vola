use iced::{
    event,
    mouse::{self},
    widget::{
        canvas::{self, Fill, Frame, Geometry, LineDash, Path, Stroke, Text},
        Canvas,
    },
    Color, Element, Length, Point, Renderer, Theme, Vector,
};
use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::{
    glam::Vec2,
    primitives::{Prim, PrimTree},
    GraphState,
};

use crate::collide;

pub struct GraphCanvas {
    cache: canvas::Cache,
}

pub enum GraphCanvasMessage {
    Select(Option<AttribLocation>),
    Redraw,
}

impl GraphCanvas {
    pub fn new() -> Self {
        GraphCanvas {
            cache: canvas::Cache::default(),
        }
    }

    pub fn view<'a>(&'a self, graph: &'a GraphState) -> Element<'a, GraphCanvasMessage> {
        Canvas::new(GraphDrawer { gs: self, graph })
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }

    pub fn reques_redraw(&mut self) {
        self.cache.clear();
    }
}

struct GraphDrawer<'a> {
    gs: &'a GraphCanvas,
    graph: &'a GraphState,
}

struct CanvasDrawState {
    graph_zoom: f32,
    graph_offset: Vector,
    ///If the middle button is pressed,
    ///contains the mouse position at grabbing time,
    /// as well as the offset at that time.
    is_middle_pressed: Option<(Point, Vector)>,

    selected: Option<AttribLocation>,
    hovering: Option<AttribLocation>,
}

impl Default for CanvasDrawState {
    fn default() -> Self {
        CanvasDrawState {
            graph_zoom: 1.0,
            graph_offset: Vector::new(0.0, 0.0),
            is_middle_pressed: None,

            selected: None,
            hovering: None,
        }
    }
}

impl CanvasDrawState {
    fn graph_local_offset(&self, mouse: Point, bounds: &iced::Rectangle) -> Vec2 {
        let mouse_in_frame = Vec2::from_array(mouse.into()) - Vec2::new(bounds.x, bounds.y);
        let zoom_local_mouse = mouse_in_frame * (1.0 / self.graph_zoom);
        let mouse_offset_local = zoom_local_mouse - Vec2::from_array(self.graph_offset.into());
        mouse_offset_local
    }
}

impl<'a> canvas::Program<GraphCanvasMessage> for GraphDrawer<'a> {
    type State = CanvasDrawState;

    fn update(
        &self,
        state: &mut Self::State,
        event: canvas::Event,
        bounds: iced::Rectangle,
        cursor: mouse::Cursor,
    ) -> (canvas::event::Status, Option<GraphCanvasMessage>) {
        let cursor_position = if let Some(cp) = cursor.position_in(bounds) {
            cp
        } else {
            return (event::Status::Ignored, None);
        };

        match event {
            canvas::Event::Mouse(mouse::Event::WheelScrolled { delta }) => {
                let delta = match delta {
                    mouse::ScrollDelta::Lines { x: _, y } => y,
                    mouse::ScrollDelta::Pixels { x: _, y } => y,
                };

                //calculate the offset in a way, that we don't zoom in/out of the
                //top left corner, but the center.

                let new_graph_zoom = if delta < 0.0 {
                    state.graph_zoom * 0.5
                } else if delta > 0.0 {
                    state.graph_zoom * 2.0
                } else {
                    state.graph_zoom
                };

                let bound_difference: Vector = (Vector::from(bounds.size())
                    * (1.0 / state.graph_zoom))
                    - (Vector::from(bounds.size()) * (1.0 / new_graph_zoom));
                //offset _half_ the bound difference

                state.graph_offset = state.graph_offset - (bound_difference * 0.5);
                state.graph_zoom = new_graph_zoom;
                (event::Status::Captured, Some(GraphCanvasMessage::Redraw))
            }
            canvas::Event::Mouse(mouse::Event::CursorMoved { position }) => {
                if let Some((src, moving_atm)) = &mut state.is_middle_pressed {
                    state.graph_offset =
                        *moving_atm - ((*src - cursor_position) * (1.0 / state.graph_zoom));
                }

                let graph_local_offset = state.graph_local_offset(position, &bounds);
                if let Some(new_collision) =
                    collide::find_collision(&self.graph.display, graph_local_offset)
                {
                    state.hovering = Some(new_collision);
                } else {
                    state.hovering = None;
                }
                (event::Status::Captured, Some(GraphCanvasMessage::Redraw))
            }
            canvas::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Middle)) => {
                state.is_middle_pressed = Some((cursor_position, state.graph_offset));
                (event::Status::Captured, Some(GraphCanvasMessage::Redraw))
            }
            canvas::Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Middle))
            | canvas::Event::Mouse(mouse::Event::CursorLeft) => {
                if let Some((src_pos, initial_offset)) = state.is_middle_pressed.take() {
                    let final_offset = src_pos - cursor_position;
                    state.graph_offset = initial_offset - (final_offset * (1.0 / state.graph_zoom));
                }

                (event::Status::Captured, Some(GraphCanvasMessage::Redraw))
            }
            canvas::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(hoverin) = &state.hovering {
                    state.selected = Some(hoverin.clone());
                } else {
                    state.selected = None;
                }
                (
                    event::Status::Captured,
                    Some(GraphCanvasMessage::Select(state.selected.clone())),
                )
            }
            _ => (event::Status::Captured, None),
        }
    }

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: iced::Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let content = self
            .gs
            .cache
            .draw(renderer, bounds.size(), |frame: &mut Frame| {
                frame.scale(state.graph_zoom);
                frame.translate(state.graph_offset);
                draw_tree(frame, &self.graph.display, &state.hovering, &state.selected);
            });

        vec![content]
    }
}

fn draw_tree(
    frame: &mut Frame,
    tree: &PrimTree,
    hovering: &Option<AttribLocation>,
    selected: &Option<AttribLocation>,
) {
    let _ = draw_tree_inner(frame, tree, hovering, selected);
}

fn draw_tree_inner(
    frame: &mut Frame,
    subtree: &PrimTree,
    hovering: &Option<AttribLocation>,
    selected: &Option<AttribLocation>,
) {
    let new_offset = draw_prim(frame, &subtree.prim, &subtree.id, hovering, selected);

    if let Some(local_offset) = new_offset {
        frame.translate(local_offset);
    }
    for sub in subtree.children.iter() {
        draw_tree_inner(frame, sub, hovering, selected);
    }

    if let Some(local_offset) = new_offset {
        frame.translate(local_offset * -1.0)
    }
}

fn draw_prim(
    frame: &mut Frame,
    prim: &Prim,
    attrib: &AttribLocation,
    hovering: &Option<AttribLocation>,
    selected: &Option<AttribLocation>,
) -> Option<Vector> {
    let is_selected = Some(attrib) == selected.as_ref();
    let is_hovering = Some(attrib) == hovering.as_ref();

    match prim {
        Prim::Box(b) => {
            let color = Color::new(b.color.r, b.color.g, b.color.b, 1.0);
            let mut with_border =
                if let AttribLocation::Node(_) | AttribLocation::Region(_) = attrib {
                    true
                } else {
                    false
                };
            frame.fill(
                &Path::rectangle(b.from.to_array().into(), b.extent().to_array().into()),
                Fill {
                    style: canvas::Style::Solid(color),
                    ..Default::default()
                },
            );

            let mut border_color = Color::new(0.0, 0.0, 0.0, 1.0);
            //Overwrite border condition and color for ports
            if is_selected || is_hovering {
                if let AttribLocation::InPort(_) | AttribLocation::OutPort(_) = attrib {
                    with_border = true;
                    border_color = Color::new(0.0, 1.0, 0.0, 1.0);
                }
            }

            let mut border_width = 2.0;
            if is_selected || is_hovering {
                border_width *= 2.0;
            }

            if with_border {
                frame.stroke(
                    &Path::rectangle(b.from.to_array().into(), b.extent().to_array().into()),
                    Stroke::default()
                        .with_color(border_color)
                        .with_width(border_width),
                );
            }
        }

        Prim::Line(l) => {
            let color = Color::new(l.color.r, l.color.g, l.color.b, l.color.a);
            let mut width = l.width * 2.0;
            if is_selected || is_hovering {
                width *= 2.0;
            }
            frame.stroke(
                &Path::line(l.from.to_array().into(), l.to.to_array().into()),
                Stroke::default().with_color(color).with_width(width),
            );
        }
        Prim::Path(p) => {
            let color = Color::new(p.color.r, p.color.g, p.color.b, p.color.a);
            let mut width = p.width * 2.0;
            if is_selected || is_hovering {
                width *= 2.0;
            }
            let path = canvas::path::Path::new(|b| {
                b.move_to(p.points[0].to_array().into());
                for pidx in 1..p.points.len() {
                    b.line_to(p.points[pidx].to_array().into());
                }
            });

            let stroke = match p.stroke {
                rvsdg_viewer::Stroke::Line => Stroke::default().with_color(color).with_width(width),
                rvsdg_viewer::Stroke::Dashs => Stroke {
                    width,
                    style: canvas::Style::Solid(color),
                    line_dash: LineDash {
                        segments: &[3.0],
                        offset: 1,
                    },
                    ..Default::default()
                },
                rvsdg_viewer::Stroke::Dots => Stroke {
                    width,
                    style: canvas::Style::Solid(color),
                    line_dash: LineDash {
                        segments: &[1.0],
                        offset: 1,
                    },
                    ..Default::default()
                },
            };

            frame.stroke(&path, stroke)
        }
        Prim::Text(text) => {
            let color = Color::new(text.color.r, text.color.g, text.color.b, text.color.a);
            let local_offset = Vec2::new(text.size / 2.0, 0.0);
            let at = Vec2::new(text.at.x + local_offset.x, text.at.y);

            frame.fill_text(Text {
                content: text.string.clone(),
                position: at.to_array().into(),
                color,
                size: text.size.into(),
                ..Default::default()
            });
        }
        Prim::Offset(o) => return Some(o.to_array().into()),
    }

    None
}
