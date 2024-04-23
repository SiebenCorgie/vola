use graph_canvas::{GraphCanvas, GraphCanvasMessage};
use iced::widget::canvas::path::lyon_path::geom::euclid::num::Round;
/*
use core::panic;
use std::path::Path;

use draw::DrawState;

use rvsdg_viewer::ViewerState;

mod collide;
mod draw;
mod ui;


fn main() {
    //try to load the viewer stat
    let path = if let Some(p) = std::env::args().skip(1).next() {
        p
    } else {
        panic!("expected file argument!");
    };

    let path = Path::new(&path);
    if !path.exists() {
        panic!("File {:?} does not exist!", path);
    }

    let state = ViewerState::read_from_file(&path).expect("Could not load viewer-state from disk!");
    let mut draw_state = DrawState::new();

    let mut ui_state = ui::UiState {
        selected_attrib: None,
        hover_over: None,
        timeline_value: 0.0,
    };
}
*/
use iced::widget::{
    button, column, container, row, scrollable, slider, text, vertical_space, Column, Scrollable,
};
use iced::{Alignment, Element, Length, Sandbox, Settings, Theme};
use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::ViewerState;

pub(crate) mod collide;
mod graph_canvas;

pub fn main() -> iced::Result {
    Ui::run(Settings {
        antialiasing: true,
        ..Settings::default()
    })
}

struct Ui {
    ///Attrib that is currently selected
    selected: Option<AttribLocation>,
    viewer: ViewerState,
    selected_state: usize,
    graph_canvas: GraphCanvas,
}

#[derive(Debug, Clone)]
pub enum Message {
    Select(Option<AttribLocation>),
    TimelineSelect(usize),
    ResetCanvas,
    Dummy,
}

impl Sandbox for Ui {
    type Message = Message;

    fn theme(&self) -> Theme {
        Theme::GruvboxDark
    }

    fn new() -> Self {
        //read states from command line

        let path = if let Some(p) = std::env::args().skip(1).next() {
            p
        } else {
            panic!("expected file argument!");
        };

        let path = std::path::Path::new(&path);
        if !path.exists() {
            panic!("File {:?} does not exist!", path);
        }

        let viewer =
            ViewerState::read_from_file(&path).expect("Could not load viewer-state from disk!");

        Ui {
            selected: None,
            viewer,
            selected_state: 0,
            graph_canvas: GraphCanvas::new(),
        }
    }

    fn title(&self) -> String {
        String::from("RVSDG - Viewer")
    }

    fn update(&mut self, message: Message) {
        match message {
            Message::ResetCanvas => self.graph_canvas.reques_redraw(),
            Message::Select(t) => {
                self.selected = t;
            }
            Message::TimelineSelect(s) => {
                self.selected_state = s;
                self.graph_canvas.reques_redraw();
            }
            _ => println!("Au bagge!"),
        }
    }

    fn view(&self) -> Element<Message> {
        //Our ui works by positioning

        let timeline_slider = slider(
            0u8..=(self.viewer.states.len() - 1) as u8,
            self.selected_state as u8,
            |changed| Message::TimelineSelect(changed as usize),
        );

        let properties: Element<Message> = if let Some(selected) = &self.selected {
            if let Some(properties) = self.viewer.states[self.selected_state]
                .auxilary_data
                .get(selected)
            {
                let mut items = Column::new();
                for (pname, props) in properties {
                    items = items.push(
                        container(
                            row![text(pname.clone()), {
                                let mut plist = Column::new();
                                for p in props {
                                    plist = plist.push(text(p.clone()));
                                }

                                plist.padding(2.5)
                            }]
                            .spacing(5.0),
                        )
                        .padding(10.0),
                    );
                }

                Scrollable::new(items).into()
            } else {
                text("No auxilary data").into()
            }
        } else {
            text("Nothing selected!").into()
        };
        let sidepanel = column![text("Properties").size(20.0), properties];

        column![
            text(&self.viewer.states[self.selected_state].name)
                .width(Length::Shrink)
                .size(20),
            row![
                column![
                    self.graph_canvas
                        .view(&self.viewer.states[self.selected_state])
                        .map(|msg| {
                            match msg {
                                GraphCanvasMessage::Redraw => Message::ResetCanvas,
                                GraphCanvasMessage::Select(s) => Message::Select(s),
                            }
                        }),
                    column![text("Timeline"), timeline_slider]
                ],
                sidepanel.padding(20.0).max_width(400.0)
            ]
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }
}
