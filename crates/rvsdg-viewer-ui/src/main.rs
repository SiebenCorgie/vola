use graph_canvas::{GraphCanvas, GraphCanvasMessage};
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
use iced::widget::{button, column, text};
use iced::{Alignment, Element, Length, Sandbox, Settings};
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
    graph_canvas: GraphCanvas,
}

#[derive(Debug, Clone)]
pub enum Message {
    Select(Option<AttribLocation>),
    ResetCanvas,
    Dummy,
}

impl Sandbox for Ui {
    type Message = Message;

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
                println!("Select override!");
                self.selected = t
            }
            _ => println!("Au bagge!"),
        }
    }

    fn view(&self) -> Element<Message> {
        column![
            text("This would be a graph name")
                .width(Length::Shrink)
                .size(50),
            self.graph_canvas.view(&self.viewer.states[0]).map(|msg| {
                match msg {
                    GraphCanvasMessage::Redraw => Message::ResetCanvas,
                    GraphCanvasMessage::Select(s) => Message::Select(s),
                }
            }),
            button("Clear").padding(8).on_press(Message::Dummy),
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }
}
