use graph_canvas::{GraphCanvas, GraphCanvasMessage};
use iced::widget::{column, container, row, slider, text, Column, Rule, Scrollable, Toggler};
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
    is_dark: bool,
}

#[derive(Debug, Clone)]
pub enum Message {
    Select(Option<AttribLocation>),
    TimelineSelect(usize),
    ResetCanvas,
    ToggleTheme(bool),

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
            selected_state: 0,
            graph_canvas: GraphCanvas::new(),
            is_dark: false,
        }
    }

    fn theme(&self) -> Theme {
        if self.is_dark {
            Theme::SolarizedDark
        } else {
            Theme::SolarizedLight
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
            Message::ToggleTheme(pl) => self.is_dark = pl,
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
                            row![text(pname.clone()).size(20.0), {
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

                    items = items.push(Rule::horizontal(1.0));
                }

                Scrollable::new(items).into()
            } else {
                text("No auxilary data").into()
            }
        } else {
            text("Nothing selected!").into()
        };
        let sidepanel = column![text("Properties").size(30.0), properties];

        column![
            text(&self.viewer.states[self.selected_state].name)
                .width(Length::Shrink)
                .size(20),
            Rule::horizontal(2.0),
            column![
                text("Timeline"),
                timeline_slider,
                row![Toggler::new(
                    "Toggle dark mode".to_owned(),
                    self.is_dark,
                    |state| { Message::ToggleTheme(state) }
                ),],
            ]
            .spacing(10.0),
            Rule::horizontal(2.0),
            row![
                sidepanel.padding(20.0).width(350.0),
                Rule::vertical(2.0),
                column![
                    self.graph_canvas
                        .view(&self.viewer.states[self.selected_state])
                        .map(|msg| {
                            match msg {
                                GraphCanvasMessage::Redraw => Message::ResetCanvas,
                                GraphCanvasMessage::Select(s) => Message::Select(s),
                            }
                        }),
                    Rule::horizontal(2.0),
                ],
            ]
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }
}
