/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use graph_canvas::{GraphCanvas, GraphCanvasMessage};
use iced::widget::{column, container, row, slider, text, Column, Rule, Scrollable, Toggler};
use iced::{Alignment, Element, Length, Task, Theme};
use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::ViewerState;

pub(crate) mod collide;
mod graph_canvas;

pub fn main() -> iced::Result {
    iced::application("RVSDG-Ui", Ui::update, Ui::view)
        .antialiasing(true)
        .run()
}

struct Ui {
    ///Attrib that is currently selected
    selected: Option<AttribLocation>,
    viewer: ViewerState,
    selected_state: usize,
    selection_string: String,
    graph_canvas: GraphCanvas,
    is_dark: bool,
}

#[derive(Debug, Clone)]
pub enum Message {
    Select(Option<AttribLocation>),
    TimelineSelect(usize),
    ResetCanvas,
    ToggleTheme(bool),
    SelectionStringChanged(String),
    Dummy,
}

impl Default for Ui {
    fn default() -> Self {
        //read states from command line this is some strange text.

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
            selection_string: String::new(),
            is_dark: false,
        }
    }
}

impl Ui {
    fn update(&mut self, message: Message) -> Task<Message> {
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
            Message::SelectionStringChanged(new) => {
                self.selection_string = new;
                if let Ok(new_attrib_location) = self.selection_string.parse::<AttribLocation>() {
                    //update internal graph
                    *self.graph_canvas.selection.lock().unwrap() =
                        Some(new_attrib_location.clone());
                    self.graph_canvas.reques_redraw();
                    return Task::done(Message::Select(Some(new_attrib_location)));
                }
            }
            other => println!("Unhandeled message: {other:?}"),
        }

        Task::none()
    }

    fn view(&self) -> Element<Message> {
        //Our ui works by positioning

        let timeline_slider = slider(
            0u8..=(self
                .viewer
                .states
                .len()
                .checked_sub(1)
                .expect("contained no viewer-state slice")) as u8,
            self.selected_state as u8,
            |changed| Message::TimelineSelect(changed as usize),
        );

        let properties: Element<Message> = if let Some(selected) = &self.selected {
            if let Some(properties) = self.viewer.states[self.selected_state]
                .auxilary_data
                .get(selected)
            {
                let mut items = Column::new();
                //always prepend the attrib name
                items = items
                    .push(container(text(format!("{}", selected)).size(20.0)).padding(10.0))
                    .push(Rule::horizontal(1.0));

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
                text(format!("{}", selected)).size(20.0).into()
            }
        } else {
            text("Nothing selected!").into()
        };
        let sidepanel = column![text("Properties").size(30.0), properties];

        //Try to parse the string, if okay, use _success_ styling
        let is_parsing = self.selection_string.parse::<AttribLocation>().is_ok();
        let is_empty = self.selection_string.is_empty();
        let selection_string_styling = move |t: &Theme, s: _| {
            let mut default_style = iced::widget::text_input::default(t, s);
            if is_empty {
                default_style
            } else {
                if is_parsing {
                    default_style.border.color = t.extended_palette().success.strong.color;
                    default_style
                } else {
                    default_style.border.color = t.extended_palette().danger.strong.color;
                    default_style
                }
            }
        };

        column![
            text(&self.viewer.states[self.selected_state].name)
                .width(Length::Shrink)
                .size(20),
            Rule::horizontal(2.0),
            column![
                text("Timeline"),
                timeline_slider,
                row![
                    Toggler::new(self.is_dark)
                        .label("Toggle dark mode".to_owned())
                        .on_toggle(Message::ToggleTheme),
                    iced::widget::text_input("TypeSelection", &self.selection_string)
                        .style(selection_string_styling)
                        .on_input(|input| Message::SelectionStringChanged(input))
                ]
                .spacing(20.0),
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
        .align_x(Alignment::Center)
        .into()
    }
}
