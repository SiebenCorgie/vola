use macroquad::{
    prelude::*,
    ui::{
        hash, root_ui,
        widgets::{self},
    },
};
use rvsdg::attrib::AttribLocation;
use rvsdg_viewer::GraphState;

pub struct UiState {
    pub selected_attrib: Option<AttribLocation>,
    pub hover_over: Option<AttribLocation>,
    pub timeline_value: f32,
}

impl UiState {
    const SIDEPANEL_WIDTH: f32 = 200.0;
    const TIMELINE_WIDTH: f32 = 400.0;
    const TIMELINE_HEIGHT: f32 = 60.0;
    pub fn draw(&mut self, state: &GraphState) -> bool {
        let timeline_at = Vec2::new(
            screen_width() / 2.0 - (Self::TIMELINE_WIDTH / 2.0),
            screen_height() - Self::TIMELINE_HEIGHT,
        );
        let properties_at = Vec2::new(screen_width() - Self::SIDEPANEL_WIDTH, 0.0);

        let mut consumed_input = false;

        if Rect::new(
            timeline_at.x,
            timeline_at.y,
            Self::TIMELINE_WIDTH,
            Self::TIMELINE_HEIGHT,
        )
        .contains(mouse_position().into())
        {
            consumed_input = true;
        }

        widgets::Window::new(
            hash!(),
            timeline_at,
            vec2(Self::TIMELINE_WIDTH, Self::TIMELINE_HEIGHT),
        )
        .label(&format!("{}", state.name))
        .movable(false)
        .titlebar(true)
        .ui(&mut *root_ui(), |ui| {
            ui.slider(hash!(), "timeline", 0.0..1.0, &mut self.timeline_value)
        });

        if let Some(selected) = &self.selected_attrib {
            if Rect::new(
                properties_at.x,
                properties_at.y,
                Self::SIDEPANEL_WIDTH,
                screen_height(),
            )
            .contains(mouse_position().into())
            {
                consumed_input = true;
            }

            widgets::Window::new(
                hash!(),
                properties_at,
                Vec2::new(Self::SIDEPANEL_WIDTH, screen_height()),
            )
            .label("Properties")
            .movable(false)
            .titlebar(true)
            .ui(&mut *root_ui(), |ui| {
                if let Some(auxdata) = state.auxilary_data.get(selected) {
                    for (name, data) in auxdata {
                        ui.label(None, name);
                        for d in data {
                            ui.label(None, &format!("   {d}"));
                        }
                        ui.separator();
                        ui.separator();
                    }
                } else {
                    ui.label(None, "No Data");
                }
            });
        }

        consumed_input
    }

    pub fn get_graph_index(&self, state_count: usize) -> usize {
        ((self.timeline_value * state_count as f32).round() as usize).clamp(0, state_count - 1)
    }
}
