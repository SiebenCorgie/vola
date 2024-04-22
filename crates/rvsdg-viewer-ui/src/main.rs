use core::panic;
use std::path::Path;

use draw::DrawState;
use macroquad::prelude::*;

use rvsdg_viewer::ViewerState;

mod collide;
mod draw;
mod ui;

fn window_conf() -> Conf {
    Conf {
        window_title: "RVSDG Viewer".to_owned(),
        sample_count: 4,
        window_resizable: true,
        ..Default::default()
    }
}
#[macroquad::main(window_conf)]
async fn main() {
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
    let mut draw_state = DrawState::new().await;

    let mut ui_state = ui::UiState {
        selected_attrib: None,
        hover_over: None,
        timeline_value: 0.0,
    };

    loop {
        draw_state.update_camera();
        clear_background(Color::from_rgba(80, 80, 80, 255));

        let selected_graph_index = ui_state.get_graph_index(state.states.len());

        draw_state.draw_tree(&state.states[selected_graph_index].display, &ui_state);

        let ignore_input = ui_state.draw(&state.states[selected_graph_index]);

        //Update the hover / selection state.
        if !ignore_input {
            let pos = draw_state.get_relative_cursor_location();
            if let Some(attr) =
                collide::find_collision(&state.states[selected_graph_index].display, pos)
            {
                ui_state.hover_over = Some(attr.clone());

                if is_mouse_button_pressed(MouseButton::Left) {
                    ui_state.selected_attrib = Some(attr);
                }
            } else {
                ui_state.hover_over = None;

                if is_mouse_button_pressed(MouseButton::Left) {
                    ui_state.selected_attrib = None;
                }
            }
        }

        next_frame().await
    }
}
