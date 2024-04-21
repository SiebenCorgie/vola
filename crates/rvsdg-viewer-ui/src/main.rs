use core::panic;
use std::path::Path;

use font_kit::{family_name::FamilyName, properties::Properties, source::Source};
use macroquad::prelude::*;

use rvsdg_viewer::ViewerState;

async fn load_font() -> macroquad::text::Font {
    let source = font_kit::source::SystemSource::new();

    match source
        .select_best_match(&[FamilyName::SansSerif], &Properties::new())
        .expect("Failed to load *any* font")
    {
        font_kit::handle::Handle::Path { path, font_index } => {
            load_ttf_font(path.to_str().unwrap())
                .await
                .expect(&format!("Failed to load font from {:?}", path))
        }
        font_kit::handle::Handle::Memory { bytes, font_index } => {
            load_ttf_font_from_bytes(&bytes).expect("Failed to load font from bytes")
        }
    }
}

#[macroquad::main("Text")]
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

    let state = ViewerState::read_from_file(&path);

    let font = load_font().await;

    let mut angle = 0.0;

    loop {
        clear_background(BLACK);

        draw_text_ex("Custom font size:", 20.0, 20.0, TextParams::default());
        let mut y = 20.0;

        for font_size in (30..100).step_by(20) {
            let text = "abcdef";
            let params = TextParams {
                font_size,
                ..Default::default()
            };

            y += font_size as f32;
            draw_text_ex(text, 20.0, y, params);
        }

        draw_text_ex("Dynamic font scale:", 20.0, 400.0, TextParams::default());
        draw_text_ex(
            "abcd",
            20.0,
            450.0,
            TextParams {
                font_size: 50,
                font_scale: get_time().sin() as f32 / 2.0 + 1.0,
                ..Default::default()
            },
        );

        draw_text_ex("Custom font:", 400.0, 20.0, TextParams::default());
        draw_text_ex(
            "abcd",
            400.0,
            70.0,
            TextParams {
                font_size: 50,
                font: Some(&font),
                ..Default::default()
            },
        );

        draw_text_ex(
            "abcd",
            400.0,
            160.0,
            TextParams {
                font_size: 100,
                font: Some(&font),
                ..Default::default()
            },
        );

        draw_text_ex(
            "abcd",
            screen_width() / 4.0 * 2.0,
            screen_height() / 3.0 * 2.0,
            TextParams {
                font_size: 70,
                font: Some(&font),
                rotation: angle,
                ..Default::default()
            },
        );

        let center = get_text_center("abcd", Option::None, 70, 1.0, angle * 2.0);
        draw_text_ex(
            "abcd",
            screen_width() / 4.0 * 3.0 - center.x,
            screen_height() / 3.0 * 2.0 - center.y,
            TextParams {
                font_size: 70,
                rotation: angle * 2.0,
                ..Default::default()
            },
        );

        angle -= 0.030;

        next_frame().await
    }
}
