//! A simple softbuffer based CPU raytracer that utilizes Vola's bridge helper to update the ray-traced scene
//! whenever `scene.vola` changes.

use std::sync::mpsc::Receiver;

use hotwatch::Hotwatch;

enum State {
    Idle,
    Running,
}

struct App {
    state: State,

    code_update: Receiver<vola_bridge::Module>,
    hotwatch: Hotwatch,
}

fn main() {
    let (s, r) = std::sync::mpsc::channel();
    let mut hotwatch = Hotwatch::new_with_custom_delay(std::time::Duration::from_millis(500))
        .expect("Could not init hotwatch");
    hotwatch
        .watch("scene.vola", move |e| {
            match e.kind {
                hotwatch::EventKind::Modify(hotwatch::notify::event::ModifyKind::Data(_)) => {
                    //On change, start the compiler, once finished, init the new
                    // bridge and send it back.
                    //
                    // Usually its enough to just call vola_bridge::load() if the loaded file
                    // is relative to the library its using. In our case though we'll attach 
                    // 
                    
                }
                _ => {}
            }
        })
        .expect("Could not watch scene file...");

    let app = App {
        state: State::Idle,
        code_update: r,
        hotwatch,
    };

    loop {
        if let Ok(new) = app.code_update.try_recv() {
            println!("got new code!");
        }

        std::thread::yield_now();
    }
}
