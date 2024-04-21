#[path = "../../../examples/rvsdg-simple/src/nodes.rs"]
mod nodes;
pub use nodes::{LNode, MyNodes};

#[path = "../../../examples/rvsdg-simple/src/ex_3a.rs"]
mod ex_3a;

///Simple example that shows, how to turn a Rvsdg into a viewer state,
///write that to disk and start the viewer for that state.
fn main() {
    let graph = ex_3a::emit();
    let mut viewer_state = rvsdg_viewer::ViewerState::new();

    viewer_state
        .new_state_builder("My Graph State :3", &graph)
        .build();

    //now write to disk and load the viewer

    viewer_state.write_to_file(&"myViewerState.bin");

    //now call the viewer.
    //NOTE: you might want to configure this to a prebuild .exe or something in a real world scenarion.
    //      OR
    //      just write your viewer state and load the viewer ui afterwards _by hand_.
    std::process::Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("rvsdg-viewer-ui")
        .arg("--")
        .arg("myViewerState.bin")
        .output()
        .expect("Failed to run viewer!");

    println!("bye bye!");
}
