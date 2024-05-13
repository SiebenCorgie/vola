#[path = "../../../examples/rvsdg-simple/src/nodes.rs"]
mod nodes;
pub use nodes::{LNode, MyNodes};
use rvsdg::attrib::{AttribStore, FlagStore};
use rvsdg_viewer::layout::LayoutConfig;

#[path = "../../../examples/rvsdg-simple/src/ex_3a.rs"]
mod ex_3a;

///Simple example that shows, how to turn a Rvsdg into a viewer state,
///write that to disk and start the viewer for that state.
fn main() {
    let graph = ex_3a::emit();
    let mut tagging_data = AttribStore::new();
    let mut flags = FlagStore::new();

    let toplevel_region = graph.toplevel_region();
    tagging_data.push_attrib(
        &toplevel_region.into(),
        "This is the Toplevel region!".to_string(),
    );
    tagging_data.push_attrib(
        &toplevel_region.into(),
        "And this is additional data.".to_string(),
    );

    flags.set(toplevel_region.into(), format!("True"));

    let mut viewer_state = rvsdg_viewer::ViewerState::new();

    viewer_state
        .new_state_builder("My Graph State :3", &graph, &LayoutConfig::default())
        .with_store("MyStringStore", &tagging_data)
        .with_flags("SomeFlags", &flags)
        .build();

    //now write to disk and load the viewer

    viewer_state.write_to_file(&"myViewerState.bin");

    //now call the viewer.
    //NOTE: you might want to configure this to a prebuild .exe or something in a real world scenarion.
    //      OR
    //      just write your viewer state and load the viewer ui afterwards _by hand_.
    let spawned = std::process::Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("rvsdg-viewer-ui")
        .arg("--")
        .arg("myViewerState.bin")
        .spawn()
        .expect("Failed to run viewer!");

    if let Err(_e) = spawned.wait_with_output() {
        println!("Viewer failed")
    }
}
