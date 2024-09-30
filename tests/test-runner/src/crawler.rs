use std::{
    error::Error,
    ffi::OsStr,
    fs::FileType,
    path::{Path, PathBuf},
};

use yansi::Paint;

///Crawls the `ui/` directory and lists all vola files

pub fn crawl_ui() -> Result<Vec<PathBuf>, Box<dyn Error>> {
    if !Path::new("tests/ui").exists() {
        return Err("Tests must be run from workspace directory!".into());
    }

    let mut paths = Vec::new();
    crawl_directory(&mut paths, "tests/ui")?;
    Ok(paths)
}
fn crawl_directory(
    buffer: &mut Vec<PathBuf>,
    directory: impl AsRef<Path>,
) -> Result<(), Box<dyn Error>> {
    let mut entries = std::fs::read_dir(directory.as_ref()).unwrap();
    while let Some(next) = entries.next() {
        let entry = if let Ok(e) = next {
            e
        } else {
            //pass over errors silently
            continue;
        };

        match entry.file_type() {
            Ok(ft) => {
                if ft.is_dir() {
                    crawl_directory(buffer, &entry.path())?;
                } else {
                    if ft.is_file() {
                        let path = entry.path();
                        if path.extension() == Some(&OsStr::new("vola")) {
                            buffer.push(path);
                        } else {
                            println!("Unexpected file type {}", format!("{:?}", path).red());
                        }
                    } else {
                        println!(
                            "Encountered unsupported filetype: {}",
                            format!("{:?}", ft).red()
                        );
                    }
                }
            }
            Err(e) => return Err(format!("{:?} had no filetype: {e}!", entry.path()).into()),
        }
    }

    Ok(())
}
