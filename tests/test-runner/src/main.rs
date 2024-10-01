use std::{
    error::Error,
    fmt::Display,
    path::PathBuf,
    thread::JoinHandle,
    time::{Duration, Instant},
};

use crawler::crawl_ui;
use run::{run_file, TestResult};
use yansi::Paint;

mod config;
mod crawler;
mod run;
mod wasm_executor;

use clap::{Parser, ValueEnum};

#[derive(Parser)]
#[command(name = "test-runner")]
#[command(version, about, long_about = "Vola's test runner!")]
struct Args {
    ///Specifies a file that should be tested. Can be used multiple times to test multiple files.
    ///If not used, all files are tested.
    #[arg(long, short = 'f')]
    files: Option<Vec<PathBuf>>,
}

const TIMEOUT: Duration = Duration::from_secs(5);

enum LaunchState {
    Launched {
        path: PathBuf,
        start: Instant,
        handle: Option<JoinHandle<TestResult>>,
    },
    Ended(TestResult),
    TestThreadCrashed(PathBuf),
    TimedOut(PathBuf),
}

impl LaunchState {
    fn is_success(&self) -> bool {
        if let LaunchState::Ended(e) = self {
            e.only_successes()
        } else {
            false
        }
    }
}

impl Display for LaunchState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ended(result) => {
                if result.only_successes() {
                    writeln!(f, "{}", "All Passed".bold().green())?
                } else {
                    if result.partial_success() {
                        writeln!(f, "{}", "Partially passed".bold().yellow())?
                    } else {
                        writeln!(f, "{}", "None passed".bold().red())?
                    }
                }
                for r in &result.runs {
                    writeln!(f, "    {}", r)?;
                }

                Ok(())
            }
            Self::TimedOut(file) => write!(f, "{}: {:?}", "Timeout".bold().yellow().bold(), file),
            Self::TestThreadCrashed(file) => write!(f, "{}: {:?}", "Crash".bold().red(), file),
            Self::Launched { path, .. } => {
                write!(f, "{}: {:?}", "Still running".bold().red(), path)
            }
        }
    }
}

///The test runner spawn a new thread for eac test, executes it, based on the test's
///parsed config, and sends the result back to the runner. Once all test finished, the runner collects
///the results and pretty prints them.
fn main() -> Result<(), Box<dyn Error>> {
    pretty_env_logger::init();
    let args = Args::parse();

    let test_files = if let Some(files) = args.files {
        files
    } else {
        crawl_ui()?
    };

    let mut launch_states = test_files
        .into_iter()
        .try_fold(Vec::new(), |mut coll, file| {
            let hdl = run_file(file.clone())?;

            coll.push(LaunchState::Launched {
                path: file,
                start: Instant::now(),
                handle: Some(hdl),
            });
            Ok::<Vec<LaunchState>, Box<dyn Error>>(coll)
        })?;

    //Now poll all handles until either the time out was reached, or the result is send back

    'wait: loop {
        let mut contained_running = false;

        for state in &mut launch_states {
            match state {
                LaunchState::Launched {
                    path,
                    start,
                    handle,
                } => {
                    //try to poll
                    if handle.as_ref().unwrap().is_finished() {
                        match handle.take().unwrap().join() {
                            Ok(test_result) => *state = LaunchState::Ended(test_result),
                            Err(_e) => *state = LaunchState::TestThreadCrashed(path.clone()),
                        }
                    } else {
                        //check if we exceed the timeout, in that case kill the thread and end
                        if start.elapsed() > TIMEOUT {
                            *state = LaunchState::TimedOut(path.clone());
                        } else {
                            //Not in timeout, and not ended, keep running
                            contained_running = true;
                        }
                    }
                }
                //All others are not important
                _ => {}
            }
        }

        if !contained_running {
            break 'wait;
        }
        //Sleep a little, then try again
        std::thread::sleep(Duration::from_millis(100));
    }

    //After running all tests, iterate all result, first print all successes,

    let mut success_count = 0;
    let mut crash_count = 0;
    let mut timeout = 0;
    let mut partial_count = 0;
    let mut failed_count = 0;

    for result in &launch_states {
        if result.is_success() {
            success_count += 1;
            println!("{}", result);
        }
    }

    //now all error like states
    for result in &launch_states {
        if !result.is_success() {
            match result {
                LaunchState::TestThreadCrashed(_) => crash_count += 1,
                LaunchState::TimedOut(_) => timeout += 1,
                LaunchState::Ended(e) => {
                    if e.partial_success() {
                        partial_count += 1;
                    }
                    if e.only_successes() {
                        partial_count += 1;
                    }

                    if !e.partial_success() && !e.only_successes() {
                        failed_count += 1
                    }
                }
                _ => {}
            }

            println!("{}", result)
        }
    }

    println!("{}", Box::new("TLDR:"));
    if success_count > 0 {
        println!("    {}={success_count}", "Success".green().bold());
    }
    if timeout > 0 {
        println!("    {}={timeout}", "Timeout".yellow().bold());
    }
    if partial_count > 0 {
        println!("    {}={partial_count}", "Partial Success".yellow().bold());
    }
    if crash_count > 0 {
        println!("    {}={crash_count}", "Crashed".red().bold());
    }
    if failed_count > 0 {
        println!("    {}={failed_count}", "Failed".red().bold());
    }

    Ok(())
}
