use thiserror::Error;

#[derive(Error, Debug)]
pub enum HirErr {
    #[error("Any error occured")]
    Any,
}
