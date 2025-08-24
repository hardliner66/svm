use thiserror::Error;

#[derive(Error, Debug)]
pub enum ImageReadError {
    #[error("IO Error")]
    IoError(#[from] std::io::Error),
    #[error("Decode Error: {0:?}")]
    DecodeError(bincode::error::DecodeError),
    #[error("unknown data store error")]
    Unknown,
}

#[derive(Error, Debug)]
pub enum ImageWriteError {
    #[error("IO Error")]
    IoError(#[from] std::io::Error),
    #[error("Encode Error: {0:?}")]
    EncodeError(bincode::error::EncodeError),
    #[error("unknown data store error")]
    Unknown,
}
