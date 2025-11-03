pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    msg: String,
}

impl Error {
    pub fn new(msg: String) -> Self {
        Error { msg }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
