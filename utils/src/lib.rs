#[derive(Debug)]
pub struct FrontendError {
    pub message: String,
    pub line: usize,
    pub offset: usize,
}
