#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Expected literal '{0}'")]
    /// Thrown when a literal substring is expected.
    ExpectedLiteral(&'static str),
    #[error("Expected {0}")]
    /// Thrown when an unspecified parsing operation associated with a named parser fails.
    ExpectedToken(&'static str),
}
