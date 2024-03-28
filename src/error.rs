#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Expected literal '{0}'")]
    ExpectedLiteral(&'static str),
    #[error("Expected {0}")]
    ExpectedToken(&'static str),
}

pub trait AsParserError: From<ParserError> {
    fn as_parser_err(&self) -> Option<&ParserError>;
}

impl AsParserError for ParserError {
    fn as_parser_err(&self) -> Option<&ParserError> {
        Some(self)
    }
}
