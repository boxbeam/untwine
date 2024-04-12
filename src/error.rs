use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum ParserError {
    /// Thrown when a literal substring is expected.
    ExpectedLiteral(&'static str),
    /// Thrown when an unspecified parsing operation associated with a named parser fails.
    ExpectedToken(&'static str),
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedLiteral(lit) => write!(
                f,
                "Expected literal '{}'",
                lit.replace("\n", "<newline>").replace("\t", "<tab>")
            ),
            ParserError::ExpectedToken(token) => write!(f, "Expected {}", token.replace("_", " ")),
        }
    }
}
