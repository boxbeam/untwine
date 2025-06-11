use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    /// Thrown when a literal substring is expected.
    ExpectedLiteral(&'static str, &'static str),
    /// Thrown when an unspecified parsing operation associated with a named parser fails.
    ExpectedToken(&'static str),
    /// Thrown when a delimiter, such as a paren, is opened but never closed
    UnmatchedDelimiter(&'static str),
    /// Thrown when parsing succeeds, but there are more tokens left in the input which were not consumed.
    UnexpectedToken,
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedLiteral(lit, parser_name) => write!(
                f,
                "Expected '{lit}' while parsing {parser_name}",
                lit = lit.replace('\n', "<newline>").replace('\t', "<tab>"),
                parser_name = parser_name.replace('_', " ")
            ),
            ParserError::ExpectedToken(token) => write!(f, "Expected {}", token.replace('_', " ")),
            ParserError::UnexpectedToken => write!(f, "Unexpected token"),
            ParserError::UnmatchedDelimiter(delim) => write!(f, "Unmatched delimiter '{delim}'"),
        }
    }
}
