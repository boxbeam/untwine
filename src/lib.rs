//! A declarative parsing library for extremely simple, concise parsing.
//! See [parser!] for usage instructions.
//!
//! Example JSON parser using Untwine:
//! ```ignore
//! untwine::parser! {
//!     [error = ParseJSONError]
//!     sep = #{char::is_ascii_whitespace}*;
//!     comma = sep "," sep;
//!     int: num=<"-"? '0'-'9'+> -> JSONValue { JSONValue::Int(num.parse()?) }
//!     float: num=<"-"? '0'-'9'+ "." '0'-'9'+> -> JSONValue { JSONValue::Float(num.parse()?) }
//!     str_char = ("\\" . | [^"\""]) -> char;
//!     str: "\"" chars=str_char* "\"" -> JSONValue { JSONValue::String(chars.into_iter().collect()) }
//!     null: "null" -> JSONValue { JSONValue::Null }
//!     bool: bool=<"true" | "false"> -> JSONValue { JSONValue::Bool(bool == "true") }
//!     list: "[" sep values=json$comma* sep "]" -> JSONValue { JSONValue::List(values) }
//!     map_entry: key=str sep ":" sep value=json -> (String, JSONValue) { (key.string().unwrap(), value) }
//!     map: "{" sep values=map_entry$comma* sep "}" -> JSONValue { JSONValue::Map(values.into_iter().collect()) }
//!     pub json = (bool | null | str | float | int | list | map) -> JSONValue;
//! }
//! ```

use std::{
    fmt::{Debug, Display},
    io::Write,
    ops::{Deref, DerefMut, Range},
};

pub mod error;
pub use error::ParserError;
pub use macros::parser;
pub mod attr;
pub mod context;
pub mod parser;
pub mod parsers;
pub mod pretty;
pub mod recoverable;

pub mod prelude {
    use super::*;
    pub use attr::*;
    pub use context::ParserContext;
    pub use error::ParserError;
    pub use macros::{parser, Recoverable};
    pub use parser::*;
    pub use parsers::*;
}

use prelude::*;
use pretty::PrettyOptions;
pub use recoverable::Recoverable;

/// Represents data corresponding to a specific portion of a parsing input.
pub struct Span<T = ()> {
    pub span: Range<usize>,
    pub data: T,
}

impl<T> Deref for Span<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Span<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Span<T> {
    /// Convert the inner data using a mapping function
    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> Span<V> {
        Span {
            span: self.span,
            data: f(self.data),
        }
    }
}

/// Parse a value with a parser function created by the [parser!] block.
pub fn parse<C, T, E>(
    parser: impl for<'a> Fn(&'a ParserContext<'a, C, E>) -> Option<T>,
    input: &str,
) -> Result<T, Vec<(Range<usize>, E)>>
where
    C: Default,
    E: From<ParserError>,
{
    let mut ctx = ParserContext::new(input, Default::default());
    let res = parser(&ctx);
    ctx.result(res)
}

/// Parse a value with a parser function created by the [parser!] block,
/// and convert the error to a pretty string if there is one.
pub fn parse_pretty<C, T, E>(
    parser: impl for<'a> Fn(&'a ParserContext<'a, C, E>) -> Option<T>,
    input: &str,
    colors: PrettyOptions,
) -> Result<T, String>
where
    C: Default,
    E: Display + From<ParserError>,
{
    let mut ctx = ParserContext::new(input, Default::default());
    let res = parser(&ctx);
    ctx.pretty_result(res, colors)
}

/// Launches a (very) simple REPL where you can enter individual lines and see the parser output, useful for testing.
/// Multiline inputs are not supported, so literal `\n` in the input will be replaced with a newline.
pub fn parser_repl<C, T, E>(parser: impl for<'a> Fn(&'a ParserContext<'a, C, E>) -> Option<T>)
where
    T: Debug,
    C: Default,
    E: Display + From<ParserError>,
{
    print!("> ");
    std::io::stdout().flush().unwrap();
    for line in std::io::stdin().lines() {
        println!();
        match parse_pretty(
            &parser,
            &line.unwrap().replace("\\n", "\n"),
            PrettyOptions::default(),
        ) {
            Ok(val) => println!("{val:#?}"),
            Err(err) => println!("{err}"),
        }
        print!("\n> ");
        std::io::stdout().flush().unwrap();
    }
}
