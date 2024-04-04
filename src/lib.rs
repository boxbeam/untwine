use std::fmt::Display;

pub mod any_stack;
pub mod error;
pub use error::ParserError;
pub use macros::parser;
pub mod attr;
pub mod context;
pub mod parser;
pub mod parsers;
pub mod result;

pub mod prelude {
    use super::*;
    pub use attr::*;
    pub use context::ParserContext;
    pub use error::ParserError;
    pub use macros::parser;
    pub use parser::*;
    pub use parsers::*;
    pub use result::ParserResult;
}

use prelude::*;

pub fn parse<C, T, E>(
    parser: impl for<'a> Fn(&'a ParserContext<'a, C>) -> ParserResult<T, E>,
    input: &str,
) -> Result<T, E>
where
    C: Default,
    E: Default,
{
    let ctx = ParserContext::new(input, Default::default());
    parser(&ctx).result(&ctx)
}

pub fn parse_pretty<C, T, E>(
    parser: impl for<'a> Fn(&'a ParserContext<'a, C>) -> ParserResult<T, E>,
    input: &str,
) -> Result<T, String>
where
    C: Default,
    E: Display,
{
    let ctx = ParserContext::new(input, Default::default());
    parser(&ctx).pretty(&ctx).result(&ctx)
}
