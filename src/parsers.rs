use crate::{parser, Parser, ParserError};

/// Create a parser which parses a string literal. Generates errors for partial matches.
#[inline(always)]
pub fn literal<'p, C, E>(
    literal: &'static str,
    parser_name: &'static str,
) -> impl Parser<'p, C, (), E>
where
    C: 'p,
    E: From<ParserError> + 'p + std::fmt::Debug,
{
    parser(move |ctx| {
        let matched = literal
            .chars()
            .zip(ctx.slice().chars())
            .take_while(|(a, b)| a == b)
            .count();
        ctx.advance(matched);
        if matched == literal.len() {
            return Some(());
        }
        let err = ParserError::ExpectedLiteral(literal, parser_name).into();
        ctx.err(err);
        ctx.reset(ctx.cursor() - matched);
        None
    })
}

/// Create a parser which parses a single character matching a predicate.
#[inline(always)]
pub fn char_filter<'p, C, E>(
    f: impl Fn(&char) -> bool + 'static,
    token_name: &'static str,
) -> impl Parser<'p, C, char, E>
where
    E: From<ParserError> + 'p + std::fmt::Debug,
    C: 'p,
{
    parser(move |ctx| {
        let next = ctx.slice().chars().next();
        if let Some(next) = next {
            if f(&next) {
                ctx.advance(next.len_utf8());
                return Some(next);
            }
        };
        let error = ParserError::ExpectedToken(token_name).into();
        ctx.err(error);
        None
    })
}
