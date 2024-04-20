use crate::{literal, parser, Parser, ParserError, ParserResult, Recoverable};
use std::fmt::Debug;

/// Static data provided to parser attributes.
pub struct PatternMeta {
    /// The name of the parser rule which the pattern is part of.
    pub parser_name: &'static str,
    /// The pattern as a string.
    pub pattern_string: &'static str,
}

/// Debug prints the output of the parser and returns it unchanged.
pub fn dbg<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    meta: PatternMeta,
) -> impl Parser<'p, C, T, E>
where
    E: Debug + 'p,
    C: 'p,
    T: Debug + 'p,
{
    let name = meta.parser_name;
    let pattern = meta.pattern_string;
    crate::parser(move |ctx| {
        let res = parser.parse(ctx);
        println!(
            "[{name}:{line}:{col}] {pattern} = {res:#?}",
            line = ctx.line(),
            col = ctx.col(),
        );
        res
    })
}

/// Ignores the error output of the pattern.
pub fn ignore_err<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
) -> impl Parser<'p, C, T, E>
where
    E: Debug + 'p,
    C: 'p,
    T: 'p,
{
    parser.ignore_err()
}

pub fn recover_to<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    meta: PatternMeta,
    anchor: &'static str,
) -> impl Parser<'p, C, T, E>
where
    E: Debug + From<ParserError> + 'p,
    C: 'p,
    T: Recoverable + 'p,
{
    parser.recover_to::<_, false>(literal(anchor, meta.parser_name), 30)
}

pub fn recover_to_any<'p, C, T, E, const N: usize>(
    input: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
    anchor: [&'static str; N],
) -> impl Parser<'p, C, T, E>
where
    E: Debug + From<ParserError> + 'p,
    C: 'p,
    T: Recoverable + 'p,
{
    input.recover_to::<_, false>(
        parser(move |ctx| {
            println!("HI");
            ParserResult::new(
                anchor
                    .iter()
                    .any(|s| ctx.slice().starts_with(s))
                    .then_some(()),
                None,
                ctx.cursor()..ctx.cursor(),
            )
        }),
        30,
    )
}
