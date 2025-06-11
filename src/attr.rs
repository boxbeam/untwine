use crate::{literal, parser, Parser, ParserError, Recoverable, Span};
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

/// When the wrapped pattern fails to parse, try to jump ahead to a specific literal,
/// but do not consume it. This allows you to specify an "anchor" from which parsing
/// can continue.
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
    parser.recover_to::<_, false>(literal(anchor, meta.parser_name), 150)
}

/// When the wrapped pattern fails to parse, try to jump ahead to one of several
/// specific literals, but do not consume it. This allows you to specify "anchors"
/// from which parsing can continue.
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
            anchor
                .iter()
                .any(|s| ctx.slice().starts_with(s))
                .then_some(())
        }),
        150,
    )
}

/// Wrap the output data in a Span, which contains a range representing the portion of
/// the input corresponding to the parsed data.
pub fn span<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
) -> impl Parser<'p, C, Span<T>, E>
where
    C: 'p,
    E: 'p,
    T: 'p,
{
    crate::parser(move |ctx| {
        let start = ctx.cursor();
        let data = parser.parse(ctx)?;
        let span = start..ctx.cursor();
        Some(Span { span, data })
    })
}
