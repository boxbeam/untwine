use crate::{literal, parser, Parser, ParserError, Recoverable, Span};
use std::{
    fmt::Debug,
    ops::{Bound, RangeBounds},
};

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

/// Map the output type of the parser using a mapping function.
pub fn map<'p, C, T, E, V>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
    map_fn: impl Fn(T) -> V + 'p,
) -> impl Parser<'p, C, V, E>
where
    C: 'p,
    T: 'p,
    E: 'p,
    V: 'p,
{
    parser.map(map_fn)
}

/// Map the output type of the parser using a mapping function.
/// Identical to [map], but with a different name since `map` is
/// a common name for a parser to have.
pub fn convert<'p, C, T, E, V>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
    map_fn: impl Fn(T) -> V + 'p,
) -> impl Parser<'p, C, V, E>
where
    C: 'p,
    T: 'p,
    E: 'p,
    V: 'p,
{
    parser.map(map_fn)
}

pub trait IntoCountBound {
    fn into_range(self) -> impl RangeBounds<usize>;
}

impl IntoCountBound for usize {
    fn into_range(self) -> impl RangeBounds<usize> {
        self..=self
    }
}

macro_rules! impl_into_range {
    ($t:ty) => {
        impl IntoCountBound for $t {
            fn into_range(self) -> impl RangeBounds<usize> {
                self
            }
        }
    };
}

impl_into_range!(std::ops::Range<usize>);
impl_into_range!(std::ops::RangeTo<usize>);
impl_into_range!(std::ops::RangeFull);
impl_into_range!(std::ops::RangeFrom<usize>);
impl_into_range!(std::ops::RangeToInclusive<usize>);
impl_into_range!(std::ops::RangeInclusive<usize>);

fn contains(end_bound: Bound<&usize>, elem: usize) -> bool {
    match end_bound {
        Bound::Included(&end) => elem <= end,
        Bound::Excluded(&end) => elem < end,
        Bound::Unbounded => true,
    }
}

/// Attempts to match a pattern a specific number of times, and return a Vec.
/// Can take either a usize or a range.
pub fn repeat<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: PatternMeta,
    range: impl IntoCountBound + 'p,
) -> impl Parser<'p, C, Vec<T>, E>
where
    C: 'p,
    T: 'p,
    E: 'p,
{
    let range = range.into_range();
    crate::parser(move |ctx| {
        let mut elems = vec![];
        while contains(range.end_bound(), elems.len() + 1) {
            match parser.parse(ctx) {
                Some(elem) => elems.push(elem),
                None => break,
            }
        }
        if !range.contains(&elems.len()) {
            None
        } else {
            Some(elems)
        }
    })
}
