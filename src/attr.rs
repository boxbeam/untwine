use crate::{error::AsParserError, Parser, ParserMeta};
use std::fmt::Debug;

pub fn dbg<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    meta: ParserMeta,
) -> impl Parser<'p, C, T, E>
where
    E: Debug + AsParserError + 'p,
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
            col = ctx.col()
        );
        res
    })
}

pub fn ignore_err<'p, C, T, E>(
    parser: impl Parser<'p, C, T, E> + 'p,
    _meta: ParserMeta,
) -> impl Parser<'p, C, T, E>
where
    E: Debug + AsParserError + 'p,
    C: 'p,
    T: 'p,
{
    parser.ignore_err()
}
