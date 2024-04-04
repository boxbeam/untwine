use crate::{parser, Parser, ParserError};

pub fn literal<'p, C, E>(s: &'static str) -> impl Parser<'p, C, (), E>
where
    C: 'p,
    E: From<ParserError> + 'p,
{
    parser(move |ctx| {
        let matched = s
            .chars()
            .zip(ctx.slice().chars())
            .take_while(|(a, b)| a == b)
            .count();
        ctx.advance(matched);
        if matched == s.len() {
            return ctx.result(Some(()), None);
        }
        let err = ParserError::ExpectedLiteral(s).into();
        let res = ctx.result(None, Some(err));
        ctx.reset(ctx.cursor() - matched);
        res
    })
}

pub fn char_filter<'p, C, E>(
    f: impl Fn(&char) -> bool + 'static,
    token_name: &'static str,
) -> impl Parser<'p, C, char, E>
where
    E: From<ParserError> + 'p,
    C: 'p,
{
    parser(move |ctx| {
        let next = ctx.slice().chars().next();
        if let Some(next) = next {
            if f(&next) {
                ctx.advance(next.len_utf8());
                return ctx.result(Some(next), None);
            }
        };
        let error = ParserError::ExpectedToken(token_name).into();
        ctx.result(None, Some(error))
    })
}
