use std::{cell::Cell, marker::PhantomData};

use any_stack::AnySplit;

pub extern crate macros;

pub mod any_stack;
pub mod delimited_list;

pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedToken(&'static str),
}

pub struct ParserContext<'a> {
    cur: Cell<usize>,
    input: &'a str,
    split: Cell<&'a AnySplit<'a>>,
}

struct ParserImpl<'a, F, T, E>(F, PhantomData<&'a (T, E)>)
where
    F: Fn(&'a ParserContext<'a>) -> Result<T, E> + 'a,
    T: 'a,
    E: From<ParserError> + 'a;

fn parser<'a, T, E>(
    f: impl Fn(&'a ParserContext<'a>) -> Result<T, E> + 'a,
) -> impl Parser<'a, T, E> + 'a
where
    T: 'a,
    E: From<ParserError> + 'a,
{
    ParserImpl(f, PhantomData)
}

trait Parser<'p, T: 'p, E: From<ParserError> + 'p> {
    fn parse(&self, ctx: &'p ParserContext<'p>) -> Result<T, E>;
    fn map<V: 'p>(self, f: impl Fn(T) -> V + 'p) -> impl Parser<'p, V, E> + 'p
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).map(&f))
    }
    fn optional(self) -> impl Parser<'p, Option<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| Ok(self.parse(ctx).ok()))
    }
}

impl<'a, F, T, E> Parser<'a, T, E> for ParserImpl<'a, F, T, E>
where
    F: Fn(&'a ParserContext<'a>) -> Result<T, E>,
    T: 'a,
    E: From<ParserError> + 'a,
{
    fn parse(&self, ctx: &'a ParserContext<'a>) -> Result<T, E> {
        (self.0)(ctx)
    }
}
