use std::{cell::Cell, marker::PhantomData};

use any_stack::AnySplit;

pub extern crate macros;

pub mod any_stack;

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

trait Parser<'a, T: 'a, E: From<ParserError> + 'a> {
    fn parse(&self, ctx: &'a ParserContext<'a>) -> Result<T, E>;
    fn map<V: 'a>(self, f: impl Fn(T) -> V + 'a) -> impl Parser<'a, V, E> + 'a
    where
        Self: Sized + 'a,
    {
        parser(move |ctx| self.parse(ctx).map(&f))
    }
    fn optional(self) -> impl Parser<'a, Option<T>, E>
    where
        Self: Sized + 'a,
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
