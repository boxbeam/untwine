use std::{cell::Cell, marker::PhantomData};

use any_stack::AnySplit;

pub extern crate macros;

pub mod any_stack;

pub type Result<T> = std::result::Result<T, ParserError>;

pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedToken(&'static str),
}

pub struct ParserContext<'a> {
    cur: Cell<usize>,
    input: &'a str,
}

pub trait Parser<T> {
    fn parse(&self, ctx: &ParserContext) -> Result<T>;
    fn map<V>(self, func: impl Fn(T) -> V) -> impl Parser<V>
    where
        Self: Sized,
    {
        MapParser {
            parser: self,
            func,
            phantom: PhantomData,
        }
    }
    fn optional(self) -> impl Parser<Option<T>>
    where
        Self: Sized,
    {
        OptionalParser {
            parser: self,
            phantom: PhantomData,
        }
    }
    fn repeating(self) -> impl Parser<Vec<T>>
    where
        Self: Sized,
    {
        RepeatingParser {
            parser: self,
            phantom: PhantomData,
        }
    }
    fn optional_repeating(self) -> impl Parser<Vec<T>>
    where
        Self: Sized,
    {
        OptionalRepeatingParser {
            parser: self,
            phantom: PhantomData,
        }
    }
}

struct MapParser<A, B, F, P>
where
    F: Fn(A) -> B,
    P: Parser<A>,
{
    parser: P,
    func: F,
    phantom: PhantomData<(A, B)>,
}

impl<A, B, F, P> Parser<B> for MapParser<A, B, F, P>
where
    F: Fn(A) -> B,
    P: Parser<A>,
{
    fn parse(&self, ctx: &ParserContext) -> Result<B> {
        self.parser.parse(ctx).map(&self.func)
    }
}

struct OptionalParser<T, P>
where
    P: Parser<T>,
{
    parser: P,
    phantom: PhantomData<T>,
}

impl<T, P> Parser<Option<T>> for OptionalParser<T, P>
where
    P: Parser<T>,
{
    fn parse(&self, ctx: &ParserContext) -> Result<Option<T>> {
        let start = ctx.cur.get();
        let res = self.parser.parse(ctx);
        if res.is_err() {
            ctx.cur.set(start);
        }
        Ok(res.ok())
    }
}

struct RepeatingParser<T, P>
where
    P: Parser<T>,
{
    parser: P,
    phantom: PhantomData<T>,
}

impl<T, P> Parser<Vec<T>> for RepeatingParser<T, P>
where
    P: Parser<T>,
{
    fn parse(&self, ctx: &ParserContext) -> Result<Vec<T>> {
        let mut vec = vec![];
        vec.push(self.parser.parse(ctx)?);
        while let Ok(elem) = self.parser.parse(ctx) {
            vec.push(elem);
        }
        Ok(vec)
    }
}

struct OptionalRepeatingParser<T, P>
where
    P: Parser<T>,
{
    parser: P,
    phantom: PhantomData<T>,
}

impl<T, P> Parser<Vec<T>> for OptionalRepeatingParser<T, P>
where
    P: Parser<T>,
{
    fn parse(&self, ctx: &ParserContext) -> Result<Vec<T>> {
        let mut vec = vec![];
        while let Ok(elem) = self.parser.parse(ctx) {
            vec.push(elem);
        }
        Ok(vec)
    }
}
