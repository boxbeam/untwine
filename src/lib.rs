use std::{cell::Cell, marker::PhantomData};

pub extern crate macros;

pub mod any_stack;
pub mod delimited_list;

pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedChar(&'static str),
}

pub struct ParserContext<'a> {
    cur: Cell<usize>,
    input: &'a str,
}

impl<'a> ParserContext<'a> {
    pub fn slice(&self) -> &str {
        &self.input[self.cur.get()..]
    }

    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().max(self.cur.get() + bytes));
    }
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

mod private {
    pub trait SealedParser<T, E> {}
}

impl<'p, T, E, P> private::SealedParser<T, E> for P
where
    E: From<ParserError> + 'p,
    P: Parser<'p, T, E>,
    T: 'p,
{
}

pub trait Parser<'p, T: 'p, E: From<ParserError> + 'p>: private::SealedParser<T, E> {
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

    fn or(self, other: impl Parser<'p, T, E> + 'p + Sized) -> impl Parser<'p, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).or_else(|_| other.parse(ctx)))
    }

    fn repeating(self) -> impl Parser<'p, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            elems.push(self.parse(ctx)?);
            while let Ok(elem) = self.parse(ctx) {
                elems.push(elem);
            }
            Ok(elems)
        })
    }

    fn optional_repeating(self) -> impl Parser<'p, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            while let Ok(elem) = self.parse(ctx) {
                elems.push(elem);
            }
            Ok(elems)
        })
    }

    fn delimited<D>(self, delim: impl Parser<'p, D, E> + 'p) -> impl Parser<'p, Vec<T>, E>
    where
        Self: Sized + 'p,
        D: 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            elems.push(self.parse(ctx)?);
            while let Ok(_) = delim.parse(ctx) {
                elems.push(self.parse(ctx)?);
            }
            Ok(elems)
        })
    }

    fn optional_delimited<D>(self, delim: impl Parser<'p, D, E> + 'p) -> impl Parser<'p, Vec<T>, E>
    where
        Self: Sized + 'p,
        D: 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];

            let Ok(elem) = self.parse(ctx) else {
                return Ok(elems);
            };
            elems.push(elem);

            while let Ok(_) = delim.parse(ctx) {
                elems.push(self.parse(ctx)?);
            }
            Ok(elems)
        })
    }

    fn ignore(self) -> impl Parser<'p, (), E>
    where
        Self: Sized + 'p,
    {
        self.map(|_| ())
    }

    fn span(self) -> impl Parser<'p, &'p str, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            self.parse(ctx)?;
            Ok(&ctx.input[start..ctx.cur.get()])
        })
    }
}

pub fn literal<E>(s: &'static str) -> impl Parser<(), E>
where
    E: From<ParserError> + 'static,
{
    parser(move |ctx| {
        if ctx.slice().starts_with(s) {
            ctx.advance(s.len());
            Ok(())
        } else {
            Err(ParserError::ExpectedLiteral(s).into())
        }
    })
}

pub fn char_filter<E>(
    f: impl Fn(&char) -> bool + 'static,
    token_name: &'static str,
) -> impl Parser<'static, char, E>
where
    E: From<ParserError> + 'static,
{
    parser(move |ctx| {
        let next = ctx.slice().chars().next();
        if let Some(next) = next {
            if f(&next) {
                ctx.advance(next.len_utf8());
                return Ok(next);
            }
        };
        Err(ParserError::ExpectedChar(token_name).into())
    })
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
