use std::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

pub mod any_stack;
pub mod delimited_list;
pub use macros::parser;

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Expected literal '{0}'")]
    ExpectedLiteral(&'static str),
    #[error("Expected {0}")]
    ExpectedToken(&'static str),
}

pub struct ParserContext<'p, C = ()> {
    cur: Cell<usize>,
    input: &'p str,
    data: RefCell<C>,
}

impl<'p, C> ParserContext<'p, C> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: Default::default(),
            input,
            data: RefCell::new(data),
        }
    }

    pub fn slice(&self) -> &str {
        &self.input[self.cur.get()..]
    }

    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().min(self.cur.get() + bytes));
    }

    pub fn data(&self) -> impl Deref<Target = C> + '_ {
        self.data.borrow()
    }

    pub fn data_mut(&self) -> impl DerefMut<Target = C> + '_ {
        self.data.borrow_mut()
    }
}

struct ParserImpl<'p, F, C, T, E>(F, PhantomData<&'p (C, T, E)>)
where
    F: Fn(&'p ParserContext<'p, C>) -> Result<T, E> + 'p,
    T: 'p,
    E: From<ParserError> + 'p;

pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C>) -> Result<T, E> + 'p,
) -> impl Parser<'p, C, T, E> + 'p
where
    C: 'p,
    T: 'p,
    E: From<ParserError> + 'p,
{
    ParserImpl(f, PhantomData)
}

mod private {
    pub trait SealedParser<C, T, E> {}
}

impl<'p, C, T, E, P> private::SealedParser<C, T, E> for P
where
    C: 'p,
    E: From<ParserError> + 'p,
    P: Parser<'p, C, T, E>,
    T: 'p,
{
}

pub trait Parser<'p, C: 'p, T: 'p, E: From<ParserError> + 'p>:
    private::SealedParser<C, T, E>
{
    fn parse(&self, ctx: &'p ParserContext<'p, C>) -> Result<T, E>;

    fn map<V: 'p>(self, f: impl Fn(T) -> V + 'p) -> impl Parser<'p, C, V, E> + 'p
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).map(&f))
    }

    fn optional(self) -> impl Parser<'p, C, Option<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| Ok(self.parse(ctx).ok()))
    }

    fn or(self, other: impl Parser<'p, C, T, E> + 'p + Sized) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).or_else(|_| other.parse(ctx)))
    }

    fn repeating(self) -> impl Parser<'p, C, Vec<T>, E>
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

    fn optional_repeating(self) -> impl Parser<'p, C, Vec<T>, E>
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

    fn delimited<D>(self, delim: impl Parser<'p, C, D, E> + 'p) -> impl Parser<'p, C, Vec<T>, E>
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

    fn optional_delimited<D>(
        self,
        delim: impl Parser<'p, C, D, E> + 'p,
    ) -> impl Parser<'p, C, Vec<T>, E>
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

    fn ignore(self) -> impl Parser<'p, C, (), E>
    where
        Self: Sized + 'p,
    {
        self.map(|_| ())
    }

    fn span(self) -> impl Parser<'p, C, &'p str, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            self.parse(ctx)?;
            Ok(&ctx.input[start..ctx.cur.get()])
        })
    }

    fn unilateral(self) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            let res = self.parse(ctx);
            if res.is_err() {
                ctx.cur.set(start);
            }
            res
        })
    }
}

pub fn literal<'p, C, E>(s: &'static str) -> impl Parser<'p, C, (), E>
where
    C: 'p,
    E: From<ParserError> + 'p,
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
                return Ok(next);
            }
        };
        Err(ParserError::ExpectedToken(token_name).into())
    })
}

impl<'p, F, C, T, E> Parser<'p, C, T, E> for ParserImpl<'p, F, C, T, E>
where
    F: Fn(&'p ParserContext<'p, C>) -> Result<T, E>,
    T: 'p,
    E: From<ParserError> + 'p,
{
    fn parse(&self, ctx: &'p ParserContext<'p, C>) -> Result<T, E> {
        (self.0)(ctx)
    }
}
