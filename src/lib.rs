use std::{
    cell::{Cell, RefCell, UnsafeCell},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

pub mod any_stack;
mod circular_queue;
pub mod error;
use error::AsParserError;
pub use error::ParserError;
pub use macros::parser;
pub mod attr;

pub struct ParserMeta {
    pub parser_name: &'static str,
    pub pattern_string: &'static str,
}

struct WriteCell<T> {
    inner: UnsafeCell<T>,
}

impl<T> WriteCell<T> {
    fn new(inner: T) -> Self {
        WriteCell {
            inner: UnsafeCell::new(inner),
        }
    }

    fn write(&self, val: T) -> T {
        unsafe { std::mem::replace(&mut *self.inner.get(), val) }
    }

    fn into_inner(self) -> T {
        self.inner.into_inner()
    }
}

pub struct ParserContext<'p, C, E> {
    cur: Cell<usize>,
    max_error_pos: Cell<usize>,
    deepest_error: WriteCell<Option<E>>,
    data: RefCell<C>,
    input: &'p str,
}

impl<'p, C, E> ParserContext<'p, C, E> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: Default::default(),
            max_error_pos: Default::default(),
            deepest_error: WriteCell::new(None),
            data: RefCell::new(data),
            input,
        }
    }

    pub fn into_err(self) -> Option<E> {
        self.deepest_error.into_inner()
    }

    pub fn slice(&self) -> &str {
        &self.input[self.cur.get()..]
    }

    pub fn line(&self) -> usize {
        self.input[..self.cur.get()].lines().count().max(1)
    }

    pub fn col(&self) -> usize {
        self.input[..self.cur.get()]
            .chars()
            .rev()
            .take_while(|c| *c != '\n')
            .count()
    }

    pub fn err<T, E2>(&self, err: E2) -> Option<T>
    where
        E: From<E2> + AsParserError,
    {
        let err: E = err.into();
        let mut max = self.max_error_pos.get();
        if err.as_parser_err().is_none() {
            max -= 1;
        }
        if self.cur.get() + 1 > max {
            self.deepest_error.write(Some(err));
            self.max_error_pos.set(self.cur.get() + 1);
        }
        None
    }

    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().min(self.cur.get() + bytes));
    }

    pub fn reset(&self, bytes: usize) {
        self.cur.set(bytes);
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
    F: Fn(&'p ParserContext<'p, C, E>) -> Option<T> + 'p,
    T: 'p,
    E: AsParserError + 'p;

pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C, E>) -> Option<T> + 'p,
) -> impl Parser<'p, C, T, E> + 'p
where
    C: 'p,
    T: 'p,
    E: AsParserError + 'p,
{
    ParserImpl(f, PhantomData)
}

mod private {
    pub trait SealedParser<C, T, E> {}
}

impl<'p, C, T, E, P> private::SealedParser<C, T, E> for P
where
    C: 'p,
    E: AsParserError + 'p,
    P: Parser<'p, C, T, E>,
    T: 'p,
{
}

pub trait Parser<'p, C: 'p, T: 'p, E: AsParserError + 'p>: private::SealedParser<C, T, E> {
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> Option<T>;

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
        parser(move |ctx| Some(self.parse(ctx)))
    }

    fn or(
        self,
        other: impl Parser<'p, C, T, E> + 'p + Sized,
        name: &'static str,
    ) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let res = self.parse(ctx).or_else(|| other.parse(ctx));
            if res.is_none() {
                ctx.err::<T, _>(ParserError::ExpectedToken(name));
            }
            res
        })
    }

    fn repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            elems.push(self.parse(ctx)?);
            while let Some(elem) = self.parse(ctx) {
                elems.push(elem);
            }
            Some(elems)
        })
    }

    fn optional_repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            while let Some(elem) = self.parse(ctx) {
                elems.push(elem);
            }
            Some(elems)
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
            while delim.parse(ctx).is_some() {
                elems.push(self.parse(ctx)?);
            }
            Some(elems)
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

            let Some(elem) = self.parse(ctx) else {
                return Some(elems);
            };
            elems.push(elem);

            while delim.parse(ctx).is_some() {
                elems.push(self.parse(ctx)?);
            }
            Some(elems)
        })
    }

    fn ignore(self) -> impl Parser<'p, C, (), E>
    where
        Self: Sized + 'p,
    {
        self.map(|_| ())
    }

    fn ignore_err(self) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let prev = ctx.deepest_error.write(None);
            let max = ctx.max_error_pos.get();
            let res = self.parse(ctx);
            ctx.deepest_error.write(prev);
            ctx.max_error_pos.set(max);
            res
        })
    }

    fn span(self) -> impl Parser<'p, C, &'p str, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            self.parse(ctx)?;
            Some(&ctx.input[start..ctx.cur.get()])
        })
    }

    fn unilateral(self) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
        E: std::fmt::Debug,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            let res = self.parse(ctx);
            if res.is_none() {
                ctx.reset(start);
            }
            res
        })
    }
}

pub fn literal<'p, C, E>(s: &'static str) -> impl Parser<'p, C, (), E>
where
    C: 'p,
    E: AsParserError + 'p,
{
    parser(move |ctx| {
        let matched = s
            .chars()
            .zip(ctx.slice().chars())
            .take_while(|(a, b)| a == b)
            .count();
        ctx.advance(matched);
        if matched == s.len() {
            return Some(());
        } else if matched > 0 {
            ctx.err::<(), _>(ParserError::ExpectedLiteral(s).into());
            ctx.reset(ctx.cur.get() - matched);
        }
        None
    })
}

pub fn char_filter<'p, C, E>(
    f: impl Fn(&char) -> bool + 'static,
    token_name: &'static str,
) -> impl Parser<'p, C, char, E>
where
    E: AsParserError + 'p,
    C: 'p,
{
    parser(move |ctx| {
        let next = ctx.slice().chars().next();
        if let Some(next) = next {
            if f(&next) {
                ctx.advance(next.len_utf8());
                return Some(next);
            }
        };
        ctx.err(ParserError::ExpectedToken(token_name).into())
    })
}

impl<'p, F, C, T, E> Parser<'p, C, T, E> for ParserImpl<'p, F, C, T, E>
where
    F: Fn(&'p ParserContext<'p, C, E>) -> Option<T>,
    T: 'p,
    E: AsParserError + 'p,
{
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> Option<T> {
        (self.0)(ctx)
    }
}
