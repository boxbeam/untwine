use std::{
    cell::{Cell, RefCell},
    fmt::Debug,
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

#[derive(Debug)]
pub struct ParserResult<T, E> {
    pub success: Option<T>,
    pub error: Option<InternalError<E>>,
}

impl<T, E> ParserResult<T, E> {
    fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResult<V, E> {
        ParserResult {
            success: self.success.map(f),
            error: self.error,
        }
    }

    pub fn new(success: Option<T>, error: Option<InternalError<E>>) -> Self {
        ParserResult { success, error }
    }

    pub fn success(result: T) -> Self {
        ParserResult {
            success: Some(result),
            error: None,
        }
    }
}

#[derive(Debug)]
pub struct InternalError<E> {
    pos: usize,
    err: E,
}

impl<E> InternalError<E> {
    pub fn max(self, other: Self) -> Self {
        if self.pos > other.pos {
            self
        } else {
            other
        }
    }

    pub fn max_optional(first: Option<Self>, second: Option<Self>) -> Option<Self> {
        match (first, second) {
            (None, None) => None,
            (None, Some(b)) => Some(b),
            (Some(a), None) => Some(a),
            (Some(a), Some(b)) => Some(a.max(b)),
        }
    }
}

pub struct ParserContext<'p, C> {
    cur: Cell<usize>,
    data: RefCell<C>,
    input: &'p str,
}

impl<'p, C> ParserContext<'p, C> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: Default::default(),
            data: RefCell::new(data),
            input,
        }
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

    pub fn internal_error<E>(&self, err: E) -> InternalError<E> {
        InternalError {
            pos: self.cur.get(),
            err: err.into(),
        }
    }

    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().min(self.cur.get() + bytes));
    }

    pub fn reset(&self, bytes: usize) {
        self.cur.set(bytes);
    }

    pub fn cursor(&self) -> usize {
        self.cur.get()
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
    F: Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E> + 'p,
    T: 'p,
    E: AsParserError + 'p;

pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E> + 'p,
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
    fn parse(&self, ctx: &'p ParserContext<'p, C>) -> ParserResult<T, E>;

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
        parser(move |ctx| {
            let res = self.parse(ctx);
            ParserResult::new(Some(res.success), res.error)
        })
    }

    fn or(self, other: impl Parser<'p, C, T, E> + 'p + Sized) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let res = self.parse(ctx);
            if res.success.is_some() {
                return res;
            }
            let mut other = other.parse(ctx);
            other.error = InternalError::max_optional(other.error, res.error);
            other
        })
    }

    fn repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let res = self.parse(ctx);
            let Some(elem) = res.success else {
                return ParserResult::new(None, res.error);
            };
            elems.push(elem);
            let err;

            loop {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    err = res.error;
                    break;
                };
                elems.push(elem);
            }
            ParserResult::new(Some(elems), err)
        })
    }

    fn optional_repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let mut err;
            loop {
                let res = self.parse(ctx);
                err = res.error;
                let Some(elem) = res.success else {
                    break;
                };
                elems.push(elem);
            }
            ParserResult::new(Some(elems), err)
        })
    }

    fn delimited<D>(self, delim: impl Parser<'p, C, D, E> + 'p) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
        D: 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let res = self.parse(ctx);

            let Some(elem) = res.success else {
                return ParserResult::new(Some(elems), res.error);
            };
            elems.push(elem);

            let mut err = res.error;
            while delim.parse(ctx).success.is_some() {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    return ParserResult::new(None, InternalError::max_optional(err, res.error));
                };
                err = res.error;
                elems.push(elem);
            }
            ParserResult::new(Some(elems), err)
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

            let res = self.parse(ctx);
            let Some(elem) = res.success else {
                return ParserResult::new(Some(elems), res.error);
            };
            elems.push(elem);

            let mut err = res.error;
            while delim.parse(ctx).success.is_some() {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    return ParserResult::new(None, InternalError::max_optional(err, res.error));
                };
                err = res.error;
                elems.push(elem);
            }
            ParserResult::new(Some(elems), err)
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
            let mut res = self.parse(ctx);
            res.error = None;
            res
        })
    }

    fn span(self) -> impl Parser<'p, C, &'p str, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cur.get();
            let res = self.parse(ctx);
            res.map(|_| &ctx.input[start..ctx.cur.get()])
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
            if res.success.is_none() {
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
            return ParserResult::success(());
        }
        let err = ctx.internal_error(ParserError::ExpectedLiteral(s).into());
        ctx.reset(ctx.cur.get() - matched);
        ParserResult::new(None, Some(err))
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
                return ParserResult::success(next);
            }
        };
        let error = ctx.internal_error(ParserError::ExpectedToken(token_name).into());
        ParserResult::new(None, Some(error))
    })
}

impl<'p, F, C, T, E> Parser<'p, C, T, E> for ParserImpl<'p, F, C, T, E>
where
    F: Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E>,
    T: 'p,
    E: AsParserError + 'p,
{
    fn parse(&self, ctx: &'p ParserContext<'p, C>) -> ParserResult<T, E> {
        (self.0)(ctx)
    }
}
