use std::{cell::UnsafeCell, marker::PhantomData};

use crate::{context::ParserContext, result::ParserResult, ParserError, Recoverable};

pub struct AppendCell<T> {
    inner: UnsafeCell<Vec<T>>,
}

impl<T> Default for AppendCell<T> {
    fn default() -> Self {
        Self {
            inner: UnsafeCell::new(vec![]),
        }
    }
}

impl<T> AppendCell<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn append(&self, item: T) {
        unsafe { (*self.inner.get()).push(item) }
    }

    pub fn extend(&self, items: impl IntoIterator<Item = T>) {
        unsafe { (*self.inner.get()).extend(items) }
    }

    pub fn into_inner(self) -> Vec<T> {
        let val = unsafe { std::ptr::read(self.inner.get()) };
        std::mem::forget(self);
        val
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.inner.get()).len() }
    }

    pub fn truncate(&self, len: usize) -> Vec<T> {
        unsafe { (*self.inner.get()).drain(len..).collect() }
    }

    pub fn inspect_last<V: 'static>(&self, f: impl FnOnce(&T) -> V) -> Option<V> {
        unsafe { (*self.inner.get()).last().map(f) }
    }
}

/// The fundamental parsing construct for untwine. A simple parser which comes with combinators
/// for modifying its behavior.
pub trait Parser<'p, C: 'p, T: 'p, E: 'p>: private::SealedParser<C, T, E> {
    /// Parse a value from the [`ParserContext`]. On fail, the context will be reset to the starting position.
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> ParserResult<T, E>;

    /// Map the output value using a mapping function.
    fn map<V: 'p>(self, f: impl Fn(T) -> V + 'p) -> impl Parser<'p, C, V, E> + 'p
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).map(&f))
    }

    /// Match this token optionally. This parsing operation will always succeed, returning [None] if the the child failed.
    fn optional(self) -> impl Parser<'p, C, Option<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let res = self.parse(ctx);
            ParserResult::new(Some(res.success), res.error, res.pos)
        })
    }

    /// Try another parser if this one fails. This can mess up error reporting.
    fn or(self, other: impl Parser<'p, C, T, E> + 'p + Sized) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let res = self.parse(ctx);
            if res.success.is_some() {
                return res;
            }
            other.parse(ctx).integrate_error(res)
        })
    }

    /// Try to parse as many times as possible in sequence, requiring at least one match.
    fn repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let mut last_res = self.parse(ctx);
            let Some(elem) = last_res.success.take() else {
                return ParserResult::new(None, last_res.error, last_res.pos);
            };
            elems.push(elem);

            loop {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    last_res = res.integrate_error(last_res);
                    break;
                };
                elems.push(elem);
            }
            ParserResult::new(Some(elems), last_res.error, last_res.pos)
        })
    }

    /// Try to parse as many times as possible in sequence, allowing zero matches.
    fn optional_repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let mut last_res = self.parse(ctx);

            let Some(elem) = last_res.success.take() else {
                return ParserResult::new(Some(elems), last_res.error, last_res.pos);
            };
            elems.push(elem);

            loop {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    last_res = res.integrate_error(last_res);
                    break;
                };
                elems.push(elem);
            }
            ParserResult::new(Some(elems), last_res.error, last_res.pos)
        })
    }

    /// Parse a list using this parser, separated by another delimiter parser. Discards delimiters, and requires at least one match.
    fn delimited<D, const REQUIRED: bool>(
        self,
        delim: impl Parser<'p, C, D, E> + 'p,
    ) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
        D: 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let mut res = self.parse(ctx);

            let Some(elem) = res.success.take() else {
                if REQUIRED {
                    return ParserResult::new(None, res.error, res.pos);
                } else {
                    return ParserResult::new(Some(elems), res.error, res.pos);
                }
            };
            elems.push(elem);

            let mut last_res = res;
            let mut delim_start = ctx.cursor();
            loop {
                let delim_res = delim.parse(ctx);
                if delim_res.success.is_none() {
                    return ParserResult::new(Some(elems), delim_res.error, delim_res.pos)
                        .integrate_error(last_res);
                }
                let mut res = self.parse(ctx);
                let Some(elem) = res.success.take() else {
                    return ParserResult::new(None, res.error, res.pos)
                        .integrate_error(last_res)
                        .set_start_if_empty(delim_start);
                };
                last_res = res;
                elems.push(elem);
                delim_start = ctx.cursor();
            }
        })
    }

    /// Ignore the output of this parser by dropping it.
    fn ignore(self) -> impl Parser<'p, C, (), E>
    where
        Self: Sized + 'p,
    {
        self.map(drop)
    }

    /// Ignore the error output of this parser.
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

    /// Capture the span of the parsed value as a [&str] instead of the value of itself.
    fn span(self) -> impl Parser<'p, C, &'p str, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let start = ctx.cursor();
            let res = self.parse(ctx);
            res.map(|_| &ctx.input[start..ctx.cursor()])
        })
    }

    /// Recover by looking ahead a specific number of characters to find the specified pattern.
    /// Can optionally consume the pattern once it is found, or leave it unparsed.
    fn recover_to<V, const CONSUME: bool>(
        self,
        anchor: impl Parser<'p, C, V, E> + 'p,
        max_distance: usize,
    ) -> impl Parser<'p, C, T, E>
    where
        T: Recoverable,
        V: 'p,
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            ctx.slice();
            let start = ctx.cursor();
            let res = self.parse(ctx);
            if res.success.is_some() && res.pos.end == ctx.cursor() {
                return res;
            }

            let mut distance = 0;
            ctx.advance(res.pos.end - ctx.cursor());
            while distance < max_distance
                && !ctx
                    .recover_terminator
                    .get()
                    .is_some_and(|terminator| ctx.slice().starts_with(terminator))
            {
                let cursor_before = ctx.cursor();

                if anchor.parse(ctx).success.is_some() {
                    if let Some(err) = res.error {
                        ctx.add_recovered_error(err, res.pos.clone());
                    }
                    if !CONSUME {
                        ctx.reset(cursor_before);
                    }
                    return ParserResult::new(
                        Some(
                            res.success
                                .unwrap_or_else(|| T::error_value(start..ctx.cursor())),
                        ),
                        None,
                        res.pos,
                    );
                }

                if let Some(c) = ctx.slice().chars().next() {
                    if !c.is_ascii_whitespace() {
                        distance += 1;
                    }
                    ctx.advance(c.len_utf8());
                } else {
                    break;
                }
            }
            ctx.reset(start);
            res
        })
    }

    /// Recover for a parser which parses values within a set of wrapping literals, usually parens
    /// or braces. Will respect balancing these delimiters when recovering.
    fn recover_wrapped(
        self,
        open: &'static str,
        close: &'static str,
        max_distance: usize,
    ) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
        T: Recoverable,
        E: From<ParserError>,
    {
        parser(move |ctx| {
            let start = ctx.cursor();
            let parent_terminator = ctx.recover_terminator.get();
            ctx.recover_terminator.set(Some(close));
            let res = self.parse(ctx);
            if res.success.is_some() || res.pos.end - ctx.cursor() <= open.len() {
                ctx.recover_terminator.set(parent_terminator);
                return res;
            }

            ctx.reset(res.pos.end.max(ctx.last_recovered_end()));
            let mut depth = 1;
            let mut distance = 0;
            while let Some(c) = ctx.slice().chars().next() {
                if ctx.slice().starts_with(open) && open != close {
                    depth += 1;
                }

                if ctx.slice().starts_with(close) {
                    depth -= 1;
                    if depth == 0 {
                        ctx.advance(close.len());
                        if let Some(err) = res.error {
                            ctx.add_recovered_error(err, res.pos.clone());
                        }
                        ctx.recover_terminator.set(parent_terminator);
                        return ParserResult::new(
                            Some(Recoverable::error_value(start..ctx.cursor())),
                            None,
                            res.pos,
                        );
                    }
                }

                ctx.advance(c.len_utf8());
                if !c.is_ascii_whitespace() {
                    distance += 1;
                }
                if distance >= max_distance {
                    break;
                }
            }

            ctx.reset(start);
            if res.pos.start != start {
                ctx.add_recovered_error(ParserError::UnmatchedDelimiter(open).into(), start..start);
            }
            res
        })
    }
}

/// Create a Parser from a lambda which takes in a &[`ParserContext`] and returns a [`ParserResult`].
pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C, E>) -> ParserResult<T, E> + 'p,
) -> impl Parser<'p, C, T, E> + 'p
where
    C: 'p,
    T: 'p,
    E: 'p,
{
    ParserImpl(f, PhantomData)
}

struct ParserImpl<'p, F, C, T, E>(F, PhantomData<&'p (C, T, E)>)
where
    F: Fn(&'p ParserContext<'p, C, E>) -> ParserResult<T, E> + 'p,
    T: 'p,
    E: 'p;

mod private {
    pub trait SealedParser<C, T, E> {}
}

impl<'p, C, T, E, P> private::SealedParser<C, T, E> for P
where
    C: 'p,
    E: 'p,
    P: Parser<'p, C, T, E>,
    T: 'p,
{
}

impl<'p, F, C, T, E> Parser<'p, C, T, E> for ParserImpl<'p, F, C, T, E>
where
    F: Fn(&'p ParserContext<'p, C, E>) -> ParserResult<T, E>,
    T: 'p,
    E: 'p,
{
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> ParserResult<T, E> {
        (self.0)(ctx)
    }
}
