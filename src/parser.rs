use std::{cell::UnsafeCell, marker::PhantomData};

use crate::{context::ParserContext, ParserError, Recoverable};

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
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> Option<T>;

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
            let priority = ctx.get_err_priority();
            let output = Some(self.parse(ctx));
            ctx.set_err_priority(priority);
            output
        })
    }

    /// Try another parser if this one fails. This can mess up error reporting.
    fn or(self, other: impl Parser<'p, C, T, E> + 'p + Sized) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| self.parse(ctx).or_else(|| other.parse(ctx)))
    }

    /// Try to parse as many times as possible in sequence, optionally requiring at least one match.
    fn repeating<const REQUIRED: bool>(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let priority = ctx.get_err_priority();
            if !REQUIRED {
                ctx.set_err_priority(0);
            }
            let res = self.parse(ctx);
            let Some(elem) = res else {
                if REQUIRED {
                    return None;
                } else {
                    ctx.set_err_priority(priority);
                    return Some(elems);
                }
            };
            if REQUIRED {
                ctx.set_err_priority(0);
            }
            elems.push(elem);

            loop {
                let Some(elem) = self.parse(ctx) else {
                    break;
                };
                elems.push(elem);
            }
            ctx.set_err_priority(priority);
            Some(elems)
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

            let Some(elem) = self.parse(ctx) else {
                if REQUIRED {
                    return None;
                } else {
                    return Some(elems);
                }
            };
            elems.push(elem);

            let mut delim_start = ctx.cursor();
            loop {
                let delim_res = delim.parse(ctx);
                if delim_res.is_none() {
                    return Some(elems);
                }
                let Some(elem) = self.parse(ctx) else {
                    ctx.set_err_start(delim_start);
                    return None;
                };
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
        E: std::fmt::Debug,
    {
        parser(move |ctx| {
            ctx.slice();
            let start = ctx.cursor();
            let res = self.parse(ctx);
            if res.is_some() || ctx.deepest_err_pos() <= ctx.cursor() {
                return res;
            }

            let mut distance = 0;
            println!("Errs locked");
            let _lock = ctx.lock_errors();
            ctx.reset(ctx.deepest_err_pos().max(ctx.cursor()));
            while distance < max_distance
                && !ctx
                    .recover_terminator
                    .get()
                    .is_some_and(|terminator| ctx.slice().starts_with(terminator))
            {
                let cursor_before = ctx.cursor();

                if anchor.parse(ctx).is_some() {
                    if !CONSUME {
                        ctx.reset(cursor_before);
                    }
                    println!("Recovered at {}", ctx.cursor());
                    ctx.recover_err();
                    return Some(res.unwrap_or_else(|| T::error_value(start..ctx.cursor())));
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
        E: From<ParserError> + std::fmt::Debug,
    {
        parser(move |ctx| {
            let start = ctx.cursor();
            let parent_terminator = ctx.recover_terminator.get();
            ctx.recover_terminator.set(Some(close));
            let res = self.parse(ctx);
            if res.is_some() && ctx.cursor() - start >= open.len() {
                ctx.recover_terminator.set(parent_terminator);
                return res;
            }

            ctx.reset(ctx.deepest_err_pos().max(ctx.deepest_recovered_err_pos()));
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
                        ctx.recover_err();
                        ctx.recover_terminator.set(parent_terminator);
                        return Some(Recoverable::error_value(start..ctx.cursor()));
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
            ctx.recover_terminator.set(parent_terminator);
            if ctx.err_range().start != start {
                ctx.add_recovered_err(start..start, ParserError::UnmatchedDelimiter(open).into());
            }
            res
        })
    }
}

/// Create a Parser from a lambda which takes in a &[`ParserContext`] and returns a [`ParserResult`].
pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C, E>) -> Option<T> + 'p,
) -> impl Parser<'p, C, T, E> + 'p
where
    C: 'p,
    T: 'p,
    E: 'p,
{
    ParserImpl {
        func: f,
        phantom: PhantomData,
    }
}

struct ParserImpl<'p, F, C, T, E>
where
    F: Fn(&'p ParserContext<'p, C, E>) -> Option<T> + 'p,
    T: 'p,
    E: 'p,
{
    func: F,
    phantom: PhantomData<&'p (C, T, E)>,
}

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
    F: Fn(&'p ParserContext<'p, C, E>) -> Option<T>,
    T: 'p,
    E: 'p,
{
    #[inline(always)]
    fn parse(&self, ctx: &'p ParserContext<'p, C, E>) -> Option<T> {
        (self.func)(ctx)
    }
}
