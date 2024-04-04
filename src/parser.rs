use std::marker::PhantomData;

use crate::{context::ParserContext, result::ParserResult};

pub trait Parser<'p, C: 'p, T: 'p, E: 'p>: private::SealedParser<C, T, E> {
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
            ParserResult::new(Some(res.success), res.error, res.pos)
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
            other.parse(ctx).integrate_error(res)
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
                return ParserResult::new(None, res.error, res.pos);
            };
            elems.push(elem);

            let failed = loop {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    break res;
                };
                elems.push(elem);
            };
            ParserResult::new(Some(elems), failed.error, failed.pos)
        })
    }

    fn optional_repeating(self) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let failed = loop {
                let res = self.parse(ctx);
                let Some(elem) = res.success else {
                    break res;
                };
                elems.push(elem);
            };
            ParserResult::new(Some(elems), failed.error, failed.pos)
        })
    }

    fn delimited<D>(self, delim: impl Parser<'p, C, D, E> + 'p) -> impl Parser<'p, C, Vec<T>, E>
    where
        Self: Sized + 'p,
        D: 'p,
    {
        parser(move |ctx| {
            let mut elems = vec![];
            let mut res = self.parse(ctx);

            let Some(elem) = res.success.take() else {
                return ParserResult::new(Some(elems), res.error, res.pos);
            };
            elems.push(elem);

            let mut last_res = res;
            while delim.parse(ctx).success.is_some() {
                let mut res = self.parse(ctx);
                let Some(elem) = res.success.take() else {
                    return ParserResult::new(None, res.error, res.pos).integrate_error(last_res);
                };
                last_res = res;
                elems.push(elem);
            }
            ParserResult::new(Some(elems), last_res.error, last_res.pos)
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

            let mut res = self.parse(ctx);
            let Some(elem) = res.success.take() else {
                return ParserResult::new(Some(elems), res.error, res.pos);
            };
            elems.push(elem);

            let mut last_res = res;
            while delim.parse(ctx).success.is_some() {
                let mut res = self.parse(ctx);
                let Some(elem) = res.success.take() else {
                    return ParserResult::new(None, res.error, res.pos).integrate_error(last_res);
                };
                last_res = res;
                elems.push(elem);
            }
            ParserResult::new(Some(elems), last_res.error, last_res.pos)
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
            let start = ctx.cursor();
            let res = self.parse(ctx);
            res.map(|_| &ctx.input[start..ctx.cursor()])
        })
    }

    fn unilateral(self) -> impl Parser<'p, C, T, E>
    where
        Self: Sized + 'p,
        E: std::fmt::Debug,
    {
        parser(move |ctx| {
            let start = ctx.cursor();
            let res = self.parse(ctx);
            if res.success.is_none() {
                ctx.reset(start);
            }
            res
        })
    }
}

struct ParserImpl<'p, F, C, T, E>(F, PhantomData<&'p (C, T, E)>)
where
    F: Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E> + 'p,
    T: 'p,
    E: 'p;

pub fn parser<'p, C, T, E>(
    f: impl Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E> + 'p,
) -> impl Parser<'p, C, T, E> + 'p
where
    C: 'p,
    T: 'p,
    E: 'p,
{
    ParserImpl(f, PhantomData)
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
    F: Fn(&'p ParserContext<'p, C>) -> ParserResult<T, E>,
    T: 'p,
    E: 'p,
{
    fn parse(&self, ctx: &'p ParserContext<'p, C>) -> ParserResult<T, E> {
        (self.0)(ctx)
    }
}
