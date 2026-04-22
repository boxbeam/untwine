use std::{
    cell::UnsafeCell,
    collections::HashMap,
    fmt::Debug,
    marker::PhantomData,
    num::ParseIntError,
    ops::{Range, RangeInclusive},
};

use crate::ParserError;

pub trait Source {
    type Input<'a>;
    type Symbol;
}

#[derive(Clone, Copy)]
pub struct Input<'a> {
    src: &'a str,
    cur: usize,
}

impl<'a> Input<'a> {
    fn slice(&'a self) -> &'a str {
        &self.src[self.cur..]
    }
}

impl<'a> From<&'a str> for Input<'a> {
    fn from(value: &'a str) -> Self {
        Input { src: value, cur: 0 }
    }
}

impl<'a> Input<'a> {
    fn skip(self, len: usize) -> Self {
        Input {
            cur: self.cur + len,
            ..self
        }
    }
}

pub trait ErrorHandler<E>: Clone {
    fn error(&self, err: impl Into<E>, loc: Range<usize>);
}

impl<F, E> ErrorHandler<E> for &F
where
    F: Fn(E, Range<usize>),
{
    fn error(&self, err: impl Into<E>, loc: Range<usize>) {
        self(err.into(), loc)
    }
}

#[derive(Clone)]
pub struct DebugErrorHandler;

impl<E> ErrorHandler<E> for DebugErrorHandler
where
    E: Debug,
{
    fn error(&self, err: impl Into<E>, loc: Range<usize>) {
        let err = err.into();
        eprintln!("Error at {loc:?}: {err:?}");
    }
}

#[derive(Debug)]
pub struct ErrorLocation<E>(E, Range<usize>);

struct ErrorCell<E> {
    inner: UnsafeCell<Option<ErrorLocation<E>>>,
}

impl<E> Default for ErrorCell<E> {
    fn default() -> Self {
        Self { inner: None.into() }
    }
}

impl<E> ErrorHandler<E> for &ErrorCell<E> {
    fn error(&self, err: impl Into<E>, loc: Range<usize>) {
        unsafe {
            let inner = self.inner.get();
            (*inner).replace(ErrorLocation(err.into(), loc));
        }
    }
}

impl<E> ErrorCell<E> {
    fn into_inner(self) -> Option<ErrorLocation<E>> {
        self.inner.into_inner()
    }
}

type ParserResult<T> = Option<(usize, T)>;
type Context<'a, T> = &'a mut T;

struct ToVec;

pub trait Collector<T> {
    type Container: Default;

    fn consume(&self, container: &mut Self::Container, elem: T);
}

trait Insert {
    type Elem;
    fn insert(&mut self, elem: Self::Elem);
}

impl<T> Insert for Vec<T> {
    type Elem = T;

    fn insert(&mut self, elem: Self::Elem) {
        self.push(elem);
    }
}

impl<K, V> Insert for HashMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    type Elem = (K, V);

    fn insert(&mut self, (k, v): Self::Elem) {
        HashMap::insert(self, k, v);
    }
}

struct Collect<C>(PhantomData<C>);

impl<C, T> Collector<T> for Collect<C>
where
    C: Default + Insert<Elem = T>,
{
    type Container = C;

    fn consume(&self, container: &mut Self::Container, elem: T) {
        container.insert(elem);
    }
}

impl<C, T, D> DelimitedCollector<T, D> for Collect<C>
where
    C: Default + Insert<Elem = T>,
{
    type Container = C;

    fn from(&self, elem: T) -> Self::Container {
        let mut empty = C::default();
        empty.insert(elem);
        empty
    }

    fn consume(&mut self, mut container: Self::Container, _delim: D, elem: T) -> Self::Container {
        container.insert(elem);
        container
    }
}

fn collect<C>() -> Collect<C>
where
    C: Default + Insert,
{
    Collect(PhantomData)
}

impl<T> Collector<T> for ToVec {
    type Container = Vec<T>;

    fn consume(&self, container: &mut Self::Container, elem: T) {
        container.push(elem);
    }
}

impl<T> Collector<T> for Ignore {
    type Container = ();

    fn consume(&self, _container: &mut Self::Container, _elem: T) {}
}

pub trait DelimitedCollector<T, D> {
    type Container;
    fn from(&self, elem: T) -> Self::Container;
    fn consume(&mut self, container: Self::Container, delim: D, elem: T) -> Self::Container;
}

impl<'a, T, D> DelimitedCollector<T, D> for ToVec {
    type Container = Vec<T>;
    fn consume(&mut self, mut container: Self::Container, _delim: D, elem: T) -> Self::Container {
        container.push(elem);
        container
    }

    fn from(&self, elem: T) -> Self::Container {
        vec![elem]
    }
}

impl<'a, T, D> DelimitedCollector<T, D> for Ignore {
    type Container = ();
    fn consume(&mut self, _container: Self::Container, _delim: D, _elem: T) -> Self::Container {
        ()
    }

    fn from(&self, _elem: T) -> Self::Container {
        ()
    }
}

fn lfold<'a, Elem, Delim, F>(f: F) -> impl DelimitedCollector<Elem, Delim, Container = Elem>
where
    F: FnMut(Elem, Delim, Elem) -> Elem,
{
    struct LFold<F, Elem, Delim> {
        f: F,
        phantom: PhantomData<(Elem, Delim)>,
    }

    impl<'a, F, Elem, Delim> DelimitedCollector<Elem, Delim> for LFold<F, Elem, Delim>
    where
        F: FnMut(Elem, Delim, Elem) -> Elem,
    {
        type Container = Elem;

        fn from(&self, elem: Elem) -> Self::Container {
            elem
        }

        fn consume(
            &mut self,
            container: Self::Container,
            delim: Delim,
            elem: Elem,
        ) -> Self::Container {
            (self.f)(container, delim, elem)
        }
    }

    LFold {
        f,
        phantom: PhantomData,
    }
}

pub trait OptionalOutput {
    type Output<T>;

    fn convert<V>(val: V) -> Self::Output<V>;
}

impl OptionalOutput for Keep {
    type Output<T> = T;

    fn convert<V>(val: V) -> Self::Output<V> {
        val
    }
}

impl OptionalOutput for Ignore {
    type Output<T> = ();

    fn convert<V>(_val: V) -> Self::Output<V> {
        ()
    }
}

pub trait Chain {
    type Output<A, B>;
    type NextKind;

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B>;
}

pub struct Ignore;
pub struct Keep;

pub struct ChainImpl<L, R> {
    phantom: PhantomData<(L, R)>,
}

impl Chain for ChainImpl<Keep, Keep> {
    type Output<A, B> = (A, B);
    type NextKind = Keep;

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B> {
        (a, b)
    }
}

impl Chain for ChainImpl<Keep, Ignore> {
    type Output<A, B> = A;
    type NextKind = Keep;

    fn chain<A, B>(a: A, _b: B) -> Self::Output<A, B> {
        a
    }
}

impl Chain for ChainImpl<Ignore, Keep> {
    type Output<A, B> = B;
    type NextKind = Keep;

    fn chain<A, B>(_a: A, b: B) -> Self::Output<A, B> {
        b
    }
}

impl Chain for ChainImpl<Ignore, Ignore> {
    type Output<A, B> = ();
    type NextKind = Ignore;

    fn chain<A, B>(_a: A, _b: B) -> Self::Output<A, B> {
        ()
    }
}

pub trait Parser<Err, Ctx = ()> {
    type Output<'a>;
    type Kind;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        errs: impl ErrorHandler<Err>,
        ctx: Context<Ctx>,
    ) -> ParserResult<Self::Output<'a>>;

    fn try_match<'a>(
        &mut self,
        input: &'a str,
    ) -> Result<Self::Output<'a>, Option<ErrorLocation<Err>>>
    where
        Ctx: Default,
    {
        let input = Input { src: input, cur: 0 };
        let mut ctx = Default::default();
        let errs = ErrorCell::default();
        match self.parse(input, &errs, &mut ctx) {
            Some((_, e)) => Ok(e),
            None => Err(errs.into_inner()),
        }
    }

    fn rep<Coll>(
        self,
        coll: Coll,
    ) -> impl for<'a> Parser<
        Err,
        Ctx,
        Output<'a> = <Coll as Collector<Self::Output<'a>>>::Container,
        Kind = Self::Kind,
    >
    where
        Self: Sized,
        Coll: for<'a> Collector<Self::Output<'a>>,
    {
        struct Repeat<P, Coll> {
            p: P,
            coll: Coll,
        }

        impl<P, E, C, Coll> Parser<E, C> for Repeat<P, Coll>
        where
            P: Parser<E, C>,
            Coll: for<'a> Collector<P::Output<'a>>,
        {
            type Kind = P::Kind;
            type Output<'a> = <Coll as Collector<P::Output<'a>>>::Container;
            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (mut offset, _first) = self.p.parse(input, errs.clone(), ctx)?;
                let mut elems = Coll::Container::default();
                while let Some((len, elem)) = self.p.parse(input.skip(offset), errs.clone(), ctx) {
                    offset += len;
                    self.coll.consume(&mut elems, elem);
                }
                Some((offset, elems))
            }
        }

        Repeat { p: self, coll }
    }

    fn or<P>(
        self,
        other: P,
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Self::Output<'a>, Kind = Self::Kind>
    where
        Self: Sized,
        P: for<'a> Parser<Err, Ctx, Output<'a> = Self::Output<'a>>,
    {
        struct Or<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, E, C> Parser<E, C> for Or<P1, P2>
        where
            P1: for<'a> Parser<E, C, Output<'a> = P2::Output<'a>>,
            P2: Parser<E, C>,
        {
            type Kind = P1::Kind;
            type Output<'a> = P1::Output<'a>;
            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<P1::Output<'a>> {
                let err = ErrorCell::default();
                let parsed = self.l.parse(input, &err, ctx);
                if parsed.is_some() {
                    return parsed;
                }
                match self.r.parse(input, &err, ctx) {
                    Some(val) => Some(val),
                    None => {
                        if let Some(ErrorLocation(err, pos)) = err.into_inner() {
                            errs.error(err, pos);
                        }
                        None
                    }
                }
            }
        }

        Or { l: self, r: other }
    }

    fn map<F, V>(self, f: F) -> impl for<'a> Parser<Err, Ctx, Output<'a> = V, Kind = Keep>
    where
        Self: Sized,
        F: for<'a> Fn(Self::Output<'a>) -> V,
    {
        struct Map<P, F> {
            p: P,
            f: F,
        }

        impl<P, F, V, E, C> Parser<E, C> for Map<P, F>
        where
            P: Parser<E, C>,
            F: for<'a> Fn(P::Output<'a>) -> V,
        {
            type Kind = Keep;
            type Output<'a> = V;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<V> {
                self.p
                    .parse(input, errs, ctx)
                    .map(|(len, val)| (len, (self.f)(val)))
            }
        }

        Map { p: self, f }
    }

    fn try_map<F, V, E>(
        self,
        f: F,
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = V, Kind = Self::Kind>
    where
        Self: Sized,
        F: for<'a> Fn(Self::Output<'a>) -> Result<V, E>,
        E: Into<Err>,
    {
        struct TryMap<P, F> {
            p: P,
            f: F,
        }

        impl<P, F, V, E, E2, C> Parser<E, C> for TryMap<P, F>
        where
            P: Parser<E, C>,
            F: for<'a> Fn(P::Output<'a>) -> Result<V, E2>,
            E2: Into<E>,
        {
            type Kind = P::Kind;
            type Output<'a> = V;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<V> {
                let (len, result) = self.p.parse(input, errs.clone(), ctx)?;
                let result = (self.f)(result);
                match result {
                    Ok(v) => Some((len, v)),
                    Err(e) => {
                        errs.error(e, input.cur..input.cur + len);
                        None
                    }
                }
            }
        }

        TryMap { p: self, f }
    }

    fn opt(
        self,
    ) -> impl for<'a> Parser<
        Err,
        Ctx,
        Output<'a> = <Self::Kind as OptionalOutput>::Output<Option<Self::Output<'a>>>,
        Kind = Self::Kind,
    >
    where
        Self: Sized,
        Self::Kind: OptionalOutput,
    {
        struct Optional<P> {
            p: P,
        }
        impl<P, Err, Ctx> Parser<Err, Ctx> for Optional<P>
        where
            P: Parser<Err, Ctx>,
            P::Kind: OptionalOutput,
        {
            type Output<'a> = <P::Kind as OptionalOutput>::Output<Option<P::Output<'a>>>;
            type Kind = P::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<Err>,
                ctx: Context<Ctx>,
            ) -> ParserResult<Self::Output<'a>> {
                match self.p.parse(input, errs, ctx) {
                    Some((len, elem)) => Some((len, P::Kind::convert(Some(elem)))),
                    None => Some((0, P::Kind::convert(None))),
                }
            }
        }

        Optional { p: self }
    }

    fn drop(self) -> impl for<'a> Parser<Err, Ctx, Output<'a> = (), Kind = Ignore>
    where
        Self: Sized,
    {
        struct Drop<P> {
            p: P,
        }
        impl<P, E, C> Parser<E, C> for Drop<P>
        where
            P: Parser<E, C>,
        {
            type Output<'a> = ();
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (len, _) = self.p.parse(input, errs, ctx)?;
                Some((len, ()))
            }
        }
        Drop { p: self }
    }

    fn slice(self) -> impl for<'a> Parser<Err, Ctx, Output<'a> = &'a str, Kind = Keep>
    where
        Self: Sized,
    {
        struct Slice<P> {
            p: P,
        }
        impl<P, E, C> Parser<E, C> for Slice<P>
        where
            P: Parser<E, C>,
        {
            type Output<'a> = &'a str;
            type Kind = Keep;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (len, _) = self.p.parse(input, errs, ctx)?;
                let slice = &input.src[input.cur..input.cur + len];
                Some((len, slice))
            }
        }
        Slice { p: self }
    }

    fn delim_by<P, Coll>(
        self,
        delim: P,
        collect: Coll,
    ) -> impl for<'a> Parser<
        Err,
        Ctx,
        Output<'a> = <Coll as DelimitedCollector<Self::Output<'a>, P::Output<'a>>>::Container,
    >
    where
        P: Parser<Err, Ctx>,
        Self: Sized,
        Coll: for<'a> DelimitedCollector<Self::Output<'a>, P::Output<'a>>,
    {
        struct DelimBy<P1, P2, Coll> {
            elem: P1,
            delim: P2,
            coll: Coll,
        }

        impl<P1, P2, Coll, E, C> Parser<E, C> for DelimBy<P1, P2, Coll>
        where
            P1: Parser<E, C>,
            P2: Parser<E, C>,
            Coll: for<'a> DelimitedCollector<P1::Output<'a>, P2::Output<'a>>,
        {
            type Output<'a> =
                <Coll as DelimitedCollector<P1::Output<'a>, P2::Output<'a>>>::Container;
            type Kind = Keep;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (mut offset, first) = self.elem.parse(input, errs.clone(), ctx)?;
                let mut container = self.coll.from(first);
                while let Some((delim_len, delim)) =
                    self.delim.parse(input.skip(offset), errs.clone(), ctx)
                {
                    match self
                        .elem
                        .parse(input.skip(offset + delim_len), errs.clone(), ctx)
                    {
                        Some((elem_len, next)) => {
                            offset += delim_len + elem_len;
                            container = self.coll.consume(container, delim, next);
                        }
                        None => break,
                    }
                }
                Some((offset, container))
            }
        }

        DelimBy {
            elem: self,
            delim,
            coll: collect,
        }
    }

    fn then<P>(
        self,
        other: P,
    ) -> impl for<'a> Parser<
        Err,
        Ctx,
        Output<'a> = <ChainImpl<Self::Kind, P::Kind> as Chain>::Output<
            Self::Output<'a>,
            P::Output<'a>,
        >,
        Kind = <ChainImpl<Self::Kind, P::Kind> as Chain>::NextKind,
    >
    where
        Self: Sized,
        P: Parser<Err, Ctx>,
        ChainImpl<Self::Kind, P::Kind>: Chain,
    {
        struct Then<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, E, C> Parser<E, C> for Then<P1, P2>
        where
            P1: Parser<E, C>,
            P2: Parser<E, C>,
            ChainImpl<P1::Kind, P2::Kind>: Chain,
        {
            type Kind = <ChainImpl<P1::Kind, P2::Kind> as Chain>::NextKind;
            type Output<'a> =
                <ChainImpl<P1::Kind, P2::Kind> as Chain>::Output<P1::Output<'a>, P2::Output<'a>>;
            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (l_len, l_val) = self.l.parse(input, errs.clone(), ctx)?;
                let (r_len, r_val) = self.r.parse(
                    input.skip(l_len),
                    &|e, mut r: Range<usize>| {
                        if r.len() == 0 {
                            r.start = input.cur;
                        }
                        errs.error(e, r);
                    },
                    ctx,
                )?;
                let output = ChainImpl::<P1::Kind, P2::Kind>::chain(l_val, r_val);
                Some((l_len + r_len, output))
            }
        }

        Then { l: self, r: other }
    }

    fn map_err<'a, E2, F>(
        self,
        f: F,
    ) -> impl Parser<E2, Ctx, Output<'a> = Self::Output<'a>, Kind = Self::Kind>
    where
        Self: Sized,
        F: Fn(Err) -> E2,
    {
        struct MapErr<P, F, E> {
            p: P,
            f: F,
            phantom: PhantomData<E>,
        }

        impl<P, F, C, E, E2> Parser<E2, C> for MapErr<P, F, E>
        where
            P: Parser<E, C>,
            F: Fn(E) -> E2,
        {
            type Output<'a> = P::Output<'a>;
            type Kind = P::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E2>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let handler = |err: E, loc| errs.error((self.f)(err), loc);
                self.p.parse(input, &handler, ctx)
            }
        }

        MapErr {
            p: self,
            f,
            phantom: PhantomData,
        }
    }

    fn with_context<'a, C>(
        self,
        ctx: Ctx,
    ) -> impl Parser<Err, (), Output<'a> = Self::Output<'a>, Kind = Self::Kind>
    where
        Self: Sized,
    {
        struct WithContext<P, C> {
            p: P,
            c: C,
        }

        impl<P, E, C> Parser<E, ()> for WithContext<P, C>
        where
            P: Parser<E, C>,
        {
            type Output<'a> = P::Output<'a>;
            type Kind = P::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                _ctx: Context<()>,
            ) -> ParserResult<Self::Output<'a>> {
                self.p.parse(input, errs, &mut self.c)
            }
        }

        WithContext { p: self, c: ctx }
    }

    fn wrapped(
        self,
        before: &'static str,
        after: &'static str,
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Self::Output<'a>, Kind = Self::Kind>
    where
        Self: Sized,
        Err: From<ParserError>,
    {
        struct Wrapped<P> {
            p: P,
            before: &'static str,
            after: &'static str,
        }

        impl<P, E, C> Parser<E, C> for Wrapped<P>
        where
            P: Parser<E, C>,
            E: From<ParserError>,
        {
            type Output<'a> = P::Output<'a>;
            type Kind = P::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let errs_clone = errs.clone();
                let handler = |e: ParserError, loc| errs_clone.error(E::from(e), loc);
                Parser::parse(&mut self.before, input, &handler, &mut ())?;
                let (len, val) = self.p.parse(input.skip(self.before.len()), errs, ctx)?;
                Parser::parse(
                    &mut self.after,
                    input.skip(self.before.len() + len),
                    &handler,
                    &mut (),
                )?;
                Some((self.before.len() + self.after.len() + len, val))
            }
        }
        Wrapped {
            p: self,
            before,
            after,
        }
    }

    fn lookahead(self) -> impl for<'a> Parser<Err, Ctx, Output<'a> = (), Kind = Ignore>
    where
        Self: Sized,
    {
        struct Lookahead<P> {
            p: P,
        }
        impl<P, C, E> Parser<C, E> for Lookahead<P>
        where
            P: Parser<C, E>,
        {
            type Output<'a> = ();
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<C>,
                ctx: Context<E>,
            ) -> ParserResult<Self::Output<'a>> {
                let _ = self.p.parse(input, errs, ctx)?;
                Some((0, ()))
            }
        }

        Lookahead { p: self }
    }

    fn not(self) -> impl for<'a> Parser<Err, Ctx, Output<'a> = (), Kind = Ignore>
    where
        Self: Sized,
        Err: From<ParserError>,
    {
        struct Not<P> {
            p: P,
        }
        impl<P, E, C> Parser<E, C> for Not<P>
        where
            P: Parser<E, C>,
            E: From<ParserError>,
        {
            type Output<'a> = ();
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let result = self.p.parse(input, errs.clone(), ctx);
                if result.is_some() {
                    errs.error(ParserError::UnexpectedToken, input.cur..input.cur);
                    None
                } else {
                    Some((0, ()))
                }
            }
        }

        Not { p: self }
    }

    fn pad<P>(
        self,
        pad: P,
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Self::Output<'a>, Kind = Self::Kind>
    where
        P: Parser<Err, Ctx>,
        Self: Sized,
    {
        struct Pad<P1, P2> {
            elem: P1,
            pad: P2,
        }
        impl<P1, P2, E, C> Parser<E, C> for Pad<P1, P2>
        where
            P1: Parser<E, C>,
            P2: Parser<E, C>,
        {
            type Output<'a> = P1::Output<'a>;

            type Kind = P1::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let (len, _) = self.pad.parse(input, errs.clone(), ctx)?;
                let mut offset = len;
                let (len, val) = self.elem.parse(input.skip(offset), errs.clone(), ctx)?;
                offset += len;
                let (len, _) = self.pad.parse(input.skip(offset), errs, ctx)?;
                offset += len;
                Some((offset, val))
            }
        }

        Pad { elem: self, pad }
    }
}

pub trait FixedLengthParser<E, C>: Parser<E, C> {
    fn parsed_len(&self) -> usize;

    fn lookbehind(self) -> impl for<'a> Parser<E, C, Output<'a> = (), Kind = Ignore>
    where
        Self: Sized,
    {
        struct Lookbehind<P> {
            p: P,
        }
        impl<P, C, E> Parser<C, E> for Lookbehind<P>
        where
            P: FixedLengthParser<C, E>,
        {
            type Output<'a> = ();
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<C>,
                ctx: Context<E>,
            ) -> ParserResult<Self::Output<'a>> {
                let new_input = Input {
                    src: input.src,
                    cur: input.cur - self.p.parsed_len(),
                };
                let _ = self.p.parse(new_input, errs, ctx)?;
                Some((0, ()))
            }
        }

        Lookbehind { p: self }
    }

    fn negative_lookbehind(self) -> impl for<'a> Parser<E, C, Output<'a> = (), Kind = Ignore>
    where
        Self: Sized,
        E: From<ParserError>,
    {
        struct NegativeLookbehind<P> {
            p: P,
        }
        impl<P, E, C> Parser<E, C> for NegativeLookbehind<P>
        where
            P: FixedLengthParser<E, C>,
            E: From<ParserError>,
        {
            type Output<'a> = ();
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                let new_input = Input {
                    src: input.src,
                    cur: input.cur - self.p.parsed_len(),
                };
                let result = self.p.parse(new_input, errs.clone(), ctx);
                if result.is_some() {
                    errs.error(ParserError::UnexpectedToken, input.cur..input.cur);
                    None
                } else {
                    Some((0, ()))
                }
            }
        }

        NegativeLookbehind { p: self }
    }
}

fn lit<E, C>(
    lit: &'static str,
    parser_name: &'static str,
) -> impl for<'a> FixedLengthParser<E, C, Output<'a> = (), Kind = Ignore>
where
    E: From<ParserError>,
{
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }

    impl<E, C> Parser<E, C> for LitParser
    where
        E: From<ParserError>,
    {
        type Kind = Ignore;
        type Output<'a> = ();
        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<E>,
            _ctx: Context<C>,
        ) -> ParserResult<()> {
            let num_matching = input
                .slice()
                .bytes()
                .zip(self.lit.bytes())
                .take_while(|(a, b)| a == b)
                .count();
            if num_matching == self.lit.len() {
                Some((self.lit.len(), ()))
            } else {
                errs.error(
                    ParserError::ExpectedLiteral(self.lit, self.parser_name),
                    input.cur..input.cur + num_matching,
                );
                None
            }
        }
    }

    impl<E, C> FixedLengthParser<E, C> for LitParser
    where
        E: From<ParserError>,
    {
        fn parsed_len(&self) -> usize {
            self.lit.len()
        }
    }

    LitParser { lit, parser_name }
}

pub fn char_filter<E>(
    filter: impl Fn(char) -> bool,
    parser_name: &'static str,
) -> impl for<'a> Parser<E, Output<'a> = char, Kind = Keep>
where
    E: From<ParserError>,
{
    struct Filter<F> {
        f: F,
        parser_name: &'static str,
    }
    impl<E, F, C> Parser<E, C> for Filter<F>
    where
        F: Fn(char) -> bool,
        E: From<ParserError>,
    {
        type Output<'a> = char;
        type Kind = Keep;

        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<E>,
            _ctx: Context<C>,
        ) -> ParserResult<Self::Output<'_>> {
            let next_char = input.slice().chars().next();
            if let Some(c) = next_char
                && (self.f)(c)
            {
                Some((c.len_utf8(), c))
            } else {
                errs.error(
                    ParserError::ExpectedToken(self.parser_name),
                    input.cur..input.cur,
                );
                None
            }
        }
    }
    Filter {
        f: filter,
        parser_name,
    }
}

impl<E, C> Parser<E, C> for &'static str
where
    E: From<ParserError>,
{
    type Output<'b> = ();
    type Kind = Ignore;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        errs: impl ErrorHandler<E>,
        ctx: Context<C>,
    ) -> ParserResult<Self::Output<'a>> {
        lit(self, self).parse(input, errs, ctx)
    }
}

impl<E, C> FixedLengthParser<E, C> for &'static str
where
    E: From<ParserError>,
{
    fn parsed_len(&self) -> usize {
        str::len(self)
    }
}

impl<F> Parser<ParserError, ()> for F
where
    F: Fn(char) -> bool,
{
    type Output<'a> = char;
    type Kind = Keep;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        _errs: impl ErrorHandler<ParserError>,
        _ctx: Context<()>,
    ) -> ParserResult<Self::Output<'a>> {
        let c = input.slice().chars().next().filter(|c| self(*c))?;
        Some((c.len_utf8(), c))
    }
}

impl Parser<ParserError, ()> for char {
    type Output<'a> = ();
    type Kind = Ignore;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        _errs: impl ErrorHandler<ParserError>,
        _ctx: Context<()>,
    ) -> ParserResult<Self::Output<'a>> {
        if input.slice().starts_with(*self) {
            Some((self.len_utf8(), ()))
        } else {
            None
        }
    }
}

impl Parser<ParserError, ()> for RangeInclusive<char> {
    type Output<'a> = char;
    type Kind = Keep;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        _errs: impl ErrorHandler<ParserError>,
        _ctx: Context<()>,
    ) -> ParserResult<Self::Output<'a>> {
        let c = input.slice().chars().next();
        if let Some(c) = c
            && self.contains(&c)
        {
            Some((c.len_utf8(), c))
        } else {
            None
        }
    }
}

struct Int;
impl Parser<ParserError, ()> for Int {
    type Output<'a> = i64;
    type Kind = Keep;
    fn parse(
        &mut self,
        input: Input,
        _errs: impl ErrorHandler<ParserError>,
        _ctx: Context<()>,
    ) -> ParserResult<Self::Output<'_>> {
        let bytes = input
            .slice()
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .count();
        let digits = &input.slice()[..bytes];
        let Ok(num) = digits.parse() else {
            return None;
        };
        Some((bytes, num))
    }
}

impl<E> Parser<E, ()> for &'static [char]
where
    E: From<ParserError>,
{
    type Output<'a> = char;
    type Kind = Keep;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        errs: impl ErrorHandler<E>,
        _ctx: Context<()>,
    ) -> ParserResult<Self::Output<'a>> {
        let c = input.slice().chars().next();
        if let Some(c) = c
            && self.contains(&c)
        {
            Some((c.len_utf8(), c))
        } else {
            errs.error(ParserError::ExpectedSymbol(self), input.cur..input.cur);
            None
        }
    }
}

macro_rules! parsers_choice {
    ($val:expr) => {
        $val
    };
    ($val:expr $(, $($rest:expr),+ )?) => { $val.or(parsers_choice!( $( $($rest),+ )? )) };
}

#[macro_export]
macro_rules! p {
    ($val:expr) => {
        $val
    };
    ($val:expr $(, $($rest:expr),+ )?) => { $val.then(p!( $( $($rest),+ )? )) };
}

macro_rules! not_drop {
    ($parser:expr) => {
        $parser.drop()
    };
    ($parser:expr, $name:ident) => {
        $parser
    };
}

macro_rules! names_pattern {
    ($name:ident $(,)?) => {
        $name
    };
    ($a:ident , $($rest:ident),+ $(,)?) => {
        ($a, names_pattern!($($rest),+) , )
    };
    (,) => {};
    () => {
        _
    };
}

macro_rules! ret_type {
    ($ty:ty) => {
        $ty
    };
    () => {
        ()
    };
}

macro_rules! err_type {
    ($ty:ty) => {
        $ty
    };
    () => {
        ParserError
    };
}

macro_rules! keep_type {
    ($ty:ty) => {
        Keep
    };
    () => {
        Ignore
    };
}

macro_rules! map_or_try_map {
    ($err_typ:ty | $parser:expr ; $names_pat:pat => $block:block) => {
        $parser.try_map(|$names_pat| $block)
    };
    (| $parser:expr ; $names_pat:pat => $block:block) => {
        $parser.map(|$names_pat| $block)
    };
}

macro_rules! parser_fn {
    ($vis:vis $name:ident ($( $(@ $match_name:ident =)? $parser:expr),*) -> $ret:ty $(, $err_ret:ty)? $block:block) => {
        #[allow(non_camel_case_types)]
        struct $name;

        impl<E> Parser<E, ()> for $name where E: From<err_type!($($err_ret)?)> {
            type Output<'a> = $ret;
            type Kind = Keep;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: &mut (),
            ) -> ParserResult<$ret> {
                let (__len, val) = Parser::<err_type!($($err_ret)?), _>::parse(&mut map_or_try_map!(
                    $($err_ret)? |
                    p!($(not_drop!($parser $(, $match_name)?)),*) ;
                    names_pattern!($($($match_name ,)?)*) => $block
                ), input, &|e, r| errs.error(e, r), ctx)?;
                Some((__len, val))
            }
        }
    };
    ($vis:vis $name:ident ($( $parser:expr),*) $(-> $ret:ty $(, $err_ret:ty)?)?) => {
        #[allow(non_camel_case_types)]
        $vis struct $name;

        impl<E> Parser<E, ()> for $name where E: From<err_type!($($($err_ret)?)?)> {
            type Output<'a> = ret_type!($($ret)?);
            type Kind = keep_type!($($ret)?);

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: &mut (),
            ) -> ParserResult<ret_type!($($ret)?)> {
                let (__len, val) = Parser::<err_type!($($($err_ret)?)?), _>::parse(&mut p!($($parser),*), input, &|e, r| errs.error(e, r), ctx)?;
                Some((__len, val))
            }
        }
    }
}

macro_rules! parser_fns {
    ($($vis:vis $name:ident ($($tt:tt)*) $(-> $ret:ty $(, $err_ret:ty)?)? $($block:block)? ;)* ) => { $( parser_fn!( $vis $name ( $($tt)* ) $(-> $ret $(, $err_ret)?)? $($block)? ); )+ };
}

macro_rules! pmatch {
    ( $( $($(@ $match_name:ident =)? $parser:expr),* => $val:expr ),+ $(,)?) => {
        parsers_choice!( $( p!( $(not_drop!($parser $(, $match_name)?)),* ).map(|names_pattern!( $($($match_name,)?)? )| { $val } ) ),* )
    };
}

fn operate(l: i64, o: Operation, r: i64) -> i64 {
    match o {
        Operation::Add => l + r,
        Operation::Mul => l - r,
        Operation::Div => l / r,
        Operation::Sub => l * r,
    }
}

enum Operation {
    Add,
    Mul,
    Div,
    Sub,
}

parser_fns! {
    sep(char::is_whitespace.drop().rep(Ignore).opt());

    op(pmatch! {
        "+" => Operation::Add,
        "-" => Operation::Sub,
        "/" => Operation::Div,
        "*" => Operation::Mul,
    }) -> Operation;

    add(mul.delim_by(['+', '-'].lookahead().then(op).pad(sep), lfold(operate))) -> i64;
    mul(term.delim_by(['*', '/'].lookahead().then(op).pad(sep), lfold(operate))) -> i64;
    neg('-', sep, term.map(|i| -i)) -> i64;
    term(neg.or(Int).or(expr.pad(sep).wrapped("(", ")"))) -> i64;

    pub expr(add) -> i64;
}

#[derive(Debug, PartialEq)]
pub enum JSONValue {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    List(Vec<JSONValue>),
    Map(HashMap<String, JSONValue>),
}

parser_fns! {
    StrChar((|c: char| c != '"' && c != '\\')) -> char;

    EscapeSeq('\\', |_| true) -> char;

    Str('"', @s=EscapeSeq.or(StrChar).rep(Ignore).slice(), '"') -> String { s.to_string() };

    Null("null") -> JSONValue { JSONValue::Null };

    Digits(('0'..='9').rep(Ignore));

    Float(@i=p!("-".opt(), Digits, ".".then(Digits)).slice()) -> JSONValue { JSONValue::Float(i.parse().unwrap()) };

    Bool(pmatch!{
        "true" => JSONValue::Bool(true),
        "false" => JSONValue::Bool(false),
    }) -> JSONValue;

    List(Value.delim_by(",".pad(sep), ToVec).pad(sep).wrapped("[", "]").map(JSONValue::List)) -> JSONValue;

    MapEntry(Str.then(":".pad(sep)).then(Value)) -> (String, JSONValue);
    Map(MapEntry.delim_by(",".pad(sep), collect()).map(JSONValue::Map).pad(sep).wrapped("{", "}")) -> JSONValue;

    pub Value(List.or(Map).or(Bool).or(Float).or(Int.map(JSONValue::Int)).or(Null).or(Str.map(JSONValue::String))) -> JSONValue;
}

enum MyError {
    Int(ParseIntError),
    Parse(ParserError),
}

impl From<ParseIntError> for MyError {
    fn from(value: ParseIntError) -> Self {
        MyError::Int(value)
    }
}

impl From<ParserError> for MyError {
    fn from(value: ParserError) -> Self {
        MyError::Parse(value)
    }
}

parser_fns! {
    Int3("-".opt().then(Digits).slice().try_map(str::parse)) -> i64, MyError;
    Thing("") -> (), MyError;
}

pub type MatchResult<T, E> = Result<T, Option<ErrorLocation<E>>>;

#[test]
fn thing() {
    let input = "[1, 2, 3, 4]";
    let val: MatchResult<JSONValue, ParserError> = Value.try_match(input);
    println!("{:?}", val.unwrap());
}
