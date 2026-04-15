use std::{
    cell::{RefCell, UnsafeCell},
    fmt::Debug,
    marker::PhantomData,
    ops::Range,
};

use crate::ParserError;

#[derive(Clone, Copy)]
pub struct Input<'a> {
    src: &'a str,
    cur: usize,
}

impl Input<'_> {
    fn slice(&self) -> &str {
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
struct DebugErrorHandler;

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
struct ErrorLocation<E>(E, Range<usize>);

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

fn parser<P, T, C, E, I>()
where
    P: FnMut(Input, &RefCell<C>) -> Result<(usize, T), I>,
    I: IntoIterator<Item = E>,
{
}

type ParserResult<T> = Option<(usize, T)>;
type Context<'a, T> = &'a RefCell<T>;

trait Chain {
    type Output<A, B>;
    type NextKind;

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B>;
}

struct Ignore;
struct Keep;

struct ChainImpl<L, R> {
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

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B> {
        a
    }
}

impl Chain for ChainImpl<Ignore, Keep> {
    type Output<A, B> = B;
    type NextKind = Keep;

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B> {
        b
    }
}

impl Chain for ChainImpl<Ignore, Ignore> {
    type Output<A, B> = ();
    type NextKind = Ignore;

    fn chain<A, B>(a: A, b: B) -> Self::Output<A, B> {
        ()
    }
}

pub trait Parser<Err, Ctx = ()> {
    type Output;
    type Kind;

    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<Err>,
        ctx: &RefCell<Ctx>,
    ) -> ParserResult<Self::Output>;

    fn repeat(self) -> impl Parser<Err, Ctx, Output = Vec<Self::Output>, Kind = Self::Kind>
    where
        Self: Sized,
    {
        struct Repeat<P> {
            p: P,
        }

        impl<P, E, C> Parser<E, C> for Repeat<P>
        where
            P: Parser<E, C>,
        {
            type Kind = P::Kind;
            type Output = Vec<P::Output>;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output> {
                let (mut offset, first) = self.p.parse(input, errs.clone(), ctx)?;
                let mut elems = vec![first];
                while let Some((len, elem)) = self.p.parse(input.skip(offset), errs.clone(), ctx) {
                    offset += len;
                    elems.push(elem);
                }
                Some((offset, elems))
            }
        }

        Repeat { p: self }
    }

    fn or<P>(self, other: P) -> impl Parser<Err, Ctx, Output = Self::Output, Kind = Self::Kind>
    where
        Self: Sized,
        P: Parser<Err, Ctx, Output = Self::Output>,
    {
        struct Or<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, E, C> Parser<E, C> for Or<P1, P2>
        where
            P1: Parser<E, C, Output = P2::Output>,
            P2: Parser<E, C>,
        {
            type Kind = P1::Kind;
            type Output = P1::Output;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<P1::Output> {
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

    fn map<F, V>(self, f: F) -> impl Parser<Err, Ctx, Output = V, Kind = Self::Kind>
    where
        Self: Sized,
        F: Fn(Self::Output) -> V,
    {
        struct Map<P, F> {
            p: P,
            f: F,
        }

        impl<P, F, V, E, C> Parser<E, C> for Map<P, F>
        where
            P: Parser<E, C>,
            F: Fn(P::Output) -> V,
        {
            type Kind = P::Kind;
            type Output = V;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<V> {
                self.p
                    .parse(input, errs, ctx)
                    .map(|(len, val)| (len, (self.f)(val)))
            }
        }

        Map { p: self, f }
    }

    fn drop(self) -> impl Parser<Err, Ctx, Output = Self::Output, Kind = Ignore>
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
            type Output = P::Output;
            type Kind = Ignore;

            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<Self::Output> {
                self.p.parse(input, errs, ctx)
            }
        }
        Drop { p: self }
    }

    fn then<P>(
        self,
        other: P,
    ) -> impl Parser<
        Err,
        Ctx,
        Output = <ChainImpl<Self::Kind, P::Kind> as Chain>::Output<Self::Output, P::Output>,
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
            type Output = <ChainImpl<P1::Kind, P2::Kind> as Chain>::Output<P1::Output, P2::Output>;
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<Self::Output> {
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
}

fn lit(
    lit: &'static str,
    parser_name: &'static str,
) -> impl Parser<ParserError, Output = (), Kind = Ignore> {
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }
    impl Parser<ParserError> for LitParser {
        type Kind = Ignore;
        type Output = ();
        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<ParserError>,
            _ctx: Context<()>,
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
    LitParser { lit, parser_name }
}

fn char_filter(
    filter: impl Fn(char) -> bool,
    parser_name: &'static str,
) -> impl Parser<ParserError, Output = char, Kind = Keep> {
    struct Filter<F> {
        f: F,
        parser_name: &'static str,
    }
    impl<F, C> Parser<ParserError, C> for Filter<F>
    where
        F: Fn(char) -> bool,
    {
        type Output = char;
        type Kind = Keep;

        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<ParserError>,
            ctx: &RefCell<C>,
        ) -> ParserResult<Self::Output> {
            let next_char = input.slice().chars().next();
            if let Some(c) = next_char
                && (self.f)(c)
            {
                todo!()
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

struct int;
impl Parser<ParserError, ()> for int {
    type Output = i32;
    type Kind = Keep;
    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<ParserError>,
        ctx: &RefCell<()>,
    ) -> ParserResult<Self::Output> {
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

struct greeting;
impl Parser<ParserError, ()> for greeting {
    type Kind = Keep;
    type Output = ();
    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<ParserError>,
        ctx: &RefCell<()>,
    ) -> ParserResult<()> {
        todo!()
    }
}

#[test]
fn thing() {
    let input = "a1a2".into();
    let ctx = RefCell::new(());
    let output =
        lit("a", "a")
            .then(int)
            .then(lit("a", "a"))
            .then(int)
            .parse(input, DebugErrorHandler, &ctx);
    println!("{output:?}");
    panic!();
}
