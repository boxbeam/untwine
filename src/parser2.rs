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
    type ChainLeft<A, B>;
    type ChainRight<A, B>;
    fn chain_left<A, B>(a: A, b: B) -> Self::ChainLeft<A, B>;
    fn chain_right<A, B>(a: A, b: B) -> Self::ChainRight<A, B>;
}

struct Ignore {}

impl Chain for Ignore {
    type ChainLeft<A, B> = B;
    type ChainRight<A, B> = A;

    fn chain_left<A, B>(a: A, b: B) -> Self::ChainLeft<A, B> {
        b
    }

    fn chain_right<A, B>(a: A, b: B) -> Self::ChainRight<A, B> {
        a
    }
}

struct Combine;

impl Chain for Combine {
    type ChainLeft<A, B> = (A, B);
    type ChainRight<A, B> = (A, B);

    fn chain_left<A, B>(a: A, b: B) -> Self::ChainLeft<A, B> {
        (a, b)
    }

    fn chain_right<A, B>(a: A, b: B) -> Self::ChainRight<A, B> {
        (a, b)
    }
}

pub trait Parser<Err, Ctx = ()> {
    type Output;
    type Chain: Chain;

    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<Err>,
        ctx: &RefCell<Ctx>,
    ) -> ParserResult<Self::Output>;

    fn repeat(self) -> impl Parser<Err, Ctx, Output = Vec<Self::Output>, Chain = Self::Chain>
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
            type Chain = P::Chain;
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

    fn or<P>(self, other: P) -> impl Parser<Err, Ctx, Output = Self::Output, Chain = Self::Chain>
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
            type Chain = P1::Chain;
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

    fn map<F, V>(self, f: F) -> impl Parser<Err, Ctx, Output = V, Chain = Self::Chain>
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
            type Chain = P::Chain;
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

    fn drop(self) -> impl Parser<Err, Ctx, Output = Self::Output, Chain = Ignore>
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
            type Chain = Ignore;

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
        Output = <Self::Chain as Chain>::ChainLeft<Self::Output, P::Output>,
        Chain = P::Chain,
    >
    where
        Self: Sized,
        P: Parser<Err, Ctx>,
    {
        struct Then<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, E, C> Parser<E, C> for Then<P1, P2>
        where
            P1: Parser<E, C>,
            P2: Parser<E, C>,
        {
            type Chain = P2::Chain;
            type Output = <P1::Chain as Chain>::ChainLeft<P1::Output, P2::Output>;
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
                let output = P1::Chain::chain_left(l_val, r_val);
                Some((l_len + r_len, output))
            }
        }

        Then { l: self, r: other }
    }
}

fn lit(
    lit: &'static str,
    parser_name: &'static str,
) -> impl Parser<ParserError, Output = (), Chain = Ignore> {
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }
    impl Parser<ParserError> for LitParser {
        type Chain = Ignore;
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
) -> impl Parser<ParserError, Output = char, Chain = Combine> {
    struct Filter<F> {
        f: F,
        parser_name: &'static str,
    }
    impl<F, C> Parser<ParserError, C> for Filter<F>
    where
        F: Fn(char) -> bool,
    {
        type Output = char;
        type Chain = Combine;

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
    type Chain = Combine;
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
    type Chain = Combine;
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
    let input = "ab".into();
    let ctx = RefCell::new(());
    let output = lit("a", "a")
        .then(greeting)
        .parse(input, DebugErrorHandler, &ctx);
    println!("{output:?}");
    panic!();
}
