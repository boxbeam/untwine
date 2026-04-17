use std::{cell::UnsafeCell, fmt::Debug, marker::PhantomData, ops::Range};

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

pub struct Nop;

impl<E, C> Parser<E, C> for Nop {
    type Output<'a> = ();
    type Kind = Ignore;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        errs: impl ErrorHandler<E>,
        ctx: Context<C>,
    ) -> ParserResult<Self::Output<'a>> {
        Some((0, ()))
    }
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

type ParserResult<T> = Option<(usize, T)>;
type Context<'a, T> = &'a mut T;

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

    fn rep(
        self,
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Vec<Self::Output<'a>>, Kind = Self::Kind>
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
            type Output<'a> = Vec<P::Output<'a>>;
            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
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
    ) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Option<Self::Output<'a>>, Kind = Self::Kind>
    where
        Self: Sized,
    {
        struct Optional<P> {
            p: P,
        }
        impl<P, Err, Ctx> Parser<Err, Ctx> for Optional<P>
        where
            P: Parser<Err, Ctx>,
        {
            type Output<'a> = Option<P::Output<'a>>;
            type Kind = P::Kind;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<Err>,
                ctx: Context<Ctx>,
            ) -> ParserResult<Self::Output<'a>> {
                match self.p.parse(input, errs, ctx) {
                    Some((len, elem)) => Some((len, Some(elem))),
                    None => Some((0, None)),
                }
            }
        }

        Optional { p: self }
    }

    fn drop(self) -> impl for<'a> Parser<Err, Ctx, Output<'a> = Self::Output<'a>, Kind = Ignore>
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
            type Output<'a> = P::Output<'a>;
            type Kind = Ignore;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Self::Output<'a>> {
                self.p.parse(input, errs, ctx)
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
}

fn lit<E>(
    lit: &'static str,
    parser_name: &'static str,
) -> impl for<'a> Parser<E, Output<'a> = (), Kind = Ignore>
where
    E: From<ParserError>,
{
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }
    impl<E> Parser<E, ()> for LitParser
    where
        E: From<ParserError>,
    {
        type Kind = Ignore;
        type Output<'a> = ();
        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<E>,
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

fn char_filter<E>(
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
            ctx: Context<C>,
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

impl Parser<ParserError, ()> for &'static str {
    type Output<'b> = ();
    type Kind = Ignore;

    fn parse<'a>(
        &mut self,
        input: Input<'a>,
        errs: impl ErrorHandler<ParserError>,
        ctx: Context<()>,
    ) -> ParserResult<Self::Output<'a>> {
        lit(self, self).parse(input, errs, ctx)
    }
}

struct int;
impl Parser<ParserError, ()> for int {
    type Output<'a> = i32;
    type Kind = Keep;
    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<ParserError>,
        ctx: Context<()>,
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

impl<const N: usize> Parser<ParserError, ()> for [char; N] {
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

macro_rules! parsers_choice {
    ($val:expr) => {
        $val
    };
    ($val:expr $(, $($rest:expr),+ )?) => { $val.or(parsers_choice!( $( $($rest),+ )? )) };
}

#[macro_export]
macro_rules! parsers {
    ($val:expr) => {
        $val
    };
    ($val:expr $(, $($rest:expr),+ )?) => { $val.then(parsers!( $( $($rest),+ )? )) };
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

macro_rules! parser_fn {
    ($name:ident ($( $(@ $match_name:ident =)? $parser:expr),*) -> $ret:ty $block:block) => {
        #[allow(non_camel_case_types)]
        struct $name;

        impl Parser<ParserError, ()> for $name {
            type Output<'a> = $ret;
            type Kind = Keep;

            fn parse<'a>(
                &mut self,
                input: Input<'a>,
                errs: impl ErrorHandler<ParserError>,
                ctx: &mut (),
            ) -> ParserResult<$ret> {
                let (__len, names_pattern!($( $($match_name,)? )*)) = parsers!($(not_drop!($parser $(, $match_name)?)),*).parse(input, errs, ctx)?;
                Some((__len, $block))
            }
        }
    };
}

macro_rules! parser_fns {
    ($($name:ident ($($tt:tt)*) -> $ret:ty $block:block $(;)?)* ) => { $( parser_fn!( $name ( $($tt)* ) -> $ret $block ); )+ };
}

macro_rules! parser_match {
    ( $( $($(@ $match_name:ident =)? $parser:expr),* => $val:expr ),+ $(,)?) => {
        parsers_choice!( $( parsers!( $(not_drop!($parser $(, $match_name)?)),* ).map(|names_pattern!( $($($match_name,)?)? )| { $val } ) ),* )
    };
}

enum Op {
    Add,
    Sub,
    Div,
    Mul,
}

parser_fns! {
    sep(char_filter(|c| c.is_ascii_whitespace(), "whitespace")) -> () {}

    op(@o=parser_match! {
        "+" => Op::Add,
        "-" => Op::Sub,
        "/" => Op::Div,
        "*" => Op::Mul,
    }) -> Op {o}

    term(@t=int.or("(".then(expr).then(")"))) -> i32 {t}

    expr(@left=term, sep.opt(), @o=op, sep.opt(), @right=term) -> i32 {
        match o {
            Op::Add => left + right,
            Op::Sub => left - right,
            Op::Div => left / right,
            Op::Mul => left * right,
        }
    }
}

#[test]
fn thing() {
    let val = expr.try_match("1+(2*3)");
    println!("{val:?}");
    panic!();
}
