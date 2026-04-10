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

pub trait Parser<T, Err, Ctx = ()> {
    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<Err>,
        ctx: &RefCell<Ctx>,
    ) -> ParserResult<T>;

    fn repeat(self) -> impl Parser<Vec<T>, Err, Ctx>
    where
        Self: Sized,
    {
        struct Repeat<P> {
            p: P,
        }

        impl<P, T2, E, C> Parser<Vec<T2>, E, C> for Repeat<P>
        where
            P: Parser<T2, E, C>,
        {
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: Context<C>,
            ) -> ParserResult<Vec<T2>> {
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

    fn or<P>(self, other: P) -> impl Parser<T, Err, Ctx>
    where
        Self: Sized,
        P: Parser<T, Err, Ctx>,
    {
        struct Or<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, T, E, C> Parser<T, E, C> for Or<P1, P2>
        where
            P1: Parser<T, E, C>,
            P2: Parser<T, E, C>,
        {
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<T> {
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

    fn map<F, V>(self, f: F) -> impl Parser<V, Err, Ctx>
    where
        Self: Sized,
        F: Fn(T) -> V,
    {
        struct Map<P, F, T> {
            p: P,
            f: F,
            phantom: PhantomData<T>,
        }

        impl<P, F, T, V, E, C> Parser<V, E, C> for Map<P, F, T>
        where
            P: Parser<T, E, C>,
            F: Fn(T) -> V,
        {
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

        Map {
            p: self,
            f,
            phantom: PhantomData,
        }
    }

    fn then<P, V>(self, other: P) -> impl Parser<(T, V), Err, Ctx>
    where
        Self: Sized,
        P: Parser<V, Err, Ctx>,
    {
        struct Then<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, T, V, E, C> Parser<(T, V), E, C> for Then<P1, P2>
        where
            P1: Parser<T, E, C>,
            P2: Parser<V, E, C>,
        {
            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<E>,
                ctx: &RefCell<C>,
            ) -> ParserResult<(T, V)> {
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
                Some((l_len + r_len, (l_val, r_val)))
            }
        }

        Then { l: self, r: other }
    }
}

fn lit(lit: &'static str, parser_name: &'static str) -> impl Parser<(), ParserError> {
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }
    impl Parser<(), ParserError> for LitParser {
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

#[test]
fn thing() {
    let input = "hellohihello!".into();
    let ctx = RefCell::new(());
    let output = lit("hi", "hi")
        .or(lit("hello", "hello"))
        .repeat()
        .then(lit("!", "!").repeat())
        .map(|_| "test")
        .parse(input, DebugErrorHandler, &ctx);
    println!("{output:?}");
    panic!();
}
