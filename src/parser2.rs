use std::{
    cell::{RefCell, UnsafeCell},
    fmt::Debug,
    marker::PhantomData,
};

use crate::ParserError;

#[derive(Clone, Copy)]
pub struct Input<'a> {
    src: &'a str,
    cur: usize,
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
    fn error(&self, err: impl Into<E>);
}

impl<F, E> ErrorHandler<E> for &F
where
    F: Fn(E),
{
    fn error(&self, err: impl Into<E>) {
        self(err.into())
    }
}

#[derive(Clone)]
struct DebugErrorHandler;

impl<E> ErrorHandler<E> for DebugErrorHandler
where
    E: Debug,
{
    fn error(&self, err: impl Into<E>) {
        let err = err.into();
        eprintln!("Error: {err:?}");
    }
}

struct ErrorCell<E> {
    inner: UnsafeCell<Option<E>>,
}

impl<E> Default for ErrorCell<E> {
    fn default() -> Self {
        Self { inner: None.into() }
    }
}

impl<E> ErrorHandler<E> for &ErrorCell<E> {
    fn error(&self, err: impl Into<E>) {
        unsafe {
            let inner = self.inner.get();
            (*inner).replace(err.into());
        }
    }
}

impl<E> ErrorCell<E> {
    fn into_inner(self) -> Option<E> {
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

pub trait Parser<T> {
    type Err: Debug;
    type Ctx;

    fn parse(
        &mut self,
        input: Input,
        errs: impl ErrorHandler<Self::Err>,
        ctx: &RefCell<Self::Ctx>,
    ) -> ParserResult<T>;

    fn repeat(self) -> impl Parser<Vec<T>, Err = Self::Err, Ctx = Self::Ctx>
    where
        Self: Sized,
    {
        struct Repeat<P> {
            p: P,
        }

        impl<P, T2> Parser<Vec<T2>> for Repeat<P>
        where
            P: Parser<T2>,
        {
            type Err = P::Err;
            type Ctx = P::Ctx;

            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<Self::Err>,
                ctx: Context<Self::Ctx>,
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

    fn or<P>(self, other: P) -> impl Parser<T, Ctx = Self::Ctx, Err = Self::Err>
    where
        Self: Sized,
        P: Parser<T, Ctx = Self::Ctx, Err = Self::Err>,
    {
        struct Or<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, T> Parser<T> for Or<P1, P2>
        where
            P1: Parser<T, Err = P2::Err, Ctx = P2::Ctx>,
            P2: Parser<T>,
        {
            type Err = P1::Err;
            type Ctx = P1::Ctx;

            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<Self::Err>,
                ctx: &RefCell<Self::Ctx>,
            ) -> ParserResult<T> {
                let err = ErrorCell::default();
                let parsed = self.l.parse(input, &err, ctx);
                if parsed.is_some() {
                    return parsed;
                }
                match self.r.parse(input, &err, ctx) {
                    Some(val) => Some(val),
                    None => {
                        if let Some(err) = err.into_inner() {
                            errs.error(err);
                        }
                        None
                    }
                }
            }
        }

        Or { l: self, r: other }
    }

    fn map<F, V>(self, f: F) -> impl Parser<V, Ctx = Self::Ctx, Err = Self::Err>
    where
        Self: Sized,
        F: Fn(T) -> V,
    {
        struct Map<P, F, T> {
            p: P,
            f: F,
            phantom: PhantomData<T>,
        }

        impl<P, F, T, V> Parser<V> for Map<P, F, T>
        where
            P: Parser<T>,
            F: Fn(T) -> V,
        {
            type Err = P::Err;
            type Ctx = P::Ctx;

            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<Self::Err>,
                ctx: &RefCell<Self::Ctx>,
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

    fn then<P, V>(self, other: P) -> impl Parser<(T, V), Ctx = Self::Ctx, Err = Self::Err>
    where
        Self: Sized,
        P: Parser<V, Ctx = Self::Ctx, Err = Self::Err>,
    {
        struct Then<P1, P2> {
            l: P1,
            r: P2,
        }

        impl<P1, P2, T, V> Parser<(T, V)> for Then<P1, P2>
        where
            P1: Parser<T, Err = P2::Err, Ctx = P2::Ctx>,
            P2: Parser<V>,
        {
            type Err = P1::Err;
            type Ctx = P1::Ctx;

            fn parse(
                &mut self,
                input: Input,
                errs: impl ErrorHandler<Self::Err>,
                ctx: &RefCell<Self::Ctx>,
            ) -> ParserResult<(T, V)> {
                let (l_len, l_val) = self.l.parse(input, errs.clone(), ctx)?;
                let (r_len, r_val) = self.r.parse(input.skip(l_len), errs, ctx)?;
                Some((l_len + r_len, (l_val, r_val)))
            }
        }

        Then { l: self, r: other }
    }
}

fn lit(
    lit: &'static str,
    parser_name: &'static str,
) -> impl Parser<(), Err = ParserError, Ctx = ()> {
    struct LitParser {
        lit: &'static str,
        parser_name: &'static str,
    }
    impl Parser<()> for LitParser {
        type Err = ParserError;
        type Ctx = ();
        fn parse(
            &mut self,
            input: Input,
            errs: impl ErrorHandler<Self::Err>,
            _ctx: Context<Self::Ctx>,
        ) -> ParserResult<()> {
            if input.src[input.cur..].starts_with(self.lit) {
                Some((self.lit.len(), ()))
            } else {
                errs.error(ParserError::ExpectedLiteral(self.lit, self.parser_name));
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
