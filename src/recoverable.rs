use std::{collections::HashMap, ops::Range};

/// A type which can be recovered from when parsing, by generating a default error value.
pub trait Recoverable {
    /// Create an error value using the span of the error.
    fn error_value(pos: Range<usize>) -> Self;
}

macro_rules! impl_recoverable_tuple {
    ($($ty:ident),*) => {
        impl<$($ty: Recoverable),*> Recoverable for ($($ty),*) {
            #[allow(unused)]
            fn error_value(pos: Range<usize>) -> Self {
                ( $($ty::error_value(pos.clone())),* )
            }
        }
    }
}

impl_recoverable_tuple!();
impl_recoverable_tuple!(A, B);
impl_recoverable_tuple!(A, B, C);
impl_recoverable_tuple!(A, B, C, D);
impl_recoverable_tuple!(A, B, C, D, E);
impl_recoverable_tuple!(A, B, C, D, E, F);
impl_recoverable_tuple!(A, B, C, D, E, F, G);

impl<A> Recoverable for (A,)
where
    A: Recoverable,
{
    fn error_value(pos: Range<usize>) -> Self {
        (A::error_value(pos),)
    }
}

impl Recoverable for String {
    fn error_value(pos: Range<usize>) -> Self {
        format!("Error {pos:?}")
    }
}

impl<T> Recoverable for Vec<T> {
    fn error_value(_pos: Range<usize>) -> Self {
        vec![]
    }
}

impl Recoverable for Range<usize> {
    fn error_value(pos: Range<usize>) -> Self {
        pos
    }
}

impl<K, V> Recoverable for HashMap<K, V> {
    fn error_value(_pos: Range<usize>) -> Self {
        Default::default()
    }
}

macro_rules! impl_recoverable_num {
    ($ty:ident) => {
        impl Recoverable for $ty {
            fn error_value(_pos: Range<usize>) -> Self {
                0
            }
        }
    };
}

impl_recoverable_num!(u8);
impl_recoverable_num!(u16);
impl_recoverable_num!(u32);
impl_recoverable_num!(u64);
impl_recoverable_num!(u128);
impl_recoverable_num!(i8);
impl_recoverable_num!(i16);
impl_recoverable_num!(i32);
impl_recoverable_num!(i64);
impl_recoverable_num!(i128);

impl Recoverable for f32 {
    fn error_value(_pos: Range<usize>) -> Self {
        0.0
    }
}

impl Recoverable for f64 {
    fn error_value(_pos: Range<usize>) -> Self {
        0.0
    }
}

impl<T> Recoverable for Option<T> {
    fn error_value(_pos: Range<usize>) -> Self {
        None
    }
}
