use std::{
    cell::{Cell, RefCell},
    ops::{Deref, DerefMut},
};

use crate::result::ParserResult;

/// The input for all parsers. Stores the initial [&str] input and an index for the parsing head.
pub struct ParserContext<'p, C> {
    cur: Cell<usize>,
    data: RefCell<C>,
    pub input: &'p str,
}

impl<'p, C> ParserContext<'p, C> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: Default::default(),
            data: RefCell::new(data),
            input,
        }
    }

    /// Get the portion of the input which has not yet been consumed.
    pub fn slice(&self) -> &str {
        &self.input[self.cur.get()..]
    }

    /// Get the line number of the parsing head.
    pub fn line(&self) -> usize {
        line(&self.input[..self.cursor()])
    }

    /// Get the column number of the parsing head.
    pub fn col(&self) -> usize {
        col(&self.input[..self.cursor()])
    }

    /// Create a [`ParserResult`] using the position of the parsing head.
    pub fn result<T, E>(&self, success: Option<T>, error: Option<E>) -> ParserResult<T, E> {
        ParserResult::new(success, error, self.cursor()..self.cursor())
    }

    /// Advance the parsing head by the given number of bytes.
    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().min(self.cur.get() + bytes));
    }

    /// Reset the parsing head to a specific byte index.
    pub fn reset(&self, bytes: usize) {
        self.cur.set(bytes);
    }

    /// Get the current index of the parsing head.
    pub fn cursor(&self) -> usize {
        self.cur.get()
    }

    /// Get an immutable reference to the inner custom context.
    pub fn data(&self) -> impl Deref<Target = C> + '_ {
        self.data.borrow()
    }

    /// Get a mutable reference to the inner custom context.
    pub fn data_mut(&self) -> impl DerefMut<Target = C> + '_ {
        self.data.borrow_mut()
    }
}

pub fn lines(s: &str) -> impl Iterator<Item = &str> {
    s.lines()
        .chain((s.ends_with('\n') || s.is_empty()).then_some(""))
}

/// Get the line count of a string.
pub fn line(s: &str) -> usize {
    lines(s).count()
}

/// Get the column index of the last line in a string.
pub fn col(s: &str) -> usize {
    s.chars().rev().take_while(|c| *c != '\n').count()
}
