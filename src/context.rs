use std::{
    cell::{Cell, RefCell},
    ops::{Deref, DerefMut, Range},
};

use crate::{result::ParserResult, AppendCell};

#[derive(Debug)]
pub enum RecoverySignal<E> {
    Reset(usize),
    Error(Range<usize>, E),
}

/// The input for all parsers. Stores the initial [&str] input and an index for the parsing head.
pub struct ParserContext<'p, C, E> {
    cur: Cell<usize>,
    data: RefCell<C>,
    pub recovered: AppendCell<(Range<usize>, E)>,
    pub input: &'p str,
}

impl<'p, C, E> ParserContext<'p, C, E> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: 0.into(),
            data: RefCell::new(data),
            recovered: Default::default(),
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
    pub fn result<T>(&self, success: Option<T>, error: Option<E>) -> ParserResult<T, E> {
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

    /// Add a recovered error.
    pub fn add_recovered_error(&self, error: E, error_range: Range<usize>) {
        self.recovered.append((error_range, error));
    }

    /// Take the errors which were recovered from while parsing.
    pub fn take_recovered_errors(&mut self) -> Vec<(Range<usize>, E)> {
        std::mem::take(&mut self.recovered).into_inner()
    }

    /// Get the number of errors which have been recovered from this parser.
    /// This is meant for internal use only, but has to be exposed since it is used by macros.
    pub fn recovered_count(&self) -> usize {
        self.recovered.len()
    }

    /// Remove all recovered errors after a given number.
    /// This is meant for internal use only, but has to be exposed since it is used by macros.
    pub fn truncate_recovered(&self, len: usize) -> Vec<(Range<usize>, E)> {
        self.recovered.truncate(len)
    }

    /// Get the ending position of the last recovered error, falling back to the current position
    /// if no errors have been recovered.
    pub fn last_recovered_end(&self) -> usize {
        self.recovered
            .inspect_last(|(pos, _err)| pos.end)
            .unwrap_or(self.cursor())
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
