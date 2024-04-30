use std::{
    cell::{Cell, RefCell, UnsafeCell},
    fmt::Display,
    ops::{Deref, DerefMut, Range},
};

use crate::{
    pretty::{pretty_error, PrettyOptions},
    AppendCell,
};

#[derive(Debug)]
pub enum RecoverySignal<E> {
    Reset(usize),
    Error(Range<usize>, E),
}

/// The input for all parsers. Stores the initial [&str] input and an index for the parsing head.
pub struct ParserContext<'p, C, E> {
    /// The current position in the input
    cur: Cell<usize>,
    /// The user-provided custom context
    data: RefCell<C>,
    /// The "best" error encountered so far
    err: UnsafeCell<Option<E>>,
    /// The position of the best error
    deepest_err: Cell<usize>,
    /// The starting position of the best error, used to generate error ranges
    err_start: Cell<Option<usize>>,
    /// The priority of the best error, usually 1
    err_priority: Cell<usize>,
    /// The priority to use for any error being sent, usually 1
    new_err_priority: Cell<usize>,
    /// Controls whether new errors should be accepted, used to prevent errors from surfacing while searching for a recovery pattern
    errs_locked: Cell<bool>,
    /// The symbol forming a boundary for any recovery operation, usually a closing delimiter
    pub(crate) recover_terminator: Cell<Option<&'static str>>,
    /// Errors which have been recovered
    pub recovered: AppendCell<(Range<usize>, E)>,
    /// The parser input
    pub input: &'p str,
}

impl<'p, C, E> ParserContext<'p, C, E> {
    pub fn new(input: &'p str, data: C) -> Self {
        ParserContext {
            cur: 0.into(),
            data: RefCell::new(data),
            recovered: Default::default(),
            input,
            recover_terminator: None.into(),
            err: None.into(),
            err_priority: 0.into(),
            new_err_priority: 1.into(),
            errs_locked: false.into(),
            deepest_err: 0.into(),
            err_start: None.into(),
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

    pub(crate) fn lock_errors(&self) -> ErrsLock<C, E> {
        self.errs_locked.set(true);
        ErrsLock { ctx: &self }
    }

    /// Convert the outcome of the parsing operation into a `Result`. If any errors were
    /// recovered, or if the entire input was not consumed, parsing will be considered
    /// to have failed. All errors will be collected into a list that includes their position.
    pub fn result<T>(&mut self, success: Option<T>) -> Result<T, Vec<(Range<usize>, E)>> {
        if self.deepest_err_pos().max(self.deepest_recovered_err_pos()) > self.cursor()
            || self.cursor() < self.input.len()
            || self.recovered_count() > 0
        {
            return Err(self.take_errs());
        }
        let Some(val) = success else {
            return Err(self.take_errs());
        };
        Ok(val)
    }

    /// Convert the output of the parsing operation into a `Result` with prettified error messages.
    /// If any errors were recovered, or if the entire input was not consumed, parsing will be
    /// considered to have failed.
    pub fn pretty_result<T>(
        &mut self,
        success: Option<T>,
        options: PrettyOptions,
    ) -> Result<T, String>
    where
        E: Display,
    {
        self.result(success).map_err(|e| {
            let messages: Vec<_> = e
                .into_iter()
                .map(|(pos, err)| pretty_error(self.input, pos, err.to_string(), &options))
                .collect();

            messages.join(options.error_separator)
        })
    }

    /// Take all the errors (primary error and recovered errors) in the context.
    pub fn take_errs(&mut self) -> Vec<(Range<usize>, E)> {
        let mut errs = vec![];
        if let Some(err) = unsafe { std::mem::take(&mut *self.err.get()) } {
            errs.push((self.err_range(), err));
        }
        errs.extend(self.take_recovered_errors());
        errs.sort_by_key(|(range, _err)| range.start);
        errs
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

    /// Set the priority to be used for new errors. Intended for internal use.
    pub fn set_err_priority(&self, priority: usize) {
        self.new_err_priority.set(priority);
    }

    /// Get the priority to be used for new errors. Intended for internal use.
    pub fn get_err_priority(&self) -> usize {
        self.new_err_priority.get()
    }

    /// Overwrite the error stored in the context, if the position is further or priority
    /// higher than the existing error.
    #[inline(always)]
    pub fn err(&self, error: E) {
        if self.errs_locked.get() {
            return;
        }
        if self.cursor() > self.deepest_err.get()
            || (self.cursor() == self.deepest_err.get()
                && self.new_err_priority.get() > self.err_priority.get())
        {
            unsafe { *self.err.get() = Some(error) }
            self.deepest_err.set(self.cursor());
            self.err_start.set(None);
            self.err_priority.set(self.new_err_priority.get());
        }
    }

    /// Overwrite the error stored in the context as long as it is at least as deep as
    /// the existing error.
    #[inline(always)]
    pub fn replace_err(&self, error: E) {
        if self.errs_locked.get() {
            return;
        }
        if self.cursor() > self.deepest_err.get()
            || (self.cursor() == self.deepest_err.get()
                && self.new_err_priority.get() >= self.err_priority.get())
        {
            unsafe { *self.err.get() = Some(error) }
            self.deepest_err.set(self.cursor());
            self.err_start.set(None);
            self.err_priority.set(self.new_err_priority.get());
        }
    }

    /// Get the position of the deepest error encountered while parsing so far.
    pub fn deepest_err_pos(&self) -> usize {
        self.deepest_err.get()
    }

    /// Get the span of the deepest error encountered while parsing so far.
    pub fn err_range(&self) -> Range<usize> {
        self.err_start.get().unwrap_or(self.deepest_err.get())..self.deepest_err.get()
    }

    /// Set the starting position of the current error, if it has not been set already.
    #[inline(always)]
    pub fn set_err_start(&self, start: usize) {
        if self.err_start.get().is_none() {
            self.err_start.set(Some(start));
        }
    }

    /// Take the primary error encountered during parsing.
    pub fn take_err(&self) -> Option<E> {
        self.err_priority.set(0);
        unsafe { std::mem::take(&mut *self.err.get()) }
    }

    /// Move the primary error into the recovered errors list.
    pub fn recover_err(&self) {
        if let Some(err) = unsafe { std::mem::take(&mut *self.err.get()) } {
            let range = self.err_range();
            self.deepest_err.set(self.cursor());
            self.err_start.set(None);
            self.recovered.append((range, err));
            self.err_priority.set(0);
        }
    }

    /// Add an error to the recovered errors list.
    pub fn add_recovered_err(&self, range: Range<usize>, err: E) {
        self.recovered.append((range, err));
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
    pub fn deepest_recovered_err_pos(&self) -> usize {
        self.recovered
            .inspect_last(|(pos, _err)| pos.end)
            .unwrap_or(self.cursor())
    }
}

pub struct ErrsLock<'a, C, E> {
    ctx: &'a ParserContext<'a, C, E>,
}

impl<'a, C, E> Drop for ErrsLock<'a, C, E> {
    fn drop(&mut self) {
        self.ctx.errs_locked.set(false);
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
