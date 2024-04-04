use std::{
    cell::{Cell, RefCell},
    ops::{Deref, DerefMut},
};

use crate::result::ParserResult;

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

    pub fn slice(&self) -> &str {
        &self.input[self.cur.get()..]
    }

    pub fn line(&self) -> usize {
        line(&self.input[..self.cursor()])
    }

    pub fn col(&self) -> usize {
        col(&self.input[..self.cursor()])
    }

    pub fn result<T, E>(&self, success: Option<T>, error: Option<E>) -> ParserResult<T, E> {
        ParserResult::new(success, error, self.cursor())
    }

    pub fn advance(&self, bytes: usize) {
        self.cur.set(self.input.len().min(self.cur.get() + bytes));
    }

    pub fn reset(&self, bytes: usize) {
        self.cur.set(bytes);
    }

    pub fn cursor(&self) -> usize {
        self.cur.get()
    }

    pub fn data(&self) -> impl Deref<Target = C> + '_ {
        self.data.borrow()
    }

    pub fn data_mut(&self) -> impl DerefMut<Target = C> + '_ {
        self.data.borrow_mut()
    }
}

pub fn line(s: &str) -> usize {
    s.lines().count()
}

pub fn col(s: &str) -> usize {
    s.chars().rev().take_while(|c| *c != '\n').count()
}
