use std::{
    cell::Cell,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

pub extern crate macros;

/// A static chunk of memory which you can split stacks off of.
/// Each stack remains mutable until you split another stack off of it,
/// and stacks can be of any type. Only one will be mutable at a time,
/// and you must drop the child of a stack before splitting it again.
pub struct AnyStack {
    stack: Box<[u8]>,
}

impl AnyStack {
    pub fn new(capacity: usize) -> Self {
        Self {
            stack: vec![0; capacity].into_boxed_slice(),
        }
    }

    /// Create the root stack
    pub fn stack<T>(&mut self) -> Stack<T> {
        Stack::new(NonNull::new(&mut *self.stack as *mut [u8]).unwrap())
    }
}

/// An untyped head of an [AnyStack] which can be split into a [Stack] of any type
pub struct AnySplit<'a> {
    mem: NonNull<[u8]>,
    parent_split: &'a Cell<bool>,
}

impl<'a> AnySplit<'a> {
    /// Split a typed stack off the [AnyStack] head.
    pub fn split<T>(self) -> Stack<'a, T> {
        let mut stack = Stack::new(self.mem);
        stack.parent_split = Some(self.parent_split);
        stack
    }
}

impl<'a> Drop for AnySplit<'a> {
    fn drop(&mut self) {
        self.parent_split.replace(false);
    }
}

/// The head of an [AnyStack]. Can be treated like a normal stack, dereferences to a slice,
/// and can be split to create a new stack of any type.
pub struct Stack<'a, T> {
    mem: NonNull<[u8]>,
    int: &'a mut [T],
    len: usize,
    skip_bytes: usize,
    split: Cell<bool>,
    parent_split: Option<&'a Cell<bool>>,
}

impl<'a, T> Deref for Stack<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.int[..self.len]
    }
}

impl<'a, T> DerefMut for Stack<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.int[..self.len]
    }
}

impl<'a, T> Stack<'a, T> {
    fn new(stack: NonNull<[u8]>) -> Self {
        // Safe because the stack pointer is known valid for 'a
        let (before, int, _) = unsafe { (*stack.as_ptr()).align_to_mut() };
        Stack {
            mem: stack,
            int,
            len: 0,
            skip_bytes: before.len(),
            split: Default::default(),
            parent_split: None,
        }
    }

    fn split_ptr(&self) -> NonNull<[u8]> {
        if self.split.get() {
            panic!("Stack was split from twice");
        }
        let pos = (std::mem::size_of::<T>() * self.len) + self.skip_bytes;
        // Safe because the [u8] is only stored behind a pointer instead of a reference
        // because the &mut [T] is an alias to the same data, but rust has no way of knowing that.
        // This just prevents it from "looking like" there are two mutable borrows of the same data.
        let mem = unsafe { NonNull::new_unchecked((*self.mem.as_ptr()).split_at_mut(pos).1) };
        mem
    }

    /// Split off another stack, making this one immutable.
    /// Cannot split the same stack twice unless the split-off child has been dropped.
    pub fn split<V>(&self) -> Stack<V> {
        let mem = self.split_ptr();
        let mut stack = Stack::new(mem);
        stack.parent_split = Some(&self.split);
        stack
    }

    /// Split off the head without any specific stack type, so it can be chosen by the receiver.
    pub fn split_any(&self) -> AnySplit {
        AnySplit {
            mem: self.split_ptr(),
            parent_split: &self.split,
        }
    }

    /// Push an element onto the stack.
    pub fn push(&mut self, val: T) {
        self.int[self.len] = val;
        self.len += 1;
    }

    /// Remove the top element from the stack.
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        // Safe because this data's destructor will never be run and it will never be accessed
        // again except to overwrite it with valid data
        let elem = unsafe {
            std::mem::replace(
                &mut self.int[self.len],
                std::mem::MaybeUninit::uninit().assume_init(),
            )
        };
        self.len -= 1;
        Some(elem)
    }
}

impl<'a, T> Drop for Stack<'a, T> {
    fn drop(&mut self) {
        for t in &mut **self {
            // Safe because this &mut [T] is constructed by casting a &mut [u8],
            // which means the destructors would not be run normally.
            // When this is dropped, we need to manually run the destructors.
            unsafe {
                std::ptr::drop_in_place(t as *mut _);
            }
        }
        if let Some(parent_split) = self.parent_split {
            parent_split.replace(false);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_universal_stack() {
        let mut a = AnyStack::new(1000);
        let mut stack: Stack<i32> = a.stack();
        stack.push(1);
        let mut stack_2 = stack.split();
        stack_2.push("Hello".to_string());

        assert_eq!(&*stack, &[1]);
        assert_eq!(&*stack_2, &["Hello"]);
    }

    #[test]
    #[should_panic]
    fn test_multi_split() {
        let mut stack = AnyStack::new(1000);
        let first = stack.stack::<i32>();
        let mut second = first.split::<i32>();
        let mut third = first.split::<i32>();
        second.push(1);
        third.push(1);
    }

    #[test]
    fn test_multi_split_scope() {
        let mut stack = AnyStack::new(1000);
        let first = stack.stack::<i32>();
        {
            let mut split = first.split::<i32>();
            split.push(1);
        }
        first.split::<i32>();
    }
}
