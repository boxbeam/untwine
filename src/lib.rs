use std::{
    cell::Cell,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

pub extern crate macros;

pub struct UniversalStack {
    stack: Box<[u8]>,
}

impl UniversalStack {
    pub fn new(capacity: usize) -> Self {
        Self {
            stack: vec![0; capacity].into_boxed_slice(),
        }
    }

    pub fn stack<T>(&mut self) -> Stack<T> {
        Stack::new(&mut *self.stack as *mut [u8])
    }
}

pub struct Stack<'a, T> {
    mem: *mut [u8],
    int: &'a mut [T],
    len: usize,
    phantom: PhantomData<T>,
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
    fn new(stack: *mut [u8]) -> Self {
        unsafe {
            let (_, int, _) = (*stack).align_to_mut();
            Stack {
                mem: stack,
                int,
                len: 0,
                phantom: PhantomData,
                split: Default::default(),
                parent_split: None,
            }
        }
    }

    pub fn split<V>(&self) -> Stack<V> {
        if self.split.get() {
            panic!("Stack was split from twice");
        }
        self.split.replace(true);
        let pos = (std::mem::size_of::<T>() * self.len) + std::mem::align_of::<T>();
        let mem = unsafe { (*self.mem).split_at_mut(pos).1 };
        let mut stack = Stack::new(mem as *mut _);
        stack.parent_split = Some(&self.split);
        stack
    }

    pub fn push(&mut self, val: T) {
        self.int[self.len] = val;
        self.len += 1;
    }
}

impl<'a, T> Drop for Stack<'a, T> {
    fn drop(&mut self) {
        for t in &mut *self.int {
            unsafe {
                drop(std::mem::replace(
                    t,
                    std::mem::MaybeUninit::uninit().assume_init(),
                ));
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
        let mut a = UniversalStack::new(1000);
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
        let mut stack = UniversalStack::new(1000);
        let first = stack.stack::<i32>();
        let mut second = first.split::<i32>();
        let mut third = first.split::<i32>();
        second.push(1);
        third.push(1);
    }

    #[test]
    fn test_multi_split_scope() {
        let mut stack = UniversalStack::new(1000);
        let first = stack.stack::<i32>();
        {
            let mut split = first.split::<i32>();
            split.push(1);
        }
        first.split::<i32>();
    }
}
