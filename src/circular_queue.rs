use std::{mem::ManuallyDrop, ops::Deref};

pub struct CircularQueue<T, const N: usize> {
    inner: ManuallyDrop<[T; N]>,
    len: usize,
}

impl<T, const N: usize> Default for CircularQueue<T, N> {
    fn default() -> Self {
        CircularQueue {
            inner: ManuallyDrop::new(std::array::from_fn(|_| unsafe { std::mem::zeroed() })),
            len: 0,
        }
    }
}

impl<T, const N: usize> CircularQueue<T, N> {
    pub fn len(&self) -> usize {
        self.len.min(N)
    }

    fn next_index(&self) -> usize {
        self.len % N
    }

    pub fn push(&mut self, elem: T) {
        let next = self.next_index();
        self.inner[next] = elem;
        self.len += 1;
    }

    pub fn clear(&mut self) {
        for elem in self.iter() {
            unsafe { drop(std::ptr::read(elem as *const T)) }
        }
        self.len = 0;
    }
}

impl<T, const N: usize> Deref for CircularQueue<T, N> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.inner[..self.len()]
    }
}

impl<T, const N: usize> Drop for CircularQueue<T, N> {
    fn drop(&mut self) {
        for elem in self.iter() {
            unsafe { drop(std::ptr::read(elem as *const T)) }
        }
    }
}
