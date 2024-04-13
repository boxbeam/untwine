use std::{
    collections::{HashMap, HashSet, LinkedList, VecDeque},
    hash::Hash,
};

pub trait Collect<T>: Default {
    fn add(&mut self, elem: T);
}

impl<T> Collect<T> for Vec<T> {
    fn add(&mut self, elem: T) {
        self.push(elem);
    }
}

impl<T> Collect<T> for VecDeque<T> {
    fn add(&mut self, elem: T) {
        self.push_back(elem);
    }
}

impl<K, V> Collect<(K, V)> for HashMap<K, V>
where
    K: Hash + Eq,
{
    fn add(&mut self, (key, value): (K, V)) {
        self.insert(key, value);
    }
}

impl<T> Collect<T> for HashSet<T>
where
    T: Hash + Eq,
{
    fn add(&mut self, elem: T) {
        self.insert(elem);
    }
}

impl<T> Collect<T> for LinkedList<T> {
    fn add(&mut self, elem: T) {
        self.push_back(elem);
    }
}
