use std::{cell::RefCell, marker::PhantomData, rc::Rc};

pub trait SeqHelper<C>: Sized {
    fn is_variant(enumerator: &C) -> bool;
    fn get_variant(enumerator: C) -> Option<Self>;
}

pub struct SeqIter<T, C: SeqHelper<T>> {
    index: usize,
    lenght: usize,
    inner: Rc<RefCell<Vec<Option<T>>>>,
    iter_type: PhantomData<C>,
}

impl<T, C: SeqHelper<T>> SeqIter<T, C> {
    pub fn as_iter<N: SeqHelper<T>>(&self) -> SeqIter<T, N> {
        SeqIter {
            index: 0,
            lenght: self.inner.borrow().len(),
            inner: self.inner.clone(),
            iter_type: PhantomData::<N>,
        }
    }
}

impl<T, C: SeqHelper<T>> SeqIter<T, C> {
    pub fn new(inner: impl IntoIterator<Item = T>) -> Self {
        let vec: Vec<Option<T>> = inner.into_iter().map(|i| Some(i)).collect();
        Self {
            index: 0,
            lenght: vec.len(),
            inner: Rc::new(RefCell::new(vec)),
            iter_type: PhantomData,
        }
    }
}

impl<T, C: SeqHelper<T>> Iterator for SeqIter<T, C> {
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.lenght {
            return None;
        }

        let current = {
            match &mut self.inner.borrow_mut()[self.index] {
                value if value.as_ref().map_or(false, |t| C::is_variant(t)) => {
                    C::get_variant(std::mem::take(value).unwrap())
                }
                _ => None,
            }
        };

        self.index += 1;
        current.or_else(|| self.next())
    }
}
