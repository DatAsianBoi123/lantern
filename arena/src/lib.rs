use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct Arena<T> {
    chunks: UnsafeCell<Vec<Chunk<T>>>,
    chunk_cap: usize,
}

impl<T> Arena<T> {
    pub fn new(chunk_cap: usize) -> Self {
        Self {
            chunks: UnsafeCell::new(vec![Chunk::new(chunk_cap)]),
            chunk_cap,
        }
    }

    // this is allowed because &mut T will always be valid (Chunk never reallocates) and only one
    // &mut T can exist at a time
    #[allow(clippy::mut_from_ref)]
    pub fn allocate(&self, element: T) -> &mut T {
        // SAFETY: this fn is the only place we get a mutable reference to chunks and it is not
        // recursive, therefore we can guarantee this mutable reference is unique
        let chunks = unsafe { &mut *self.chunks.get() };
        match chunks.last_mut().expect("at least one chunk").allocate(element) {
            Ok(ret) => ret,
            Err(element) => {
                match chunks.push_mut(Chunk::new(self.chunk_cap)).allocate(element) {
                    Ok(ret) => ret,
                    Err(_) => panic!("chunk cap of 0 is a logic error"),
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Chunk<T> {
    inner: Vec<T>,
}

impl<T> Chunk<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: Vec::with_capacity(capacity),
        }
    }

    pub fn allocate(&mut self, element: T) -> Result<&mut T, T> {
        // don't reallocate!
        if self.inner.len() < self.inner.capacity() {
            Ok(self.inner.push_mut(element))
        } else {
            Err(element)
        }
    }
}

