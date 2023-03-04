use std::fmt::Debug;

pub(crate) struct Stack<T> {
    queue: Vec<T>,
}

impl<T> Stack<T>
where
    T: Debug,
{
    pub(crate) fn new() -> Self {
        Stack { queue: vec![] }
    }

    // Insert a new element at the top of the stack
    pub(crate) fn push(&mut self, e: T) {
        self.queue.push(e)
    }

    // Remove the element from the top of the stack and return it
    pub(crate) fn pop(&mut self) -> Option<T> {
        self.queue.pop()
    }

    // Remove the element from the top of the stack, if 'f' return true. Otherwise insert the
    // argument 'e' at the top of the stack.
    pub(crate) fn pop_or_push<F>(&mut self, e: T, f: F) -> Option<T>
    where
        F: Fn(&T) -> bool,
    {
        if let Some(e) = self.queue.last() {
            if f(e) {
                return self.pop();
            }
        }
        self.push(e);
        None
    }

    // Remove and return all elements of a range from 'f' being true to the top of the stack.
    pub(crate) fn pop_range<F>(&mut self, f: F) -> Vec<T>
    where
        F: Fn(&T) -> bool,
    {
        let mut pops: Vec<T> = vec![];
        let mut position: Option<usize> = None;

        for (ix, e) in self.queue.iter().rev().enumerate() {
            if f(e) {
                position = Some(self.queue.len() - 1 - ix);
                break;
            }
        }
        if let Some(p) = position {
            loop {
                if let Some(pop) = self.pop() {
                    pops.push(pop);
                    if self.queue.len() == p {
                        break;
                    }
                } else {
                    unreachable!()
                }
            }
            pops.reverse();
        }
        pops
    }

    pub(crate) fn all_mut(&mut self) -> &mut Vec<T> {
        &mut self.queue
    }
}
