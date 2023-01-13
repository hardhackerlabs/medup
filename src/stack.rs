pub(crate) struct Stack<T> {
    buff: Vec<T>,
}

impl<T> Stack<T> {
    pub(crate) fn new() -> Self {
        Stack { buff: vec![] }
    }

    pub(crate) fn push(&mut self, e: T) {
        self.buff.push(e)
    }

    pub(crate) fn pop(&mut self) -> Option<T> {
        self.buff.pop()
    }

    pub(crate) fn pop_or_push<F>(&mut self, e: T, f: F) -> Option<T>
    where
        F: Fn(&T) -> bool,
    {
        if let Some(e) = self.buff.last() {
            if f(e) {
                return self.pop();
            }
        }
        self.push(e);
        None
    }

    pub(crate) fn pop_range<F>(&mut self, f: F) -> Vec<T>
    where
        F: Fn(&T) -> bool,
    {
        let mut pops: Vec<T> = vec![];
        let mut position: Option<usize> = None;

        for (ix, e) in self.buff.iter().rev().enumerate() {
            if f(e) {
                position = Some(self.buff.len() - 1 - ix);
                break;
            }
        }
        if let Some(position) = position {
            loop {
                if let Some(pop) = self.pop() {
                    pops.push(pop);
                    if self.buff.len() == position {
                        break;
                    }
                }
                unreachable!()
            }
            pops.reverse();
        }
        pops
    }

    pub(crate) fn all_mut(&mut self) -> &mut Vec<T> {
        &mut self.buff
    }
}
