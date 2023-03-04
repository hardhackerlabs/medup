pub(crate) struct Cursor<'cursor> {
    s: &'cursor str,
    index: usize,
}

impl<'cursor> Cursor<'cursor> {
    // Create a cursor instance
    pub(crate) fn new(s: &'cursor str) -> Self {
        Cursor { s, index: 0 }
    }

    pub(crate) fn index(&self) -> usize {
        self.index
    }

    pub(crate) fn move_one(&mut self) {
        self.index += 1;
    }

    pub(crate) fn rest_slice(&self) -> &str {
        utf8_slice::from(self.s, self.index)
    }

    // Get the sub-string after the cursor, contains the position of the cursor
    // not contains the end position.
    pub(crate) fn slice_to(&self, end: usize) -> &str {
        if self.index > end {
            panic!("begin > end: {}, {}", self.index, end);
        }
        utf8_slice::slice(self.s, self.index, end)
    }

    // Get the sub-string before the cursor, not contains the position of the cursor
    // contains the begin position.
    pub(crate) fn _before_slice(&self, begin: usize) -> &str {
        if begin > self.index {
            panic!("begine > end: {}, {}", begin, self.index);
        }
        utf8_slice::slice(self.s, begin, self.index)
    }

    // Move the cursor to the end position
    pub(crate) fn consume_to<F>(&mut self, end: usize, mut f: F)
    where
        F: FnMut(&str),
    {
        let sub = self.slice_to(end);
        if !sub.is_empty() {
            f(sub);
        }
        self.index = end;
    }
}
