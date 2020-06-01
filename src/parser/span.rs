use nom::{
    Compare, CompareResult, InputIter, InputLength, InputTake, Offset, Slice, UnspecializedInput, FindSubstring
};
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Span<'a> {
    lexeme: &'a str,
    column: usize,
    line: usize,
    offset: usize,
}

impl<'a> Span<'a> {
    pub fn to_str(&self) -> &'a str {
        self.lexeme
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn new(lexeme: &'_ str) -> Span<'_> {
        Span {
            lexeme,
            column: 1,
            line: 1,
            offset: 0,
        }
    }
}

// Used only in the parser crate
pub fn span_at(lexeme: &str, column: usize, line: usize, offset: usize) -> Span {
    Span {
        lexeme,
        column,
        line,
        offset,
    }
}

impl<'a> InputLength for Span<'a> {
    fn input_len(&self) -> usize {
        self.lexeme.len()
    }
}

impl<'a> InputIter for Span<'a> {
    type Item = char;
    type Iter = std::str::CharIndices<'a>;
    type IterElem = std::str::Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.lexeme.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.lexeme.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.lexeme.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        self.lexeme.slice_index(count)
    }
}

impl<'a> InputTake for Span<'a> {
    fn take(&self, count: usize) -> Self {
        Span {
            lexeme: &self.lexeme[..count],
            column: self.column,
            line: self.line,
            offset: self.offset,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let mut line = self.line;
        let mut column = self.column;
        let mut offset = self.offset;

        for c in self.lexeme[..count].chars() {
            match c {
                '\n' => {
                    line += 1;
                    column = 1;
                    offset += 1;
                }
                '\t' => {
                    column += 4;
                    offset += 1;
                }
                _ => {
                    column += 1;
                    offset += 1;
                }
            }
        }

        (
            Span {
                lexeme: &self.lexeme[count..],
                line,
                column,
                offset,
            },
            Span {
                lexeme: &self.lexeme[..count],
                line: self.line,
                column: self.column,
                offset: self.offset,
            },
        )
    }
}

impl<'a> UnspecializedInput for Span<'a> {}

impl<'a> Compare<&str> for Span<'a> {
    fn compare(&self, s: &str) -> CompareResult {
        self.lexeme.compare(s)
    }

    fn compare_no_case(&self, s: &str) -> CompareResult {
        self.lexeme.compare_no_case(s)
    }
}

impl<'a> FindSubstring<&str> for Span<'a> {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.to_str().find_substring(substr)
    }
}

macro_rules! impl_slice_for_range {
    ($range:ty) => {
        impl<'a> Slice<$range> for Span<'a> {
            fn slice(&self, range: $range) -> Self {
                let next_slice = &self.lexeme[range];

                if next_slice == self.lexeme {
                    return *self;
                }

                let next_offset = self.lexeme.offset(next_slice);

                if next_offset == 0 {
                    return Span {
                        offset: self.offset,
                        line: self.line,
                        column: self.column,
                        lexeme: next_slice,
                    };
                }

                let mut line = self.line;
                let mut column = self.column;
                let mut offset = self.offset;

                for c in self.lexeme[..next_offset].chars() {
                    match c {
                        '\n' => {
                            line += 1;
                            column = 1;
                            offset += 1;
                        }
                        '\t' => {
                            column += 4;
                            offset += 1;
                        }
                        _ => {
                            column += 1;
                            offset += 1;
                        }
                    }
                }

                Span {
                    offset,
                    line,
                    column,
                    lexeme: next_slice,
                }
            }
        }
    };
}

impl_slice_for_range!(Range<usize>);
impl_slice_for_range!(RangeFrom<usize>);
impl_slice_for_range!(RangeTo<usize>);
impl_slice_for_range!(RangeFull);

impl<'a> From<&'a str> for Span<'a> {
    fn from(s: &'a str) -> Self {
        Span::new(s)
    }
}

impl<'a> From<Span<'a>> for &'a str {
    fn from(s: Span<'a>) -> Self {
        s.lexeme
    }
}

impl<'a> From<&Span<'a>> for &'a str {
    fn from(s: &Span<'a>) -> Self {
        s.lexeme
    }
}
