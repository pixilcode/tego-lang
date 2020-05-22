use nom::{InputIter, InputLength, InputTake, UnspecializedInput};

#[derive(PartialEq, Debug, Clone)]
pub struct Span<'a> {
    lexeme: &'a str,
    column: usize,
    line: usize,
}

impl<'a> Span<'a> {
    pub fn to_str(&self) -> &str {
        self.lexeme
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn new(lexeme: &'_ str) -> Span<'_> {
        Span {
            lexeme,
            column: 1,
            line: 1,
        }
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
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let mut line = self.line;
        let mut column = self.column;

        for c in self.lexeme[..count].chars() {
            match c {
                '\n' => {
                    line += 1;
                    column = 1;
                }
                '\t' => {
                    column += 4;
                }
                _ => {
                    column += 1;
                }
            }
        }

        (
            Span {
                lexeme: &self.lexeme[count..],
                line,
                column,
            },
            Span {
                lexeme: &self.lexeme[..count],
                line: self.line,
                column: self.column,
            },
        )
    }
}

impl<'a> UnspecializedInput for Span<'a> {}
