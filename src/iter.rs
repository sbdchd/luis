use crate::lex::{Token, TokenKind};
use std::cmp::max;

pub trait PeekableIterator: Iterator {
    fn peek(&mut self) -> Option<Self::Item>;
}

pub trait ForwardBackwardIterator: Iterator {
    fn prev(&mut self) -> Option<Self::Item>;
}

#[derive(Debug)]
pub struct TokenIter<'a, Item>
where
    Item: 'a + PartialEq,
{
    index: Option<usize>,
    vector: &'a [Item],
}

impl<'a, Item> TokenIter<'a, Item>
where
    Item: PartialEq,
{
    pub fn new(vector: &'a [Item]) -> TokenIter<'a, Item> {
        TokenIter {
            index: None,
            vector,
        }
    }
}

impl<'a, Item> PeekableIterator for TokenIter<'a, Item>
where
    Item: PartialEq,
{
    fn peek(&mut self) -> Option<&'a Item> {
        let index = match self.index {
            Some(i) => i + 1,
            None => 0,
        };

        self.vector.get(index)
    }
}

impl<'a, Item> Iterator for TokenIter<'a, Item>
where
    Item: PartialEq,
{
    type Item = &'a Item;

    fn next(&mut self) -> Option<&'a Item> {
        let index = match self.index {
            Some(i) => i + 1,
            None => 0,
        };

        self.index = Some(index);
        self.vector.get(index)
    }
}

impl<'a, Item> ForwardBackwardIterator for TokenIter<'a, Item>
where
    Item: PartialEq,
{
    fn prev(&mut self) -> Option<&'a Item> {
        let index = match self.index {
            None => return None,
            Some(0) => {
                self.index = None;
                return None;
            }
            Some(i) => max(i - 1, 0),
        };

        self.index = Some(index);
        self.vector.get(index)
    }
}

impl<'a> TokenIter<'a, Token> {
    pub fn assert_next(&mut self, knd: &TokenKind) -> Result<(), ()> {
        match self.next() {
            Some(Token { kind, .. }) => {
                if kind == knd {
                    Ok(())
                } else {
                    Err(())
                }
            }
            None => Err(()),
        }
    }
}
