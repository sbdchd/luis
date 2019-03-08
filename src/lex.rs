// for specifics on syntax, see:
// https://www.lua.org/manual/5.3/manual.html

#[derive(PartialEq, Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    False,
    Until,
    While,
    IntDiv,
    Concat,
    Dots,
    Period,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
    Comma,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    BitXor,
    Assign,
    NewLine,
    EQ,
    NEQ,
    GTE,
    LTE,
    LT,
    GT,
    SHL,
    SHR,
    Hash,
    SemiColon,
    DBColon,
    Colon,
    // TODO(sbdchd): we may want to add a string type for those with single
    // quotes and those with double quotes
    String(String),
    Number(f64),
    Ident(String),
    Comment(Comment),
    Unknown(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: String::from(input),
            cursor: 0,
        }
    }

    fn eat_chars(&mut self, n: usize) {
        self.cursor += n;
    }

    fn eat_char(&mut self) -> Option<char> {
        let c = self.input.chars().nth(self.cursor);
        self.cursor += 1;
        c
    }

    fn peek(&self, n: usize) -> Option<&str> {
        if self.input.len() <= self.cursor + n {
            None
        } else {
            Some(&self.input[self.cursor..self.cursor + n])
        }
    }

    fn peek_is_number(&mut self) -> bool {
        if let Some(c) = self.next_char() {
            return match c {
                '0'...'9' => true,
                _ => false,
            };
        }

        false
    }

    fn next_char_is_number(&self) -> bool {
        match self.next_char() {
            Some('0'...'9') => true,
            _ => false,
        }
    }

    fn cur_char(&self) -> Option<char> {
        self.input.chars().nth(self.cursor)
    }

    fn next_char(&self) -> Option<char> {
        self.input.chars().nth(self.cursor + 1)
    }

    /// ```
    /// use luis::lex::Lexer;
    /// let mut l = Lexer::new("foo");
    /// assert!(l.match_chars("foo"));
    /// ```
    pub fn match_chars(&mut self, other: &str) -> bool {
        if self.input.len() < other.len() {
            return false;
        }
        for (i, c) in self.input.chars().skip(self.cursor).enumerate() {
            if Some(c) != other.chars().nth(i) {
                return false;
            }
        }
        true
    }

    // '--[[' CONTENT ']]--'
    fn multi_line_comment(&mut self) -> Option<Token> {
        let start = self.cursor;

        self.eat_chars(4);

        let mut comment = String::new();

        loop {
            if self.match_chars("]]--") {
                self.eat_chars(4);
                let end = self.cursor;
                break Some(Token {
                    kind: TokenKind::Comment(Comment::MultiLine(comment)),
                    span: Span { start, end },
                });
            } else if let Some(c) = self.eat_char() {
                comment.push(c);
            } else {
                break None;
            }
        }
    }

    // '--' CONTENT '\n'?
    fn single_line_comment(&mut self) -> Option<Token> {
        let start = self.cursor;
        self.eat_chars(2);
        let mut comment = String::new();
        loop {
            if Some('\n') == self.cur_char() {
                break;
            }
            if let Some(c) = self.eat_char() {
                comment.push(c);
            } else {
                break;
            }
        }
        let end = self.cursor;
        Some(Token {
            kind: TokenKind::Comment(Comment::SingleLine(comment)),
            span: Span { start, end },
        })
    }

    // '\'' CONTENT '\'' | ''' CONTENT '"'
    fn single_line_string(&mut self) -> Option<Token> {
        let start = self.cursor;
        let closing = self.eat_char();
        let mut s = String::new();
        loop {
            match self.eat_char() {
                Some(e) if Some(e) == closing => break,
                Some(sc) if sc == '\n' => return None,
                Some(sc) => s.push(sc),
                None => return None,
            }
        }
        let end = self.cursor;
        Some(Token {
            kind: TokenKind::String(s),
            span: Span { start, end },
        })
    }

    // '[[' CONTENT ']]'
    fn multi_line_string(&mut self) -> Option<Token> {
        let start = self.cursor;
        self.eat_chars(2);
        let mut s = String::new();
        let end = self.cursor;
        loop {
            if self.match_chars("]]") {
                self.eat_chars(2);
                break Some(Token {
                    kind: TokenKind::String(s),
                    span: Span { start, end },
                });
            }
            match self.eat_char() {
                Some(sc) => s.push(sc),
                None => break None,
            }
        }
    }

    // [A-z][A-z0-9]
    fn identifier(&mut self) -> Option<Token> {
        let mut s = String::new();
        let start = self.cursor;

        // TODO(sbdchd): this should handle the case where the ident starts with a letter.
        // Right now we just don't allow numbers in idents
        while let Some(n) = self.cur_char() {
            match n {
                'A'...'Z' | 'a'...'z' | '0'...'9' | '_' => {
                    if let Some(c) = self.eat_char() {
                        s.push(c);
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        let end = self.cursor;

        let span = Span { start, end };

        let kind = match s.as_str() {
            "false" => TokenKind::False,
            "true" => TokenKind::True,
            "nil" => TokenKind::Nil,
            "not" => TokenKind::Not,
            "for" => TokenKind::For,
            "do" => TokenKind::Do,
            "in" => TokenKind::In,
            "function" => TokenKind::Function,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,
            "while" => TokenKind::While,
            "repeat" => TokenKind::Repeat,
            "until" => TokenKind::Until,
            "or" => TokenKind::Or,
            "and" => TokenKind::And,
            "goto" => TokenKind::Goto,
            "end" => TokenKind::End,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "elseif" => TokenKind::ElseIf,
            "else" => TokenKind::Else,
            "local" => TokenKind::Local,
            _ => TokenKind::Ident(s),
        };

        Some(Token { kind, span })
    }

    // ^-?[0-9](\.[0-9])?
    fn number(&mut self) -> Option<Token> {
        let start = self.cursor;
        // TODO(sbdchd): I think there are more formats. Check the reference.
        let mut s = String::new();

        // we only want a minus sign at the front
        // ^-
        if self.cur_char() == Some('-') {
            if let Some(c) = self.eat_char() {
                s.push(c);
            }
        }

        while let Some(n) = self.cur_char() {
            match n {
                '0'...'9' | '.' => {
                    if let Some(c) = self.eat_char() {
                        s.push(c);
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        let span = Span {
            start,
            end: self.cursor,
        };

        match s.parse() {
            Ok(num) => Some(Token {
                kind: TokenKind::Number(num),
                span,
            }),
            _ => Some(Token {
                kind: TokenKind::Unknown(s),
                span,
            }),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        if let Some("--[[") = &self.peek(4) {
            self.multi_line_comment()
        } else if let Some(c) = self.cur_char() {
            let start = self.cursor;
            let next = self.next_char();
            match c {
                '\'' | '"' => self.single_line_string(),
                '[' if next == Some('[') => self.multi_line_string(),
                '=' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::EQ,
                        span: Span { start, end },
                    })
                }
                '=' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Assign,
                        span: Span { start, end },
                    })
                }
                ';' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::SemiColon,
                        span: Span { start, end },
                    })
                }
                '[' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LBracket,
                        span: Span { start, end },
                    })
                }
                ']' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RBracket,
                        span: Span { start, end },
                    })
                }
                'A'...'Z' | 'a'...'z' | '_' => self.identifier(),
                '\n' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::NewLine,
                        span: Span { start, end },
                    })
                }
                ' ' => {
                    self.eat_char();
                    self.next()
                }
                '.' if self.next_char_is_number() => self.number(),
                '0'...'9' => self.number(),
                '-' if next == Some('-') => self.single_line_comment(),
                '-' => {
                    if self.peek_is_number() {
                        return self.number();
                    }
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Minus,
                        span: Span { start, end },
                    })
                }
                '(' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LParen,
                        span: Span { start, end },
                    })
                }
                ')' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RParen,
                        span: Span { start, end },
                    })
                }
                '{' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LCurly,
                        span: Span { start, end },
                    })
                }
                '}' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RCurly,
                        span: Span { start, end },
                    })
                }
                ',' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Comma,
                        span: Span { start, end },
                    })
                }
                '.' if next == Some('.') => {
                    self.eat_chars(2);
                    if self.cur_char() == Some('.') {
                        self.eat_char();
                        let end = self.cursor;
                        Some(Token {
                            kind: TokenKind::Dots,
                            span: Span { start, end },
                        })
                    } else {
                        let end = self.cursor;
                        Some(Token {
                            kind: TokenKind::Concat,
                            span: Span { start, end },
                        })
                    }
                }
                '.' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Period,
                        span: Span { start, end },
                    })
                }
                ':' if next == Some(':') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::DBColon,
                        span: Span { start, end },
                    })
                }
                ':' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Colon,
                        span: Span { start, end },
                    })
                }
                '<' if next == Some('<') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::SHL,
                        span: Span { start, end },
                    })
                }
                '<' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LTE,
                        span: Span { start, end },
                    })
                }
                '<' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LT,
                        span: Span { start, end },
                    })
                }
                '>' if next == Some('>') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::SHR,
                        span: Span { start, end },
                    })
                }
                '>' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::GTE,
                        span: Span { start, end },
                    })
                }
                '>' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::GT,
                        span: Span { start, end },
                    })
                }
                '+' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Plus,
                        span: Span { start, end },
                    })
                }
                '#' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Hash,
                        span: Span { start, end },
                    })
                }
                '*' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Mul,
                        span: Span { start, end },
                    })
                }
                '/' if next == Some('/') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::IntDiv,
                        span: Span { start, end },
                    })
                }
                '/' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Div,
                        span: Span { start, end },
                    })
                }
                '%' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Mod,
                        span: Span { start, end },
                    })
                }
                '^' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Pow,
                        span: Span { start, end },
                    })
                }
                '&' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::BitAnd,
                        span: Span { start, end },
                    })
                }
                '|' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::BitOr,
                        span: Span { start, end },
                    })
                }
                '~' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::NEQ,
                        span: Span { start, end },
                    })
                }
                '~' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::BitXor,
                        span: Span { start, end },
                    })
                }
                unknown => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Unknown(unknown.to_string()),
                        span: Span { start, end },
                    })
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test_lex {
    use super::*;
    use insta::assert_debug_snapshot_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn whitespace() {
        let ws = "  ";

        let mut lex = Lexer::new(ws);

        assert_eq!(lex.next(), None);
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn single_line_comment() {
        let single_line_comment = "-- This is an example lua comment";

        let mut lex = Lexer::new(single_line_comment);

        assert_eq!(
            lex.next(),
            Some(Token {
                kind: TokenKind::Comment(Comment::SingleLine(String::from(
                    " This is an example lua comment"
                ))),
                span: Span { start: 0, end: 34 }
            })
        );
    }

    #[test]
    fn multi_line_comment() {
        let multi_line_comment = "--[[ multi-line comment ]]--";

        let mut lex = Lexer::new(multi_line_comment);

        assert_eq!(
            lex.next(),
            Some(Token {
                kind: TokenKind::Comment(Comment::MultiLine(String::from(" multi-line comment "))),
                span: Span { start: 0, end: 28 }
            })
        )
    }

    #[test]
    fn single_line_string() {
        let single_quote = r#"'example string'"#;
        let mut lex = Lexer::new(single_quote);
        assert_eq!(
            lex.next(),
            Some(Token {
                kind: TokenKind::String(String::from("example string")),
                span: Span { start: 0, end: 16 }
            })
        );

        let double_quote = r#""example string""#;
        let mut lex = Lexer::new(double_quote);
        assert_eq!(
            lex.next(),
            Some(Token {
                kind: TokenKind::String(String::from("example string")),
                span: Span { start: 0, end: 16 }
            })
        );

        let bad_new_line = r#""example string
        ""#;
        let mut lex = Lexer::new(bad_new_line);
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn multi_line_string() {
        let multi_line = "[[ This is a multi-line string ]]";

        let mut lex = Lexer::new(multi_line);
        assert_eq!(
            lex.next(),
            Some(Token {
                kind: TokenKind::String(String::from(" This is a multi-line string ")),
                span: Span { start: 0, end: 2 }
            })
        );
    }

    #[test]
    fn ident_with_bracket() {
        let p = "foo[1]";
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn basic_assignment() {
        let nil_assignment = r#"x =nil"#;
        let actual: Vec<_> = Lexer::new(nil_assignment).collect();

        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::Nil,
                    span: Span { start: 3, end: 6 }
                },
            ]
        );

        let p = r#"local _x = 1"#;
        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Local,
                    span: Span { start: 0, end: 5 }
                },
                Token {
                    kind: TokenKind::Ident(String::from("_x")),
                    span: Span { start: 6, end: 8 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 9, end: 10 }
                },
                Token {
                    kind: TokenKind::Number(1f64),
                    span: Span { start: 11, end: 12 }
                },
            ]
        );
    }

    #[test]
    fn arithmetic() {
        let p = r#"
            x = 2 + 3
            x = 2 - 3
            x = 2 * 3
            x = 2 / 3
            x = 2 % 3
            x = 2 ^ 3
            x = 2 // 3
        "#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn varargs() {
        let p = r#"
        function foo(...)
            return ...
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn bitwise() {
        // &: bitwise AND
        // |: bitwise OR
        // ~: bitwise exclusive OR
        // >>: right shift
        // <<: left shift
        // ~: unary bitwise NOT
        let p = r#"x = (2 & 3) | (~3) | (5 >> 2) | (5 << 2) | (4 ~ 4)"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn equality() {
        let p = r#"foo ~= bar == bizz >= 4 > 3 < 10 <= 2 "#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn goto() {
        let p = r#"
            for z=1,10 do
                goto done
            end
            ::done::"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn for_in_pairs() {
        let p = r#"for k,v in pairs(t) do print(k, v) end"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn local_var_assignment() {
        let nil_assignment = r#"local x=nil"#;
        let lex = Lexer::new(nil_assignment);
        let actual: Vec<_> = lex.collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn number_assignment() {
        let s = r#"x = 15"#;
        let lex = Lexer::new(s);
        let actual: Vec<_> = lex.collect();
        assert_debug_snapshot_matches!(actual);

        let s = r#"x = -15"#;
        let lex = Lexer::new(s);
        let actual: Vec<_> = lex.collect();
        assert_debug_snapshot_matches!(actual);

        let s = r#"x = 1.50"#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_debug_snapshot_matches!(actual);

        let s = r#"x = .5"#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_debug_snapshot_matches!(actual);

        let s = r#"x = 5.40."#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn assignment() {
        let nil_assignment = r#"x = nil"#;
        let lex = Lexer::new(nil_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::Nil,
                    span: Span { start: 4, end: 7 }
                },
            ]
        );

        let string_assignment = r#"x = 'foo'"#;

        let lex = Lexer::new(string_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::String(String::from("foo")),
                    span: Span { start: 4, end: 9 }
                },
            ]
        );

        let bool_assignment = r#"x = false"#;
        let lex = Lexer::new(bool_assignment);
        let actual: Vec<_> = lex.collect();

        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::False,
                    span: Span { start: 4, end: 9 }
                },
            ]
        );

        let var_assignment = r#"x = y"#;
        let lex = Lexer::new(var_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::Ident(String::from("y")),
                    span: Span { start: 4, end: 5 }
                },
            ]
        );
    }

    #[test]
    fn while_loop() {
        let loop_text = r#"
        while x < 100 do
            x = x + 1
            break
        end"#;

        let lex = Lexer::new(loop_text);
        let actual: Vec<_> = lex.collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn if_stmt() {
        let loop_text = r#"
            if x == 1 then
                x = x + 1
            elseif x > 5 then
                x = -1
            else
                x = 0
            end
        "#;

        let lex = Lexer::new(loop_text);
        let actual: Vec<_> = lex.collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn method_call() {
        let p = r#"io.write("foo", "\n")"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn string_concat() {
        let p = r#"'foo' .. bar"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::String(String::from("foo")),
                    span: Span { start: 0, end: 5 }
                },
                Token {
                    kind: TokenKind::Concat,
                    span: Span { start: 6, end: 8 }
                },
                Token {
                    kind: TokenKind::Ident(String::from("bar")),
                    span: Span { start: 9, end: 12 }
                },
            ]
        )
    }

    #[test]
    fn not_expressions() {
        let p = r#"not true"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Not,
                    span: Span { start: 0, end: 3 }
                },
                Token {
                    kind: TokenKind::True,
                    span: Span { start: 4, end: 8 }
                },
            ]
        )
    }

    #[test]
    fn bool_expressions() {
        let p = r#"foo = true and 'bar' or 'buzz'"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn for_loop() {
        let p = r#"
        sum = 0
        for i = 1, 100 do
            sum = sum + i
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn repeat_loop() {
        let p = r#"
        n = 100
        repeat
            print(n)
            n = n - 1
        until n == 0"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn semi_colon() {
        let p = r#"foo = 'bar'; buzz = 'bot';"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn function_def() {
        let p = r#"
        function fib(n)
            if n < 2 then return n end
            return fib(n - 2) + fib(n - 1)
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn empty_table() {
        let p = r#"x = {}"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token {
                    kind: TokenKind::Ident(String::from("x")),
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenKind::Assign,
                    span: Span { start: 2, end: 3 }
                },
                Token {
                    kind: TokenKind::LCurly,
                    span: Span { start: 4, end: 5 }
                },
                Token {
                    kind: TokenKind::RCurly,
                    span: Span { start: 5, end: 6 }
                },
            ]
        );
    }

    #[test]
    fn table_with_keys() {
        let p = r#"x = {foo = 'foo', bar = false}"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn table_with_calculated_keys() {
        let p = r#"x = {['bar'] = 'bar', [1.5] = 'foo', [{}] = false}"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn list_size() {
        let p = r#"
        local a = 10
        print(#a)"#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

    #[test]
    fn classes_via_tables() {
        let p = r#"
        Foo = {}

        function Foo:new()
            local obj = {someData = 'foo!'}
            self.__index = self
            return setmetatable(obj, self)
        end

        function Foo:bar()
            return self.someData
        end

        f = Foo:new()
        f:bar()
        "#;

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_debug_snapshot_matches!(actual);
    }

}
