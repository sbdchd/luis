// for specifics on syntax, see:
// https://www.lua.org/manual/5.3/manual.html

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
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
        self.eat_chars(4);

        let mut comment = String::new();

        loop {
            if self.match_chars("]]--") {
                self.eat_chars(4);
                break Some(Token::Comment(Comment::MultiLine(comment)));
            } else if let Some(c) = self.eat_char() {
                comment.push(c);
            } else {
                break None;
            }
        }
    }

    // '--' CONTENT '\n'?
    fn single_line_comment(&mut self) -> Option<Token> {
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
        Some(Token::Comment(Comment::SingleLine(comment)))
    }

    // '\'' CONTENT '\'' | ''' CONTENT '"'
    fn single_line_string(&mut self) -> Option<Token> {
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
        Some(Token::String(s))
    }

    // '[[' CONTENT ']]'
    fn multi_line_string(&mut self) -> Option<Token> {
        self.eat_chars(2);
        let mut s = String::new();
        loop {
            if self.match_chars("]]") {
                self.eat_chars(2);
                break Some(Token::String(s));
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
        match s.as_str() {
            "false" => Some(Token::False),
            "true" => Some(Token::True),
            "nil" => Some(Token::Nil),
            "not" => Some(Token::Not),
            "for" => Some(Token::For),
            "do" => Some(Token::Do),
            "in" => Some(Token::In),
            "function" => Some(Token::Function),
            "break" => Some(Token::Break),
            "return" => Some(Token::Return),
            "while" => Some(Token::While),
            "repeat" => Some(Token::Repeat),
            "until" => Some(Token::Until),
            "or" => Some(Token::Or),
            "and" => Some(Token::And),
            "goto" => Some(Token::Goto),
            "end" => Some(Token::End),
            "if" => Some(Token::If),
            "then" => Some(Token::Then),
            "elseif" => Some(Token::ElseIf),
            "else" => Some(Token::Else),
            "local" => Some(Token::Local),
            _ => Some(Token::Ident(s)),
        }
    }

    // ^-?[0-9](\.[0-9])?
    fn number(&mut self) -> Option<Token> {
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

        match s.parse() {
            Ok(num) => Some(Token::Number(num)),
            _ => Some(Token::Unknown(s)),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        if let Some("--[[") = &self.peek(4) {
            self.multi_line_comment()
        } else if let Some(c) = self.cur_char() {
            let next = self.next_char();
            match c {
                '\'' | '"' => self.single_line_string(),
                '[' if next == Some('[') => self.multi_line_string(),
                '=' if next == Some('=') => {
                    self.eat_chars(2);
                    Some(Token::EQ)
                }
                '=' => {
                    self.eat_char();
                    Some(Token::Assign)
                }
                ';' => {
                    self.eat_char();
                    Some(Token::SemiColon)
                }
                '[' => {
                    self.eat_char();
                    Some(Token::LBracket)
                }
                ']' => {
                    self.eat_char();
                    Some(Token::RBracket)
                }
                'A'...'Z' | 'a'...'z' | '_' => self.identifier(),
                '\n' => {
                    self.eat_char();
                    Some(Token::NewLine)
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
                    Some(Token::Minus)
                }
                '(' => {
                    self.eat_char();
                    Some(Token::LParen)
                }
                ')' => {
                    self.eat_char();
                    Some(Token::RParen)
                }
                '{' => {
                    self.eat_char();
                    Some(Token::LCurly)
                }
                '}' => {
                    self.eat_char();
                    Some(Token::RCurly)
                }
                ',' => {
                    self.eat_char();
                    Some(Token::Comma)
                }
                '.' if next == Some('.') => {
                    self.eat_chars(2);
                    if self.cur_char() == Some('.') {
                        self.eat_char();
                        Some(Token::Dots)
                    } else {
                        Some(Token::Concat)
                    }
                }
                '.' => {
                    self.eat_char();
                    Some(Token::Period)
                }
                ':' if next == Some(':') => {
                    self.eat_chars(2);
                    Some(Token::DBColon)
                }
                ':' => {
                    self.eat_char();
                    Some(Token::Colon)
                }
                '<' if next == Some('<') => {
                    self.eat_chars(2);
                    Some(Token::SHL)
                }
                '<' if next == Some('=') => {
                    self.eat_chars(2);
                    Some(Token::LTE)
                }
                '<' => {
                    self.eat_char();
                    Some(Token::LT)
                }
                '>' if next == Some('>') => {
                    self.eat_chars(2);
                    Some(Token::SHR)
                }
                '>' if next == Some('=') => {
                    self.eat_chars(2);
                    Some(Token::GTE)
                }
                '>' => {
                    self.eat_char();
                    Some(Token::GT)
                }
                '+' => {
                    self.eat_char();
                    Some(Token::Plus)
                }
                '#' => {
                    self.eat_char();
                    Some(Token::Hash)
                }
                '*' => {
                    self.eat_char();
                    Some(Token::Mul)
                }
                '/' if next == Some('/') => {
                    self.eat_chars(2);
                    Some(Token::IntDiv)
                }
                '/' => {
                    self.eat_char();
                    Some(Token::Div)
                }
                '%' => {
                    self.eat_char();
                    Some(Token::Mod)
                }
                '^' => {
                    self.eat_char();
                    Some(Token::Pow)
                }
                '&' => {
                    self.eat_char();
                    Some(Token::BitAnd)
                }
                '|' => {
                    self.eat_char();
                    Some(Token::BitOr)
                }
                '~' if next == Some('=') => {
                    self.eat_chars(2);
                    Some(Token::NEQ)
                }
                '~' => {
                    self.eat_char();
                    Some(Token::BitXor)
                }
                unknown => {
                    self.eat_char();
                    Some(Token::Unknown(unknown.to_string()))
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
            Some(Token::Comment(Comment::SingleLine(String::from(
                " This is an example lua comment"
            ))))
        );
    }

    #[test]
    fn multi_line_comment() {
        let multi_line_comment = "--[[ multi-line comment ]]--";

        let mut lex = Lexer::new(multi_line_comment);

        assert_eq!(
            lex.next(),
            Some(Token::Comment(Comment::MultiLine(String::from(
                " multi-line comment "
            ))))
        )
    }

    #[test]
    fn single_line_string() {
        let single_quote = r#"'example string'"#;
        let mut lex = Lexer::new(single_quote);
        assert_eq!(
            lex.next(),
            Some(Token::String(String::from("example string")))
        );

        let double_quote = r#""example string""#;
        let mut lex = Lexer::new(double_quote);
        assert_eq!(
            lex.next(),
            Some(Token::String(String::from("example string")))
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
            Some(Token::String(String::from(" This is a multi-line string ")))
        );
    }

    #[test]
    fn ident_with_bracket() {
        let p = "foo[1]";

        let actual: Vec<_> = Lexer::new(p).collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("foo")),
                Token::LBracket,
                Token::Number(1f64),
                Token::RBracket,
            ]
        );
    }

    #[test]
    fn basic_assignment() {
        let nil_assignment = r#"x =nil"#;
        let actual: Vec<_> = Lexer::new(nil_assignment).collect();

        assert_eq!(
            actual,
            vec![Token::Ident(String::from("x")), Token::Assign, Token::Nil,]
        );

        let p = r#"local _x = 1"#;
        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Local,
                Token::Ident(String::from("_x")),
                Token::Assign,
                Token::Number(1f64),
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

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Plus,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Minus,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Mul,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Div,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Mod,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::Pow,
                Token::Number(3f64),
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(2f64),
                Token::IntDiv,
                Token::Number(3f64),
                Token::NewLine,
            ]
        );
    }

    #[test]
    fn varargs() {
        let p = r#"
        function foo(...)
            return ...
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Function,
                Token::Ident(String::from("foo")),
                Token::LParen,
                Token::Dots,
                Token::RParen,
                Token::NewLine,
                Token::Return,
                Token::Dots,
                Token::NewLine,
                Token::End,
            ]
        )
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

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::LParen,
                Token::Number(2f64),
                Token::BitAnd,
                Token::Number(3f64),
                Token::RParen,
                Token::BitOr,
                Token::LParen,
                Token::BitXor,
                Token::Number(3f64),
                Token::RParen,
                Token::BitOr,
                Token::LParen,
                Token::Number(5f64),
                Token::SHR,
                Token::Number(2f64),
                Token::RParen,
                Token::BitOr,
                Token::LParen,
                Token::Number(5f64),
                Token::SHL,
                Token::Number(2f64),
                Token::RParen,
                Token::BitOr,
                Token::LParen,
                Token::Number(4f64),
                Token::BitXor,
                Token::Number(4f64),
                Token::RParen,
            ]
        )
    }

    #[test]
    fn equality() {
        let p = r#"foo ~= bar == bizz >= 4 > 3 < 10 <= 2 "#;
        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("foo")),
                Token::NEQ,
                Token::Ident(String::from("bar")),
                Token::EQ,
                Token::Ident(String::from("bizz")),
                Token::GTE,
                Token::Number(4f64),
                Token::GT,
                Token::Number(3f64),
                Token::LT,
                Token::Number(10f64),
                Token::LTE,
                Token::Number(2f64),
            ]
        )
    }

    #[test]
    fn goto() {
        let p = r#"
            for z=1,10 do
                goto done
            end
            ::done::"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::For,
                Token::Ident(String::from("z")),
                Token::Assign,
                Token::Number(1f64),
                Token::Comma,
                Token::Number(10f64),
                Token::Do,
                Token::NewLine,
                Token::Goto,
                Token::Ident(String::from("done")),
                Token::NewLine,
                Token::End,
                Token::NewLine,
                Token::DBColon,
                Token::Ident(String::from("done")),
                Token::DBColon,
            ]
        )
    }

    #[test]
    fn for_in_pairs() {
        let p = r#"for k,v in pairs(t) do print(k, v) end"#;
        let actual: Vec<_> = Lexer::new(p).collect();
        assert_eq!(
            actual,
            vec![
                Token::For,
                Token::Ident(String::from("k")),
                Token::Comma,
                Token::Ident(String::from("v")),
                Token::In,
                Token::Ident(String::from("pairs")),
                Token::LParen,
                Token::Ident(String::from("t")),
                Token::RParen,
                Token::Do,
                Token::Ident(String::from("print")),
                Token::LParen,
                Token::Ident(String::from("k")),
                Token::Comma,
                Token::Ident(String::from("v")),
                Token::RParen,
                Token::End,
            ]
        )
    }

    #[test]
    fn local_var_assignment() {
        let nil_assignment = r#"local x=nil"#;
        let lex = Lexer::new(nil_assignment);

        let actual: Vec<_> = lex.collect();

        assert_eq!(
            actual,
            vec![
                Token::Local,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Nil,
            ]
        );
    }

    #[test]
    fn number_assignment() {
        let s = r#"x = 15"#;
        let lex = Lexer::new(s);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(15f64)
            ]
        );

        let s = r#"x = -15"#;
        let lex = Lexer::new(s);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(-15f64)
            ]
        );

        let s = r#"x = 1.50"#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(1.5f64)
            ]
        );

        let s = r#"x = .5"#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(0.5f64)
            ]
        );

        let s = r#"x = 5.40."#;
        let actual: Vec<_> = Lexer::new(s).collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Unknown(String::from("5.40."))
            ]
        );
    }

    #[test]
    fn assignment() {
        let nil_assignment = r#"x = nil"#;
        let lex = Lexer::new(nil_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![Token::Ident(String::from("x")), Token::Assign, Token::Nil]
        );

        let string_assignment = r#"x = 'foo'"#;

        let lex = Lexer::new(string_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::String(String::from("foo"))
            ]
        );

        let bool_assignment = r#"x = false"#;
        let lex = Lexer::new(bool_assignment);
        let actual: Vec<_> = lex.collect();

        assert_eq!(
            actual,
            vec![Token::Ident(String::from("x")), Token::Assign, Token::False]
        );

        let var_assignment = r#"x = y"#;
        let lex = Lexer::new(var_assignment);
        let actual: Vec<_> = lex.collect();
        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Ident(String::from("y"))
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
        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::While,
                Token::Ident(String::from("x")),
                Token::LT,
                Token::Number(100f64),
                Token::Do,
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Ident(String::from("x")),
                Token::Plus,
                Token::Number(1f64),
                Token::NewLine,
                Token::Break,
                Token::NewLine,
                Token::End,
            ]
        )
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
        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::If,
                Token::Ident(String::from("x")),
                Token::EQ,
                Token::Number(1f64),
                Token::Then,
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Ident(String::from("x")),
                Token::Plus,
                Token::Number(1f64),
                Token::NewLine,
                Token::ElseIf,
                Token::Ident(String::from("x")),
                Token::GT,
                Token::Number(5f64),
                Token::Then,
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(-1f64),
                Token::NewLine,
                Token::Else,
                Token::NewLine,
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::Number(0f64),
                Token::NewLine,
                Token::End,
                Token::NewLine,
            ]
        )
    }

    #[test]
    fn method_call() {
        let p = r#"io.write("foo", "\n")"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("io")),
                Token::Period,
                Token::Ident(String::from("write")),
                Token::LParen,
                Token::String(String::from("foo")),
                Token::Comma,
                Token::String(String::from(r"\n")),
                Token::RParen,
            ]
        )
    }

    #[test]
    fn string_concat() {
        let p = r#"'foo' .. bar"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::String(String::from("foo")),
                Token::Concat,
                Token::Ident(String::from("bar")),
            ]
        )
    }

    #[test]
    fn not_expressions() {
        let p = r#"not true"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(actual, vec![Token::Not, Token::True,])
    }

    #[test]
    fn bool_expressions() {
        let p = r#"foo = true and 'bar' or 'buzz'"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("foo")),
                Token::Assign,
                Token::True,
                Token::And,
                Token::String(String::from("bar")),
                Token::Or,
                Token::String(String::from("buzz"))
            ]
        )
    }

    #[test]
    fn for_loop() {
        let p = r#"
        sum = 0
        for i = 1, 100 do
            sum = sum + i
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Ident(String::from("sum")),
                Token::Assign,
                Token::Number(0f64),
                Token::NewLine,
                Token::For,
                Token::Ident(String::from("i")),
                Token::Assign,
                Token::Number(1f64),
                Token::Comma,
                Token::Number(100f64),
                Token::Do,
                Token::NewLine,
                Token::Ident(String::from("sum")),
                Token::Assign,
                Token::Ident(String::from("sum")),
                Token::Plus,
                Token::Ident(String::from("i")),
                Token::NewLine,
                Token::End,
            ]
        )
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

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Ident(String::from("n")),
                Token::Assign,
                Token::Number(100f64),
                Token::NewLine,
                Token::Repeat,
                Token::NewLine,
                Token::Ident(String::from("print")),
                Token::LParen,
                Token::Ident(String::from("n")),
                Token::RParen,
                Token::NewLine,
                Token::Ident(String::from("n")),
                Token::Assign,
                Token::Ident(String::from("n")),
                Token::Minus,
                Token::Number(1f64),
                Token::NewLine,
                Token::Until,
                Token::Ident(String::from("n")),
                Token::EQ,
                Token::Number(0f64),
            ]
        )
    }

    #[test]
    fn semi_colon() {
        let p = r#"foo = 'bar'; buzz = 'bot';"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("foo")),
                Token::Assign,
                Token::String(String::from("bar")),
                Token::SemiColon,
                Token::Ident(String::from("buzz")),
                Token::Assign,
                Token::String(String::from("bot")),
                Token::SemiColon,
            ]
        )
    }

    #[test]
    fn function_def() {
        let p = r#"
        function fib(n)
            if n < 2 then return n end
            return fib(n - 2) + fib(n - 1)
        end"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Function,
                Token::Ident(String::from("fib")),
                Token::LParen,
                Token::Ident(String::from("n")),
                Token::RParen,
                Token::NewLine,
                Token::If,
                Token::Ident(String::from("n")),
                Token::LT,
                Token::Number(2f64),
                Token::Then,
                Token::Return,
                Token::Ident(String::from("n")),
                Token::End,
                Token::NewLine,
                Token::Return,
                Token::Ident(String::from("fib")),
                Token::LParen,
                Token::Ident(String::from("n")),
                Token::Minus,
                Token::Number(2f64),
                Token::RParen,
                Token::Plus,
                Token::Ident(String::from("fib")),
                Token::LParen,
                Token::Ident(String::from("n")),
                Token::Minus,
                Token::Number(1f64),
                Token::RParen,
                Token::NewLine,
                Token::End,
            ]
        )
    }

    #[test]
    fn empty_table() {
        let p = r#"x = {}"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::LCurly,
                Token::RCurly,
            ]
        );
    }

    #[test]
    fn table_with_keys() {
        let p = r#"x = {foo = 'foo', bar = false}"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::LCurly,
                Token::Ident(String::from("foo")),
                Token::Assign,
                Token::String(String::from("foo")),
                Token::Comma,
                Token::Ident(String::from("bar")),
                Token::Assign,
                Token::False,
                Token::RCurly
            ]
        );
    }

    #[test]
    fn table_with_calculated_keys() {
        let p = r#"x = {['bar'] = 'bar', [1.5] = 'foo', [{}] = false}"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::Ident(String::from("x")),
                Token::Assign,
                Token::LCurly,
                Token::LBracket,
                Token::String(String::from("bar")),
                Token::RBracket,
                Token::Assign,
                Token::String(String::from("bar")),
                Token::Comma,
                Token::LBracket,
                Token::Number(1.5),
                Token::RBracket,
                Token::Assign,
                Token::String(String::from("foo")),
                Token::Comma,
                Token::LBracket,
                Token::LCurly,
                Token::RCurly,
                Token::RBracket,
                Token::Assign,
                Token::False,
                Token::RCurly,
            ]
        );
    }

    #[test]
    fn list_size() {
        let p = r#"
        local a = 10
        print(#a)"#;

        let actual: Vec<_> = Lexer::new(p).collect();

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Local,
                Token::Ident(String::from("a")),
                Token::Assign,
                Token::Number(10f64),
                Token::NewLine,
                Token::Ident(String::from("print")),
                Token::LParen,
                Token::Hash,
                Token::Ident(String::from("a")),
                Token::RParen,
            ]
        )
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

        assert_eq!(
            actual,
            vec![
                Token::NewLine,
                Token::Ident(String::from("Foo")),
                Token::Assign,
                Token::LCurly,
                Token::RCurly,
                Token::NewLine,
                Token::NewLine,
                Token::Function,
                Token::Ident(String::from("Foo")),
                Token::Colon,
                Token::Ident(String::from("new")),
                Token::LParen,
                Token::RParen,
                Token::NewLine,
                Token::Local,
                Token::Ident(String::from("obj")),
                Token::Assign,
                Token::LCurly,
                Token::Ident(String::from("someData")),
                Token::Assign,
                Token::String(String::from("foo!")),
                Token::RCurly,
                Token::NewLine,
                Token::Ident(String::from("self")),
                Token::Period,
                Token::Ident(String::from("__index")),
                Token::Assign,
                Token::Ident(String::from("self")),
                Token::NewLine,
                Token::Return,
                Token::Ident(String::from("setmetatable")),
                Token::LParen,
                Token::Ident(String::from("obj")),
                Token::Comma,
                Token::Ident(String::from("self")),
                Token::RParen,
                Token::NewLine,
                Token::End,
                Token::NewLine,
                Token::NewLine,
                Token::Function,
                Token::Ident(String::from("Foo")),
                Token::Colon,
                Token::Ident(String::from("bar")),
                Token::LParen,
                Token::RParen,
                Token::NewLine,
                Token::Return,
                Token::Ident(String::from("self")),
                Token::Period,
                Token::Ident(String::from("someData")),
                Token::NewLine,
                Token::End,
                Token::NewLine,
                Token::NewLine,
                Token::Ident(String::from("f")),
                Token::Assign,
                Token::Ident(String::from("Foo")),
                Token::Colon,
                Token::Ident(String::from("new")),
                Token::LParen,
                Token::RParen,
                Token::NewLine,
                Token::Ident(String::from("f")),
                Token::Colon,
                Token::Ident(String::from("bar")),
                Token::LParen,
                Token::RParen,
                Token::NewLine,
            ]
        )
    }

}
