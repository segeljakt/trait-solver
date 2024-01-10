use std::ops::Range;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Token<'a> {
    // Punctuations
    Eq,
    EqEq,
    BangEq,
    LAngle,
    LAngleEq,
    RAngle,
    RAngleEq,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    DotDot,
    Colon,
    ColonColon,
    SemiColon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    Underscore,
    Question,
    // Keywords
    Def,
    Impl,
    Struct,
    Enum,
    Where,
    Var,
    Type,
    From,
    Into,
    Select,
    True,
    False,
    // Literals
    Code(&'a str),
    Name(&'a str),
    Int(&'a str),
    Float(&'a str),
    String(&'a str),
    Char(char),
    // Error
    Err,
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl PartialEq<Span> for Range<usize> {
    fn eq(&self, other: &Span) -> bool {
        self.start == other.start as usize && self.end == other.end as usize
    }
}

impl PartialEq<Range<usize>> for Span {
    fn eq(&self, other: &Range<usize>) -> bool {
        self.start as usize == other.start && self.end as usize == other.end
    }
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        (span.start as usize)..(span.end as usize)
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl std::ops::Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        Lexer { input, pos: 0 }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Span, Token<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let start = self.pos;
            let mut chars = self.input[self.pos..].chars();
            let c = chars.next()?;
            self.pos += c.len_utf8();

            let token = match c {
                '#' => {
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            break;
                        }
                        self.pos += c.len_utf8();
                    }
                    continue;
                }
                ' ' | '\n' | '\t' => continue,
                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some(c1) = chars.next() {
                        if c1.is_alphanumeric() || c1 == '_' {
                            self.pos += c1.len_utf8();
                        } else {
                            break;
                        }
                    }
                    if self.pos - start == 1 && c == '_' {
                        return Some((Span::from(start..self.pos), Token::Underscore));
                    }

                    match &self.input[start..self.pos] {
                        "def" => Token::Def,
                        "impl" => Token::Impl,
                        "where" => Token::Where,
                        "var" => Token::Var,
                        "true" => Token::True,
                        "false" => Token::False,
                        "type" => Token::Type,
                        "from" => Token::From,
                        "into" => Token::Into,
                        "select" => Token::Select,
                        "enum" => Token::Enum,
                        "struct" => Token::Struct,
                        s => Token::Name(s),
                    }
                }
                '0'..='9' => {
                    while let Some(c) = chars.next() {
                        if c.is_numeric() {
                            self.pos += c.len_utf8();
                        } else {
                            if c == '.' {
                                self.pos += c.len_utf8();
                                while let Some(c) = chars.next() {
                                    if c.is_numeric() {
                                        self.pos += c.len_utf8();
                                    } else {
                                        break;
                                    }
                                }
                                let s = &self.input[start..self.pos];
                                return Some((Span::from(start..self.pos), Token::Float(s)));
                            }
                            break;
                        }
                    }

                    let s = &self.input[start..self.pos];
                    Token::Int(s)
                }
                '"' => {
                    while let Some(c) = chars.next() {
                        self.pos += c.len_utf8();
                        if c == '"' {
                            break;
                        }
                    }

                    let s = &self.input[start + 1..self.pos - 1];
                    Token::String(s)
                }
                '\'' => {
                    while let Some(c) = chars.next() {
                        self.pos += c.len_utf8();
                        if c == '\'' {
                            break;
                        }
                    }

                    let s = &self.input[start + 1..self.pos - 1];
                    if s.len() != 1 {
                        return None;
                    }
                    Token::Char(s.chars().next().unwrap())
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '[' => Token::LBrack,
                ']' => Token::RBrack,
                '=' => {
                    if let Some('=') = chars.next() {
                        self.pos += 1;
                        Token::EqEq
                    } else {
                        Token::Eq
                    }
                }
                ':' => Token::Colon,
                '!' => {
                    if let Some('=') = chars.next() {
                        self.pos += 1;
                        Token::BangEq
                    } else {
                        return None;
                    }
                }
                '<' => {
                    if let Some('=') = chars.next() {
                        self.pos += 1;
                        Token::LAngleEq
                    } else {
                        Token::LAngle
                    }
                }
                '>' => {
                    if let Some('=') = chars.next() {
                        self.pos += 1;
                        Token::RAngleEq
                    } else {
                        Token::RAngle
                    }
                }
                '.' => {
                    if let Some('.') = chars.next() {
                        self.pos += 1;
                        Token::DotDot
                    } else {
                        Token::Dot
                    }
                }
                ';' => Token::SemiColon,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => {
                    if let Some('-') = chars.next() {
                        if let Some('-') = chars.next() {
                            self.pos += 2;
                            while let Some(c) = chars.next() {
                                self.pos += c.len_utf8();
                                if c == '-' {
                                    if let Some('-') = chars.next() {
                                        if let Some('-') = chars.next() {
                                            self.pos += 2;
                                            break;
                                        }
                                    }
                                }
                            }
                            Token::Code(&self.input[start + 3..self.pos - 3])
                        } else {
                            Token::Minus
                        }
                    } else {
                        Token::Minus
                    }
                }
                '*' => Token::Star,
                '/' => Token::Slash,
                '?' => Token::Question,
                _ => Token::Err,
            };

            return Some((Span::from(start..self.pos), token));
        }
    }
}
