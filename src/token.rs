use std::ops::Range;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Token<'a> {
    Def,
    Impl,
    Where,
    Var,
    Type,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
    Colon,
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
    True,
    False,
    Name(&'a str),
    Int(&'a str),
    Float(&'a str),
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
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
                ' ' | '\n' | '\t' => continue,
                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some(c) = chars.next() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => self.pos += c.len_utf8(),
                            _ => {
                                if c == '_' {
                                    return Some((Span::from(start..self.pos), Token::Underscore));
                                }
                                break;
                            }
                        }
                    }

                    match &self.input[start..self.pos] {
                        "def" => Token::Def,
                        "impl" => Token::Impl,
                        "where" => Token::Where,
                        "var" => Token::Var,
                        "true" => Token::True,
                        "false" => Token::False,
                        s => Token::Name(s),
                    }
                }
                '0'..='9' => {
                    while let Some(c) = chars.next() {
                        if c.is_numeric() {
                            self.pos += c.len_utf8();
                        } else {
                            break;
                        }
                    }

                    let s = &self.input[start..self.pos];
                    Token::Int(s)
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '[' => Token::LBrack,
                ']' => Token::RBrack,
                '=' => Token::Eq,
                ':' => Token::Colon,
                ';' => Token::SemiColon,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '?' => Token::Question,
                _ => return None,
            };

            return Some((Span::from(start..self.pos), token));
        }
    }
}
