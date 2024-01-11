use crate::diag::Diags;

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
    Char(&'a str),
    // Special
    Err,
    Eof,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::BangEq => write!(f, "!="),
            Token::LAngle => write!(f, "<"),
            Token::LAngleEq => write!(f, "<="),
            Token::RAngle => write!(f, ">"),
            Token::RAngleEq => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::Underscore => write!(f, "_"),
            Token::Question => write!(f, "?"),
            Token::Def => write!(f, "def"),
            Token::Impl => write!(f, "impl"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Where => write!(f, "where"),
            Token::Var => write!(f, "var"),
            Token::Type => write!(f, "type"),
            Token::From => write!(f, "from"),
            Token::Into => write!(f, "into"),
            Token::Select => write!(f, "select"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Code(s) => write!(f, "{s}"),
            Token::Name(s) => write!(f, "{s}"),
            Token::Int(s) => write!(f, "{s}"),
            Token::Float(s) => write!(f, "{s}"),
            Token::String(s) => write!(f, "{s}"),
            Token::Char(s) => write!(f, "{s}"),
            Token::Err => write!(f, "<err>"),
            Token::Eof => write!(f, "<eof>"),
        }
    }
}

#[derive(Eq, Clone, Copy, Hash)]
pub enum Span {
    Source(u16, u32, u32),
    Builtin,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Span::Source(file, start, end) => write!(f, "{:?}:{}-{}", file, start, end),
            Span::Builtin => write!(f, "<builtin>"),
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Span::Source(f0, s0, e0), Span::Source(f1, s1, e1)) => {
                f0 == f1 && s0 == s1 && e0 == e1
            }
            (Span::Builtin, _) | (_, Span::Builtin) => true,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::Builtin
    }
}

impl Span {
    pub fn new(file: u16, start: u32, end: u32) -> Span {
        Span::Source(file, start, end)
    }

    pub fn file(&self) -> &u16 {
        match self {
            Span::Source(file, _, _) => file,
            Span::Builtin => unreachable!(),
        }
    }

    pub fn start(&self) -> &u32 {
        match self {
            Span::Source(_, start, _) => start,
            Span::Builtin => unreachable!(),
        }
    }

    pub fn end(&self) -> &u32 {
        match self {
            Span::Source(_, _, end) => end,
            Span::Builtin => unreachable!(),
        }
    }
}

impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Self::Output {
        match (self, other) {
            (Span::Builtin, Span::Builtin) => Span::Builtin,
            (Span::Builtin, Span::Source(file, start, end)) => Span::new(file, start, end),
            (Span::Source(file, start, end), Span::Builtin) => Span::new(file, start, end),
            (Span::Source(file, start, _), Span::Source(_, _, end)) => Span::new(file, start, end),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    eof: bool,
    file: u16,
    pub diags: Diags,
}

impl<'a> Lexer<'a> {
    pub fn new(file: u16, input: &'a str) -> Lexer<'a> {
        Lexer {
            file,
            input,
            eof: false,
            pos: 0,
            diags: Diags::new(),
        }
    }

    pub fn lex(&mut self) -> Option<(Span, Token<'a>)> {
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
                        return Some((
                            Span::new(self.file, start as u32, self.pos as u32),
                            Token::Underscore,
                        ));
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
                                return Some((
                                    Span::new(self.file, start as u32, self.pos as u32),
                                    Token::Float(s),
                                ));
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
                        if c == '\\' {
                            let c = chars.next()?;
                            self.pos += c.len_utf8();
                        }
                        if c == '"' {
                            break;
                        }
                    }
                    Token::String(&self.input[start + 1..self.pos - 1])
                }
                '\'' => {
                    let c = chars.next()?;
                    self.pos += c.len_utf8();
                    if c == '\\' {
                        let c = chars.next()?;
                        self.pos += c.len_utf8();
                    }
                    let c = chars.next()?;
                    self.pos += c.len_utf8();
                    if c == '\'' {
                        Token::Char(&self.input[start + 1..self.pos - 1])
                    } else {
                        return None;
                    }
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
                t => {
                    self.diags.err(
                        Span::new(self.file, start as u32, self.pos as u32),
                        "Unexpected character",
                        format!("Unexpected character '{t}'"),
                    );
                    Token::Err
                }
            };
            return Some((Span::new(self.file, start as u32, self.pos as u32), token));
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Span, Token<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        self.lex().or_else(|| {
            if self.eof {
                return None;
            } else {
                self.eof = true;
                Some((
                    Span::new(self.file, self.pos as u32, self.pos as u32),
                    Token::Eof,
                ))
            }
        })
    }
}
