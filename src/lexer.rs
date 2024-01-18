use crate::diag::Report;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Token<'a> {
    // Punctuations
    Eq,
    EqEq,
    Not,
    NotEq,
    Lt,
    Le,
    Gt,
    Ge,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    DotDot,
    Colon,
    ColonColon,
    AtSign,
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
    FatArrow,
    Bar,
    // Keywords
    And,
    As,
    Break,
    Continue,
    Compute,
    Def,
    Desc,
    Else,
    Enum,
    False,
    For,
    From,
    Group,
    If,
    Impl,
    Trait,
    In,
    Into,
    Join,
    Match,
    Of,
    On,
    Or,
    Order,
    Over,
    Return,
    Select,
    Struct,
    True,
    Type,
    Var,
    Where,
    While,
    With,
    // Literals
    Code(&'a str),
    Name(&'a str),
    Int(&'a str),
    IntSuffix(&'a str, &'a str),
    Float(&'a str),
    FloatSuffix(&'a str, &'a str),
    String(&'a str),
    Char(&'a str),
    // Special
    Err,
    Eof,
    Fun,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::Not => write!(f, "!"),
            Token::NotEq => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::Le => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::Ge => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::AtSign => write!(f, "@"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Colon => write!(f, ":"),
            Token::ColonColon => write!(f, "::"),
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
            Token::FatArrow => write!(f, "=>"),
            Token::Bar => write!(f, "|"),
            Token::And => write!(f, "and"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Def => write!(f, "def"),
            Token::Desc => write!(f, "desc"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::False => write!(f, "false"),
            Token::For => write!(f, "for"),
            Token::From => write!(f, "from"),
            Token::Group => write!(f, "group"),
            Token::If => write!(f, "if"),
            Token::Impl => write!(f, "impl"),
            Token::In => write!(f, "in"),
            Token::Into => write!(f, "into"),
            Token::Match => write!(f, "match"),
            Token::On => write!(f, "on"),
            Token::Or => write!(f, "or"),
            Token::Over => write!(f, "over"),
            Token::Return => write!(f, "return"),
            Token::Select => write!(f, "select"),
            Token::Struct => write!(f, "struct"),
            Token::True => write!(f, "true"),
            Token::Type => write!(f, "type"),
            Token::Var => write!(f, "var"),
            Token::Where => write!(f, "where"),
            Token::While => write!(f, "while"),
            Token::With => write!(f, "with"),
            Token::Join => write!(f, "join"),
            Token::Fun => write!(f, "fun"),
            Token::Order => write!(f, "order"),
            Token::Code(v) => write!(f, "{v}"),
            Token::Name(v) => write!(f, "{v}"),
            Token::Int(v) => write!(f, "{v}"),
            Token::IntSuffix(v, s) => write!(f, "{v}{s}"),
            Token::Float(v) => write!(f, "{v}"),
            Token::FloatSuffix(v, s) => write!(f, "{v}{s}"),
            Token::String(v) => write!(f, "{v}"),
            Token::Char(v) => write!(f, "{v}"),
            Token::Err => write!(f, "<err>"),
            Token::Eof => write!(f, "<eof>"),
            Token::Of => write!(f, "of"),
            Token::As => write!(f, "as"),
            Token::Compute => write!(f, "compute"),
            Token::Trait => write!(f, "trait"),
        }
    }
}

#[derive(Eq, Clone, Copy, Hash)]
pub enum Span {
    Source(u16, u32, u32),
    Generated,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Span::Source(file, start, end) => write!(f, "{file}:{start}..{end}"),
            Span::Generated => write!(f, "..."),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Span::Source(file, start, end) => write!(f, "{:?}:{}-{}", file, start, end),
            Span::Generated => write!(f, "<builtin>"),
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Span::Source(f0, s0, e0), Span::Source(f1, s1, e1)) => {
                f0 == f1 && s0 == s1 && e0 == e1
            }
            (Span::Generated, _) | (_, Span::Generated) => true,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::Generated
    }
}

impl Span {
    pub fn new(file: u16, range: std::ops::Range<u32>) -> Span {
        Span::Source(file, range.start, range.end)
    }

    pub fn file(&self) -> &u16 {
        match self {
            Span::Source(file, _, _) => file,
            Span::Generated => &0,
        }
    }

    pub fn start(&self) -> &u32 {
        match self {
            Span::Source(_, start, _) => start,
            Span::Generated => &0,
        }
    }

    pub fn end(&self) -> &u32 {
        match self {
            Span::Source(_, _, end) => end,
            Span::Generated => &0,
        }
    }
}

impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Self::Output {
        match (self, other) {
            (Span::Generated, Span::Generated) => Span::Generated,
            (Span::Generated, Span::Source(file, start, end)) => Span::new(file, start..end),
            (Span::Source(file, start, end), Span::Generated) => Span::new(file, start..end),
            (Span::Source(file, start, _), Span::Source(_, _, end)) => Span::new(file, start..end),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    eof: bool,
    pub file: u16,
    pub report: Report,
}

impl<'a> Lexer<'a> {
    pub fn new(file: u16, input: &'a str) -> Lexer<'a> {
        Lexer {
            file,
            input,
            eof: false,
            pos: 0,
            report: Report::new(),
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
                    while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = chars.next() {
                        self.pos += 1;
                    }
                    if self.pos - start == 1 && c == '_' {
                        return Some((
                            Span::new(self.file, start as u32..self.pos as u32),
                            Token::Underscore,
                        ));
                    }

                    match &self.input[start..self.pos] {
                        "and" => Token::And,
                        "as" => Token::As,
                        "break" => Token::Break,
                        "compute" => Token::Compute,
                        "continue" => Token::Continue,
                        "def" => Token::Def,
                        "else" => Token::Else,
                        "enum" => Token::Enum,
                        "false" => Token::False,
                        "for" => Token::For,
                        "from" => Token::From,
                        "fun" => Token::Fun,
                        "group" => Token::Group,
                        "trait" => Token::Trait,
                        "if" => Token::If,
                        "impl" => Token::Impl,
                        "in" => Token::In,
                        "into" => Token::Into,
                        "join" => Token::Join,
                        "match" => Token::Match,
                        "of" => Token::Of,
                        "on" => Token::On,
                        "or" => Token::Or,
                        "order" => Token::Order,
                        "over" => Token::Over,
                        "return" => Token::Return,
                        "select" => Token::Select,
                        "struct" => Token::Struct,
                        "true" => Token::True,
                        "type" => Token::Type,
                        "var" => Token::Var,
                        "where" => Token::Where,
                        "while" => Token::While,
                        "with" => Token::With,
                        s => Token::Name(s),
                    }
                }
                '0'..='9' => loop {
                    match chars.next() {
                        Some('0'..='9') => self.pos += 1,
                        Some('.') => match chars.next() {
                            Some('a'..='z' | 'A'..='Z' | '_') => {
                                let v = &self.input[start..self.pos];
                                break Token::Int(v);
                            }
                            Some('0'..='9') => {
                                self.pos += 1;
                                self.pos += 1;
                                let mut c = chars.next();
                                while let Some('0'..='9') = c {
                                    self.pos += 1;
                                    c = chars.next();
                                }
                                let v = &self.input[start..self.pos];
                                if let Some('a'..='z' | 'A'..='Z' | '_') = c {
                                    let start = self.pos;
                                    self.pos += 1;
                                    while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') =
                                        chars.next()
                                    {
                                        self.pos += 1;
                                    }
                                    let s = &self.input[start..self.pos];
                                    break Token::FloatSuffix(v, s);
                                } else {
                                    break Token::Float(v);
                                }
                            }
                            _ => {
                                self.pos += 1;
                                let v = &self.input[start..self.pos];
                                break Token::Float(v);
                            }
                        },
                        Some(c) => {
                            let v = &self.input[start..self.pos];
                            if let 'a'..='z' | 'A'..='Z' | '_' = c {
                                let start = self.pos;
                                self.pos += 1;
                                while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') =
                                    chars.next()
                                {
                                    self.pos += 1;
                                }
                                let s = &self.input[start..self.pos];
                                break Token::IntSuffix(v, s);
                            } else {
                                break Token::Int(v);
                            }
                        }
                        None => {
                            let s = &self.input[start..self.pos];
                            break Token::Int(s);
                        }
                    }
                },
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
                    let l = '"'.len_utf8();
                    Token::String(&self.input[start + l..self.pos - l])
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
                        let l = '\''.len_utf8();
                        Token::Char(&self.input[start + l..self.pos - l])
                    } else {
                        self.report.err(
                            Span::new(self.file, start as u32..self.pos as u32),
                            "Unexpected character",
                            format!("Unexpected character '{c}'"),
                        );
                        continue;
                    }
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '[' => Token::LBrack,
                ']' => Token::RBrack,
                '=' => match chars.next() {
                    Some('=') => {
                        self.pos += '='.len_utf8();
                        Token::EqEq
                    }
                    Some('>') => {
                        self.pos += '>'.len_utf8();
                        Token::FatArrow
                    }
                    _ => Token::Eq,
                },
                ':' => {
                    if let Some(':') = chars.next() {
                        self.pos += ':'.len_utf8();
                        Token::ColonColon
                    } else {
                        Token::Colon
                    }
                }
                '!' => {
                    if let Some('=') = chars.next() {
                        self.pos += '='.len_utf8();
                        Token::NotEq
                    } else {
                        Token::Not
                    }
                }
                '<' => {
                    if let Some('=') = chars.next() {
                        self.pos += '='.len_utf8();
                        Token::Le
                    } else {
                        Token::Lt
                    }
                }
                '>' => {
                    if let Some('=') = chars.next() {
                        self.pos += '='.len_utf8();
                        Token::Ge
                    } else {
                        Token::Gt
                    }
                }
                '.' => {
                    if let Some('.') = chars.next() {
                        self.pos += '.'.len_utf8();
                        Token::DotDot
                    } else {
                        Token::Dot
                    }
                }
                ';' => Token::SemiColon,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '|' => Token::Bar,
                '-' => {
                    if let (Some('-'), Some('-')) = (chars.next(), chars.next()) {
                        self.pos += '-'.len_utf8() * 2;
                        loop {
                            let c = chars.next()?;
                            self.pos += c.len_utf8();
                            if c == '-' {
                                let c = chars.next()?;
                                self.pos += c.len_utf8();
                                if c == '-' {
                                    let c = chars.next()?;
                                    self.pos += c.len_utf8();
                                    if c == '-' {
                                        break;
                                    }
                                }
                            }
                        }
                        let l = '-'.len_utf8() * 3;
                        Token::Code(&self.input[start + l..self.pos - l])
                    } else {
                        Token::Minus
                    }
                }
                '*' => Token::Star,
                '/' => Token::Slash,
                '?' => Token::Question,
                '@' => Token::AtSign,
                t => {
                    self.report.err(
                        Span::new(self.file, start as u32..self.pos as u32),
                        "Unexpected character",
                        format!("Unexpected character '{t}'"),
                    );
                    Token::Err
                }
            };
            return Some((Span::new(self.file, start as u32..self.pos as u32), token));
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
                let span = Span::new(self.file, self.pos as u32..self.pos as u32);
                Some((span, Token::Eof))
            }
        })
    }
}
