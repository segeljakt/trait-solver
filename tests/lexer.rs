use aqua::diag::Sources;
use aqua::lexer::Lexer;
use aqua::lexer::Span;
use aqua::lexer::Token;

#[test]
fn test_lexer_int0() {
    let mut lexer = Lexer::new(0, "123");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..3), Token::Int("123"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 3..3), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_float0() {
    let mut lexer = Lexer::new(0, "123.456");
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 0..7), Token::Float("123.456")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 7..7), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_float1() {
    let mut lexer = Lexer::new(0, "123.");
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 0..4), Token::Float("123.")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 4..4), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_name0() {
    let mut lexer = Lexer::new(0, "abc");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..3), Token::Name("abc"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 3..3), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_name1() {
    let mut lexer = Lexer::new(0, "_ _1 _a a_ a_1");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Underscore)));
    assert_eq!(lexer.next(), Some((Span::new(0, 2..4), Token::Name("_1"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..7), Token::Name("_a"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..10), Token::Name("a_"))));
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 11..14), Token::Name("a_1")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 14..14), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_keywords0() {
    let mut lexer = Lexer::new(0, "def impl struct enum var type");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..3), Token::Def)));
    assert_eq!(lexer.next(), Some((Span::new(0, 4..8), Token::Impl)));
    assert_eq!(lexer.next(), Some((Span::new(0, 9..15), Token::Struct)));
    assert_eq!(lexer.next(), Some((Span::new(0, 16..20), Token::Enum)));
    assert_eq!(lexer.next(), Some((Span::new(0, 21..24), Token::Var)));
    assert_eq!(lexer.next(), Some((Span::new(0, 25..29), Token::Type)));
    assert_eq!(lexer.next(), Some((Span::new(0, 29..29), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_keywords1() {
    let mut lexer = Lexer::new(
        0,
        "true false or and if else while for break continue return match fun",
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 0..4), Token::True)));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..10), Token::False)));
    assert_eq!(lexer.next(), Some((Span::new(0, 11..13), Token::Or)));
    assert_eq!(lexer.next(), Some((Span::new(0, 14..17), Token::And)));
    assert_eq!(lexer.next(), Some((Span::new(0, 18..20), Token::If)));
    assert_eq!(lexer.next(), Some((Span::new(0, 21..25), Token::Else)));
    assert_eq!(lexer.next(), Some((Span::new(0, 26..31), Token::While)));
    assert_eq!(lexer.next(), Some((Span::new(0, 32..35), Token::For)));
    assert_eq!(lexer.next(), Some((Span::new(0, 36..41), Token::Break)));
    assert_eq!(lexer.next(), Some((Span::new(0, 42..50), Token::Continue)));
    assert_eq!(lexer.next(), Some((Span::new(0, 51..57), Token::Return)));
    assert_eq!(lexer.next(), Some((Span::new(0, 58..63), Token::Match)));
    assert_eq!(lexer.next(), Some((Span::new(0, 64..67), Token::Fun)));
    assert_eq!(lexer.next(), Some((Span::new(0, 67..67), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_keywords2() {
    let mut lexer = Lexer::new(0, "from into where select group with over join on");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..4), Token::From)));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..9), Token::Into)));
    assert_eq!(lexer.next(), Some((Span::new(0, 10..15), Token::Where)));
    assert_eq!(lexer.next(), Some((Span::new(0, 16..22), Token::Select)));
    assert_eq!(lexer.next(), Some((Span::new(0, 23..28), Token::Group)));
    assert_eq!(lexer.next(), Some((Span::new(0, 29..33), Token::With)));
    assert_eq!(lexer.next(), Some((Span::new(0, 34..38), Token::Over)));
    assert_eq!(lexer.next(), Some((Span::new(0, 39..43), Token::Join)));
    assert_eq!(lexer.next(), Some((Span::new(0, 44..46), Token::On)));
    assert_eq!(lexer.next(), Some((Span::new(0, 46..46), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_punct0() {
    let mut lexer = Lexer::new(0, "= == != < <= > >= + - * / . .. , : ; ? _ => @ ::");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Eq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 2..4), Token::EqEq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..7), Token::BangEq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..9), Token::LAngle)));
    assert_eq!(lexer.next(), Some((Span::new(0, 10..12), Token::LAngleEq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 13..14), Token::RAngle)));
    assert_eq!(lexer.next(), Some((Span::new(0, 15..17), Token::RAngleEq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 18..19), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 20..21), Token::Minus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 22..23), Token::Star)));
    assert_eq!(lexer.next(), Some((Span::new(0, 24..25), Token::Slash)));
    assert_eq!(lexer.next(), Some((Span::new(0, 26..27), Token::Dot)));
    assert_eq!(lexer.next(), Some((Span::new(0, 28..30), Token::DotDot)));
    assert_eq!(lexer.next(), Some((Span::new(0, 31..32), Token::Comma)));
    assert_eq!(lexer.next(), Some((Span::new(0, 33..34), Token::Colon)));
    assert_eq!(lexer.next(), Some((Span::new(0, 35..36), Token::SemiColon)));
    assert_eq!(lexer.next(), Some((Span::new(0, 37..38), Token::Question)));
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 39..40), Token::Underscore))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 41..43), Token::Arrow)));
    assert_eq!(lexer.next(), Some((Span::new(0, 44..45), Token::AtSign)));
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 46..48), Token::ColonColon))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 48..48), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_string0() {
    let mut lexer = Lexer::new(0, r#""abc""#);
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 0..5), Token::String("abc")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 5..5), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_string1() {
    let mut lexer = Lexer::new(0, r#""abc\"def""#);
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 0..10), Token::String(r#"abc\"def"#)))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 10..10), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_char0() {
    let mut lexer = Lexer::new(0, "'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j'");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..3), Token::Char("a"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 4..7), Token::Char("b"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..11), Token::Char("c"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 12..15), Token::Char("d"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 16..19), Token::Char("e"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 20..23), Token::Char("f"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 24..27), Token::Char("g"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 28..31), Token::Char("h"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 32..35), Token::Char("i"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 36..39), Token::Char("j"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 39..39), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_char1() {
    let mut lexer = Lexer::new(0, r"'\n' '\t' '\r' '\0' '\\' '\''");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..4), Token::Char(r"\n"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..9), Token::Char(r"\t"))));
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 10..14), Token::Char(r"\r")))
    );
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 15..19), Token::Char(r"\0")))
    );
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 20..24), Token::Char(r"\\")))
    );
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 25..29), Token::Char(r"\'")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 29..29), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_separators0() {
    let mut lexer = Lexer::new(0, "[[]] (()) {{}}");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::LBrack)));
    assert_eq!(lexer.next(), Some((Span::new(0, 1..2), Token::LBrack)));
    assert_eq!(lexer.next(), Some((Span::new(0, 2..3), Token::RBrack)));
    assert_eq!(lexer.next(), Some((Span::new(0, 3..4), Token::RBrack)));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..6), Token::LParen)));
    assert_eq!(lexer.next(), Some((Span::new(0, 6..7), Token::LParen)));
    assert_eq!(lexer.next(), Some((Span::new(0, 7..8), Token::RParen)));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..9), Token::RParen)));
    assert_eq!(lexer.next(), Some((Span::new(0, 10..11), Token::LBrace)));
    assert_eq!(lexer.next(), Some((Span::new(0, 11..12), Token::LBrace)));
    assert_eq!(lexer.next(), Some((Span::new(0, 12..13), Token::RBrace)));
    assert_eq!(lexer.next(), Some((Span::new(0, 13..14), Token::RBrace)));
    assert_eq!(lexer.next(), Some((Span::new(0, 14..14), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_code0() {
    let mut lexer = Lexer::new(0, "--- fn main() {} ---");
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 0..20), Token::Code(" fn main() {} ")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 20..20), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_code1() {
    let mut lexer = Lexer::new(0, "1 + 2; --- fn main() {} --- 3 + 4;");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Int("1"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 2..3), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 4..5), Token::Int("2"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 5..6), Token::SemiColon)));
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 7..27), Token::Code(" fn main() {} ")))
    );
    assert_eq!(lexer.next(), Some((Span::new(0, 28..29), Token::Int("3"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 30..31), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 32..33), Token::Int("4"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 33..34), Token::SemiColon)));
    assert_eq!(lexer.next(), Some((Span::new(0, 34..34), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_comments0() {
    let mut lexer = Lexer::new(0, "1 # + 2");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Int("1"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 7..7), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_comments1() {
    let mut lexer = Lexer::new(0, "1 # + 2\n3");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Int("1"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..9), Token::Int("3"))));
    assert_eq!(lexer.next(), Some((Span::new(0, 9..9), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_err0() {
    let mut lexer = Lexer::new(0, "\\ % ^ & | ~ ` $");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 2..3), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 4..5), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 6..7), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..9), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 10..11), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 12..13), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 14..15), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 15..15), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_err1() {
    let mut sources = Sources::new();
    let source = "%";
    let file = sources.add("file", source);
    let mut lexer = Lexer::new(file, source);
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(0, 1..1), Token::Eof)));
    assert_eq!(lexer.next(), None);
    assert_eq!(lexer.diags.string(&mut sources).unwrap(), "Error: Unexpected character\n   ╭─[file:1:1]\n   │\n 1 │ %\n   │ ┬  \n   │ ╰── Unexpected character '%'\n───╯\n\n");
}

#[test]
fn test_lexer_unused0() {
    let mut lexer = Lexer::new(0, "-> <- ..=");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..1), Token::Minus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 1..2), Token::RAngle)));
    assert_eq!(lexer.next(), Some((Span::new(0, 3..4), Token::LAngle)));
    assert_eq!(lexer.next(), Some((Span::new(0, 4..5), Token::Minus)));
    assert_eq!(lexer.next(), Some((Span::new(0, 6..8), Token::DotDot)));
    assert_eq!(lexer.next(), Some((Span::new(0, 8..9), Token::Eq)));
    assert_eq!(lexer.next(), Some((Span::new(0, 9..9), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_eof0() {
    let mut lexer = Lexer::new(0, "");
    assert_eq!(lexer.next(), Some((Span::new(0, 0..0), Token::Eof)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_eof1() {
    let mut lexer = Lexer::new(0, " ");
    assert_eq!(lexer.next(), Some((Span::new(0, 1..1), Token::Eof)));
    assert_eq!(lexer.next(), None);
}
