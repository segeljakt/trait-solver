use trait_solver::token::Lexer;
use trait_solver::token::Span;
use trait_solver::token::Token;

#[test]
fn test_lexer_int0() {
    let mut lexer = Lexer::new("123");
    assert_eq!(lexer.next(), Some((Span::new(0, 3), Token::Int("123"))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_float0() {
    let mut lexer = Lexer::new("123.456");
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 7), Token::Float("123.456")))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_name0() {
    let mut lexer = Lexer::new("abc");
    assert_eq!(lexer.next(), Some((Span::new(0, 3), Token::Name("abc"))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_name1() {
    let mut lexer = Lexer::new("_ _1 _a a_ a_1");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::Underscore)));
    assert_eq!(lexer.next(), Some((Span::new(2, 4), Token::Name("_1"))));
    assert_eq!(lexer.next(), Some((Span::new(5, 7), Token::Name("_a"))));
    assert_eq!(lexer.next(), Some((Span::new(8, 10), Token::Name("a_"))));
    assert_eq!(lexer.next(), Some((Span::new(11, 14), Token::Name("a_1"))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_keywords0() {
    let mut lexer = Lexer::new("def impl struct enum where var type from into select true false");
    assert_eq!(lexer.next(), Some((Span::new(0, 3), Token::Def)));
    assert_eq!(lexer.next(), Some((Span::new(4, 8), Token::Impl)));
    assert_eq!(lexer.next(), Some((Span::new(9, 15), Token::Struct)));
    assert_eq!(lexer.next(), Some((Span::new(16, 20), Token::Enum)));
    assert_eq!(lexer.next(), Some((Span::new(21, 26), Token::Where)));
    assert_eq!(lexer.next(), Some((Span::new(27, 30), Token::Var)));
    assert_eq!(lexer.next(), Some((Span::new(31, 35), Token::Type)));
    assert_eq!(lexer.next(), Some((Span::new(36, 40), Token::From)));
    assert_eq!(lexer.next(), Some((Span::new(41, 45), Token::Into)));
    assert_eq!(lexer.next(), Some((Span::new(46, 52), Token::Select)));
    assert_eq!(lexer.next(), Some((Span::new(53, 57), Token::True)));
    assert_eq!(lexer.next(), Some((Span::new(58, 63), Token::False)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_punct0() {
    let mut lexer = Lexer::new("= == != < <= > >= + - * / . .. , : ;");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::Eq)));
    assert_eq!(lexer.next(), Some((Span::new(2, 4), Token::EqEq)));
    assert_eq!(lexer.next(), Some((Span::new(5, 7), Token::BangEq)));
    assert_eq!(lexer.next(), Some((Span::new(8, 9), Token::LAngle)));
    assert_eq!(lexer.next(), Some((Span::new(10, 12), Token::LAngleEq)));
    assert_eq!(lexer.next(), Some((Span::new(13, 14), Token::RAngle)));
    assert_eq!(lexer.next(), Some((Span::new(15, 17), Token::RAngleEq)));
    assert_eq!(lexer.next(), Some((Span::new(18, 19), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(20, 21), Token::Minus)));
    assert_eq!(lexer.next(), Some((Span::new(22, 23), Token::Star)));
    assert_eq!(lexer.next(), Some((Span::new(24, 25), Token::Slash)));
    assert_eq!(lexer.next(), Some((Span::new(26, 27), Token::Dot)));
    assert_eq!(lexer.next(), Some((Span::new(28, 30), Token::DotDot)));
    assert_eq!(lexer.next(), Some((Span::new(31, 32), Token::Comma)));
    assert_eq!(lexer.next(), Some((Span::new(33, 34), Token::Colon)));
    assert_eq!(lexer.next(), Some((Span::new(35, 36), Token::SemiColon)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_string0() {
    let mut lexer = Lexer::new(r#""abc""#);
    assert_eq!(lexer.next(), Some((Span::new(0, 5), Token::String("abc"))));
    assert_eq!(lexer.next(), None);
}

// #[test]
// fn test_lexer_string1() {
//     let mut lexer = Lexer::new(r#""abc\"def""#);
//     assert_eq!(
//         lexer.next(),
//         Some((Span::new(0, 10), Token::String(r#"abc\"def"#)))
//     );
//     assert_eq!(lexer.next(), None);
// }

#[test]
fn test_lexer_char0() {
    let mut lexer = Lexer::new("'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j'");
    assert_eq!(lexer.next(), Some((Span::new(0, 3), Token::Char('a'))));
    assert_eq!(lexer.next(), Some((Span::new(4, 7), Token::Char('b'))));
    assert_eq!(lexer.next(), Some((Span::new(8, 11), Token::Char('c'))));
    assert_eq!(lexer.next(), Some((Span::new(12, 15), Token::Char('d'))));
    assert_eq!(lexer.next(), Some((Span::new(16, 19), Token::Char('e'))));
    assert_eq!(lexer.next(), Some((Span::new(20, 23), Token::Char('f'))));
    assert_eq!(lexer.next(), Some((Span::new(24, 27), Token::Char('g'))));
    assert_eq!(lexer.next(), Some((Span::new(28, 31), Token::Char('h'))));
    assert_eq!(lexer.next(), Some((Span::new(32, 35), Token::Char('i'))));
    assert_eq!(lexer.next(), Some((Span::new(36, 39), Token::Char('j'))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_separators0() {
    let mut lexer = Lexer::new("[[]] (()) {{}}");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::LBrack)));
    assert_eq!(lexer.next(), Some((Span::new(1, 2), Token::LBrack)));
    assert_eq!(lexer.next(), Some((Span::new(2, 3), Token::RBrack)));
    assert_eq!(lexer.next(), Some((Span::new(3, 4), Token::RBrack)));
    assert_eq!(lexer.next(), Some((Span::new(5, 6), Token::LParen)));
    assert_eq!(lexer.next(), Some((Span::new(6, 7), Token::LParen)));
    assert_eq!(lexer.next(), Some((Span::new(7, 8), Token::RParen)));
    assert_eq!(lexer.next(), Some((Span::new(8, 9), Token::RParen)));
    assert_eq!(lexer.next(), Some((Span::new(10, 11), Token::LBrace)));
    assert_eq!(lexer.next(), Some((Span::new(11, 12), Token::LBrace)));
    assert_eq!(lexer.next(), Some((Span::new(12, 13), Token::RBrace)));
    assert_eq!(lexer.next(), Some((Span::new(13, 14), Token::RBrace)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_code0() {
    let mut lexer = Lexer::new("--- fn main() {} ---");
    assert_eq!(
        lexer.next(),
        Some((Span::new(0, 20), Token::Code(" fn main() {} ")))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_code1() {
    let mut lexer = Lexer::new("1 + 2; --- fn main() {} --- 3 + 4;");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::Int("1"))));
    assert_eq!(lexer.next(), Some((Span::new(2, 3), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(4, 5), Token::Int("2"))));
    assert_eq!(lexer.next(), Some((Span::new(5, 6), Token::SemiColon)));
    assert_eq!(
        lexer.next(),
        Some((Span::new(7, 27), Token::Code(" fn main() {} ")))
    );
    assert_eq!(lexer.next(), Some((Span::new(28, 29), Token::Int("3"))));
    assert_eq!(lexer.next(), Some((Span::new(30, 31), Token::Plus)));
    assert_eq!(lexer.next(), Some((Span::new(32, 33), Token::Int("4"))));
    assert_eq!(lexer.next(), Some((Span::new(33, 34), Token::SemiColon)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_comments0() {
    let mut lexer = Lexer::new("1 # + 2");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::Int("1"))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_lexer_err0() {
    let mut lexer = Lexer::new("\\ % ^ & | ~ ` @ $ :: -> =>");
    assert_eq!(lexer.next(), Some((Span::new(0, 1), Token::Err)));
    assert_eq!(lexer.next(), Some((Span::new(2, 3), Token::Err)));
}
