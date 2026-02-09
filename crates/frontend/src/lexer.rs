use crate::token::{Keyword, Operator, Punctuator, Span, Token, TokenKind};
use std::{iter::Peekable, result, str::Chars};

pub struct Lexer {
    start: usize,
    index: usize,
}

#[derive(Debug, Clone)]
pub struct LexerErr {
    pub span: Span,
    pub msg: String,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer { start: 0, index: 0 }
    }

    fn bump(&mut self, chars: &mut Peekable<Chars>) -> Option<char> {
        let ch = chars.next();
        if ch.is_some() {
            self.index += 1;
        }
        ch
    }

    ///
    /// Takes source code as input and returns Vector of supported C tokens
    ///
    pub fn tokenize(&mut self, source_code: &str) -> Result<Vec<Token>, LexerErr> {
        let mut result: Vec<Token> = Vec::new();
        let mut chars = source_code.chars().peekable();
        let mut cur_token = String::new();

        while let Some(&ch) = chars.peek() {
            // if ch is whitespace current token ends
            if ch.is_whitespace() {
                if let Some(kind) = parse_long_token(&cur_token) {
                    result.push(self.token(kind));
                    self.start = self.index;
                }
                self.bump(&mut chars);
                self.start = self.index;
                cur_token.clear();
            } else if ch as u32 == 0x22 {
                // check ", span starts from opening " to closing "
                let token = self.parse_string_literal(&mut chars)?;
                result.push(token);
            } else if ch as u32 == 0x27 {
                // check '
                let token = self.parse_char_literal(&mut chars)?;
                result.push(token);
                self.start = self.index;
            } else if let Ok(_) = Operator::from_str(&ch.to_string()) {
                if let Some(kind) = parse_long_token(&cur_token) {
                    result.push(self.token(kind));
                    self.start = self.index;
                };
                // Check for two-character operators
                let mut op = ch.to_string();
                self.bump(&mut chars); // Consume first character
                if let Some(&next_ch) = chars.peek() {
                    let possible_op = format!("{op}{next_ch}");
                    if Operator::from_str(&possible_op).is_ok() {
                        op = possible_op;
                        self.bump(&mut chars); // Consume second character
                    }
                }
                result.push(self.token(TokenKind::Operator(Operator::from_str(&op).unwrap())));
                self.start = self.index;
                cur_token.clear();
            } else if let Ok(punctuator) = Punctuator::from_str(&ch.to_string()) {
                if let Some(kind) = parse_long_token(&cur_token) {
                    result.push(self.token(kind));
                    self.start = self.index;
                }
                cur_token.clear();
                self.bump(&mut chars);
                result.push(self.token(TokenKind::Punctuator(punctuator)));
                self.start = self.index;
            } else {
                cur_token.push(ch);
                self.bump(&mut chars);
            }
        }
        if let Some(kind) = parse_long_token(&cur_token) {
            result.push(self.token(kind));
            self.start = self.index;
        }
        assert!(self.bump(&mut chars).is_none()); // should be None
        // start = len(source_code) and end = start + 1;
        result.push(self.token(TokenKind::EOF));
        Ok(result)
    }

    fn parse_char_literal(&mut self, chars: &mut Peekable<Chars>) -> Result<Token, LexerErr> {
        // bump opening '
        let opening = self.expect_char(chars, '\'', "expected opening ' for char literal")?;
        let val = self.parse_char_value(chars)?;
        let closing = self.expect_char(chars, '\'', "expected closing ' for char literal")?;
        Ok(self.token(TokenKind::CharLiteral(val)))
    }

    fn parse_char_value(&mut self, chars: &mut Peekable<Chars>) -> Result<i32, LexerErr> {
        let ch = self
            .bump(chars)
            .ok_or_else(|| self.lex_err("unexpected end of input in char literal"))?;

        if ch != '\\' {
            return Ok(ch as i32);
        }

        // escape sequence
        let esc = self
            .bump(chars)
            .ok_or_else(|| self.lex_err("incomplete escape sequence"))?;

        if esc.is_digit(8) {
            let ch = self
                .parse_octal_literal(chars, esc)
                .map_err(|err| self.lex_err(&err))?;
            return Ok(ch);
        };

        let result = match esc {
            'a' => Ok('\u{07}' as i32),
            'b' => Ok('\u{08}' as i32),
            'f' => Ok('\u{0C}' as i32),
            'n' => Ok('\n' as i32),
            't' => Ok('\t' as i32),
            'v' => Ok('\u{0B}' as i32),
            'r' => Ok('\r' as i32),
            '\'' => Ok('\'' as i32),
            '\"' => Ok('\"' as i32),
            '\\' => Ok('\\' as i32),
            '?' => Ok('?' as i32),
            'x' => self.parse_hex_literal(chars),
            'u' | 'U' => self.parse_unicode_escape(chars, esc),
            _ => Err(format!("unknown escape: {}", esc)),
        };

        return result.map_err(|msg| self.lex_err(&msg));
    }

    fn parse_octal_literal(
        &mut self,
        chars: &mut Peekable<Chars>,
        first_digit: char,
    ) -> Result<i32, String> {
        let mut octal = String::new();
        octal.push(first_digit);
        for _ in 0..2 {
            if let Some(next) = chars.clone().next() {
                if next.is_digit(8) {
                    // consume it
                    octal.push(self.bump(chars).unwrap());
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if octal.len() == 0 || octal.len() > 3 {
            return Err(format!("invalid octal literal length: {}", octal.len()));
        }
        let num = i32::from_str_radix(&octal, 8)
            .map_err(|e| format!("invalid octal escape literal '{}': {:?}", octal, e))?;

        if num > 0x10FFFF || (0xD800..=0xDFFF).contains(&num) {
            return Err(format!(
                "invalid code point from octal literal: 0o{} (0x{:X})",
                octal, num
            ));
        }
        Ok(num)
    }

    fn parse_hex_literal(&mut self, chars: &mut Peekable<Chars>) -> Result<i32, String> {
        let mut hex = String::new();
        while let Some(next) = chars.clone().next() {
            if next.is_digit(16) {
                hex.push(self.bump(chars).unwrap());
            } else {
                break;
            }
        }
        if hex.len() == 0 {
            return Err("incomplete hex escape: expected at least 1 hex digit".to_string());
        }

        let num = i32::from_str_radix(&hex, 16)
            .map_err(|e| format!("invalid hex escape literal '{}': {:?}", hex, e))?;

        if num > 0x10FFFF || (0xD800..=0xDFFF).contains(&num) {
            return Err(format!(
                "invalid code point from hex literal: {} (0x{:X})",
                hex, num
            ));
        }

        Ok(num)
    }

    fn parse_unicode_escape(
        &mut self,
        chars: &mut Peekable<Chars>,
        kind: char,
    ) -> Result<i32, String> {
        let required_digits = match kind {
            'u' => 4,
            'U' => 8,
            _ => return Err(format!("invalid Unicode escape prefix: {}", kind)),
        };
        let mut value: u32 = 0;
        for i in 0..required_digits {
            let ch = self.bump(chars).ok_or(format!(
                "incomplete Unicode escape: expected {} hex digits, found {}",
                required_digits, i
            ))?;
            let digit = ch
                .to_digit(16)
                .ok_or(format!("invalid hex digit '{}' in Unicode escape", ch))?;
            value = (value << 4) | digit;
        }

        // Check if the value is a valid Unicode scalar
        if (value > 0x10FFFF) || (value >= 0xD800 && value <= 0xDFFF) {
            return Err(format!("invalid Unicode code point: 0x{:X}", value));
        }

        // Convert to int
        Ok(value as i32)
    }

    fn parse_string_literal(&mut self, chars: &mut Peekable<Chars>) -> Result<Token, LexerErr> {
        let opening = self.expect_char(chars, '\"', "expected opening \"")?;
        let mut string = Vec::new();
        while let Some(&next) = chars.peek() {
            if next == '\"' {
                break;
            }
            let ch = self.parse_char_value(chars)?;
            string.push(ch);
        }
        let closing = self.expect_char(chars, '\"', "expected closing\"")?;
        Ok(self.token(TokenKind::StringLiteral(string)))
    }

    fn expect_char(
        &mut self,
        chars: &mut Peekable<Chars>,
        expected: char,
        msg: &str,
    ) -> Result<char, LexerErr> {
        let ch = self.bump(chars).ok_or(LexerErr {
            span: Span {
                start: self.start,
                end: self.index,
            },
            msg: msg.to_string(),
        })?;
        if ch != expected {
            return Err(LexerErr {
                span: Span {
                    start: self.start,
                    end: self.index,
                },
                msg: format!("{}, found: {}", msg, ch),
            });
        }
        Ok(ch)
    }

    fn lex_err(&self, msg: &str) -> LexerErr {
        LexerErr {
            span: Span {
                start: self.start,
                end: self.index,
            },
            msg: msg.to_string(),
        }
    }

    fn token(&self, kind: TokenKind) -> Token {
        Token {
            kind: kind,
            span: Span {
                start: self.start,
                end: self.index,
            },
        }
    }
}

/// parses long tokens such as keywords, constants and identifiers
fn parse_long_token(s: &str) -> Option<TokenKind> {
    if s.is_empty() {
        None
    } else if let Ok(keyword) = Keyword::from_str(s) {
        Some(TokenKind::Keyword(keyword))
    } else if is_int_literal(s) {
        Some(TokenKind::IntLiteral(s.to_string()))
    } else if is_identifier(s) {
        Some(TokenKind::Identifier(s.to_string()))
    } else {
        None
    }
}

fn is_int_literal(token: &str) -> bool {
    token.chars().all(|c| c.is_ascii_digit())
}

fn is_float_literal(token: &str) -> bool {
    token.contains(".") || token.contains("e") || token.contains("E") || token.contains("f")
}

fn is_identifier(token: &str) -> bool {
    let valid_lowercase_range = 'a'..='z';
    let valid_uppercase_range = 'A'..='Z';
    let valid_numbers_range = '0'..='9';
    let mut iterator = token.chars();
    if let Some(first_letter) = iterator.next() {
        if !(valid_lowercase_range.contains(&first_letter)
            || valid_uppercase_range.contains(&first_letter)
            || first_letter == '_')
        {
            return false;
        }
    } else {
        return false;
    }
    for ch in token.chars() {
        if !(valid_uppercase_range.contains(&ch)
            || valid_lowercase_range.contains(&ch)
            || valid_numbers_range.contains(&ch)
            || ch == '_')
        {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn only_keywords() {
        let input = "for while if else";
        let mut lexer = Lexer::new();
        let output = lexer.tokenize(input).unwrap();
        output.iter().for_each(|token| println!("{token:?}"));
        assert_eq!(output.len(), 5);
    }

    #[test]
    fn simple_return() {
        let input = "\
        int main() {\
            return 2;\
        }";
        let mut lexer = Lexer::new();
        let output = lexer.tokenize(input);
        output.iter().for_each(|token| println!("{token:?}"));
        assert_eq!(output.unwrap().len(), 10);
    }

    #[test]
    fn for_loop() {
        let input_regular = "for(int i = 0; i < 1; i++) {";
        let input_minimal_spaces = "for(int i=0;i<1;i++){";
        let mut regular_lexer = Lexer::new();
        let mut short_lexer = Lexer::new();
        let first_output = regular_lexer.tokenize(input_regular);
        let second_output = short_lexer.tokenize(input_minimal_spaces);
        first_output
            .iter()
            .for_each(|cur| println!("{:?} cur token", cur));
        second_output
            .iter()
            .for_each(|cur| println!("{:?} cur token", cur));
        assert_eq!(first_output.unwrap().len(), second_output.unwrap().len());
    }

    #[test]
    fn simple_expressions() {
        let input1 = "int a = 5;";
        let input2 = "int a=5;";
        let input3 = "int a= 5;";
        let incorrect_input = "inta = 5;";
    }

    #[test]
    fn identifier() {
        let input1 = "identifier";
        let input2 = "_blabala";
        let invalid_input1 = "9funct";
        let invalid_input2 = "sdfdLsf>>>";
        assert!(is_identifier(input1));
        assert!(!is_identifier(invalid_input1));
        assert!(is_identifier(input2));
        assert!(!is_identifier(invalid_input2));
    }

    #[test]
    fn test_operator_maximal_munch() {
        let code = "a+++b; c-- - d; e+=f; g==h; i!=j; k>=l; m<=n; o&&p; q||r;";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        let expected_tokens = vec![
            TokenKind::Identifier("a".into()),
            TokenKind::Operator(Operator::PlusPlus),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Identifier("b".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("c".into()),
            TokenKind::Operator(Operator::MinusMinus),
            TokenKind::Operator(Operator::Minus),
            TokenKind::Identifier("d".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("e".into()),
            TokenKind::Operator(Operator::PlusEqual), // +=
            TokenKind::Identifier("f".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("g".into()),
            TokenKind::Operator(Operator::EqualEqual),
            TokenKind::Identifier("h".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("i".into()),
            TokenKind::Operator(Operator::NotEqual),
            TokenKind::Identifier("j".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("k".into()),
            TokenKind::Operator(Operator::GreaterEqual),
            TokenKind::Identifier("l".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("m".into()),
            TokenKind::Operator(Operator::LessEqual),
            TokenKind::Identifier("n".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("o".into()),
            TokenKind::Operator(Operator::AndAnd),
            TokenKind::Identifier("p".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("q".into()),
            TokenKind::Operator(Operator::OrOr),
            TokenKind::Identifier("r".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::EOF,
        ];

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, expected_tokens[i]);
        }
    }

    #[test]
    fn test_comma_operator_and_arguments() {
        let code = "int x = (a=1, b+2); f(a,b,c);";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Identifier("x".into()),
            TokenKind::Operator(Operator::Assign),
            TokenKind::Punctuator(Punctuator::LeftParenthesis),
            TokenKind::Identifier("a".into()),
            TokenKind::Operator(Operator::Assign),
            TokenKind::IntLiteral("1".into()),
            TokenKind::Punctuator(Punctuator::Comma),
            TokenKind::Identifier("b".into()),
            TokenKind::Operator(Operator::Plus),
            TokenKind::IntLiteral("2".into()),
            TokenKind::Punctuator(Punctuator::RightParenthesis),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::Identifier("f".into()),
            TokenKind::Punctuator(Punctuator::LeftParenthesis),
            TokenKind::Identifier("a".into()),
            TokenKind::Punctuator(Punctuator::Comma),
            TokenKind::Identifier("b".into()),
            TokenKind::Punctuator(Punctuator::Comma),
            TokenKind::Identifier("c".into()),
            TokenKind::Punctuator(Punctuator::RightParenthesis),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::EOF,
        ];

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, expected_tokens[i]);
        }
    }

    #[test]
    fn test_struct_access() {
        let code = "a.b->c";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        let expected_tokens = vec![
            TokenKind::Identifier("a".into()),
            TokenKind::Operator(Operator::Dot), // .
            TokenKind::Identifier("b".into()),
            TokenKind::Operator(Operator::Arrow), // ->
            TokenKind::Identifier("c".into()),
            TokenKind::EOF,
        ];

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, expected_tokens[i]);
        }
    }

    #[test]
    fn test_grouping_and_arrays() {
        let code = "f(a[b] + (c + d) * { e });";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        let expected_tokens = vec![
            TokenKind::Identifier("f".into()),
            TokenKind::Punctuator(Punctuator::LeftParenthesis),
            TokenKind::Identifier("a".into()),
            TokenKind::Punctuator(Punctuator::LeftSquareBracket),
            TokenKind::Identifier("b".into()),
            TokenKind::Punctuator(Punctuator::RightSquareBracket),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Punctuator(Punctuator::LeftParenthesis),
            TokenKind::Identifier("c".into()),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Identifier("d".into()),
            TokenKind::Punctuator(Punctuator::RightParenthesis),
            TokenKind::Operator(Operator::Star),
            TokenKind::Punctuator(Punctuator::LeftCurlyBracket),
            TokenKind::Identifier("e".into()),
            TokenKind::Punctuator(Punctuator::RightCurlyBracket),
            TokenKind::Punctuator(Punctuator::RightParenthesis),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::EOF,
        ];

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, expected_tokens[i]);
        }
    }

    #[test]
    fn test_unary_and_binary_plus() {
        let code = "a + +b - -c;";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        let expected_tokens = vec![
            TokenKind::Identifier("a".into()),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Identifier("b".into()),
            TokenKind::Operator(Operator::Minus),
            TokenKind::Operator(Operator::Minus),
            TokenKind::Identifier("c".into()),
            TokenKind::Punctuator(Punctuator::SemiColon),
            TokenKind::EOF,
        ];

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, expected_tokens[i]);
        }
    }

    #[test]
    fn test_simple_char_literals() {
        let code = "char a = 'a', b = 'z', c = '0'";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral('a' as i32));
        assert_eq!(tokens[7].kind, TokenKind::CharLiteral('z' as i32));
        assert_eq!(tokens[11].kind, TokenKind::CharLiteral('0' as i32));
    }

    #[test]
    fn test_common_escape_sequences() {
        let code = r#"char a='\n', b='\t', c='\r', d='\\', e='\'', f='\"', g='\?', h='\a', i='\b', j='\f', k='\v';"#;
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral('\n' as i32));
        assert_eq!(tokens[7].kind, TokenKind::CharLiteral('\t' as i32));
        assert_eq!(tokens[11].kind, TokenKind::CharLiteral('\r' as i32));
        assert_eq!(tokens[15].kind, TokenKind::CharLiteral('\\' as i32));
        assert_eq!(tokens[19].kind, TokenKind::CharLiteral('\'' as i32));
        assert_eq!(tokens[23].kind, TokenKind::CharLiteral('\"' as i32));
        assert_eq!(tokens[27].kind, TokenKind::CharLiteral('?' as i32));
        assert_eq!(tokens[31].kind, TokenKind::CharLiteral('\u{07}' as i32)); // \a
        assert_eq!(tokens[35].kind, TokenKind::CharLiteral('\u{08}' as i32)); // \b
        assert_eq!(tokens[39].kind, TokenKind::CharLiteral('\u{0C}' as i32)); // \f
        assert_eq!(tokens[43].kind, TokenKind::CharLiteral('\u{0B}' as i32)); // \v
    }

    #[test]
    fn test_hex_escapes() {
        let code = r#"char a='\x00', b='\x7F', c='\xFF';"#;
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral(0x00));
        assert_eq!(tokens[7].kind, TokenKind::CharLiteral(0x7F));
        assert_eq!(tokens[11].kind, TokenKind::CharLiteral(0xFF));
    }

    #[test]
    fn test_octal_escapes() {
        let code = r#"char a='\0', b='\07', c='\377';"#;
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral(0o0));
        assert_eq!(tokens[7].kind, TokenKind::CharLiteral(0o7));
        assert_eq!(tokens[11].kind, TokenKind::CharLiteral(0o377));
    }

    #[test]
    fn test_invalid_literals() {
        let invalid_cases = [
            "'",        // empty
            "''",       // empty
            "'ab'",     // multi-char
            "'\\",      // incomplete escape
            "'\\x'",    // incomplete hex
            "'\\7777'", // octal too long
        ];

        for case in invalid_cases {
            let result = Lexer::new().tokenize(&format!("char a = {};", case));
            assert!(result.is_err(), "case should fail: {}", case);
        }
    }

    #[test]
    fn test_unicode_escapes() {
        let code = r#"char a='\u0041', b='\U0001F600';"#; // 'A' and 😀
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();

        assert_eq!(tokens[3].kind, TokenKind::CharLiteral(0x41));
        assert_eq!(tokens[7].kind, TokenKind::CharLiteral(0x1F600));
    }

    #[test]
    fn test_basic_string() {
        let code = r#"int str = "hello world""#;
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();
    }

    #[test]
    fn test_span() {
        let code = "for(int i = 0; i < 10; i++);";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(code).unwrap();
        //for
        assert_eq!(tokens[0].span.start, 0);
        assert_eq!(tokens[0].span.end, 3);
        // (
        assert_eq!(tokens[1].span.start, 3);
        assert_eq!(tokens[1].span.end, 4);
        //int
        assert_eq!(tokens[2].span.start, 4);
        assert_eq!(tokens[2].span.end, 7);
        // i
        assert_eq!(tokens[3].span.start, 8);
        assert_eq!(tokens[3].span.end, 9);
    }
}
