#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    IntLiteral(String), // only ints for now
    FloatLiteral(String),
    StringLiteral(String),
    CahrLiteral(char),
    Operator(Operator),
    Punctuator(Punctuator),
    Comments(String),
    EOF,
}

///
/// Keywords, currently supported: Type(Type), Return, Void, For, While, If, Else And SizeOf
///
#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Return,
    Void,
    For,
    While,
    If,
    Else,
    SizeOf,
    TypeDef,
    Struct,
    Do,
    Int,
    Short,
    Long,
    Char,
    Bool,
}

///
/// Operators, Currently supported: +, -, /, *(Mult), =, |, &, ~, !, <, > and ^
///
#[derive(Debug, PartialEq, Clone)]
enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Assign,
    PlusPlus,
    MinusMinus,
    PlusEqual,
    MinusEqual,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    AndAnd,
    OrOr,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    Exclamation,
    Dot,
    Arrow,
}

impl Operator {
    ///
    /// Returns true if operator is unary operator
    ///
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Exclamation | Operator::Tilde => true,
            _ => false,
        }
    }

    ///
    /// Returns true if operator is left associative
    ///
    pub fn is_left_associative(&self) -> bool {
        match self {
            Operator::Minus
            | Operator::Tilde
            | Operator::Ampersand
            | Operator::Pipe
            | Operator::Plus => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Punctuator {
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
    SemiColon,
    Comma,
}
pub struct Lexer;

impl Lexer {
    ///
    /// Takes source code as input and returns Vector of supported C tokens
    ///
    pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
        let mut result = Vec::new();
        let mut chars = source_code.chars().peekable();

        let whitespaces = " \t\x0B\x0C\n";
        let mut cur_token = String::new();

        while let Some(&ch) = chars.peek() {
            // if ch is whitespace current token ends
            if whitespaces.contains(ch) {
                if let Some(token) = parse_long_token(&cur_token) {
                    result.push(token);
                }
                chars.next();
                cur_token.clear();
            } else if ch as u32 == 0x22 {
                // check "
                match process_string_literal(&cur_token) {
                    Some(token) => result.push(token),
                    None => {
                        return Err(String::from("Expected string literal"));
                    }
                };
            } else if ch as u32 == 0x27 {
                // check '
                match process_char_literal(&cur_token) {
                    Some(token) => result.push(token),
                    None => {
                        return Err(String::from("Expected string literal"));
                    }
                };
            } else if let Ok(_) = get_operator(&ch.to_string()) {
                // Check for two-character operators
                let mut op = ch.to_string();
                chars.next(); // Consume first character
                if let Some(&next_ch) = chars.peek() {
                    let possible_op = format!("{op}{next_ch}");
                    if get_operator(&possible_op).is_ok() {
                        op = possible_op;
                        chars.next(); // Consume second character
                    }
                }
                if let Some(token) = parse_long_token(&cur_token) {
                    result.push(token);
                };
                cur_token.clear();
                result.push(Token::Operator(get_operator(&op).unwrap()));
            } else if let Ok(punctuator) = get_punctuator(&ch.to_string()) {
                if let Some(token) = parse_long_token(&cur_token) {
                    result.push(token);
                }
                cur_token.clear();
                result.push(Token::Punctuator(punctuator));
                chars.next(); // Consume special symbol
            } else {
                cur_token.push(ch);
                chars.next(); // Consume character
            }
        }
        if let Some(token) = parse_long_token(&cur_token) {
            result.push(token);
        }
        result.push(Token::EOF);
        Ok(result)
    }
}

/// parses long tokens such as keywords, constants and identifiers
fn parse_long_token(s: &str) -> Option<Token> {
    if s.is_empty() {
        None
    } else if let Ok(keyword) = get_keyword(s) {
        Some(Token::Keyword(keyword))
    } else if is_int_literal(s) {
        Some(Token::IntLiteral(s.to_string()))
    } else if is_identifier(s) {
        Some(Token::Identifier(s.to_string()))
    } else {
        None
    }
}

fn process_string_literal(cur_token: &str) -> Option<Token> {
    None
}

fn process_char_literal(cur_token: &str) -> Option<Token> {
    None
}

fn get_punctuator(token: &str) -> Result<Punctuator, &'static str> {
    if token.len() != 1 {
        return Err("Special character length must be 1");
    }
    let _allowed_chars = "[]{}(),.:;*=#~";
    let ch = token.chars().next().unwrap();
    match ch {
        '[' => Ok(Punctuator::LeftSquareBracket),
        ']' => Ok(Punctuator::RightSquareBracket),
        '{' => Ok(Punctuator::LeftCurlyBracket),
        '}' => Ok(Punctuator::RightCurlyBracket),
        '(' => Ok(Punctuator::LeftParenthesis),
        ')' => Ok(Punctuator::RightParenthesis),
        ';' => Ok(Punctuator::SemiColon),
        ',' => Ok(Punctuator::Comma),
        _ => Err("Illegal special character"),
    }
}

fn get_keyword(token: &str) -> Result<Keyword, String> {
    match token {
        "int" => Ok(Keyword::Int),
        "short" => Ok(Keyword::Short),
        "long" => Ok(Keyword::Long),
        "char" => Ok(Keyword::Char),
        "bool" => Ok(Keyword::Bool),
        "for" => Ok(Keyword::For),
        "while" => Ok(Keyword::While),
        "if" => Ok(Keyword::If),
        "else" => Ok(Keyword::Else),
        "return" => Ok(Keyword::Return),
        "typedef" => Ok(Keyword::TypeDef),
        "struct" => Ok(Keyword::Struct),
        "void" => Ok(Keyword::Void),
        "do" => Ok(Keyword::Do),
        _ => Err("unexpected error during parsing keyword".to_string()),
    }
}

fn get_operator(token: &str) -> Result<Operator, String> {
    match token {
        "+" => Ok(Operator::Plus),
        "*" => Ok(Operator::Star),
        "/" => Ok(Operator::Slash),
        "-" => Ok(Operator::Minus),
        "~" => Ok(Operator::Tilde),
        "&" => Ok(Operator::Ampersand),
        "|" => Ok(Operator::Pipe),
        "=" => Ok(Operator::Assign),
        "<" => Ok(Operator::Less),
        ">" => Ok(Operator::Greater),
        "==" => Ok(Operator::EqualEqual),
        "!=" => Ok(Operator::NotEqual),
        ">=" => Ok(Operator::GreaterEqual),
        "<=" => Ok(Operator::LessEqual),
        "!" => Ok(Operator::Exclamation),
        "||" => Ok(Operator::OrOr),
        "&&" => Ok(Operator::AndAnd),
        "+=" => Ok(Operator::PlusEqual),
        "-=" => Ok(Operator::MinusEqual),
        "++" => Ok(Operator::PlusPlus),
        "--" => Ok(Operator::MinusMinus),
        "." => Ok(Operator::Dot),
        "->" => Ok(Operator::Arrow),
        _ => Err(String::from("unexpected error during parsing operator")),
    }
}

fn is_int_literal(token: &str) -> bool {
    token.chars().all(|c| c.is_ascii_digit())
}

fn is_float_literal(token: &str) -> bool {
    token.contains(".") || token.contains("e") || token.contains("E") || token.contains("f")
}

fn is_char_literal(token: &str) -> bool {
    let chars = token.chars();
    false
}

fn is_string_literal(token: &str) -> bool {
    false
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
    fn keywords() {
        let imput1 = "for";
        let imput2 = "while";
        let imput3 = "return";
        let imput4 = "if";
        let imput5 = "else";

        assert_eq!(get_keyword(imput1), Ok(Keyword::For));
        assert_eq!(get_keyword(imput2), Ok(Keyword::While));
        assert_eq!(get_keyword(imput3), Ok(Keyword::Return));
        assert_eq!(get_keyword(imput4), Ok(Keyword::If));
        assert_eq!(get_keyword(imput5), Ok(Keyword::Else));
    }

    #[test]
    fn only_keywords() {
        let input = "for while if else";
        let output = Lexer::tokenize(&input).unwrap();
        output.iter().for_each(|token| println!("{token:?}"));
        assert_eq!(output.len(), 5);
    }

    #[test]
    fn simple_return() {
        let input = "\
        int main() {\
            return 2;\
        }";
        let output = Lexer::tokenize(&input);
        output.iter().for_each(|token| println!("{token:?}"));
        assert_eq!(output.unwrap().len(), 10);
    }

    #[test]
    fn for_loop() {
        let input_regular = "for(int i = 0; i < 1; i++) {";
        let input_minimal_spaces = "for(int i=0;i<1;i++){";
        let first_output = Lexer::tokenize(&input_regular);
        let second_output = Lexer::tokenize(&input_minimal_spaces);
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
        let tokens = Lexer::tokenize(code).unwrap();

        let expected_tokens = vec![
            Token::Identifier("a".into()),
            Token::Operator(Operator::PlusPlus),
            Token::Operator(Operator::Plus),
            Token::Identifier("b".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("c".into()),
            Token::Operator(Operator::MinusMinus),
            Token::Operator(Operator::Minus),
            Token::Identifier("d".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("e".into()),
            Token::Operator(Operator::PlusEqual), // +=
            Token::Identifier("f".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("g".into()),
            Token::Operator(Operator::EqualEqual),
            Token::Identifier("h".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("i".into()),
            Token::Operator(Operator::NotEqual),
            Token::Identifier("j".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("k".into()),
            Token::Operator(Operator::GreaterEqual),
            Token::Identifier("l".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("m".into()),
            Token::Operator(Operator::LessEqual),
            Token::Identifier("n".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("o".into()),
            Token::Operator(Operator::AndAnd),
            Token::Identifier("p".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("q".into()),
            Token::Operator(Operator::OrOr),
            Token::Identifier("r".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::EOF,
        ];
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_comma_operator_and_arguments() {
        let code = "int x = (a=1, b+2); f(a,b,c);";
        let tokens = Lexer::tokenize(code).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Int),
            Token::Identifier("x".into()),
            Token::Operator(Operator::Assign),
            Token::Punctuator(Punctuator::LeftParenthesis),
            Token::Identifier("a".into()),
            Token::Operator(Operator::Assign),
            Token::IntLiteral("1".into()),
            Token::Punctuator(Punctuator::Comma),
            Token::Identifier("b".into()),
            Token::Operator(Operator::Plus),
            Token::IntLiteral("2".into()),
            Token::Punctuator(Punctuator::RightParenthesis),
            Token::Punctuator(Punctuator::SemiColon),
            Token::Identifier("f".into()),
            Token::Punctuator(Punctuator::LeftParenthesis),
            Token::Identifier("a".into()),
            Token::Punctuator(Punctuator::Comma),
            Token::Identifier("b".into()),
            Token::Punctuator(Punctuator::Comma),
            Token::Identifier("c".into()),
            Token::Punctuator(Punctuator::RightParenthesis),
            Token::Punctuator(Punctuator::SemiColon),
            Token::EOF,
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_struct_access() {
        let code = "a.b->c";
        let tokens = Lexer::tokenize(code).unwrap();

        let expected_tokens = vec![
            Token::Identifier("a".into()),
            Token::Operator(Operator::Dot), // .
            Token::Identifier("b".into()),
            Token::Operator(Operator::Arrow), // ->
            Token::Identifier("c".into()),
            Token::EOF,
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_grouping_and_arrays() {
        let code = "f(a[b] + (c + d) * { e });";
        let tokens = Lexer::tokenize(code).unwrap();

        let expected_tokens = vec![
            Token::Identifier("f".into()),
            Token::Punctuator(Punctuator::LeftParenthesis),
            Token::Identifier("a".into()),
            Token::Punctuator(Punctuator::LeftSquareBracket),
            Token::Identifier("b".into()),
            Token::Punctuator(Punctuator::RightSquareBracket),
            Token::Operator(Operator::Plus),
            Token::Punctuator(Punctuator::LeftParenthesis),
            Token::Identifier("c".into()),
            Token::Operator(Operator::Plus),
            Token::Identifier("d".into()),
            Token::Punctuator(Punctuator::RightParenthesis),
            Token::Operator(Operator::Star),
            Token::Punctuator(Punctuator::LeftCurlyBracket),
            Token::Identifier("e".into()),
            Token::Punctuator(Punctuator::RightCurlyBracket),
            Token::Punctuator(Punctuator::RightParenthesis),
            Token::Punctuator(Punctuator::SemiColon),
            Token::EOF,
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_unary_and_binary_plus() {
        let code = "a + +b - -c;";
        let tokens = Lexer::tokenize(code).unwrap();

        let expected_tokens = vec![
            Token::Identifier("a".into()),
            Token::Operator(Operator::Plus),
            Token::Operator(Operator::Plus),
            Token::Identifier("b".into()),
            Token::Operator(Operator::Minus),
            Token::Operator(Operator::Minus),
            Token::Identifier("c".into()),
            Token::Punctuator(Punctuator::SemiColon),
            Token::EOF,
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
