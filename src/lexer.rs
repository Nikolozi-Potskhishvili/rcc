use std::ops::{Add, RangeBounds};
use crate::lexer::FoundLongToken::{Found, NotFound};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Constant(Constant),
    Operator(Operator),
    SpecialCharacter(SpecialCharacter),
    Comments(String),
    EndOFFile,
}

impl Token {
    pub fn get_operator(&self) -> Option<Operator> {
        match self {
            Token::Operator(operator) => {
                Some(operator.clone())
            }
            _ => None,
        }
    }

    pub fn get_constant(&self) -> Option<Constant> {
        match self {
            Token::Constant(constant) => {
                Some(constant.clone())
            }
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Return,
    Integer,
    Double,
    Float,
    Char,
    Void,
    For,
    While,
    If,
    Else,
    SizeOf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Division,
    Multiplication,
    Equals,
    And,
    Or,
    Not,
    Xor,
    Modulo,
    Tilde,
    Less,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Not | Operator::Minus | Operator::Tilde => true,
            _ => false,
        }
    }
    pub fn is_left_associative(&self) -> bool {
        match self {
            Operator::Minus | Operator::Tilde | Operator::And
            | Operator::Or | Operator::Plus => true,
            _ => false,
        }
    }
    pub fn get_precedence(&self) -> i32 {
        match self {
            Operator::Plus => 1,
            Operator::Division | Operator::Multiplication => 2,
            Operator::Not | Operator::Tilde | Operator::Minus => 3,
            _ => 0
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Integer(i32),
    Short(i16),
    Long(i64),
    Double(f32),
    Float(f64),
    Char(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SpecialCharacter {
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
    SemiColon,
}

pub struct Lexer;


impl Lexer {

    pub fn tokenize(source_code: &str) -> Vec<Token> {
        let mut result = Vec::new();
        source_code.lines()
            .for_each(|line| {
                line.split_whitespace()
                    .for_each(|word|  {
                        result.extend(parse_token_helper(word));
                    });
            });
        result
    }
}
/// this function parses string which certainly contains at least one token
fn parse_token_helper(s: &str) -> Vec<Token> {
    let (mut tokens, mut cur_token, mut open_parentheses) = s.chars()
        .fold(
            (Vec::new(), String::new(), 0),
            |(mut tokens, mut cur_token, mut open_parentheses), char| {
                if let Ok(operator) = get_operator(&char.to_string()) {
                    tokens = process_long_token(&cur_token, tokens, &mut open_parentheses).get_tokens();
                    cur_token.clear();
                    match operator {
                        Operator::Minus => {
                            tokens.push(Token::SpecialCharacter(SpecialCharacter::LeftParenthesis));
                            tokens.push(Token::Operator(Operator::Plus));
                            open_parentheses += 1;
                        },
                        _ => {},
                    }
                    tokens.push(Token::Operator(operator));
                } else if let Ok(special_symbol) = get_special_symbol(&char.to_string()) {
                   tokens = process_long_token(&cur_token, tokens, &mut open_parentheses).get_tokens();
                   cur_token.clear();
                   tokens.push(Token::SpecialCharacter(special_symbol));
                } else {
                    cur_token.push(char);
                }
                (tokens, cur_token, open_parentheses)
            });
        tokens = process_long_token(&cur_token, tokens, &mut open_parentheses).get_tokens();
        tokens
}



/// parses long tokens such as keywords, constants and identifiers
fn parse_long_token(s: &str) -> Option<Token> {
    if s.is_empty() {
        None
    } else if let Ok(keyword) = get_keyword(s) {
         Some(Token::Keyword(keyword))
    } else if is_const_integer(s) {
         Some(Token::Constant(Constant::Integer(s.parse::<i32>().unwrap())))
    } else {
        Some(Token::Identifier(s.to_string()))
    }
}

enum FoundLongToken {
    Found(Vec<Token>),
    NotFound(Vec<Token>),
}

impl FoundLongToken {
    fn get_tokens(self) -> Vec<Token> {
        match self {
            Found(new_tokens) => new_tokens,
            NotFound(old_tokens) => old_tokens,
        }
    }
}

fn process_long_token(
    cur_token: &str,
    mut tokens: Vec<Token>,
    open_parentheses: &mut usize,
) -> FoundLongToken {
    if let Some(token) = parse_long_token(&cur_token) {
        tokens.push(token.clone());
        if let Token::Constant(_) = token {
            if *open_parentheses > 0 {
                tokens.push(Token::SpecialCharacter(SpecialCharacter::RightParenthesis));
                *open_parentheses -= 1;
            }
        }
        Found(tokens)
    } else{
        NotFound(tokens)
    }
}

fn get_special_symbol(token: &str) -> Result<SpecialCharacter, &'static str> {
    if token.len() != 1 {
        return Err("Special character length must be 1");
    }
    let allowed_chars = "[]{}(),.:;*=#~";
    let ch = token.chars().next().unwrap();
    match  ch {
        '[' => Ok(SpecialCharacter::LeftSquareBracket),
        ']' => Ok(SpecialCharacter::RightSquareBracket),
        '{' => Ok(SpecialCharacter::LeftCurlyBracket),
        '}' => Ok(SpecialCharacter::RightCurlyBracket),
        '(' => Ok(SpecialCharacter::LeftParenthesis),
        ')' => Ok(SpecialCharacter::RightParenthesis),
        ';' => Ok(SpecialCharacter::SemiColon),
        _ => Err("Illegal special character"),
    }
}


fn get_keyword(token: &str) -> Result<Keyword, &'static str> {
     match token {
        "int" => Ok(Keyword::Integer),
        "for" => Ok(Keyword::For),
        "while" => Ok(Keyword::While),
        "if" => Ok(Keyword::If),
        "else" => Ok(Keyword::Else),
        "return" => Ok(Keyword::Return),
        _ => Err("unexpected error during parsing keyword"),
    }
}

fn get_operator(token: &str) -> Result<Operator, String> {
    match token {
        "+" => Ok(Operator::Plus),
        "*" => Ok(Operator::Multiplication),
        "/" => Ok(Operator::Division),
        "!" => Ok(Operator::Not),
        "-" => Ok(Operator::Minus),
        "~" => Ok(Operator::Tilde),
        "=" => Ok(Operator::Equals),
        "<" => Ok(Operator::Less),
        _ => Err(String::from("unexpected error during parsing operator")),
    }
}

fn is_const_integer(token: &str) -> bool {
    if let Ok(number) = token.to_string().parse::<i32>() {
        return true
    }
    false
}

fn is_const_char(token: &str) -> bool {

    false
}


fn is_const(token: &str) -> bool {
    is_const_integer(token)
}

fn is_identifier(token: &str) -> bool {
    let valid_lowercase_range = 'a'..='z';
    let valid_uppercase_range = 'A'..='Z';
    let valid_numbers_range = '0'..='9';
    let mut iterator = token.chars();
    if let Some(first_letter) = iterator.next() {
        if !(valid_lowercase_range.contains(&first_letter) ||
            valid_uppercase_range.contains(&first_letter) ||
            first_letter == '_') {
            return false;
        }
    } else {
        return false;
    }
    for ch in token.chars() {
        if !(valid_uppercase_range.contains(&ch) ||
            valid_lowercase_range.contains(&ch) ||
            valid_numbers_range.contains(&ch) ||
            ch == '_') {
            return false
        }
    }
    true
}


#[cfg(test)]
mod tests {
    use crate::lexer::{is_identifier, get_keyword, Lexer, Keyword, Token};

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
        let output = Lexer::tokenize(&input);
        output.iter().for_each(|token| {
            println!("{token:?}")
        });
        assert_eq!(output.len(), 4);
    }

    #[test]
    fn simple_return() {
        let input = "\
        int main() {\
            return 2;\
        }";
        let output = Lexer::tokenize(&input);
        output.iter().for_each(|token| {
            println!("{token:?}")
        });
        assert_eq!(output.len(), 9);
    }

    #[test]
    fn for_loop() {
        let input_regular = "for(int i = 0; i < 1; i++) {";
        let input_minimal_spaces= "for(int i=0; i<1;i++){";
        let first_output = Lexer::tokenize(&input_regular);
        let second_output = Lexer::tokenize(&input_minimal_spaces);
        first_output.iter().for_each(|cur| println!("{:?} cur token", cur));
        second_output.iter().for_each(|cur| println!("{:?} cur token", cur));
        assert_eq!(first_output.len(), second_output.len());
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

}