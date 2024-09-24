use std::arch::x86_64::_mm256_testc_pd;
use std::io::read_to_string;
use std::ops::{Add, RangeBounds};

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(String),
    Constant(Constant),
    Operator(String),
    SpecialCharacter(String),
    Comments(String),
    EndOFFile,
}

#[derive(Debug, PartialEq)]
struct Constant {
    const_type: String,
    const_value: String,
}

impl Constant {
    fn new(value_type: String, value: String) -> Self {
        Constant {
            const_type: value_type,
            const_value: value,
        }
    }
}

pub struct Lexer {
}

impl Lexer {

    pub fn tokenize(source_code: &str) -> Vec<Token> {
        let mut result : Vec<Token> = Vec::new();
        let mut cur_token = String::new();

        for line in source_code.lines() {
            for s in line.split_whitespace() {
                parse_token_helper(s, &mut result);
            }
        }
        result
    }
}
/// this function parses string which certainly contains at least one token
fn parse_token_helper(s: &str, result: &mut Vec<Token>) {
    let mut cur_token = String::new();
    for char in s.chars() {
        if is_operator(&char.to_string()) {
            result.push(Token::Operator(char.to_string()));
            if let Some(token) = parse_long_token(&cur_token) {
                result.push(token);
            }
            cur_token = String::new();
        } else if is_special_symbol(&char.to_string()) {
            result.push(Token::SpecialCharacter(char.to_string()));
            if let Some(token) = parse_long_token(&cur_token) {
                result.push(token);
            }
            cur_token = String::new();
        } else {
            cur_token.push(char);
        }
    }
    if !cur_token.is_empty() {
        if let Some(token) = parse_long_token(&cur_token) {
            result.push(token);
        }
   }
}
/// parses long tokens such as keywords, constants and identifiers
fn parse_long_token(s: &str) -> Option<Token> {
    if s.is_empty() {
        return None;
    }
    if is_keyword(s) {
        return Some(Token::Keyword(String::from(s)))
    } else if is_const_number(s) {
        return Some(Token::Constant(Constant::new(String::from("Integer"), String::from(s))))
    }
    Some(Token::Identifier(s.to_string()))
}

fn validate_token(cur_token: &mut str, result:& mut Vec<Token>) {
    if let Ok(token) = get_token_type(&cur_token) {
        result.push(token);
    }
}

fn get_token_type(token: &str) -> Result<Token, &'static str> {
    if is_keyword(token) {
        return Ok(Token::Keyword(String::from(token)));
    }
    Ok(Token::EndOFFile)
}

fn is_special_symbol(token: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let allowed_chars = "[]{}(),.:;*=#~";
    let ch = token.chars().next().unwrap();
    allowed_chars.contains(ch)
}

fn is_keyword(token: &str) -> bool {
    token == "int" || token == "for" || token == "while" || token == "if" || token == "else" ||
        token == "return"
}

fn is_operator(token: &str) -> bool {
    token == "+" || token == "-" || token == "*" || token == "/" || token == "%" ||
        token == ">" || token == "<" || token == "&" || token == "|" || token == "sizeof" ||
        token == "->"
}

fn is_const_number(token: &str) -> bool {
    if let Ok(number) = token.to_string().parse::<i64>() {
        return true
    }
    false
}

fn is_const_char(token: &str) -> bool {

    false
}


fn is_const(token: &str) -> bool {
    is_const_number(token)
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
    use crate::lexer::{is_identifier, is_keyword, Lexer};

    #[test]
    fn keywords() {
        let imput1 = "for";
        let imput2 = "while";
        let imput3 = "return";
        let imput4 = "if";
        let imput5 = "else";

        assert!(is_keyword(imput1));
        assert!(is_keyword(imput2));
        assert!(is_keyword(imput3));
        assert!(is_keyword(imput4));
        assert!(is_keyword(imput5));
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