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
    EndOFFile,
}

#[derive(Debug, PartialEq)]
struct Constant {
    const_type: String,
    const_value: String,
}

pub struct Lexer<'a> {
    source_code: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            source_code: input,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut result : Vec<Token> = Vec::new();
        let mut cur_token = String::new();

        for line in self.source_code.lines() {
            let chars = line.chars().collect::<Vec<_>>();
            for char in chars {
                if char.is_whitespace() {
                    if !cur_token.is_empty() {
                        validate_token(&mut cur_token, &mut result);
                    }
                    cur_token.clear();
                } else {
                    cur_token.push(char);
                    if let Ok(token) = get_token_type(&cur_token) {
                        validate_token(&mut cur_token, &mut result);
                        cur_token.clear();
                    }
                }
            }

            if !cur_token.is_empty() {
                validate_token(&mut cur_token, &mut result);
            }
            cur_token.clear();
        }
        result
    }

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


fn is_keyword(token: &str) -> bool {
    token == "int" || token == "for" || token == "while" || token == "if" || token == "else" ||
        token == "return"
}

fn is_operator(token: &str) -> bool {
    token == "+" || token == "-" || token == "*" || token == "/" || token == "%" ||
        token == ">" || token == "<" || token == "&" || token == "|" || token == "sizeof" ||
        token == "(" || token == ")" || token == "[" || token == "]" || token == "->"
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
        let mut lexer = Lexer::new(input);
        let output = lexer.tokenize();
        output.iter().for_each(|token| {
            println!("{token:?}")
        });
        assert_eq!(output.len(), 4);
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