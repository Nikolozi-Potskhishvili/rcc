use std::collections::HashMap;
use crate::lexer::FoundLongToken::{Found, NotFound};

///
/// The most upper-level representation of token
///
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Constant(i64),
    Operator(Operator),
    SpecialCharacter(SpecialCharacter),
    Comments(String),
    Type(Type),
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


}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolTableEntry {
    Variable(Type),
    StructDef(StructDef),
    FunDef(FunDef),
    TypeDef,
}

/// Primitive types. Currently, Are supported: Integer, Flout, Double, Char, Short, Long
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(String),
    Pointer(Box<Type>),
    Array(Box<Type>, i64),
    Struct(String),
    Function,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: HashMap<String, Type>,
    pub size: i64,
}



#[derive(Clone, Debug, PartialEq)]
pub struct  FunDef {
    FunType: Type,
    Args: Vec<(String, Type)>
}

///
/// Keywords, currently supported: Type(Type), Return, Void, For, While, If, Else And SizeOf
///
#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Type(Type),
    Return,
    Void,
    For,
    While,
    If,
    Else,
    SizeOf,
    TypeDef,
    Struct,
}

///
/// Operators, Currently supported: +, -, /, *(Mult), =, |, &, ~, !, <, > and ^
///
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Division,
    Multiplication,
    Assign,
    IncrementAssign,
    DecrementAssign,
    And,
    Or,
    Not,
    Xor,
    Modulo,
    Tilde,
    Less,
    More,
    Deref,
    Ref,
    PrefixInc,
    PostfixInc,
    ArrayAccess,
    StructAccess,
    StructPtrAccess,
}

impl Operator {
    ///
    /// Returns true if operator is unary operator
    ///
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Not | Operator::Tilde => true,
            _ => false,
        }
    }

    ///
    /// Returns true if operator is left associative
    ///
    pub fn is_left_associative(&self) -> bool {
        match self {
            Operator::Minus | Operator::Tilde | Operator::And
            | Operator::Or | Operator::Plus => true,
            _ => false,
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
    Undefined,
}

impl Constant {
    pub fn get_type(&self) -> Type {
       match self {
           Constant::Integer(_) => Type::Primitive("int".to_string()),
           Constant::Short(_) => Type::Primitive("short".to_string()),
           Constant::Long(_) => Type::Primitive("long".to_string()),
           Constant::Double(_) => Type::Primitive("double".to_string()),
           Constant::Float(_) => Type::Primitive("float".to_string()),
           Constant::Char(_) => Type::Primitive("char".to_string()),
           Constant::Undefined => Type::Function,
       }
    }

    pub fn get_val(&self) -> String {
        match self {
            Constant::Integer(val) => val.to_string(),
            Constant::Short(val) => val.to_string(),
            Constant::Long(val) => val.to_string(),
            Constant::Double(val) => val.to_string(),
            Constant::Float(val) => val.to_string(),
            Constant::Char(val) => val.to_string(),
            Constant::Undefined => "".to_string()
        }
    }
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
    ///
    /// Takes source code as input and returns string of supported C tokens
    ///
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
    let (mut tokens, cur_token, mut open_parentheses) = s.chars()
        .fold(
            (Vec::new(), String::new(), 0),
            |(mut tokens, mut cur_token, mut open_parentheses), char| {
                if let Ok(operator) = get_operator(&char.to_string()) {
                    tokens = process_long_token(&cur_token, tokens, &mut open_parentheses).get_tokens();
                    cur_token.clear();
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
         Some(Token::Constant(s.parse::<i64>().unwrap()))
    } else {
        Some(Token::Identifier(s.to_string()))
    }
}

fn get_type(string: &str) -> Result<Type, String> {
    match string {
        "int" => Ok(Type::Primitive("int".to_string())),
        "short" => Ok(Type::Primitive("short".to_string())),
        "long" => Ok(Type::Primitive("long".to_string())),
        "char" => Ok(Type::Primitive("char".to_string())),
        "bool" => Ok(Type::Primitive("bool".to_string())),
        _ => Err(format!("Unrecognized type: {string}")),
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
    let _allowed_chars = "[]{}(),.:;*=#~";
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


fn get_keyword(token: &str) -> Result<Keyword, String> {
     match token {
         "int" | "short" | "long" | "char" | "bool" =>
             Ok(Keyword::Type(get_type(token.clone())?)),
         "for" => Ok(Keyword::For),
         "while" => Ok(Keyword::While),
         "if" => Ok(Keyword::If),
         "else" => Ok(Keyword::Else),
         "return" => Ok(Keyword::Return),
         "typedef" => Ok(Keyword::TypeDef),
         "struct" => Ok(Keyword::Struct),
        _ => Err("unexpected error during parsing keyword".to_string()),
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
        "=" => Ok(Operator::Assign),
        "<" => Ok(Operator::Less),
        ">" => Ok(Operator::More),
        "&" => Ok(Operator::Ref),
        "." => Ok(Operator::StructAccess),
        _ => Err(String::from("unexpected error during parsing operator")),
    }
}

fn is_const_integer(token: &str) -> bool {
    if let Ok(_) = token.to_string().parse::<i32>() {
        return true
    }
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
    use crate::lexer::{is_identifier, get_keyword, Lexer, Keyword};

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