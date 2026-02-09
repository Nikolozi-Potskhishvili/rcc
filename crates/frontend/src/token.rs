use std::fmt::{Display, write};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    // pub lexmi: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Identifier(String),
    Keyword(Keyword),
    IntLiteral(String), // only ints for now
    FloatLiteral(String),
    StringLiteral(Vec<i32>),
    CharLiteral(i32),
    Operator(Operator),
    Punctuator(Punctuator),
    Comments(String),
    EOF,
}

impl TokenKind {
    /// returns Some(keyword) if token is keywod, None otherwise
    pub fn get_keyword(&self) -> Option<&Keyword> {
        match self {
            TokenKind::Keyword(keyword) => Some(keyword),
            _ => None,
        }
    }

    pub fn is_storage_class(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(
                Keyword::Static | Keyword::Extern | Keyword::Auto | Keyword::Register
            )
        )
    }

    pub fn is_type_qualifier(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(Keyword::Const | Keyword::Volatile | Keyword::Restrict)
        )
    }

    pub fn is_function_specifier(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(Keyword::Inline | Keyword::Noreturn)
        )
    }

    // returns true for primitive type spcefier
    pub fn is_type_specifier(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(
                Keyword::Int | Keyword::Short | Keyword::Long | Keyword::Char | Keyword::Bool
            )
        )
    }

    pub fn is_void(&self) -> bool {
        matches!(self, TokenKind::Keyword(Keyword::Void))
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, TokenKind::Identifier(_))
    }

    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            TokenKind::Identifier(s) => Some(&s),
            _ => None,
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::StringLiteral(_)
                | TokenKind::CharLiteral(_)
                | TokenKind::IntLiteral(_)
                | TokenKind::FloatLiteral(_)
        )
    }

    pub fn as__int_literal(&self) -> Option<&str> {
        match self {
            TokenKind::IntLiteral(s) => Some(&s),
            _ => None,
        }
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, TokenKind::Operator(_))
    }

    pub fn as_operator(&self) -> Option<&Operator> {
        match self {
            TokenKind::Operator(op) => Some(op),
            _ => None,
        }
    }

    pub fn is_punctuator(&self) -> bool {
        matches!(self, TokenKind::Punctuator(_))
    }

    pub fn as_pnctuator(&self) -> Option<&Punctuator> {
        match self {
            TokenKind::Punctuator(p) => Some(p),
            _ => None,
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::EOF)
    }

    pub fn starts_statement(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(
                Keyword::If
                    | Keyword::While
                    | Keyword::For
                    | Keyword::Return
                    | Keyword::Break
                    | Keyword::Continue
            )
        )
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::Identifier(_) => "identifier",
            TokenKind::Keyword(_) => "keyword",
            TokenKind::IntLiteral(_) => "integer literal",
            TokenKind::FloatLiteral(_) => "float literal",
            TokenKind::StringLiteral(_) => "string literal",
            TokenKind::CharLiteral(_) => "char literal",
            TokenKind::Operator(_) => "operator",
            TokenKind::Punctuator(_) => "punctuator",
            TokenKind::Comments(_) => "comment",
            TokenKind::EOF => "end of file",
        };
        write!(f, "{}", s)
    }
}

///
/// Keywords, currently supported: Type(Type), Return, Void, For, While, If, Else And SizeOf
///
#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Return,
    Void,
    Goto,
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
    Break,
    Continue,
    Static,
    Extern,
    Auto,
    Register,
    Const,
    Volatile,
    Restrict,
    Inline,
    Noreturn,
}

impl Keyword {
    pub fn from_str(s: &str) -> Result<Self, String> {
        match s {
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
            "static" => Ok(Keyword::Static),
            _ => Err("unexpected error during parsing keyword".to_string()),
        }
    }

    pub fn token(&self, span: Span) -> Token {
        Token {
            kind: TokenKind::Keyword(self.clone()),
            span: span,
        }
    }
}

///
/// Operators, Currently supported: +, -, /, *(Mult), =, |, &, ~, !, <, > and ^
///
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
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
    pub fn from_str(s: &str) -> Result<Self, String> {
        match s {
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

    pub fn token(&self, span: Span) -> Token {
        Token {
            kind: TokenKind::Operator(self.clone()),
            span: span,
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

impl Punctuator {
    pub fn from_str(s: &str) -> Result<Self, String> {
        if s.len() != 1 {
            return Err("Special character length must be 1".to_string());
        }
        let _allowed_chars = "[]{}(),.:;*=#~";
        let ch = s.chars().next().unwrap();
        match ch {
            '[' => Ok(Punctuator::LeftSquareBracket),
            ']' => Ok(Punctuator::RightSquareBracket),
            '{' => Ok(Punctuator::LeftCurlyBracket),
            '}' => Ok(Punctuator::RightCurlyBracket),
            '(' => Ok(Punctuator::LeftParenthesis),
            ')' => Ok(Punctuator::RightParenthesis),
            ';' => Ok(Punctuator::SemiColon),
            ',' => Ok(Punctuator::Comma),
            _ => Err("Illegal special character".to_string()),
        }
    }

    pub fn token(&self, span: Span) -> Token {
        Token {
            kind: TokenKind::Punctuator(self.clone()),
            span: span,
        }
    }
}
