use std::cell::RefCell;
use std::cmp::PartialEq;
use std::iter::Peekable;
use std::os::unix::raw::mode_t;
use std::rc::{Rc, Weak};
use std::vec::IntoIter;
use crate::lexer::{Operator, SpecialCharacter, Keyword, Token, Constant, Lexer};


pub struct ASTNode {
    node_type: ASTNodeType,
    parent_node: Option<Weak<RefCell<ASTNode>>>,
    children_nodes: Vec<Rc<RefCell<ASTNode>>>,
}

impl ASTNode {
    pub fn get_type(&self) -> &ASTNodeType {
        &self.node_type
    }

    pub fn get_parent_node(&self) -> &Option<Weak<RefCell<ASTNode>>> {
        &self.parent_node
    }

    pub fn get_children_nodes(&self) -> &Vec<Rc<RefCell<ASTNode>>> {
        &self.children_nodes
    }
}


pub enum ASTNodeType {
    Root,
    FnDeclaration{ fn_name: String, return_type: String},

    VarDecl { var_name: String, var_type: String},

    //Expressions
    IntegerLiteral(i32),
    Identifier(String),

    //operators
    BinaryOperation {
        operator: Operator,
        right: Rc<RefCell<ASTNode>>,
        left: Rc<RefCell<ASTNode>>,
    },
    UnaryOperation {
        operator: Operator,
        operand: Rc<RefCell<ASTNode>>,
    },
    OperandNode {
        value: Token,
    },

    //statements
    ReturnStatement(),
}

struct ExpressionParser {
    tokens: Vec<Token>,
    current_token: usize,
}


impl ExpressionParser {

    fn new(tokens: Vec<Token>) -> Self {
        ExpressionParser {
            tokens,
            current_token: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current_token >= self.tokens.len() || self.tokens[self.current_token] == Token::EndOFFile
            || self.tokens[self.current_token] == Token::SpecialCharacter(SpecialCharacter::SemiColon)
    }

    fn parse(&mut self, parent: Rc<RefCell<ASTNode>>) -> Rc<RefCell<ASTNode>> {

        while !self.is_at_end() {

        }
        parent
    }

    fn peek(&self) -> &Token {
        if self.is_at_end() {
            &Token::EndOFFile
        } else {
            &self.tokens[self.current_token]
        }
    }

    fn consume(&mut self) -> Token {
        if self.is_at_end() {
          return Token::EndOFFile
        }
        let token = self.tokens[self.current_token].clone();
        self.current_token += 1;
        token
    }

    fn expect(&mut self, expected: Token) -> Result<Token, String> {
        if *self.peek() == expected {
            Ok(self.consume())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.peek()))
        }
    }

    fn parse_expression(&mut self) -> Rc<RefCell<ASTNode>> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Rc<RefCell<ASTNode>> {
        let node = self.parse_multiplicative()
        
        loop {
            match self.peek() {
                Token::Operator(Operator::Plus) => {
                    self.consume();
                }
                _ => break,
            }
        }
        
        node
    }

    fn parse_multiplicative(&mut self) -> Rc<RefCell<ASTNode>> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Rc<RefCell<ASTNode>> {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Rc<RefCell<ASTNode>> {

    }
}

// current grammar:
//Expression    ::= Additive
//Additive      ::= Multiplicative ( '+' Multiplicative )*
//Multiplicative ::= Unary ( ('*' | '/') Unary )*
// Unary         ::= ('~' | '!' | '-') Unary | Primary
// Primary       ::= NUMBER

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>, current_node: &Rc<RefCell<ASTNode>>) -> Result<(), String> {
    let mut extracted_tokens: Vec<Token> = Vec::new();
    for token in tokens {
        match token {
            Token::Identifier(_) | Token::Constant(_) | Token::Operator(_) => { extracted_tokens.push(token); },
            Token::SpecialCharacter(character) => {
                match character {
                    SpecialCharacter::LeftParenthesis => {

                    }
                    SpecialCharacter::RightParenthesis => {

                    }
                    SpecialCharacter::SemiColon => { break;}
                    _ => return Err(format!("Special character {:?} not supported", character)),
                }
            }
            _ => return Err(format!("Unexpected token {:?}", token)),
        }
    }



    return Ok(())
}



pub fn generate_ast_tree<'a>(tokens: Vec<Token>) -> Result<Rc<RefCell<ASTNode>>, String> {
    let root = Rc::new(RefCell::new(ASTNode {
        node_type: ASTNodeType::Root,
        parent_node: None,
        children_nodes: Vec::new(),
    }));

    let mut current_node = Rc::clone(&root);
    let mut token_iter = tokens.into_iter().peekable();

    // Parse the tokens
    while let Some(token) = token_iter.next() {
        match token {
            // Match function declaration `int main()`
            Token::Keyword(Keyword::Integer) => {
                if let Some(Token::Identifier(name)) = token_iter.peek() {
                    if name == "main" {
                        token_iter.next(); // Consume `main`
                        // Expect parentheses `()`
                        if let Some(Token::SpecialCharacter(SpecialCharacter::LeftParenthesis)) = token_iter.next() {
                            if let Some(Token::SpecialCharacter(SpecialCharacter::RightParenthesis)) = token_iter.next() {
                                // We have parsed `int main()`
                                // Expect `{` for function body
                                if let Some(Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket)) = token_iter.next() {
                                    let function_node = Rc::new(RefCell::new(ASTNode {
                                        node_type: ASTNodeType::FnDeclaration { fn_name: "main".to_string(), return_type: "int".to_string() },
                                        parent_node: Some(Rc::downgrade(&current_node)),
                                        children_nodes: Vec::new(),
                                    }));
                                    current_node.borrow_mut().children_nodes.push(Rc::clone(&function_node));
                                    current_node = Rc::clone(&function_node);
                                    continue;
                                } else {
                                    return Err("Expected '{' after main()".to_string());
                                }
                            }
                        }
                    }
                }
            }

            // Match return statement `return <int>;`
            Token::Keyword(Keyword::Return) => {
                let return_node = Rc::new(RefCell::new(ASTNode {
                    node_type: ASTNodeType::ReturnStatement(),
                    parent_node: Some(Rc::downgrade(&current_node)),
                    children_nodes: Vec::new(),
                }));
                if let Some(Token::Constant(constant)) = token_iter.next() {
                    if let Ok(parsed_const) = parse_const(constant, &return_node) {
                        return_node.borrow_mut().children_nodes.push(parsed_const);
                    }
                    current_node.borrow_mut().children_nodes.push(return_node);
                    if let Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) = token_iter.peek() {
                        continue
                    } else {
                        return Err("no semicolon after return".to_string());
                    }
                }
            }
            // Handle block end `}`
            Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket)=> {
                if let Some(parent) = {
                    let node_ref = current_node.borrow();
                    node_ref.parent_node.as_ref().and_then(|p| p.upgrade())
                } {
                    current_node = parent;
                }
                continue;
            }

            // Skip any other tokens or syntax we don't support
            _ => continue,
        }
    }
    Ok(root)
}

fn parse_const(constant: Constant, parent_node: &Rc<RefCell<ASTNode>>) -> Result<Rc<RefCell<ASTNode>>, String> {
    match constant {
        Constant::Integer(int) => {
            Ok(Rc::new(RefCell::new(ASTNode {
                node_type: ASTNodeType::IntegerLiteral(int),
                parent_node: Some(Rc::downgrade(parent_node)),
                children_nodes: Vec::new(),
            })))
        }
        _ => Err("Error during parsing constant. {constant:?} is not valid".to_string())
    }
}



/*fn get_fn_dec_node_by_name<'a>(fn_name: &str, root_node: &'a ASTNode) -> Option<&'a ASTNode> {
    if root_node.children_nodes.is_empty() {
        return match root_node.node_type {
            ASTNodeType::FnDeclaration { .. } => Some(root_node),
            _ => None,
        }
    }
    for child in &root_node.children_nodes {
        if let Some(result) = get_fn_dec_node_by_name(fn_name, child) {
            return Some(result);
        }
    }
    None
}*/