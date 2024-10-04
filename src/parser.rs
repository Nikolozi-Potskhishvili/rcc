use std::cell::RefCell;
use std::cmp::PartialEq;
use std::iter::Peekable;
use std::rc::{Rc, Weak};
use std::vec::IntoIter;
use crate::lexer::{Operator, SpecialCharacter, Keyword, Token, Constant, Type};

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ASTNodeType {
    Root,
    FnDeclaration{ fn_name: String, return_type: String},

    VarDecl { var_name: String, var_type: Type},
    VarAssignment {var_name: String, },
    //Expressions
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
    VarCallNode {
        var_name: String,
    },
    //statements
    ReturnStatement(),
    EndOfFile,
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

    fn parse_expression(&mut self) -> Result<Rc<RefCell<ASTNode>>, String> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Result<Rc<RefCell<ASTNode>>, String> {
        println!("{:?} before parsing parsing multi in additive", self.peek());
        let mut node = self.parse_multiplicative().unwrap_or_else(|_| Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::OperandNode { value: Token::Constant(Constant::Integer(0)) },
            parent_node: None,
            children_nodes: vec![],
        })));
        println!("{:?} after parsing parsing multi additive", self.peek());
        loop {
            match self.peek() {
                Token::Operator(Operator::Plus) => {
                    println!("plus parsed");
                    let operator = self.consume();
                    let right = self.parse_additive()?;
                    let plus_node = Self::create_binary_ast_node(operator, Rc::clone(&node), Rc::clone(&right));
                    node = plus_node;
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_multiplicative(&mut self) -> Result<Rc<RefCell<ASTNode>>, String> {
        println!("{:?} before parsing parsing unary in mutli", self.peek());
        let mut node = self.parse_unary()?;
        println!("{:?} after parsing unary in multi", self.peek());
        loop {
            match self.peek() {
                Token::Operator(op) => {
                    println!("{:?} found operator", op);
                    if op.is_unary() {
                       return  Ok(self.parse_unary()?)
                    } else {
                        match self.peek() {
                            Token::Operator(Operator::Multiplication) | Token::Operator(Operator::Division) => {
                                let operator = self.consume();
                                let right = self.parse_multiplicative()?;
                                let binary_operator = Self::create_binary_ast_node(operator, node, right);
                                node = binary_operator;
                            }
                            _ => break,
                        }
                    }
                }
                _ => break
            }
        }
        Ok(node)
    }

    fn parse_unary(&mut self) -> Result<Rc<RefCell<ASTNode>>, String> {
        println!("{:?} during parsing unary", self.peek());
        match self.peek() {
            Token::Constant(_) | Token::SpecialCharacter(_) | Token::Identifier(_) => {
                Ok(self.parse_primary()?)
            }
            Token::Operator(Operator::Minus) | Token::Operator(Operator::Tilde) |
            Token::Operator(Operator::Not)=> {
                let operator = self.consume();
                let operand = self.parse_unary()?;
                Ok(Self::create_unary_ast_node(operator, operand))
            },
            _ => Err(String::from("illegal token"))
        }
    }

    fn parse_primary(&mut self) -> Result<Rc<RefCell<ASTNode>>, String> {
        match self.peek() {
            Token::Identifier(name) => {
                let name_clone = name.clone();
                self.consume();
                // if next token is right paretheses then identifier is function call if not varriable
                if let Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) = self.peek() {
                    panic!("function identifiers not supported yet!!!");
                } else {
                    Ok(Self::crate_variable_node(name_clone))
                }
            }
            Token::Constant(_) => {
                println!("constant is {}", format!("{:?}", self.peek()));
                Ok(Self::create_constant_ast_node(self.consume()))
            }
            Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) => {
                self.consume();
                let inner_expression =  self.parse_expression()?;
                if let Token::SpecialCharacter(SpecialCharacter::RightParenthesis) = self.consume() {
                    Ok(inner_expression)
                } else {
                    Err(String::from("Expected closing right parenthesis"))
                }
            }
            _ => Err(format!("unexpected token {:?}", self.peek())),
        }
    }

    fn create_unary_ast_node(token: Token, operand: Rc<RefCell<ASTNode>>) -> Rc<RefCell<ASTNode>> {
        let unary_operator = token.get_operator().expect("not found unary operator during constructing node");
        let unary = Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::UnaryOperation {operator: unary_operator, operand: Rc::clone(&operand)},
            parent_node: None,
            children_nodes: vec![],
        }));
        operand.borrow_mut().parent_node = Some(Rc::downgrade(&unary));
        unary
    }

    fn create_binary_ast_node(token: Token, left: Rc<RefCell<ASTNode>>, right: Rc<RefCell<ASTNode>>) -> Rc<RefCell<ASTNode>> {
        let binary_operator = token.get_operator().expect("not found binary operator during constructing node");
        let binary = Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::BinaryOperation {
                operator: binary_operator,
                right: Rc::clone(&right),
                left: Rc::clone(&left),
            },
            parent_node: None,
            children_nodes: vec![],
        }));
        left.borrow_mut().parent_node = Some(Rc::downgrade(&binary));
        right.borrow_mut().parent_node = Some(Rc::downgrade(&binary));
        binary
    }

    fn create_constant_ast_node(constant: Token) -> Rc<RefCell<ASTNode>> {
        Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::OperandNode { value: constant},
            parent_node: None,
            children_nodes: vec![],
        }))
    }

    fn crate_variable_node(variable_name: String) -> Rc<RefCell<ASTNode>> {
        Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::VarCallNode { var_name: variable_name.clone()},
            parent_node: None,
            children_nodes: vec![],
        }))

    }
}

// current grammar:
//Expression    ::= Additive
//Additive      ::= Multiplicative ( '+' Multiplicative )*
//Multiplicative ::= Unary ( ('*' | '/') Unary )*
// Unary         ::= ('~' | '!' | '-') Unary | Primary
// Primary       ::= NUMBER

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>, current_node: &Rc<RefCell<ASTNode>>) -> Result< Rc<RefCell<ASTNode>>, String> {
    let mut extracted_tokens: Vec<Token> = Vec::new();

    for token in tokens {
        match token {
            Token::Identifier(_) | Token::Constant(_) | Token::Operator(_)
            | Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) | Token::SpecialCharacter(SpecialCharacter::RightParenthesis)
            => { extracted_tokens.push(token); },
            Token::SpecialCharacter(SpecialCharacter::SemiColon) => break,
            _ => return Err(format!("Unexpected token {:?}", token)),
        }
    }
    for val in &extracted_tokens {
        println!("{}", format!("{:?} token", val));
    }
    let mut expression_parser = ExpressionParser::new(extracted_tokens);
    if let Ok(expression_root) = expression_parser.parse_expression() {
        return Ok(expression_root);
    };
    Err(String::from("Unexpected expression parse error"))
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
            Token::Keyword(Keyword::Type(Type::Integer)) => {
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
                    } else {
                        let int_var_node = Rc::new(RefCell::new(ASTNode {
                            node_type: ASTNodeType::VarDecl { var_name: name.to_string(), var_type: Type::Integer },
                            parent_node: Some(Rc::downgrade(&current_node)),
                            children_nodes: vec![],
                        }));
                        current_node.borrow_mut().children_nodes.push(Rc::clone(&int_var_node));
                        token_iter.next();
                        if let Some(next_token) = token_iter.peek() {
                            match next_token {
                                Token::Operator(Operator::Equals) => {
                                    token_iter.next();
                                    let parsing_result = parse_expression(&mut token_iter, &int_var_node);
                                    if let Ok(expression_root) =  parsing_result {
                                        println!("Expression root = {:?}", expression_root);
                                        expression_root.borrow_mut().parent_node = Some(Rc::downgrade(&int_var_node));
                                        int_var_node.borrow_mut().children_nodes.push(Rc::clone(&expression_root));
                                    } else if let Err(error) = parsing_result {
                                        println!("{}", error);
                                        panic!("invalid expression after declaring integer variable");
                                    }
                                }
                                Token::SpecialCharacter(SpecialCharacter::SemiColon) => {
                                    token_iter.next();
                                    continue
                                }
                                _ => return Err(String::from("unexpected token")),
                            }
                        } else {
                            panic!("unexpected token {:?} after declaring integer variable", token_iter.peek().unwrap());
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
                current_node.borrow_mut().children_nodes.push(Rc::clone(&return_node));
                if let Ok(expression_root) = parse_expression(&mut token_iter, &return_node) {
                    println!("parsed expression after return");
                    expression_root.borrow_mut().parent_node = Some(Rc::downgrade(&return_node));
                    return_node.borrow_mut().children_nodes.push(Rc::clone(&expression_root));
                } else {
                    panic!("could not parse expression");
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
            Token::Identifier(identifier) => {
                match token_iter.peek().expect("expect tokens after identifier") {
                    // identifier is function call
                    Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) => {

                    }
                    // identifier is treated as variable assignment
                    Token::Operator(Operator::Equals) => {
                        token_iter.next();
                        let var_node = Rc::new(RefCell::new(ASTNode {
                            node_type: ASTNodeType::Root,
                            parent_node: Some(Rc::downgrade(&current_node)),
                            children_nodes: vec![],
                        }));
                        if let Ok(expression_root) = parse_expression(&mut token_iter, &var_node) {
                            var_node.borrow_mut().children_nodes.push(Rc::clone(&expression_root));
                            expression_root.borrow_mut().parent_node = Some(Rc::downgrade(&current_node));
                        }  else {
                            panic!("could not parse expression");
                        }
                    }
                    _ => {

                    }
                }
            }
            // Skip any other tokens or syntax we don't support
            _ => continue,
        }
    }
    Ok(root)
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

pub fn print_ast(node: &Rc<RefCell<ASTNode>>, depth: usize) {
    let indent = "  ".repeat(depth);
    let node_borrow = node.borrow();
    println!("{}{:?}\n", indent, node_borrow);
    for child in &node_borrow.children_nodes {
        print_ast(child, depth + 1);
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use crate::lexer::{Constant, Keyword, Operator, SpecialCharacter, Token};
    use crate::lexer::Type::Integer;
    use crate::parser::{generate_ast_tree, print_ast, ASTNode, ASTNodeType, ExpressionParser};

    #[test]
    fn test_expression_parser() {
        let tokens = vec![
            Token::Constant(Constant::Integer(3)),
            Token::Operator(crate::lexer::Operator::Plus),
            Token::Operator(crate::lexer::Operator::Minus),
            Token::Constant(Constant::Integer(5)),
            Token::Operator(crate::lexer::Operator::Multiplication),
            Token::Constant(Constant::Integer(2)),
            Token::Operator(crate::lexer::Operator::Plus),
            Token::Constant(Constant::Integer(11)),
        ];
        let root_node = Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::Root,
            parent_node: None,
            children_nodes: vec![],
        }));
        let mut expression_parser = ExpressionParser::new(tokens);
        let ast = expression_parser.parse_expression();

        print_ast(&ast.unwrap(), 0);
    }

    #[test]
    fn test_simple_var_dec() {
        let tokens = vec![
            Token::Keyword(Keyword::Type(Integer)),
            Token::Identifier("main".to_string()),
            Token::SpecialCharacter(SpecialCharacter::LeftParenthesis),
            Token::SpecialCharacter(SpecialCharacter::RightParenthesis),
            Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket),
            Token::Keyword(Keyword::Type(Integer)),
            Token::Identifier("var".to_string()),
            Token::Operator(Operator::Equals),
            Token::Constant(Constant::Integer(11)),
            Token::SpecialCharacter(SpecialCharacter::SemiColon),
            Token::Keyword(Keyword::Return),
            Token::Identifier("var".to_string()),
            Token::Operator(Operator::Plus),
            Token::Constant(Constant::Integer(2)),
            Token::SpecialCharacter(SpecialCharacter::SemiColon),
            Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket),
        ];
        let mut expression_parser = generate_ast_tree(tokens).unwrap();
        print_ast(&expression_parser, 0);
    }

}