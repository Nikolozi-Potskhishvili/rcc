use std::cell::RefCell;
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

/// Represents the different types of nodes in an Abstract Syntax Tree (AST).
#[derive(Debug)]
pub enum ASTNodeType {
    /// The root node of the AST. Acts as the entry point for traversing the tree.
    Root,
    /// Represents a function declaration node.
    ///
    /// # Fields
    /// - `fn_name`: The name of the function being declared.
    /// - `return_type`: The return type of the function.
    FnDeclaration{
        fn_name: String,
        return_type: String,
        block: Rc<RefCell<ASTNodeType::Block>>
    },

    If {
        condition_root: Rc<RefCell<ASTNode>>,
        then_ref: Rc<RefCell<ASTNode>>,
        else_ref: Option<Rc<RefCell<ASTNode>>>,
    },

    Else {

    },
    /// Represents block of code.
    ///
    /// # Fields
    ///
    Block {

    },

    /// Represents a variable declaration node.
    ///
    /// # Fields
    /// - `var_name`: The name of the variable being declared.
    /// - `var_type`: The type of the variable.
    VarDecl { var_name: String, var_type: Type},

    /// Represents a variable assignment node, used when assigning a value to an existing variable.
    ///
    /// # Fields
    /// - `var_name`: The name of the variable being assigned a value.
    /// - 'val': Constant assigned to var
    VarAssignment {var_name: String},

    /// Represents an identifier in the AST, typically a variable or function name.
    ///
    /// # Fields
    /// - Contains a `String` that holds the name of the identifier.    //Expressions
    Identifier(String),

    /// Represents a binary operation node (e.g., addition, subtraction) in an expression.
    ///
    /// # Fields
    /// - `operator`: The binary operator (e.g., `+`, `-`, `*`, `/`).
    /// - `right`: A reference-counted pointer to the right operand node.
    /// - `left`: A reference-counted pointer to the left operand node.
    BinaryOperation {
        operator: Operator,
        right: Rc<RefCell<ASTNode>>,
        left: Rc<RefCell<ASTNode>>,
    },

    /// Represents a unary operation node (e.g., negation) in an expression.
    ///
    /// # Fields
    /// - `operator`: The unary operator (e.g., `-`, `!`).
    /// - `operand`: A reference-counted pointer to the operand node.
    UnaryOperation {
        operator: Operator,
        operand: Rc<RefCell<ASTNode>>,
    },

    /// Represents a literal or operand in an expression, such as a constant or number.
    ///
    /// # Fields
    /// - `value`: The value of the operand, represented as a token.
    OperandNode {
        value: Token,
    },

    /// Represents a variable reference (or call) in the AST.
    ///
    /// # Fields
    /// - `var_name`: The name of the variable being referenced.
    VarCallNode {
        var_name: String,
    },

    /// Represents a return statement, typically used to end a function and provide a return value.
    ReturnStatement(),

    /// Represents the end of the file, used to indicate the termination of the AST.
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

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Result< Rc<RefCell<ASTNode>>, String> {
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
/*    for val in &extracted_tokens {
        println!("{}", format!("{:?} token", val));
    }
*/    let mut expression_parser = ExpressionParser::new(extracted_tokens);
    if let Ok(expression_root) = expression_parser.parse_expression() {
        return Ok(expression_root);
    };
    Err(String::from("Unexpected expression parse error"))
}
///
/// Gets Vec of tokens as input and returns AST tree or error Result
///
pub fn generate_ast_tree(tokens: Vec<Token>) -> Result<Rc<RefCell<ASTNode>>, String> {
    let root = Rc::new(RefCell::new(ASTNode {
        node_type: ASTNodeType::Root,
        parent_node: None,
        children_nodes: Vec::new(),
    }));
    let mut current_node = Rc::clone(&root);
    let mut token_iter = tokens.into_iter().peekable();
    let mut parent_stack : Vec<Rc<RefCell<ASTNode>>> = Vec::new();
    parent_stack.push(Rc::clone(&root));
    // Parse the tokens
    while let Some(token) = token_iter.next() {
        current_node = match token {
            // parse int keyword
            Token::Keyword(Keyword::Type(Type::Integer)) => {
                parse_integer_declaration(&mut token_iter, &mut parent_stack)?
            },
            Token::Keyword(Keyword::If) => {
                parse_if_keyword(&mut token_iter, &mut parent_stack)?
            },
            Token::Keyword(Keyword::Else) => {
                parser_else_keyword(&mut token_iter, &mut parent_stack)?
            },
            // Match return statement `return <int>;`
            Token::Keyword(Keyword::Return) => {
              parse_return_statement(&mut token_iter, &mut parent_stack)?
            },
            // Handle block end `}`
            Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket)=> {
                handle_end_of_block(&mut token_iter, &current_node, &mut parent_stack)?
            }
            Token::Identifier(identifier) => {
                handle_identifier_usage(&mut token_iter, &current_node)?
            }
            // Skip any other tokens or syntax we don't support
            _ => continue,
        }
    }
    Ok(root)
}

fn parser_else_keyword(token_iter: &mut Peekable<IntoIter<Token>>, parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>) -> Result<Rc<RefCell<ASTNode>>, String> {
    todo!()
}

fn parse_if_keyword(token_iter: &mut Peekable<IntoIter<Token>>, parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>) -> Result<Rc<RefCell<ASTNode>>, String> {
    // consume if keyword
    token_iter.next();
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let bool_expression_root = handle_boolean_expression(token_iter)?;

    Ok()
}

fn handle_boolean_expression(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Rc<RefCell<ASTNode>>, String> {
    todo!()
}

fn handle_identifier_usage(
    token_iter: &mut Peekable<IntoIter<Token>>,
    current_node: & Rc<RefCell<ASTNode>>
) -> Result<Rc<RefCell<ASTNode>>, String> {
    match token_iter.peek() {
       Some(Token::Operator(Operator::Equals)) => {
           token_iter.next();
           let var_node = Rc::new(RefCell::new(ASTNode {
               node_type: ASTNodeType::Root,
               parent_node: Some(Rc::downgrade(&current_node)),
               children_nodes: vec![],
           }));

           let expression_root = parse_expression(token_iter)?;
           var_node.borrow_mut().children_nodes.push(Rc::clone(&expression_root));

           Ok(var_node)
       },
       Some(Token::SpecialCharacter(SpecialCharacter::LeftParenthesis)) => {
            Ok(current_node.clone())
       },
        _ => Ok(current_node.clone()),
    }

}
fn parse_integer_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>
) -> Result<Rc<RefCell<ASTNode>>, String> {
    if let Some(Token::Identifier(name)) = token_iter.peek() {
        return if name == "main" {
            parse_function_declaration(token_iter, parent_stack)
        } else {
            parse_variable_declaration(token_iter, parent_stack)
        }
    }
    Err("Unexpected token while parsing integer declaration".to_string())
}

fn parse_function_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>
) -> Result<Rc<RefCell<ASTNode>>, String> {
    token_iter.next(); // Consume `main`
    // Expect parentheses `()` and '{'
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::RightParenthesis))?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket))?;
    // Parse main function
    let function_node = Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::FnDeclaration { fn_name: "main".to_string(), return_type: "int".to_string() },
            parent_node: Some(Rc::downgrade(parent_stack.last().unwrap())),
            children_nodes: Vec::new(),
        }));
    parent_stack.last().unwrap().borrow_mut().children_nodes.push(Rc::clone(&function_node));
    parent_stack.push(Rc::clone(&function_node));
    Ok(function_node)
}

fn parse_variable_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>
) -> Result<Rc<RefCell<ASTNode>>, String> {
    if let Some(Token::Identifier(name)) = token_iter.next() {
        let int_var = Rc::new(RefCell::new(ASTNode {
            node_type: ASTNodeType::VarDecl {
                var_name: name,
                var_type: Type::Integer,
            },
            parent_node: Some(Rc::downgrade(parent_stack.last().unwrap())),
            children_nodes: vec![],
        }));
       parent_stack.last().unwrap().borrow_mut().children_nodes.push(Rc::clone(&int_var));

        match token_iter.peek() {
            Some(Token::Operator(Operator::Equals)) => {
                token_iter.next();
                let expression_root = parse_expression(token_iter)?;
                expression_root.borrow_mut().parent_node = Some(Rc::downgrade(&int_var));
                int_var.borrow_mut().children_nodes.push(Rc::clone(&expression_root));
                Ok(int_var)
            },
            Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) => {
                token_iter.next();
                Ok(int_var)
            },
            None => Err("No tokens after int var identifier".to_string()),
            _ =>  Err("Unexpected token while parsing variable declaration".to_string()),
        }

    } else {
        Err("Unexpected token while parsing variable declaration".to_string())
    }
}

fn parse_return_statement(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>,
) -> Result<Rc<RefCell<ASTNode>>, String> {
    let return_node = Rc::new(RefCell::new(ASTNode {
        node_type: ASTNodeType::ReturnStatement(),
        parent_node: Some(Rc::downgrade(&parent_stack.last().unwrap())),
        children_nodes: vec![],
    }));
    parent_stack.last().unwrap().borrow_mut().children_nodes.push(Rc::clone(&return_node));

    let expression_root = parse_expression(token_iter)?;
    expression_root.borrow_mut().parent_node = Some(Rc::downgrade(&return_node));
    return_node.borrow_mut().children_nodes.push(Rc::clone(&expression_root));

    Ok(return_node)
}


fn handle_end_of_block(
    token_iter: &mut Peekable<std::vec::IntoIter<Token>>,
    current_node: &Rc<RefCell<ASTNode>>,
    parent_stack: &mut Vec<Rc<RefCell<ASTNode>>>,
) -> Result<Rc<RefCell<ASTNode>>, String> {
    let parent_node = current_node.borrow().parent_node.as_ref().and_then(|p| p.upgrade());
    parent_stack.pop();
    parent_node.ok_or_else(|| "No parent node found when handling block end".to_string())
}

/// consumes next if it is expected token and returns () or returns error if not
fn expect_token(
    token_iter: &mut Peekable<std::vec::IntoIter<Token>>,
    expected_token: Token
) -> Result<(), String> {
    if token_iter.peek() == Some(&expected_token) {
        token_iter.next();
        Ok(())
    } else {
        Err(format!("Expected {:?}, found {:?}", expected_token, token_iter.peek()))
    }
}

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
            Token::Operator(Operator::Plus),
            Token::Operator(Operator::Minus),
            Token::Constant(Constant::Integer(5)),
            Token::Operator(Operator::Multiplication),
            Token::Constant(Constant::Integer(2)),
            Token::Operator(Operator::Plus),
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
        let expression_parser = generate_ast_tree(tokens).unwrap();
        print_ast(&expression_parser, 0);
    }

}