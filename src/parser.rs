use std::cell::{Ref, RefCell};
use std::fmt::{format, Pointer};
use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::ast_types::Stmt::{Block, If};
use crate::lexer::{Operator, SpecialCharacter, Keyword, Token, Constant, Type};
use crate::lexer::SpecialCharacter::LeftCurlyBracket;


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

    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expr, String> {
        println!("{:?} before parsing parsing logical and in logical or", self.peek());
        let mut node = self.parse_logical_and().unwrap_or_else(|_| Expr::Const(Constant::Integer(0)));
        println!("{:?} after parsing parsing logical and in logical or", self.peek());
        loop {
            match self.peek() {
                Token::Operator(Operator::Or) => {
                    println!("Logical or parsed");
                    let operator = self.consume();
                    let right_side = self.parse_logical_or()?;
                    let log_or_node = Self::create_binary_ast_node(operator, node, right_side);
                    node = log_or_node;
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, String> {
        println!("{:?} before parsing parsing logical relation in logical and", self.peek());
        let mut node = self.parse_logical_relation_op().unwrap_or_else(|_| Expr::Const(Constant::Integer(0)));
        println!("{:?} after parsing parsing logical relation in logical and", self.peek());
        loop {
            match self.peek() {
                Token::Operator(Operator::And) => {
                    println!("Logical and parsed");
                    let operator = self.consume();
                    let right_side = self.parse_logical_and()?;
                    let log_or_node = Self::create_binary_ast_node(operator, node, right_side);
                    node = log_or_node;
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_logical_relation_op(&mut self) -> Result<Expr, String> {
        println!("{:?} before parsing parsing additive in logical relation", self.peek());
        let mut node = self.parse_additive().unwrap_or_else(|_| Expr::Const(Constant::Integer(0)));
        println!("{:?} after parsing parsing additive in logical relation", self.peek());
        loop {
            match self.peek() {
                Token::Operator(Operator::More) | Token::Operator(Operator::Less) => {
                    println!("Logical and logical relation operator parsed");
                    let operator = self.consume();
                    let right_side = self.parse_logical_and()?;
                    let log_or_node = Self::create_binary_ast_node(operator, node, right_side);
                    node = log_or_node;
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        println!("{:?} before parsing parsing multi in additive", self.peek());
        let mut node = self.parse_multiplicative().unwrap_or_else(|_| Expr::Const(Constant::Integer(0)));
        println!("{:?} after parsing parsing multi additive", self.peek());
        loop {
            match self.peek() {
                Token::Operator(Operator::Plus) | Token::Operator(Operator::Minus) => {
                    println!("plus parsed or minus");
                    let operator = self.consume();
                    let right = self.parse_additive()?;
                    let plus_node = Self::create_binary_ast_node(operator, node, right);
                    node = plus_node;
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
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

    fn parse_unary(&mut self) -> Result<Expr, String> {
        println!("{:?} during parsing unary", self.peek());
        match self.peek() {
            Token::Constant(_) | Token::SpecialCharacter(_) | Token::Identifier(_) => {
                Ok(self.parse_primary()?)
            }
            Token::Operator(Operator::Tilde) | Token::Operator(Operator::Not)
            | Token::Operator(Operator::Minus) => {
                let operator = self.consume();
                println!("During paring unary we got: {:?}", operator);
                let operand = self.parse_unary()?;

                Ok(Self::create_unary_ast_node(operator, operand))
            },
            _ => Err(String::from("illegal token"))
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        println!("Parsing primary:{:?}", self.peek());
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

    fn create_unary_ast_node(token: Token, operand: Expr) -> Expr {
        let unary_operator = token.get_operator().expect("not found unary operator during constructing node");
        Expr::UnaryExpr(UnaryExpr {
            operator: unary_operator,
            operand: Box::new(operand),
        })
    }

    fn create_binary_ast_node(token: Token, left: Expr, right: Expr) -> Expr {
        let binary_operator = token.get_operator().expect("not found binary operator during constructing node");
        Expr::BinaryExpr(BinaryExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator: binary_operator,
        })
    }

    fn create_constant_ast_node(constant: Token) -> Expr {
        return match constant {
            Token::Constant(cnst) => Expr::Const(cnst),
            _ => panic!("Unexpected token during parsing of expression")
        }
    }

    fn crate_variable_node(variable_name: String) -> Expr {
        Expr::VarUsage(variable_name)
    }
}

// current grammar:
//Expression    ::= LogicalOr
//LogicalOr        ::= LogicalAnd ( '|' LogicalAnd )*
//LogicalAnd       ::= Relational ( '&' Relational )*
//Relational       ::= Additive ( ('<' | '>' | '<=' | '>=') Additive )*
//Additive      ::= Multiplicative ( '+' | '-' Multiplicative )*
//Multiplicative ::= Unary ( ('*' | '/') Unary )*
// Unary         ::= ('~' | '!') Unary | Primary
// Primary       ::= NUMBER | Var

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Expr, String> {
    let mut extracted_tokens: Vec<Token> = Vec::new();
    let mut open_paren_count = 0;
    for token in tokens {
        match token {
            Token::Identifier(_) | Token::Constant(_) | Token::Operator(_)=> {
                extracted_tokens.push(token);
            },
            Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) => {
                open_paren_count += 1;
                extracted_tokens.push(token);
            },
            Token::SpecialCharacter(SpecialCharacter::RightParenthesis) => {
                open_paren_count -= 1;
                if open_paren_count < 0 {
                    break;
                }
                extracted_tokens.push(token);
            }
            Token::SpecialCharacter(SpecialCharacter::SemiColon) | Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket) => break,
            _ => return Err(format!("Unexpected token {:?}, during expression paring", token)),
        }
    }
    println!("Extracted tokens: {:?}", extracted_tokens);
    let mut expression_parser = ExpressionParser::new(extracted_tokens);
    if let Ok(expression_root) = expression_parser.parse_expression() {
        return Ok(expression_root);
    };
    Err(String::from("Unexpected expression parse error"))
}
/// Gets Vec of tokens as input and returns AST tree or error Result
///
pub fn generate_ast_tree(tokens: Vec<Token>) -> Result<Vec<Rc<RefCell<Stmt>>>, String> {
    let mut token_iter = tokens.into_iter().peekable();
    let mut result = Vec::new();
    while let Some(token) = token_iter.next() {
        match token {
            Token::Keyword(keyword) => {
            // expect some type keyword at global scope
                match keyword {
                    Keyword::Type(type_key) => {
                        result.push(parse_integer_declaration(&mut token_iter)?);
                    }
                    _ => return Err(format!("Unexpected keyword {:?}, at global scope", keyword))
                }
            }
            _ => return Err("Unexpected token at global scope".to_string())
        }
    }
    Ok(result)
}


/// Parses scope and returns statements, that belong to it.
///
///
fn parse_scope_tokens(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Vec<Rc<RefCell<Stmt>>>, String> {
    let mut scope_statements: Vec<Rc<RefCell<Stmt>>> = Vec::new();
    // stack which tracks the latest scopes block variant reference
    let mut parent_stack : Vec<Rc<RefCell<Stmt>>> = Vec::new();
    // Parse the tokens
    while let Some(token) = token_iter.next() {
        let mut stmt = match token {
            // parse int keyword
            Token::Keyword(Keyword::Type(Type::Integer)) => {
                let name_tok = token_iter.next().expect("Expected name after integer keyword");
                let name_string = match name_tok {
                    Token::Identifier(name) => name,
                    _ => return Err(format!("Illegal token after integer keyword: {:?}", name_tok))
                };
                parse_variable_declaration(token_iter, &name_string)?
            },
            Token::Keyword(Keyword::If) => {
                parse_conditional(token_iter)?
            },
            // Match return statement `return <int>;`
            Token::Keyword(Keyword::Return) => {
                parse_return_statement(token_iter)?
            },
            // Handle block end `}`
            Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket) => {
                return Ok(scope_statements);
            },
            Token::Identifier(identifier) => {
                handle_identifier_usage(token_iter, &identifier)?
            },
            Token::Keyword(Keyword::While) => {
                handle_while_keyword(token_iter)?
            },
            // Skip any other tokens or syntax we don't support
            _ => continue,
        };
        scope_statements.push(stmt);
    }
    for statement in &scope_statements {
        println!("{:?}", statement);
    }
    Ok(scope_statements)
}

fn handle_while_keyword(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Rc<RefCell<Stmt>>, String> {
    //consume while keyword
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let expression_root = parse_expression(token_iter)?;
    expect_token(token_iter, Token::SpecialCharacter(LeftCurlyBracket))?;
    let cycle_body = parse_scope_tokens(token_iter)?;
    let body_block = Rc::new(RefCell::new(Block(cycle_body)));
    let while_node = Rc::new(RefCell::new( Stmt::While {
        condition: expression_root,
        body: body_block
    }));

    Ok(while_node)
}

/// Parses tokens after else keyword
///
fn parse_else_keyword(token_iter: &mut Peekable<IntoIter<Token>>, ) -> Result<Rc<RefCell<Stmt>>, String> {
    match token_iter.peek().ok_or_else(|| "No token after parsing else keyword")? {
        Token::Keyword(Keyword::If) => {
            // consume if keyword
            token_iter.next();
            let if_else_node = parse_conditional(token_iter)?;
            Ok(if_else_node)
        }
        Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket) => {
            token_iter.next();
            let statements = parse_scope_tokens(token_iter)?;

            let condition_node = Rc::new(RefCell::new(Block(statements)));
            Ok(condition_node)
        }
        _ => {
            Err(format!("Invalid token after else keyword {:?}", token_iter.peek()))
        }
    }
}

/// Parsers if keyword
///
///
fn parse_conditional(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Rc<RefCell<Stmt>>, String> {
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let bool_expression_root = parse_expression(token_iter)?;
    let statements = parse_scope_tokens(token_iter)?;
    let else_branch = match token_iter.peek() {
        None => None,
        Some(token) => {
            match token {
                Token::Keyword(Keyword::Else) => {
                    println!("We are parsing else keyword!!!!!");
                    token_iter.next();
                    Some(parse_else_keyword(token_iter)?)
                },
                _ => None,
            }
        }
    };

    let if_statement = Rc::new(RefCell::new(Stmt::If {
        condition: bool_expression_root,
        then_branch: Rc::new(RefCell::new(Stmt::Block(statements))),
        else_branch,
    }));

    Ok(if_statement)
}


fn handle_identifier_usage(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String
) -> Result<Rc<RefCell<Stmt>>, String> {
    match token_iter.peek() {
        // var assignment
       Some(Token::Operator(Operator::Equals)) => {
           token_iter.next();

           let expression_root = parse_expression(token_iter)?;
           let var_node = Rc::new(RefCell::new(Stmt::VarAssignment {
               name: name.to_string(),
               expr: Some(expression_root),
           }));
           Ok(var_node)
       },
       Some(Token::SpecialCharacter(SpecialCharacter::LeftParenthesis)) => {
            Err("Function call not supported yet".to_string())
       },
        _ => Err("Invalid symbol after identifier".to_string()),
    }

}

/// Parses integer keyword at global scope, returns VarDec Node or FuncDec Node,
/// All the scopes inside are parsed recursively
///
fn parse_integer_declaration(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Rc<RefCell<Stmt>>, String> {
    if let Some(Token::Identifier(name)) = token_iter.peek().cloned() {
        token_iter.next(); // consume name
        if let Some(Token::SpecialCharacter(character)) = token_iter.peek() {
            return match character {
                SpecialCharacter::LeftParenthesis => {
                    parse_function_declaration(token_iter, &name)
                },
                SpecialCharacter::SemiColon => {
                    parse_variable_declaration(token_iter, &name)
                },
                _ => Err(format!("Unexpected special character after {:?}, integer declaration", character))
            }
        } else if let Some(Token::Operator(Operator::Equals)) = token_iter.peek() {
            return parse_variable_declaration(token_iter, &name)
        }
    }
    Err(format!("Unexpected token while parsing integer declaration: {:?}", token_iter.peek()))
}

fn parse_function_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String
) -> Result<Rc<RefCell<Stmt>>, String> {
    // Expect parentheses `()` and '{'
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let args = parse_args(token_iter);
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::RightParenthesis))?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket))?;

    let function_statements = parse_scope_tokens(token_iter)?;
    let function_node = Rc::new(RefCell::new(Stmt::FnDecl {
        name: name.to_string(),
        return_type: "int".to_string(),
        args,
        body: Rc::new(RefCell::new(Stmt::Block(function_statements))),
    }));
    Ok(function_node)
}

fn parse_args(
    token_iter: &mut Peekable<IntoIter<Token>>
) -> Option<Vec<Expr>> {
    None
}

fn parse_variable_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String
) -> Result<Rc<RefCell<Stmt>>, String> {
        let int_var = Rc::new(RefCell::new(Stmt::VarDecl {
            name: name.to_string(),
            var_type: "int".to_string(),
            expr: None,
        }));
        match token_iter.peek() {
            Some(Token::Operator(Operator::Equals)) => {
                token_iter.next();
                let expression_root = parse_expression(token_iter)?;

                Ok(Rc::new(RefCell::new(Stmt::VarDecl {
                    name: name.to_string(),
                    var_type: "int".to_string(),
                    expr: Some(expression_root),
                })))
            },
            Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) => {
                token_iter.next();
                Ok(int_var)
            },
            None => Err("No tokens after int var identifier".to_string()),
            _ =>  Err("Unexpected token while parsing variable declaration".to_string()),
        }
}

fn parse_return_statement(token_iter: &mut Peekable<IntoIter<Token>>) -> Result<Rc<RefCell<Stmt>>, String> {

    let expression_root = parse_expression(token_iter)?;
    let return_node = Rc::new(RefCell::new(Stmt::Return(Some(expression_root))));
    Ok(return_node)
}


fn handle_end_of_block(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<Stmt>>>
) -> Result <(), String> {
    let res = parent_stack.pop();
    if res.is_none() {
        return Err("There is } but no {".to_string());
    }
    Ok(())
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

pub fn print_ast(root_nodes: &Vec<Rc<RefCell<Stmt>>>) {
    for node in root_nodes {
        println!("{:?}", node);
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Constant, Keyword, Operator, SpecialCharacter, Token};
    use crate::lexer::Type::Integer;
    use crate::parser::{generate_ast_tree, print_ast,  ExpressionParser};

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

        //let mut expression_parser = ExpressionParser::new(tokens);
        //let ast = expression_parser.parse_expression();
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
        //let expression_parser = generate_ast_tree(tokens).unwrap();
    }

}