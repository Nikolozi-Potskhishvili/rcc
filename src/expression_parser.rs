use std::collections::HashMap;
use crate::ast_types::{BinaryExpression, Expr, UnaryExpr};
use crate::ast_types::Expr::ArrayAccess;
use crate::lexer::{Constant, Operator, SpecialCharacter, SymbolTableEntry, Token, Type};

pub struct ExpressionParser {
    tokens: Vec<Token>,
    current_token: usize,
    type_map: HashMap<String, Type>,
    symbol_table: HashMap<String, SymbolTableEntry>
}

impl ExpressionParser {
    pub fn new(tokens: Vec<Token>, type_map: HashMap<String, Type>, symbol_table: HashMap<String, SymbolTableEntry>) -> Self {
        ExpressionParser {
            tokens,
            current_token: 0,
            type_map,
            symbol_table,
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

    pub fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, String> {
        println!("{:?} before parsing parsing an assignment", self.peek());
        let lhs = self.parse_logical_or()?;
        let is_l = Self::is_l_value(&lhs);
        if is_l {
            loop {
                match self.peek() {
                    Token::Operator(Operator::Assign) | Token::Operator(Operator::IncrementAssign)
                    | Token::Operator(Operator::DecrementAssign) => {
                        let operator = self.consume();
                        let rhs = self.parse_assignment()?;
                        return Ok(Self::create_binary_ast_node(operator, lhs, rhs));
                    },
                    _ => break,
                }
            }
        }
        Ok(lhs)
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
                let mut primary = self.parse_primary()?;
                self.parse_postfix(primary)

            }
            Token::Operator(Operator::Tilde) | Token::Operator(Operator::Not)
            | Token::Operator(Operator::Minus) | Token::Operator(Operator::Ref) => {
                let operator = self.consume();
                println!("During paring unary we got: {:?}", operator);
                let operand = self.parse_unary()?;

                Ok(Self::create_unary_ast_node(operator, operand))
            },
            Token::Operator(Operator::Multiplication) => {
                self.consume();
                let op = Token::Operator(Operator::Deref);
                let operand = self.parse_unary()?;
                Ok(Self::create_unary_ast_node(op, operand))
            },
            _ => Err(String::from("illegal token"))
        }
    }

    fn parse_postfix (&mut self, mut expr: Expr) -> Result<Expr, String> {
        println!("Parsing postfix: {:?}", self.peek());
        loop {
            match self.peek() {
                Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket) => {
                    let operator = Token::Operator(Operator::ArrayAccess);
                    self.consume();
                    let index_expression = self.parse_expression()?;
                    if self.consume() != Token::SpecialCharacter(SpecialCharacter::RightSquareBracket) {
                        return Err(String::from("Expected closing ']'"));
                    }
                    expr = ArrayAccess(Box::from(expr), Box::from(index_expression));
                },
                _ => break,
            }
        }

        Ok(expr)
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
                    Ok(self.crate_variable_node(name_clone)?)
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


    fn is_l_value(expression: &Expr) -> bool {
        if matches!(expression, Expr::VarUsage(_) | Expr::ArrayAccess(_, _)) {
            return true
        }
        false
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
            Token::Constant(cnst) => Expr::Const(Constant::Long(cnst)),
            _ => panic!("{}", format!("Unexpected token during parsing of expression: {:?}", constant))
        }
    }

    fn crate_variable_node(&mut self, variable_name: String) -> Result<Expr, String> {
        let var = self.symbol_table.get(&variable_name).expect("No var declared");
        match var {
            SymbolTableEntry::Variable(cur_type) => {
                Ok(Expr::VarUsage(variable_name))
            },
            // SymbolTableEntry::FunDef(..) => {
            //
            // }
            _ => panic!("Unexpected type of idnetifier in expression")
        }

    }
}
