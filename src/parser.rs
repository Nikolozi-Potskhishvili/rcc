use std::cell::RefCell;
use std::io::read_to_string;
use std::iter::Peekable;
use std::ops::Deref;
use std::os::unix::raw::time_t;
use std::ptr::read;
use std::rc::{Rc, Weak};
use std::vec::IntoIter;
use log::error;
use crate::lexer::{Operator, SpecialCharacter, Keyword, Token, Constant};

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

    VarDecl { var_name: String, var_type: String},

    //Expressions
    IntegerLiteral(i32),
    Identifier(String),

    //operators
    NotOperator,

    //statements
    ReturnStatement(),
}

pub fn ast_recursion_helper(token_itr: &Peekable<IntoIter<Token>>, cur_node: Rc<RefCell<ASTNode>>) {
    match cur_node.borrow().node_type {
        ASTNodeType::Root => return,
        ASTNodeType::FnDeclaration { .. } => {

        }
        ASTNodeType::VarDecl { .. } => {}
        ASTNodeType::IntegerLiteral(_) => {}
        ASTNodeType::Identifier(_) => {}
        ASTNodeType::NotOperator => {}
        ASTNodeType::ReturnStatement() => {}
        _ => {}
    }
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
                } else if Some(Token::Operator(op)) {
                    match op {
                        Operator::Not => {
                            let not_node = Rc::new(RefCell::new(ASTNode {
                                node_type: ASTNodeType::NotOperator,
                                parent_node: Some(Rc::downgrade(&return_node)),
                                children_nodes: vec![],
                            }));
                        },
                        Operator::BitCompl => {

                        },
                        Operator::Minus => {

                        },
                        _ => return Err("operator is not supported yet".to_string()),
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