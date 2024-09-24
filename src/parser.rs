use std::process::id;
use std::rc::{Rc, Weak};
use crate::lexer::Token;

pub struct ASTNode<'a> {
    token: Option<Token>,
    parent_node: Option<Weak<ASTNode<'a>>>,
    children_nodes: Vec<Rc<ASTNode<'a>>>,
}

struct Function {
    fn_name: String,
    fn_type: String,
    fn_statement: Box<dyn Statement>,
}

impl Function {
    fn new(fn_name: &str, fn_type: &str, fn_statement: Box<dyn Statement>) -> Function {
        Function {
            fn_name: fn_name.to_string(),
            fn_type: fn_type.to_string(),
            fn_statement,
        }
    }
}

struct Return {
    expression: Box<dyn Expression>,
}
impl Return {
    fn new(expression: Box<dyn Expression>) -> Return {
        Return {
            expression,
        }
    }
}
impl Statement for Return {
}
struct Constant {
    value: i32,
}
impl Expression for Constant {
}
trait Expression {
}
trait Statement {
}

pub fn generate_AST_tree(tokens: Vec<Token>) -> Result<ASTNode<>, String> {
    let mut root = ASTNode {
        token: None,
        parent_node: None,
        children_nodes: Vec::new(),
    };
    let current_node = root;
    for (index, token) in tokens.iter().enumerate() {
        match token {
            Token::Identifier(name) => {
                let function = Function::new(name, "int", Box::new(Return::new(Box::new(Constant { value: 0 }))));
            }
            _ => continue
        }
    }

   Result::Err("_".to_string())
}