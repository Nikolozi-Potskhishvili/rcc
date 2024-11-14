use std::cell::{ RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::lexer::Constant;
use crate::parser::{ASTNode, ASTNodeType};

pub struct SemanticAnalyzer {
    var_table: HashMap<String, Constant>,
    function_set: HashSet<String>,
}



impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            var_table: HashMap::new(),
            function_set: HashSet::new(),
        }
    }

    pub fn declare_variable(&mut self, name: &String) -> Result<(), String> {
        if self.var_table.contains_key(name) {
            // Return an error if the key already exists
            Err(format!("Key '{}' already exists in the map", name))
        } else {
            // Add the key-value pair to the map if the key doesn't exist
            self.var_table.insert(name.to_string(), Constant::Undefined);
            Ok(())
        }
    }

    pub fn assign_value_to_variable(&mut self, name: String, value: Constant) -> Result<(), String> {
        if self.var_table.contains_key(&name) {
            let old_val = self.var_table.insert(name, value.clone());
            assert!(old_val.is_some());
            assert_eq!(value.get_type(), old_val.unwrap().get_type());

            Ok(())
        } else {
            Err(format!("Key '{}' does not exist in the map", name.to_string()))
        }
    }

    pub fn semantic_analysis(&mut self,ast_root: Rc<RefCell<ASTNode>>) -> Rc<RefCell<ASTNode>> {
        for child in ast_root.borrow_mut().get_children_nodes() {
            match child.borrow().get_type() {
                ASTNodeType::FnDeclaration { .. } => {}
                ASTNodeType::VarDecl { var_name, var_type } => {

                }
                ASTNodeType::Identifier(_) => {}
                ASTNodeType::BinaryOperation { .. } | ASTNodeType::UnaryOperation {..}
                | ASTNodeType::OperandNode {..} | ASTNodeType::VarCallNode {..} => {
                    Self::delete_junk_node(Rc::clone(&ast_root), Rc::clone(&child));
                }
                ASTNodeType::ReturnStatement() => {
                    let expression_root = child.borrow_mut()
                        .get_children_nodes()
                        .get(0)
                        .expect("expected statement after return statement");
                    let simplified_expression = Self::simplify_integer_expression(Rc::clone(&child));
                }
                ASTNodeType::EndOfFile => break,
                _ => break,
            }

        }
        ast_root
    }

    fn delete_junk_node(parent_node: Rc<RefCell<ASTNode>>, junk_node_root: Rc<RefCell<ASTNode>>) {

    }

    fn simplify_integer_expression(expression_root: Rc<RefCell<ASTNode>>) -> Rc<RefCell<ASTNode>> {

        expression_root
    }
}
