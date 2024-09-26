use std::cell::RefCell;
use std::rc::Rc;
use crate::parser::{ASTNode, ASTNodeType};
pub fn generate_assembly(ast_root: Rc<RefCell<ASTNode>>) -> String {
    let mut result = String::new();
    let cur_node = ast_root; // Keep as Rc<RefCell<ASTNode>>

    for child in cur_node.borrow().get_children_nodes().iter() {
        match child.borrow().get_type() {
            ASTNodeType::Root => continue,

            ASTNodeType::FnDeclaration { fn_name, .. } => {
                result += ".section .text\n";
                let mut assembly_fn_name = format!("_{}", fn_name);
                if fn_name == "main" {
                    assembly_fn_name = "main".to_string();
                }
                result += &format!(".global {}\n", assembly_fn_name);
                result += &format!("{}:\n", assembly_fn_name);
                result += &generate_assembly(Rc::clone(child));
            }

            ASTNodeType::VarDecl { .. } => {
            }

            ASTNodeType::IntegerLiteral(number) => {
                result += &number.to_string();
            }

            ASTNodeType::Identifier(_) => {
            }

            ASTNodeType::ReturnStatement() => {
                let return_val = generate_assembly(Rc::clone(child));
                result += &format!("    movl    ${}, %eax\n", return_val);
                result += "    ret\n";
            }
        }
    }

    result
}
