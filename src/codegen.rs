use std::cell::RefCell;
use std::fmt::format;
use std::rc::Rc;
use crate::lexer::{Constant, Operator, Token};
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
                let return_val = evaluate_integer_expression(&child).expect("error during integer expression evaluation");
                result += &format!("    movl    ${}, %eax\n", return_val);
                result += "    ret\n";
            }
            _ => {}
        }
    }

    result
}


fn evaluate_integer_expression(expression_root: &Rc<RefCell<ASTNode>>) -> Result<i32, String> {
    match &expression_root.borrow().get_type() {
        ASTNodeType::BinaryOperation { operator, right, left } => {
            let eval_left = evaluate_integer_expression(left);
            let eval_right = evaluate_integer_expression(right);
            match operator {
                Token::Operator(op) => {
                    match op {
                        Operator::Plus => Ok(eval_left? + eval_right?),
                        Operator::Division => Ok(eval_left? / eval_right?),
                        Operator::Multiplication => Ok(eval_left? * eval_right?),
                        _ => Err(format!("{:?} invalid binary operator", op))

                    }
                }
                _ => Err(format!("{:?} is not an operator", operator))
            }
        }
        ASTNodeType::UnaryOperation { operator, operand } => {
            let eval_operand = evaluate_integer_expression(operand);
            match operator {
                Token::Operator(op) => {
                    match op {
                        Operator::Minus => Ok( - eval_operand?),
                        Operator::Not => Ok(!eval_operand?),
                        Operator::Tilde => Ok(!eval_operand?),
                        _ => Err(format!("{:?} invalid unary operator", op))
                    }
                }
                _ => Err(format!("{:?} no an unary operator", operator))
            }
        }
        ASTNodeType::OperandNode {value} => {
             match value {
                Token::Constant(Constant::Integer(int)) => {
                    Ok(*int)
                }
                _ => Err(format!("{:?} is not an integer", value))
            }
        }
        other => Err(format!("{:?} unexpected ast node type", other))
    }
}
