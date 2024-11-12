use std::cell::RefCell;
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

            ASTNodeType::VarDecl { var_name, var_type } => {

            }

            /*ASTNodeType::IntegerLiteral(number) => {
                result += &number.to_string();
            }*/

            ASTNodeType::Identifier(_) => {
            }
            ASTNodeType::BinaryOperation { operator, right, left } => {
               /* let operator_instruction = match operator {
                    Operator::Plus => "addl",
                    Operator::Division => "idvl",
                    Operator::Multiplication => "idmul",
                    _ => panic!("Unsupported binary operator"),
                };
                if *operator == Operator::Division {
                    result += &generate_division_assembly(Rc::clone(&left), Rc::clone(&right));
                } else {
                    result += &format!("    # Binary operation: {:?}\n", operator_instruction);
                    let left = generate_assembly(Rc::clone(left));
                    let right = generate_assembly(Rc::clone(right));
                    result += &left;
                    result += &right;
                    result += &format!("   {} %eax, %eax\n", operator_instruction);
                }
                let left_eval = generate_assembly(Rc::clone(left));
                let right_eval = generate_assembly(Rc::clone(right));*/
                return evaluate_integer_expression(&child).expect("Error parsing binary node").to_string();

            }
            ASTNodeType::UnaryOperation { operator, operand } => {
                /*
                let operator = match operator {
                    Operator::Minus => "negl",
                    Operator::Not | Operator::Tilde => "notl",
                    _ => panic!("Expected unary operator, but got {:?}", operator),
                };
                result += &format!("    # Unary operation: {:?}\n", operator);
                let operand = evaluate_integer_expression(&Rc::clone(operand)).expect("Error evaluating integer expression");
                result += &operand.to_string();
                result += &format!("   {}\n", operator);
                */
                return evaluate_integer_expression(&child).expect("Error parsing unary node").to_string();
            }
            ASTNodeType::OperandNode { value } => {
                let constant = value.get_constant().expect("Error evaluating operand node");
                let val = match constant {
                    Constant::Integer(int) => int.to_string(),
                    Constant::Short(short) => short.to_string(),
                    Constant::Long(long) => long.to_string(),
                    Constant::Double(double) => double.to_string(),
                    Constant::Float(float) => float.to_string(),
                    Constant::Char(char) => char.to_string(),
                    Constant::Undefined => panic!("")
                };
                result += &val.to_string();
            }
            ASTNodeType::ReturnStatement() => {
                let return_val = generate_assembly(Rc::clone(child));
                result += &format!("    movl    ${}, %eax\n", return_val);
                result += "    ret\n";
            }
            _ => {}
        }
    }

    result
}

fn generate_division_assembly(left: Rc<RefCell<ASTNode>>, right: Rc<RefCell<ASTNode>>) -> String {
    let mut asm = String::new();
    asm += "Binary Operation: Division\n";
    asm += "    cltd\n";
    asm += "    idivl %eax\n";
    asm
}

fn evaluate_integer_expression(expression_root: &Rc<RefCell<ASTNode>>) -> Result<i32, String> {
    match &expression_root.borrow().get_type() {
        ASTNodeType::BinaryOperation { operator, right, left } => {
            let eval_left = evaluate_integer_expression(left);
            let eval_right = evaluate_integer_expression(right);
            match operator {
                    Operator::Plus => Ok(eval_left? + eval_right?),
                    Operator::Division => Ok(eval_left? / eval_right?),
                    Operator::Multiplication => Ok(eval_left? * eval_right?),
                    _ => Err(format!("{:?} invalid binary operator", operator)),
            }
        }
        ASTNodeType::UnaryOperation { operator, operand } => {
            let eval_operand = evaluate_integer_expression(operand).expect("Error evaluating unary node");
            match operator {
                Operator::Minus => Ok(-eval_operand),
                Operator::Not => {
                    if eval_operand == 0 {
                        Ok(1)
                    } else {
                        Ok(0)
                    }
                },
                Operator::Tilde => Ok(!eval_operand),
                _ => Err(format!("{:?} invalid unary operator", operator))
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
