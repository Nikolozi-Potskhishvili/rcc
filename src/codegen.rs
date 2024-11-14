use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::read;
use std::rc::Rc;
use std::thread::current;
use crate::lexer::{Constant, Operator, Token, Type};
use crate::parser::{ASTNode, ASTNodeType};

struct Variable {
    var_type: Type,
    memory_offset: i64,
    register: Option<String>,
}


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
                return simplify_expression(&child).expect("Error parsing binary node").to_string();

            }
            ASTNodeType::UnaryOperation { operator, operand } => {
                /*
                let operator = match operator {
                    Operator::Minus => "negl",
                    Operator::Not | Operator::Tilde => "notl",
                    _ => panic!("Expected unary operator, but got {:?}", operator),
                };
                result += &format!("    # Unary operation: {:?}\n", operator);
                let operand = simplify_expression(&Rc::clone(operand)).expect("Error evaluating integer expression");
                result += &operand.to_string();
                result += &format!("   {}\n", operator);
                */
                return simplify_expression(&child).expect("Error parsing unary node").to_string();
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

fn allocate_int_on_stack(
    node: &Rc<RefCell<ASTNode>>,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: i64
) -> Result<String, String> {
    let nod_type = node.borrow_mut().get_type();
    let (var_name, var_type) = match nod_type {
        ASTNodeType::VarDecl { var_name, var_type } => (var_name, var_type),
        _ => return Err("Invalid node type".to_string())
    };
    var_table.insert(var_name.clone(), Variable {
        var_type: Type::Integer,
        memory_offset: cur_stack_size + 8,
        register: None,
    });
    let instruction = "SUB RSP 8".to_string();
    return Ok(instruction);
}

fn store_int_on_stack(
    node: &Rc<RefCell<ASTNode>>,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: i64
) -> Result<String, String> {
    let var_name = match node.borrow().get_type() {
        ASTNodeType::VarAssignment{var_name} => var_name,
        _ => return Err("Invalid node type".to_string())
    };

    if !var_table.contains_key((var_name)) {
        return Err("Assigment without declaring a variable".to_string());
    }

    let expression_root = node.borrow_mut().get_children_nodes().get(0)?;

}
///
///  Binary operations use R8 for storing result of left side of expression and R9 for right side
///  Unary operations use R10 for storing result
///
fn generate_expression_instructions(
    expression_root: &Rc<RefCell<ASTNode>>,
    var_table: &HashMap<String, Variable>,
    result_vec: &mut Vec<String>
) -> Result<(), String> {
    let mut current_node = Rc::clone(expression_root);
    match current_node.borrow().get_type() {
        ASTNodeType::BinaryOperation { left, right, operator} => {
             generate_expression_instructions(left, var_table, result_vec)?;
             generate_expression_instructions(right, var_table, result_vec)?;

            let instruction = match operator {
                Operator::Plus => "ADD",
                Operator::Minus => "SUB",
                Operator::Division => "DIV",
                Operator::Multiplication => "MUL",
                _ => return Err("Unsupported operator".to_string())
            };
            result_vec.push("    move R15 0".to_string());
            result_vec.push("    ADD R15 R8".to_string());
            result_vec.push(format!("    {} R15 R9", instruction));
            let register_to_save = get_register_number(&current_node)?;
            result_vec.push(format!("   move R{} R15", register_to_save));
        },
        ASTNodeType::UnaryOperation {operator, operand} => {
            generate_expression_instructions(operand, var_table, result_vec)?;

            let instruction = match operator {
                Operator::Not => "",
                Operator::Tilde => "",
                Operator::Minus => "NEG",
                _ => return Err("Unsupported operator".to_string())
            };
            result_vec.push("    move R15 0".to_string());
            result_vec.push("    ADD R15 R10".to_string());
            result_vec.push(format!("    {} R15", instruction));
            let register_to_save = get_register_number(&current_node)?;
            result_vec.push(format!("   move R{} R15", register_to_save));

        },
        ASTNodeType::OperandNode {value } => {
            let constant = value.get_constant()?;
            let val = constant.get_val();
            if val == "" {
                return Err("undefined constant".to_string());
            }
            result_vec.push("    move R15 0".to_string());
            result_vec.push(format!("    ADD R15 {}", val));
            let register_to_save = get_register_number(&current_node)?;
            result_vec.push(format!("   move R{} R15", register_to_save));
            return Ok(())
        },
        ASTNodeType::VarCallNode {var_name} => {
        },
        _ => return Err("Invalid node type during expression codgen".to_string())
    }
    Ok(())
}

///
/// Gets expression ref as argument and returns register number based on the type of its parent node.
/// For binary if it on left 8, if on right 9, for unary 10. If parent is root of expression returns 1
///
///
fn get_register_number(
    expression_node: &Rc<RefCell<ASTNode>>
) -> Result<i32, String> {
    match expression_node.borrow().get_type() {
        ASTNodeType::BinaryOperation { .. } | ASTNodeType::UnaryOperation { .. }
            | ASTNodeType::OperandNode { .. } | ASTNodeType::VarCallNode { .. } => {
            if let Some(parent_ref) = expression_node.borrow().get_parent_node() {
                let parent = parent_ref.upgrade().expect("Expected parent node to exist");
                return match parent.borrow().get_type() {
                    ASTNodeType::BinaryOperation { operator, right, left } => {
                        if Rc::ptr_eq(expression_node, left) {
                            Ok(8)
                        } else if Rc::ptr_eq(expression_node, right) {
                            Ok(9)
                        } else {
                            Err("Invalid children of binary parent node".to_string())
                        }
                    },
                    ASTNodeType::UnaryOperation {..} => {
                         Ok(10)
                    },
                    ASTNodeType::OperandNode { .. } | ASTNodeType::VarCallNode { .. } => {
                        Ok(0)
                    },
                    _ => Err("Invalid node during parsing expressions parent".to_string())
                }
            }
            return Ok(1);
        },
        _ => Err("Invalid node during parsing expression".to_string());
    }
}

fn simplify_expression(expression_root: &Rc<RefCell<ASTNode>>) -> Result<i32, String> {
    match &expression_root.borrow().get_type() {
        ASTNodeType::BinaryOperation { operator, right, left } => {
            let eval_left = simplify_expression(left);
            let eval_right = simplify_expression(right);
            match operator {
                    Operator::Plus => Ok(eval_left? + eval_right?),
                    Operator::Division => Ok(eval_left? / eval_right?),
                    Operator::Multiplication => Ok(eval_left? * eval_right?),
                    _ => Err(format!("{:?} invalid binary operator", operator)),
            }
        }
        ASTNodeType::UnaryOperation { operator, operand } => {
            let eval_operand = simplify_expression(operand).expect("Error evaluating unary node");
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
