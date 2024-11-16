use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env::var;
use std::fmt::format;
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


pub fn generate_assembly(ast_root: Rc<RefCell<ASTNode>>) -> Result<String, String> {
    let mut result = String::new();
    let cur_node = ast_root; // Keep as Rc<RefCell<ASTNode>>
    let mut stack_size = 0;
    let mut var_table = HashMap::new();

    for child in cur_node.borrow().get_children_nodes().iter() {
        match child.borrow().get_type() {
            ASTNodeType::Root => continue,

            ASTNodeType::FnDeclaration { fn_name, .. } => {
                result += ".intel_syntax noprefix\n";
                result += ".section .text\n";
                let mut assembly_fn_name = format!("_{}", fn_name);
                if fn_name == "main" {
                    assembly_fn_name = "main".to_string();
                }
                println!("Number of child nodes {}", child.borrow().get_children_nodes().len());
                result += &format!(".global {}\n", assembly_fn_name);
                result += &format!("{}:\n", assembly_fn_name);
                result += &generate_assembly(Rc::clone(child))?;
            }

            ASTNodeType::VarDecl { var_name, var_type } => {
                let instructions = allocate_int_on_stack(child, &mut var_table, &mut stack_size)?;
                result += &instructions;
                result += "\n";
                let child_binding = child.borrow();
                let expression_root_children = child_binding.get_children_nodes();
                if  expression_root_children.len() != 0 {
                    let expression_root = expression_root_children.get(0).unwrap();
                    let instructions = store_int_on_stack(child, &mut var_table, &mut stack_size)?;
                    result += &instructions;
                    result += "\n";
                }
            }

            ASTNodeType::VarAssignment { var_name } => {
                let instructions = store_int_on_stack(child, &mut var_table, &mut stack_size)?;
                result += &instructions;
            },

            ASTNodeType::VarCallNode { var_name } => {

            },

            ASTNodeType::Identifier(identifier) => {
            }
            ASTNodeType::BinaryOperation { operator, right, left } => {

            }
            ASTNodeType::UnaryOperation { operator, operand } => {
            }
            ASTNodeType::OperandNode { value } => {
            }
            ASTNodeType::ReturnStatement() => {
                println!("{:?}", child.clone().borrow().get_children_nodes().get(0).unwrap().borrow().get_parent_node().clone().unwrap());
                let (instructions, reg) = generate_return_instructions(child, &mut var_table)?;
                result += &instructions;
                result += "\n";
                result += &format!("    mov rax, r{}\n", reg);
                result += &format!("    add rsp, {}\n", stack_size);
                result += "    ret\n";
            }
            _ => {}
        }
    }

    Ok(result)
}

fn generate_return_instructions(
    node: &Rc<RefCell<ASTNode>>,
    var_table: &mut HashMap<String, Variable>,
) -> Result<(String, i32), String> {
    let binding = node.borrow();
    let node_type = binding.get_type();
    match node_type {
        ASTNodeType::ReturnStatement() => {
            let children = binding.get_children_nodes();
            if children.len() != 1 {
                return Err("Return statement has more than 1 child".to_string())
            }
            let expression_root = children.get(0).unwrap();
            let mut instruction_vec = Vec::new();
            let mut free_registers = vec![8, 9, 10];
            let reg= generate_expression_instructions(expression_root, var_table, &mut instruction_vec, &mut free_registers)?;
            Ok((instruction_vec.join("\n"), reg))
        }
        _=> Err("dfdsafadsfdasfds".to_string())
    }
}
fn allocate_int_on_stack(
    node: &Rc<RefCell<ASTNode>>,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64
) -> Result<String, String> {
    let biding = node.borrow();
    let nod_type = biding.get_type();
    let (var_name, var_type) = match nod_type {
        ASTNodeType::VarDecl { var_name, var_type } => (var_name, var_type),
        _ => return Err(format!("Invalid node type {:?} during allocation on stack", nod_type))
    };
    *cur_stack_size += 8;
    var_table.insert(var_name.clone(), Variable {
        var_type: Type::Integer,
        memory_offset: *cur_stack_size - 8,
        register: None,
    });
    let instruction = "    sub rsp, 8".to_string();
    Ok(instruction)
}

fn store_int_on_stack(
    node: &Rc<RefCell<ASTNode>>,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64,
) -> Result<String, String> {
    let binding = node.borrow();
    let var_name = match binding.get_type() {
        ASTNodeType::VarAssignment{var_name} => var_name,
        ASTNodeType::VarDecl { var_name, var_type } => var_name,
        _ => return Err(format!("Invalid node type {:?} during storing var on stack", binding.get_type()))
    };

    if !var_table.contains_key((var_name)) {
        return Err("Assigment without declaring a variable".to_string());
    }
    let var = var_table.get(var_name).unwrap();

    let binding = node.borrow();
    let expression_root = binding.get_children_nodes().get(0).expect("expected expression after var");

    let mut instruction_vec = Vec::new();
    let mut free_registers = vec![8, 9, 10];
    let reg = generate_expression_instructions(expression_root, var_table, &mut instruction_vec, &mut free_registers)?;
    instruction_vec.push(format!("    mov [rsp + {}], r{}", var.memory_offset, reg));
    Ok(instruction_vec.join("\n"))
}
///
///  Binary operations use R8 for storing result of left side of expression and R9 for right side
///  Unary operations use R8 for storing result
///
///
fn generate_expression_instructions(
    expression_root: &Rc<RefCell<ASTNode>>,
    var_table: &HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    free_registers: &mut Vec<i32>,
) -> Result<i32, String> {
    let binding = Rc::clone(expression_root);
    let mut current_node_type = binding.borrow();
    return match current_node_type.get_type() {
        ASTNodeType::BinaryOperation { left, right, operator } => {
            let left_reg = generate_expression_instructions(left, var_table, result_vec, free_registers)?;
            let right_reg = generate_expression_instructions(right, var_table, result_vec, free_registers)?;

            let instruction = match operator {
                Operator::Plus => "add",
                Operator::Minus => "sub",
                Operator::Division => "div",
                Operator::Multiplication => "imul",
                _ => return Err("Unsupported operator".to_string())
            };
            result_vec.push(format!("    {} r{}, r{}", instruction, left_reg, right_reg));
            free_registers.push(right_reg);
            Ok(left_reg)
        },
        ASTNodeType::UnaryOperation { operator, operand } => {
            let register = generate_expression_instructions(operand, var_table, result_vec, free_registers)?;

            let instruction = match operator {
                Operator::Not => {
                    result_vec.push(format!("test r{}, r{}", register, register));
                    result_vec.push(format!("setz al"));
                    result_vec.push(format!("movzx r{}, al", register));
                    return Ok(register);
                },
                Operator::Tilde => "not",
                Operator::Minus => "neg",
                _ => return Err("Unsupported operator".to_string())
            };
            result_vec.push(format!("    {} r{}", instruction, register));
            Ok(register)
        },
        ASTNodeType::OperandNode { value } => {
            let constant = value.get_constant().expect("expected constant");
            let val = constant.get_val();
            if val == "" {
                return Err("undefined constant".to_string());
            }
            let reg = free_registers.pop().unwrap();
            result_vec.push(format!("    mov r{}, 0", reg));
            result_vec.push(format!("    add r{}, {}", reg, val));
            Ok(reg)
        },
        ASTNodeType::VarCallNode { var_name } => {
            let memory_offset_op = var_table.get(var_name);
            if memory_offset_op.is_none() {
                return Err(format!("Variable {}, was not defined", var_name))
            }
            let memory_offset = memory_offset_op.unwrap().memory_offset;
            let register = free_registers.pop().unwrap();
            result_vec.push(format!("    mov r{}, [RSP + {}]", register, memory_offset));
            Ok(register)
        },
        _ => Err("Invalid node type during expression codgen".to_string())
    }
}

///
/// Gets expression ref as argument and returns register number based on the type of its parent node.
/// For binary if it on left 8, if on right 9, for unary 8. If parent is root of expression returns 1
/// For return statements returns 8 and for var assignment or declaration 8
///
fn get_register_number(
    expression_node: &Rc<RefCell<ASTNode>>
) -> Result<i32, String> {
    match expression_node.borrow().get_type() {
        ASTNodeType::BinaryOperation { .. } | ASTNodeType::UnaryOperation { .. }
            | ASTNodeType::OperandNode { .. } | ASTNodeType::VarCallNode { .. } => {
            if let Some(parent_ref) = expression_node.borrow().get_parent_node() {
                let binding = parent_ref.upgrade().expect("Expected parent node to exist");
                let pr_ref = binding.borrow();
                return match pr_ref.get_type() {
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
                         Ok(8)
                    },
                    ASTNodeType::OperandNode { .. } | ASTNodeType::VarCallNode { .. } => {
                        Ok(0)
                    },
                    ASTNodeType::VarDecl {..} | ASTNodeType::VarAssignment  {..} => {
                        Ok(8)
                    },
                    ASTNodeType::ReturnStatement(..) => {
                        Ok(8)
                    }
                    _ => Err(format!("Invalid node during parsing expressions parent {:?}", pr_ref.get_type()))
                }
            }
            Ok(1)
        },
        _ => Err("Invalid node during parsing expression".to_string())
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
