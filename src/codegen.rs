use std::cell::{RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::lexer::{Constant, Operator, Token, Type};

struct Variable {
    var_type: Type,
    memory_offset: i64,
    register: Option<String>,
}


pub fn generate_assembly(ast_root_nodes: &Vec<Rc<RefCell<Stmt>>>) -> Result<String, String> {
    let mut result = String::new();
    let mut global_vars = String::new();
    let mut functions = String::new();

    for child in ast_root_nodes {
        match & *child.borrow() {
            Stmt::FnDecl { name, return_type, args, body } => {
                let mut assembly_fn_name = format!("_{}", name);
                if name == "main" {
                    assembly_fn_name = "main".to_string();
                    functions += ".intel_syntax noprefix\n";
                    functions += ".section .text\n";
                    functions += &format!(".global {}\n", assembly_fn_name);
                }
                functions += &format!("{}:\n", assembly_fn_name);
                if let Stmt::Block(ref statements) = *body.borrow() {
                    functions += &gen_block_rec(statements)?;
                }
            },
            Stmt::VarDecl { name, var_type, expr } => {},
            _ => return Err("Unsupported root token".to_string())
        }
    }
    result += &global_vars;
    result += &functions;
    Ok(result)
}

fn gen_block_rec(statements: &Vec<Rc<RefCell<Stmt>>>) -> Result<String, String> {
    let mut result = String::new();
    let mut stack_size = 0;
    let mut var_table = HashMap::new();

    for stm in statements {
        match &mut *stm.borrow_mut() {
            Stmt::VarDecl { name, var_type, expr } => {
                let instructions = allocate_int_on_stack(name, var_type, &mut var_table, &mut stack_size)?;
                result += &instructions;
                result += "\n";
                if expr.is_some() {
                    let instructions = store_int_on_stack(name, expr.as_ref().unwrap(), &mut var_table, &mut stack_size)?;
                    result += &instructions;
                    result += "\n";
                }
            }

            Stmt::VarAssignment { name, expr } => {
                if expr.is_none() {
                    return Err(format!("No expression is assigned to var: {}", name))
                }
                let instructions = store_int_on_stack(name, expr.as_ref().unwrap(), &mut var_table, &mut stack_size)?;
                result += &instructions;
            },

            Stmt::Return(expr_option) => {
                if expr_option.is_none() {
                    return Err("No expression after return".to_string())
                }
                let (instructions, reg) = generate_return_instructions(expr_option.as_ref().unwrap(), &mut var_table)?;
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
    expression_root: &Expr,
    var_table: &mut HashMap<String, Variable>,
) -> Result<(String, i32), String> {
    let mut instruction_vec = Vec::new();
    let mut free_registers = vec![8, 9, 10];
    let reg= generate_expression_instructions(&expression_root, var_table, &mut instruction_vec, &mut free_registers)?;
    Ok((instruction_vec.join("\n"), reg))
}
fn allocate_int_on_stack(
    var_name: &String,
    var_type: &String,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64
) -> Result<String, String> {
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
    var_name: &String,
    expr: &Expr,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64,
) -> Result<String, String> {

    if !var_table.contains_key((var_name)) {
        return Err("Assigment without declaring a variable".to_string());
    }
    let var = var_table.get(var_name).unwrap();

    let mut instruction_vec = Vec::new();
    let mut free_registers = vec![8, 9, 10];
    let reg = generate_expression_instructions(expr, var_table, &mut instruction_vec, &mut free_registers)?;
    instruction_vec.push(format!("    mov [rsp + {}], r{}", var.memory_offset, reg));
    Ok(instruction_vec.join("\n"))
}
///
///  Binary operations use R8 for storing result of left side of expression and R9 for right side
///  Unary operations use R8 for storing result
///
///
fn generate_expression_instructions(
    expression_root: &Expr,
    var_table: &HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    free_registers: &mut Vec<i32>,
) -> Result<i32, String> {
    let current_node = expression_root;
    return match current_node {
        Expr::BinaryExpr(BinaryExpression{ left, right, operator })=> {
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
        Expr::UnaryExpr(UnaryExpr{ operator, operand }) => {
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
        Expr::Const(Constant::Integer(value)) => {
            /*if value == null() {
                return Err("undefined constant".to_string());
            }*/
            let reg = free_registers.pop().unwrap();
            result_vec.push(format!("    mov r{}, 0", reg));
            result_vec.push(format!("    add r{}, {}", reg, *value));
            Ok(reg)
        },
        Expr::VarUsage(var_name) => {
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


/*fn simplify_expression(expression_root: Expr) -> Result<i32, String> {
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
*/