use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::env::var;
use std::fmt::format;
use std::ops::Deref;
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
    let mut global_vars = HashMap::new();
    let mut functions = String::new();
    //Keys are labels and value is number which is written after
    let mut labels : HashMap<String, i32> = HashMap::new();

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
                    functions += &gen_block_rec(statements, &mut global_vars, &mut labels)?;
                }
            },
            Stmt::VarDecl { name, var_type, expr } => {},
            _ => return Err("Unsupported root token".to_string())
        }
    }
    //result += &global_vars;
    result += &functions;
    Ok(result)
}

/// Generates Assembly instructions for given vector of Ast statements and vars declared in upper scope.
///
/// Returns Result of String of x86_64 instructions tabulated by 4 spaces and separated by \n, or String error.
fn gen_block_rec(statements: &Vec<Rc<RefCell<Stmt>>>, upper_scope_vars: &mut HashMap<String, Variable>, labels: &mut HashMap<String, i32>) -> Result<String, String> {
    let mut result = String::new();
    let mut stack_size = 0;
    let mut local_vars : Vec<String> = Vec::new();
    for stm in statements {
        match &*stm.borrow_mut() {
            Stmt::If { condition, then_branch, else_branch } => {
                let conditional_instructions = generate_if_else_instructions(condition, then_branch, else_branch, upper_scope_vars, labels)?;
                result += conditional_instructions.as_str();
            },
            Stmt::While {condition, body} => {
                let while_loop_instructions = generate_while_instructions(condition, body, upper_scope_vars, labels)?;
                result += &*while_loop_instructions;
            },
            Stmt::VarDecl { name, var_type, expr } => {
                let instructions = allocate_int_on_stack(name, var_type, upper_scope_vars, &mut stack_size)?;
                local_vars.push(name.clone());
                result += &instructions;
                result += "\n";
                if expr.is_some() {
                    let instructions = store_int_on_stack(name, expr.as_ref().unwrap(), upper_scope_vars, &mut stack_size)?;
                    result += &instructions;
                    result += "\n";
                }
            },
            Stmt::VarAssignment { name, expr } => {
                if expr.is_none() {
                    return Err(format!("No expression is assigned to var: {}", name))
                }
                let instructions = store_int_on_stack(name, expr.as_ref().unwrap(), upper_scope_vars, &mut stack_size)?;
                result += &instructions;
            },

            Stmt::Return(expr_option) => {
                if expr_option.is_none() {
                    return Err("No expression after return".to_string())
                }
                let (instructions, reg) = generate_return_instructions(expr_option.as_ref().unwrap(), upper_scope_vars)?;
                result += &instructions;
                result += "\n";
                result += &format!("    mov rax, r{}\n", reg);
                result += &format!("    add rsp, {}\n", stack_size);
                result += "    ret\n";
            }
            _ => {}
        }
    }
    // delete local vars when scope ends
    for var in local_vars {
        if upper_scope_vars.get(&var).is_none() {
            return Err(format!("Error during clearing local Vars vec, {} var does not exist in global var vec", var))
        }
        upper_scope_vars.remove(&var);
    }
    Ok(result)
}

fn generate_while_instructions(
    condition: &Expr,
    body: &Rc<RefCell<Stmt>>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    labels: &mut HashMap<String, i32>) -> Result<String, String> {
    let mut result = String::new();
    let mut result_vec = Vec::new();
    let mut registers = vec![8, 9, 10];

    let last_while = labels.get("while_label").copied().unwrap_or(0);
    labels.insert("while_label".to_string(), last_while + 1);
    let last_end = labels.get("end_label").copied().unwrap_or(0);
    labels.insert("end_label".to_string(), last_end + 1);

    result += &*format!("while_label{}:\n", last_while);
    generate_expression_instructions(condition, upper_scope_vars, &mut result_vec, &mut registers)?;
    result += &*result_vec.join("\n");
    result += "\n";
    result += "    test al, al\n";
    result += &*format!("    jz end_label{}\n", last_end);

    let body_binding = body.borrow_mut();
    let body_vec = match body_binding.deref() {
        Stmt::Block(vec) => vec,
        _ => return Err("Unexpected AST node during parsing while body".to_string())
    };
    let body_instructions = gen_block_rec(body_vec, upper_scope_vars, labels)?;
    result += &*body_instructions;
    result += "\n";
    result += &*format!("    jmp while_label{}\n", last_while);
    result += &*format!("end_label{}:\n", last_end);
    Ok(result)
}

fn generate_if_else_instructions(
    condition: &Expr,
    then_branch: &Rc<RefCell<Stmt>>,
    else_branch: &Option<Rc<RefCell<Stmt>>>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    labels: &mut HashMap<String, i32>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut free_registers = vec![8, 9, 10];
    let mut result_vec = Vec::new();
    generate_expression_instructions(condition, upper_scope_vars,&mut result_vec, &mut free_registers)?;
    result += &*result_vec.join("\n");
    result += "\n";

    let if_label_number = labels.get("if_label").copied().unwrap_or(0);
    labels.insert("if_label".to_string(), if_label_number + 1);
    let else_label_number = labels.get("else_label").copied().unwrap_or(0);
    labels.insert("else_label".to_string(), else_label_number + 1);
    let end_label_number = labels.get("end_label").copied().unwrap_or(0);
    labels.insert("end_label".to_string(), end_label_number + 1);
    result += format!("    cmp al, 1\n    je if_label{}\n", if_label_number).as_str();
    if else_branch.is_some() {
        result += format!("    jmp else_label{}\n", else_label_number).as_str();
    }

    let then_block = match *then_branch.borrow() {
        Stmt::Block(ref vec) => {
            vec.clone()
        }
        _ => return Err(format!("Unexpected token {:?}, during generating code for if block", then_branch))
    };
    result += format!("if_label{}:\n", if_label_number).as_str();
    let then_instructions = gen_block_rec(&then_block, upper_scope_vars, labels)?;
    result += &*then_instructions;
    result += format!("\n    jmp end_label{}\n", end_label_number).as_str();;

    if let Some(else_branch_deref) = else_branch.as_ref() {
        result += format!("else_label{}:\n", else_label_number).as_str();
        match &*else_branch_deref.borrow_mut() {
            Stmt::If { condition, then_branch, else_branch } => {
                result += &*generate_if_else_instructions(condition, then_branch, else_branch, upper_scope_vars, labels)?;
            }
            Stmt::Block(statements) => {
                result += &*gen_block_rec(statements, upper_scope_vars, labels)?;
            }
            _ => return Err(format!("Unexpected statement in place of else branch: {:?}", else_branch))
        };
    }
    result += format!("\nend_label{}:\n", end_label_number).as_str();
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
        return Err(format!("Assigment without declaring a variable, {:?}", var_name))
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
        Expr::BinaryExpr(BinaryExpression{left, right, operator })=> {
            let left_reg = generate_expression_instructions(left, var_table, result_vec, free_registers)?;
            let right_reg = generate_expression_instructions(right, var_table, result_vec, free_registers)?;
            let instruction = get_instruction_by_operator(&operator)?;
            if is_logical_operator(&operator) {
                result_vec.push(format!("    cmp r{}, r{}", left_reg, right_reg));
                let comp_res : &str = match operator {
                    Operator::Assign => "",
                    Operator::And => "",
                    Operator::Or => "",
                    Operator::Less => "    setl al",
                    Operator::More => "    setg al",
                    _ => return Err(format!("Unexpected operator: {:?}, during codgen of logical op", operator))
                };
                result_vec.push(comp_res.to_string());
                free_registers.push(right_reg);
            } else {
                result_vec.push(format!("    {} r{}, r{}", instruction, left_reg, right_reg));
                free_registers.push(right_reg);
            }
            Ok(left_reg)
        },
        Expr::UnaryExpr(UnaryExpr{ operator, operand }) => {
            let register = generate_expression_instructions(operand, var_table, result_vec, free_registers)?;

            let instruction = match operator {
                Operator::Not => {
                    result_vec.push(format!("    test r{}, r{}", register, register));
                    result_vec.push("    setz al".to_string());
                    result_vec.push(format!("    movzx r{}, al", register));
                    return Ok(register);
                },
                Operator::Tilde => "not",
                Operator::Minus => "neg",
                _ => return Err("Unsupported operator".to_string())
            };
            println!("Yo We are generating fucking unary operation: {}", instruction);
            result_vec.push(format!("    {} r{}", instruction, register));
            Ok(register)
        },
        Expr::Const(Constant::Integer(value)) => {
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

fn get_instruction_by_operator(operator: &Operator) -> Result<&str, String> {
    return match operator {
        Operator::Assign => Ok(""),
        Operator::More => Ok(""),
        Operator::Less => Ok(""),
        Operator::Plus => Ok("add"),
        Operator::Minus => Ok("sub"),
        Operator::Division => Ok("div"),
        Operator::Multiplication => Ok("imul"),
        _ => return Err("Unsupported operator".to_string())
    }
}

fn is_logical_operator(operator: &Operator) -> bool {
   return match operator {
       Operator::Assign | Operator::Or | Operator::And | Operator::Less | Operator::More => true,
        _ => false
   }
}

/// Generates instructions for logical expressions: And, Or, Less than, More than
///
///
fn generate_logical_instruction(
    expression_root: &Expr,
    var_table: &HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    free_registers: &mut Vec<i32>,
) -> Result<i32, String> {
    match expression_root {
        Expr::BinaryExpr(expr) => {
            let (left, right, operator) = (&expr.right, &expr.left, &expr.operator);
            match operator {
                Operator::Assign => {}
                Operator::And => {}
                Operator::Or => {

                }
                Operator::Less => {
                    let left_reg = generate_expression_instructions(left.as_ref(), var_table, result_vec, free_registers)?;
                    let right_reg = generate_expression_instructions(right.as_ref(), var_table, result_vec, free_registers)?;
                    result_vec.push(format!("    cmp r{}, r{}", left_reg, right_reg));
                    free_registers.push(right_reg);
                    result_vec.push(format!("   setl al"));
                }
                Operator::More => {
                    let left_reg = generate_expression_instructions(left.as_ref(), var_table, result_vec, free_registers)?;
                    let right_reg = generate_expression_instructions(right.as_ref(), var_table, result_vec, free_registers)?;
                    result_vec.push(format!("    cmp r{}, r{}", left_reg, right_reg));
                    free_registers.push(right_reg);
                    //result_vec.push(format!("   setle al"));
                }
                _=> return Err(format!("Unexpected operator {:?} during paring of logical expression root", operator))
            }
        }
        _ => return Err(format!("{:?} Unexpected expression Node during codgen for logical instructions", expression_root))

    }
    return Ok(0)
}