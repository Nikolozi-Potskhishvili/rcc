use std::cell::{RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::hash::Hash;
use std::ops::{Deref};
use std::rc::Rc;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::lexer::{Constant, Operator, StructDef, Type};
#[derive(Clone, Debug, PartialEq)]
struct Variable {
    var_type: Type,
    memory_offset: i64,
    register: Option<String>,
    defined: bool,
    pointer: bool,
}

enum VariableState {
    Undefined,
}

#[derive(Clone, Debug, PartialEq)]
enum Scope {
    Global,
    Static,
    Local,
    Argument,
}

pub fn generate_assembly(
    ast_root_nodes: &Vec<Rc<RefCell<Stmt>>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, StructDef>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut global_vars = HashMap::new();
    let mut functions = String::new();
    //Keys are labels and value is number which is written after
    let mut labels : HashMap<String, i32> = HashMap::new();
    let mut stack_ptr = 0;
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
                functions += &*"    push rbp\n    mov rbp, rsp\n".to_string();
                if let Stmt::Block(ref statements) = *body.borrow() {
                    functions += &gen_block_rec(statements, &mut global_vars, &mut labels, &mut stack_ptr, type_map, symbol_table)?;
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
fn gen_block_rec(
    statements: &Vec<Rc<RefCell<Stmt>>>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    labels: &mut HashMap<String, i32>,
    stack_ptr: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut local_stack = 0;
    let mut local_vars : Vec<String> = Vec::new();

    for stm in statements {
        match &*stm.borrow_mut() {
            Stmt::If { condition, then_branch, else_branch } => {
                let conditional_instructions = generate_if_else_instructions(condition, then_branch, else_branch,
                                                                             upper_scope_vars, labels, stack_ptr, type_map, symbol_table)?;
                result += conditional_instructions.as_str();
            },
            Stmt::While {condition, body} => {
                let while_loop_instructions = generate_while_instructions(condition, body, upper_scope_vars, labels, stack_ptr, type_map, symbol_table)?;
                result += &*while_loop_instructions;
            },
            Stmt::For {initialization, condition, increment, body} => {
                let for_loop_instructions = generate_for_loop_instructions(upper_scope_vars, initialization, condition, increment, body, labels, stack_ptr, type_map, symbol_table)?;
                result += &*for_loop_instructions;
            },
            Stmt::VarDecl { name, var_type, expr } => {
                let instructions = allocate_var_on_stack(name, var_type, upper_scope_vars, stack_ptr, &mut local_stack, type_map, symbol_table)?;
                local_vars.push(name.clone());
                result += &instructions;
                if expr.is_some() {
                    let instructions = store_var_on_stack(name, expr.as_ref().unwrap(), upper_scope_vars, type_map, symbol_table)?;
                    result += &instructions;
                    result += "\n";
                }
            },
            Stmt::VarAssignment { name, expr } => {
                if expr.is_none() {
                    return Err(format!("No expression is assigned to var: {}", name))
                }
                let instructions = store_var_on_stack(name, expr.as_ref().unwrap(), upper_scope_vars, type_map, symbol_table)?;
                result += &instructions;
            },

            Stmt::Return(expr_option) => {
                if expr_option.is_none() {
                    return Err("No expression after return".to_string())
                }
                let instructions = generate_return_instructions(expr_option.as_ref().unwrap(), upper_scope_vars, type_map, symbol_table)?;
                result += &instructions;
                result += &"    pop rax\n".to_string();
                result += &format!("    add rsp, {}\n", local_stack);
                result+= "    mov rsp, rbp\n    pop rbp\n";
                result += "    ret\n";
                *stack_ptr -= local_stack;
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

fn generate_for_loop_instructions(
    upper_scope_vars: &mut HashMap<String, Variable>,
    init: &Option<Rc<RefCell<Stmt>>>,
    condition: &Option<Expr>,
    increment: &Option<Expr>,
    body: &Option<Rc<RefCell<Stmt>>>,
    labels: &mut HashMap<String, i32>,
    stack_ptr: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {

    let last_for = labels.get("for_beg").copied().unwrap_or(0);
    labels.insert("for_beg".to_string(), last_for + 1);
    let last_end = labels.get("for_end").copied().unwrap_or(0);
    labels.insert("for_end".to_string(), last_end + 1);
    let last_increment = labels.get("for_inc").copied().unwrap_or(0);
    labels.insert("for_inc".to_string(), last_increment + 1);

    let mut result_vec = Vec::new();
    let mut init_vars = HashSet::new();
    // runs just one time
    if init.is_some() {
        let init_some = init.clone().unwrap();
        let init_statements = generate_for_loop_init_instructions(init_some, &mut init_vars, upper_scope_vars, stack_ptr, type_map, symbol_table)?;
        result_vec.push(init_statements);
    }
    result_vec.push(format!("for_beg{last_for}:"));
    // is checked every time
    if condition.is_some() {
        let condition_some = condition.clone().unwrap();
        generate_expression_instructions(&condition_some, upper_scope_vars, &mut result_vec, type_map, symbol_table)?;
        result_vec.push(pop_into_reg_instruction(8));
        result_vec.push("    test al, al".to_string());
        result_vec.push(format!("    jz for_end{last_end}"));
    }
    if body.is_some() {
        let body_clone = body.clone().unwrap();
        let body_deref = body_clone.borrow_mut();
        if let Stmt::Block(statements) = body_deref.clone() {
            let instructions = gen_block_rec(&statements, upper_scope_vars, labels, stack_ptr, type_map, symbol_table)?;
            result_vec.push(instructions);
        } else {
            return Err("Invalid statement in the for loop body".to_string());
        }
    }
    // runs after each time body ends
    //result_vec.push(format!("for_inc{last_increment}:"));
    if increment.is_some() {
        let increment_clone = increment.clone().unwrap();
        generate_expression_instructions(&increment_clone, upper_scope_vars, &mut result_vec, type_map, symbol_table)?;
    }
    result_vec.push(format!("    jmp for_beg{last_for}"));
    result_vec.push(format!("for_end{last_end}:"));
    result_vec.push(delete_out_of_scope_vars(&mut init_vars, upper_scope_vars, type_map, symbol_table)?);
    Ok(result_vec.join("\n") + "\n")
}

fn generate_for_loop_init_instructions(
    statement: Rc<RefCell<Stmt>>,
    addedVars: &mut HashSet<String>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    stack_ptr: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let statement_clone = statement.clone();
    let statement_borrow= statement_clone.borrow_mut();
    let statement_deref = statement_borrow.deref();
    match statement_deref {
        Stmt::VarDecl {name, var_type, expr, } => {
            let mut instructions = allocate_var_on_stack(name, var_type, upper_scope_vars, stack_ptr, &mut 0, type_map, symbol_table)?;
            let expr_unwrap = expr.clone().unwrap();
            let mut result_vec = Vec::new();
            let expr_instructions = generate_expression_instructions(&expr_unwrap ,upper_scope_vars, &mut result_vec, type_map, symbol_table);
            instructions += &*result_vec.join("\n");
            addedVars.insert(name.clone());
            Ok(instructions)
        },
        Stmt::VarAssignment {name, expr } => {
            if expr.is_some() {
                return Ok(store_var_on_stack(name, &expr.clone().unwrap(), upper_scope_vars, type_map, symbol_table)? + "\n")
            }
            Ok("".to_string())
        },
        _ => Err(format!("Invalid token in for loop int part: {:?}", statement_clone))
    }
}

fn delete_out_of_scope_vars(
    vars_to_delete: &HashSet<String>,
    scope_vars: &mut HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let mut stack_offset = 0;
    for var in vars_to_delete {
        let var_type = scope_vars.get(var).ok_or_else(|| format!("No for init var in scope vars: {}", var))?;
        stack_offset += get_size(&var_type.var_type, type_map, symbol_table)?;
        scope_vars.remove(var).unwrap();
    }
    Ok(format!("    add rsp, {stack_offset}"))
}

fn generate_while_instructions(
    condition: &Expr,
    body: &Rc<RefCell<Stmt>>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    labels: &mut HashMap<String, i32>,
    global_stack: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut result_vec = Vec::new();

    let last_while = labels.get("while_beg").copied().unwrap_or(0);
    labels.insert("while_beg".to_string(), last_while + 1);
    let last_end = labels.get("while_end").copied().unwrap_or(0);
    labels.insert("while_end".to_string(), last_end + 1);

    result += &*format!("while_beg{}:\n", last_while);
    generate_expression_instructions(condition, upper_scope_vars, &mut result_vec, type_map, symbol_table)?;
    result += &*result_vec.join("\n");
    result += "\n";
    result += "    test al, al\n";
    result += &*format!("    jz while_end{}\n", last_end);

    let body_binding = body.borrow_mut();
    let body_vec = match body_binding.deref() {
        Stmt::Block(vec) => vec,
        _ => return Err("Unexpected AST node during parsing while body".to_string())
    };
    let body_instructions = gen_block_rec(body_vec, upper_scope_vars, labels, global_stack, type_map, symbol_table)?;
    result += &*body_instructions;
    result += "\n";
    result += &*format!("    jmp while_beg{}\n", last_while);
    result += &*format!("while_end{}:\n", last_end);
    Ok(result)
}

fn generate_if_else_instructions(
    condition: &Expr,
    then_branch: &Rc<RefCell<Stmt>>,
    else_branch: &Option<Rc<RefCell<Stmt>>>,
    upper_scope_vars: &mut HashMap<String, Variable>,
    labels: &mut HashMap<String, i32>,
    global_stack: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut result_vec = Vec::new();
    generate_expression_instructions(condition, upper_scope_vars,&mut result_vec, type_map, symbol_table)?;
    result += &*result_vec.join("\n");
    result += "\n";

    let if_label_number = labels.get("if_label").copied().unwrap_or(0);
    labels.insert("if_label".to_string(), if_label_number + 1);
    let else_label_number = labels.get("else_label").copied().unwrap_or(0);
    labels.insert("else_label".to_string(), else_label_number + 1);
    let end_label_number = labels.get("end_label").copied().unwrap_or(0);
    labels.insert("end_label".to_string(), end_label_number + 1);
    result += &*pop_into_reg_instruction(8);
    result += format!("\n    cmp al, 1\n    je if_label{}\n", if_label_number).as_str();
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
    let then_instructions = gen_block_rec(&then_block, upper_scope_vars, labels, global_stack, type_map, symbol_table)?;
    result += &*then_instructions;
    result += format!("\n    jmp end_label{}\n", end_label_number).as_str();;

    if let Some(else_branch_deref) = else_branch.as_ref() {
        result += format!("else_label{}:\n", else_label_number).as_str();
        match &*else_branch_deref.borrow_mut() {
            Stmt::If { condition, then_branch, else_branch } => {
                result += &*generate_if_else_instructions(condition, then_branch, else_branch, upper_scope_vars, labels, global_stack, type_map, symbol_table)?;
            }
            Stmt::Block(statements) => {
                result += &*gen_block_rec(statements, upper_scope_vars, labels, global_stack, type_map, symbol_table)?;
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
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<(String), String> {
    let mut instruction_vec = Vec::new();
    let reg= generate_expression_instructions(&expression_root, var_table, &mut instruction_vec, type_map, symbol_table)?;
    Ok(instruction_vec.join("\n") + "\n")
}
fn allocate_var_on_stack(
    var_name: &String,
    var_type: &Type,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64,
    local_stack_size: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {
    let var = Variable {
        var_type: var_type.clone(),
        memory_offset: *cur_stack_size + get_size(var_type, type_map, symbol_table)?,
        register: None,
        defined: false,
        pointer: false,
    };
    var_table.insert(var_name.clone(), var);
    *local_stack_size += 8;
    *cur_stack_size += 8;
    let instruction = "    sub rsp, 8\n".to_string();
    Ok(instruction)
}

fn store_var_on_stack(
    var_name: &String,
    expr: &Expr,
    var_table: &mut HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<String, String> {

    if !var_table.contains_key((var_name)) {
        return Err(format!("Assigment without declaring a variable, {:?}", var_name))
    }
    let var = var_table.get(var_name).unwrap().clone();

    let mut instruction_vec = Vec::new();
    let mem_offset = generate_expression_instructions(expr, var_table, &mut instruction_vec, type_map, symbol_table)?;
    Ok(instruction_vec.join("\n"))
}
///
///  Binary operations use R8 for storing result of left side of expression and R9 for right side
///  Unary operations use R8 for storing result
///
///
fn generate_expression_instructions(
    expression_root: &Expr,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<(), String> {
    let current_node = expression_root;
    return match current_node {
        Expr::BinaryExpr(BinaryExpression{left, right, operator })=> {
            Ok(generate_binary_institution(left, right, operator.clone(), var_table, result_vec, type_map, symbol_table)?)
        },
        Expr::UnaryExpr(UnaryExpr{ operator, operand }) => {
            Ok(generate_unary_instructions(operand, operator.clone(), var_table, result_vec, type_map, symbol_table)?)
        },
        Expr::Const(Constant::Integer(value)) => {
            result_vec.push(format!("    mov r8, {}", *value));
            result_vec.push(push_from_reg_on_stack(8));
            Ok(())
        },
        Expr::VarUsage(var_name) => {
            let memory_offset_op = var_table.get(var_name);
            if memory_offset_op.is_none() {
                return Err(format!("Variable {}, was not defined", var_name))
            }
            let memory_offset = memory_offset_op.unwrap().memory_offset;
            result_vec.push(format!("    mov r8, [rbp - {}]", memory_offset));
            result_vec.push(push_from_reg_on_stack(8));
            Ok(())
        },
        _ => Err("Invalid node type during expression codgen".to_string())
    }
}

/// Generates asm instructions for binary expression. returns i32 which represent stack of offset where value is stored
///
///
fn generate_binary_institution(
    left: &Expr,
    right: &Expr,
    operator: Operator,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<(), String> {

    let right_offset= generate_expression_instructions(right, var_table, result_vec, type_map, symbol_table)?;
    let load_right = pop_into_reg_instruction(9);

    if  let Operator::Assign = operator {
        // expect l value on left
        let l_val = match left {
            Expr::VarUsage(name) => name,
            _ => return Err("No l value before assignment operator".to_string())
            };
        if !var_table.contains_key(l_val) {
            return Err(format!("No declared var as:{l_val}").to_string())
        }
        let var = var_table.get(l_val).unwrap();
        let var_offset = var.memory_offset;
        result_vec.push(load_right);
        result_vec.push(format!("    mov [rbp - {var_offset}], r9"));
        return Ok(())
    }

    let left_offset = generate_expression_instructions(left, var_table, result_vec, type_map, symbol_table)?;
    let load_left = pop_into_reg_instruction(8);
    result_vec.push(load_left);
    result_vec.push(load_right);

    let instruction = get_instruction_by_operator(&operator)?;
    if is_logical_operator(&operator) {
        result_vec.push("    cmp r8, r9".to_string());
        let comp_res : &str = match operator {
            Operator::And => "",
            Operator::Or => "",
            Operator::Less => "    setl al",
            Operator::More => "    setg al",
            _ => return Err(format!("Unexpected operator: {:?}, during codgen of logical op", operator))
        };
        result_vec.push(comp_res.to_string());
    } else {
        result_vec.push(format!("    {} r8, r9", instruction));
    }
    //result_vec.push("    movzx r8, al".to_string());
    result_vec.push(push_from_reg_on_stack(8));
    Ok(())
}

fn generate_unary_instructions(
    operand: &Expr,
    operator: Operator,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<(), String> {
    match operator {
        Operator::Deref => {
            return handle_deref(operand, var_table, result_vec, type_map, symbol_table)
        }
        Operator::Ref => {
            return if let Expr::VarUsage(var_name) = operand {
                if var_table.get(var_name).is_none() {
                    return Err("Trying to get ref to var which is not declared".to_string())
                }
                let var = var_table.get(var_name).unwrap();
                let offset = var.memory_offset;
                result_vec.push(format!("    lea r8, [rbp - {offset}]"));
                result_vec.push(push_from_reg_on_stack(8));
                Ok(())
            } else {
                Err("Operand of deref is not lvalue".to_string())
            }
        }
        _ => {}
    }
    let mem_offset = generate_expression_instructions(operand, var_table, result_vec, type_map, symbol_table)?;
    let load_temp_res = pop_into_reg_instruction(8);
    result_vec.push(load_temp_res);
    let instruction = match operator {
        Operator::Not => {
            result_vec.push("    test r8, r8".to_string());
            result_vec.push("    setz al".to_string());
            result_vec.push("    movzx r8, al".to_string());
            result_vec.push(push_from_reg_on_stack(8));
            return Ok(());
        },
        Operator::Tilde => "not",
        Operator::Minus => "neg",
        _ => return Err("Unsupported operator".to_string())
    };
    result_vec.push(format!("    {} r8", instruction));
    result_vec.push(push_from_reg_on_stack(8));
    Ok(())
}

fn handle_deref(
    operand: &Expr,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, StructDef>,
) -> Result<(), String> {
    match operand {
        // Expr::BinaryExpr(expression) => {}
        // Expr::UnaryExpr(expression) => {}
        Expr::VarUsage(name) => {
            let var = var_table.get(name);
            if var.is_none() {
                return Err(format!("No such var as: {name}"))
            }
            let var_unwrap = var.unwrap();
            if let Type::Pointer(some) = &var_unwrap.var_type {
                result_vec.push(format!("    mov r8, [rbp - {}]", var_unwrap.memory_offset));
                result_vec.push("    mov r8, [r8]".to_string());
                result_vec.push("    push r8".to_string())
            } else {
                return Err("Var used after deref is not pointer type".to_string())
            }
        }
        Expr::Function_Call { .. } => {}
        _ => return Err(format!("Invalid expression {:?} during dereference", operand).to_string())
    }
    Ok(())
}

enum _StorageLocation {
    // register number
    Register(i32),
    //memory offset
    Memory(i32),
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
        Operator::Or | Operator::And | Operator::Less | Operator::More => true,
        _ => false
   }
}

fn get_size(cur_type: &Type, type_map: &HashMap<String, Type>, symbol_table: &HashMap<String, StructDef>) -> Result<i64, String> {
    match cur_type {
        Type::Primitive(name)=> match name.as_str() {
            "int" => Ok(4),
            _ => return Err(format!("No such type as: {name}")),
        },
        Type::Pointer(..) => Ok(8),
        Type::Array(arr_type, size) => Ok(*size as i64),
        _ => return Err(format!("Unknown type as {:?}", cur_type)),
    }
}


/// Generates Instructions for loading var from mem to register
///
fn pop_into_reg_instruction(reg: i32) -> String {
    format!("    pop r{reg}").to_string()
}

fn push_from_reg_on_stack(reg: i32) -> String {
    format!("    push r{reg}")
}