use std::cell::{RefCell};
use std::collections::{HashMap, HashSet};
use std::env::args;
use std::fmt::format;
use std::hash::Hash;
use std::ops::{Deref};
use std::rc::Rc;
use lazy_static::lazy_static;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::expression_codgen::generate_expression_instructions;
use crate::lexer::{Constant, Operator, StructDef, SymbolTableEntry, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub(crate) var_type: Type,
    pub(crate) memory_offset: i64,
    register: Option<String>,
    defined: bool,
    pointer: bool,
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
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
        let reg = generate_expression_instructions(&condition_some, upper_scope_vars, &mut result_vec, type_map, symbol_table)?;
        if reg.is_none() {
            result_vec.push(String::from("    pop rax"));
            result_vec.push(format!("    test rax, rax"));
        } else {
            let reg_str = reg.unwrap();
            result_vec.push(format!("    test {reg_str}, {reg_str}"));
        }
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut result_vec = Vec::new();

    let last_while = labels.get("while_beg").copied().unwrap_or(0);
    labels.insert("while_beg".to_string(), last_while + 1);
    let last_end = labels.get("while_end").copied().unwrap_or(0);
    labels.insert("while_end".to_string(), last_end + 1);

    result += &*format!("while_beg{}:\n", last_while);
    let reg = generate_expression_instructions(condition, upper_scope_vars, &mut result_vec, type_map, symbol_table)?;
    result += &*result_vec.join("\n");
    result += "\n";
    if reg.is_none() {
        result += "    pop rax\n";
        result += "    test rax, rax\n";
    } else {
        let reg_unwrap = reg.unwrap();
        result += format!("    test {reg_unwrap}, {reg_unwrap}\n").as_str();
    }
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<String, String> {
    let mut result = String::new();
    let mut result_vec = Vec::new();
    let if_label_number = labels.get("if_label").copied().unwrap_or(0);
    labels.insert("if_label".to_string(), if_label_number + 1);
    let else_label_number = labels.get("else_label").copied().unwrap_or(0);
    labels.insert("else_label".to_string(), else_label_number + 1);
    let end_label_number = labels.get("end_label").copied().unwrap_or(0);
    labels.insert("end_label".to_string(), end_label_number + 1);
    let reg = generate_expression_instructions(condition, upper_scope_vars,&mut result_vec, type_map, symbol_table)?;
    result += &*result_vec.join("\n");
    result += "\n";
    if reg.is_none() {
        result += "    pop rax\n";
        result += "    test rax, rax\n";
        result += format!("\n    cmp rax, 1\n    je if_label{}\n", if_label_number).as_str();
    } else {
        let reg_unwrap = reg.unwrap();
        result += format!("\n    cmp {reg_unwrap}, 1\n    je if_label{}\n", if_label_number).as_str();
    }
    //result += &*pop_into_reg_instruction(8);
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
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<(String), String> {
    let mut instruction_vec = Vec::new();
    let reg= generate_expression_instructions(&expression_root, var_table, &mut instruction_vec, type_map, symbol_table)?;
    if reg.is_none() {
        instruction_vec.push(String::from("    pop rax"));
    } else {
        let reg_unwrap = reg.unwrap();
        if has_suffix(&reg_unwrap) {
            if get_suffix_char(&reg_unwrap) == Some('d') {
                instruction_vec.push(format!("    mov eax, {}", reg_unwrap));
            } else {
                instruction_vec.push(format!("    movzx rax, {}", reg_unwrap));
            }
        } else {
            instruction_vec.push(format!("    mov rax, {}", reg_unwrap));
        }
    }
    Ok(instruction_vec.join("\n") + "\n")
}
fn allocate_var_on_stack(
    var_name: &String,
    var_type: &Type,
    var_table: &mut HashMap<String, Variable>,
    cur_stack_size: &mut i64,
    local_stack_size: &mut i64,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<String, String> {

    let size = get_size(var_type, type_map, symbol_table)?;
    let var = Variable {
        var_type: var_type.clone(),
        memory_offset: *cur_stack_size + size,
        register: None,
        defined: false,
        pointer: false,
    };
    var_table.insert(var_name.clone(), var);
    *local_stack_size += size;
    *cur_stack_size += size;
    let instruction = format!("    sub rsp, {size}\n");
    Ok(instruction)
}

fn store_var_on_stack(
    var_name: &String,
    expr: &Expr,
    var_table: &mut HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<String, String> {

    if !var_table.contains_key((var_name)) {
        return Err(format!("Assigment without declaring a variable, {:?}", var_name))
    }
    let var = var_table.get(var_name).unwrap().clone();
    let mut instruction_vec = Vec::new();
    generate_expression_instructions(expr, var_table, &mut instruction_vec, type_map, symbol_table)?;
    Ok(instruction_vec.join("\n") + "\n")
}


pub fn get_size(cur_type: &Type, type_map: &HashMap<String, Type>, symbol_table: &mut HashMap<String, SymbolTableEntry>) -> Result<i64, String> {
    match cur_type {
        Type::Primitive(name)=> match name.as_str() {
            "int" => Ok(4),
            "short" => Ok(2),
            "long" => Ok(8),
            "bool" => Ok(1),
            "char" => Ok(1),
            _ => return Err(format!("No such type as: {name}")),
        },
        Type::Pointer(..) => Ok(8),
        Type::Array(arr_type, size) => {
            get_array_size(cur_type, type_map, symbol_table)
        },
        _ => return Err(format!("Unknown type as {:?}", cur_type)),
    }
}


/// Generates Instructions for loading var from mem to register
///
pub fn pop_into_reg_instruction(reg: i32) -> String {
    format!("    pop r{reg}").to_string()
}

pub fn push_from_reg_on_stack(reg: i32) -> String {
    format!("    push r{reg}")
}

pub fn get_type_instruction(size: i64 ) -> Result<String, String> {
    Ok(match size {
        1 => "byte ",
        2 => "word ",
        4 => "dword ",
        8 => "",
        _ => return Err(format!("size {size} is illigal"))
    }.to_string())
}

pub fn get_register_suffix(size: i64) -> Result<String, String> {
    Ok(match size {
        1 => "b",
        2 => "w",
        4 => "d",
        8 => "",
        _ => return Err(format!("size {size} is illigal")),
    }.to_string())
}
pub fn get_array_dimensions(array_type: &Type) -> Vec<i64> {
    let mut tem_type = array_type;
    let mut res = Vec::new();
    while let Type::Array(inner, size) = tem_type {
        res.push(*size);
        tem_type = inner;
    }

    res
}

pub fn get_array_size(array: &Type, type_map: &HashMap<String, Type>, symbol_table: &mut HashMap<String, SymbolTableEntry>) -> Result<i64, String> {
    let mut accum = 0;
    let mut temp = array;
    while let Type::Array(inner, size) = temp {
        if accum == 0 {
            accum = *size;
        } else {
            accum *= size;
        }

        temp = inner
    }
    match temp {
        Type::Primitive(_) | Type::Struct(_) => accum *= get_size(temp, type_map, symbol_table)?,
        Type::Pointer(ptr) => accum *= 8,
        _ => panic!("Illegal type inside array:{:?}", temp)
    }
    Ok(accum)
}

pub fn get_array_underlying_size(array: &Type, type_map: &HashMap<String, Type>, symbol_table: &mut HashMap<String, SymbolTableEntry>) -> Result<i64, String> {
    let mut temp = array;
    while let Type::Array(inner, size) = temp {
        temp = inner
    }
    match temp {
        Type::Primitive(_) | Type::Struct(_) => Ok(get_size(temp, type_map, symbol_table)?),
        Type::Pointer(ptr) => Ok(8),
        _ => panic!("Illegal type inside array:{:?}", temp)
    }
}

pub fn get_array_access_depth(expression: &Expr) -> Result<i64,String> {
    if let Expr::ArrayAccess(_, _) = expression {
        let mut temp = expression.clone();
        let mut depth = 0;
        while let Expr::ArrayAccess(inner, _) = temp {
            match inner.as_ref() {
                Expr::VarUsage(_) => return Ok(depth),
                Expr::ArrayAccess(new_inner, _) => {
                    depth += 1;
                    temp = *new_inner.clone()
                },
                _ => return Err(String::from("Non array access type expression in get depth")),
            }
        }
        Err(String::from("Unexpected end of cycle in depth calculation"))
    }  else {
        return Err(String::from("Non array access type expression in get depth"))
    }
}

pub fn get_array_access_type(var_type: &Type, depth: i64, max_depth: i64) -> Type {
    let mut temp = var_type;
    let mut cur_depth = 0;
    while let Type::Array(inner, _) = temp {
        if cur_depth == depth {
            break
        }
        if cur_depth >= max_depth {
            break;
        }
        cur_depth += 1;
        temp = inner;
    }

    temp.clone()
}

pub fn has_suffix(reg: &String) -> bool {
    if let Some(last_char) = reg.chars().last() {
        if matches!(last_char, 'b' | 'w' | 'd') {
            return true;
        }
    }
    false
}

pub fn get_suffix_char(reg: &String) -> Option<char> {
    if let Some(last_char) = reg.chars().last() {
        if matches!(last_char, 'b' | 'w' | 'd') {
            return Some(last_char)
        }

    }
    None
}