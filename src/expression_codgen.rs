use std::collections::HashMap;
use crate::ast_types::{BinaryExpression, Expr, UnaryExpr};
use crate::codegen::{get_array_access_depth, get_array_dimensions, get_array_underlying_size, get_size, get_suffix_char, has_suffix, Variable};
use crate::lexer::{Constant, Operator, SymbolTableEntry, Type};

///
///  Binary operations use R8 for storing result of left side of expression and R9 for right side
///  Unary operations use R8 for storing result
///
///
pub fn generate_expression_instructions(
    expression_root: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Option<String>, String> {
    let free_regs = vec![String::from("r8"), String::from("r9"), String::from("r10"), String::from("r11")];
    let mut reg_pool = RegisterPool::new(&*free_regs);
    generate_expression(
        expression_root,
        var_table,
        result_vec,
        type_map,
        symbol_table,
        &mut reg_pool,
    )
}

fn generate_expression(
    expression_root: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    let current_node = expression_root;
    return match current_node {
        Expr::BinaryExpr(BinaryExpression{left, right, operator })=> {
            Ok(generate_binary_instruction(left, right, operator.clone(), var_table, result_vec, type_map, symbol_table, reg_pool)?)
        },
        Expr::UnaryExpr(UnaryExpr{ operator, operand }) => {
            Ok(generate_unary_expression(operand, operator.clone(), var_table, result_vec, type_map, symbol_table, reg_pool)?)
        },
        Expr::Const(Constant::Long(value)) => {
            if let Some(reg) = reg_pool.allocate() {
                result_vec.push(format!("    mov {}, {}", reg, *value));
                Ok(Some(reg))
            } else {
                result_vec.push(format!("    push {}", *value));
                Ok(None)
            }
        },
        Expr::VarUsage(var_name) => {
            let var_op = var_table.get(var_name);
            if var_op.is_none() {
                return Err(format!("Variable {}, was not defined", var_name))
            }
            let var = var_op.unwrap();
            let memory_offset = var.memory_offset;
            let size = get_size(&var.var_type, type_map, symbol_table)?;

            let instruction = crate::codegen::get_type_instruction(size)?;
            let suffix = crate::codegen::get_register_suffix(size)?;

            if let Some(reg) = reg_pool.allocate() {
                result_vec.push(format!("    mov {}{}, {}[rbp - {}]", reg, suffix, instruction, var.memory_offset));
                Ok(Some(reg + suffix.as_str()))
            } else {
                result_vec.push(format!("    push {}[rbp - {}]", instruction, var.memory_offset));
                Ok(None)
            }
        },
        Expr::ArrayAccess(inner, ident) => {
            let array_name  = get_array_name_from_access_expr(&current_node)?;
            let array_type = &var_table.get(&array_name).unwrap().var_type;
            let underlying_size = get_array_underlying_size(&array_type, type_map, symbol_table)?;
            let dimensions = get_array_dimensions(array_type);
            //generate ident instructions
            let addr_reg = generate_array_access_instructions(
                current_node,
                &mut 0,
                dimensions.len() as i64,
                underlying_size,
                var_table,
                result_vec,
                type_map,
                symbol_table,
                reg_pool,
            )?.ok_or("Array access failed")?;

            let (instr, suffix) = get_type_specifiers(underlying_size)?;
            let value_reg = reg_pool.allocate()
                .ok_or("No registers available for array element")?;

            result_vec.push(format!("    mov {}{}, {}[{}]",
                                    value_reg, suffix, instr, addr_reg
            ));
            reg_pool.release(addr_reg);

            Ok(Some(value_reg))
        }
        _ => Err("Invalid node type during expression codgen".to_string())
    }
}



fn get_array_name_from_access_expr(expression: &Expr) -> Result<String, String> {
    let mut temp_node = expression;
    while let Expr::ArrayAccess(inner, _) = temp_node {
        temp_node = inner;
    }
    match temp_node {
        Expr::VarUsage(name) => Ok(String::from(name)),
        _ => return Err("".to_string())
    }
}

fn generate_array_access_instructions(
    expression: &Expr,
    cur_depth: &mut i64,
    depth: i64,
    underlying_size: i64,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    match expression {
        Expr::VarUsage(name) => {
            let var = var_table.get(name)
                .ok_or_else(|| format!("Array {} not declared", name))?;

            // Allocate register for base address
            let base_reg = reg_pool.allocate()
                .ok_or("No registers available for array base")?;

            result_vec.push(format!("    lea {}, [rbp - {}]", base_reg, var.memory_offset));
            Ok(Some(base_reg))
        }
        Expr::ArrayAccess(inner, index_expr) => {
            *cur_depth += 1;

            // Recursively get base address
            let mut base_reg = generate_array_access_instructions(
                inner, cur_depth, depth, underlying_size,
                var_table, result_vec, type_map, symbol_table, reg_pool
            )?.ok_or("Missing base address")?;
            println!("{:?} and {:?}", inner, index_expr);
            // Evaluate index to register
            let index_reg = generate_expression(
                index_expr, var_table, result_vec,
                type_map, symbol_table, reg_pool
            )?.ok_or("Missing index value")?;

            // Calculate element size (8 for pointers except last dimension)
            let elem_size = if *cur_depth == depth {
                underlying_size
            } else {
                8  // Assuming pointer size for non-terminal dimensions
            };

            // Scale index and update base address
            result_vec.push(format!("    imul {}, {}", index_reg, elem_size));
            result_vec.push(format!("    add {}, {}", base_reg, index_reg));

            // Release index register immediately
            reg_pool.release(index_reg);

            Ok(Some(base_reg))  // Reuse base register for new address
        }
        _ => Err(format!("Invalid array access expression: {:?}", expression)),
    }
}
/// Generates asm instructions for binary expression. returns i32 which represent stack of offset where value is stored
///
///
fn generate_binary_instruction (
    left: &Expr,
    right: &Expr,
    operator: Operator,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool
) -> Result<Option<String>, String> {

    if  let Operator::Assign = operator {
        return handle_assignment(left, right, var_table, result_vec, type_map, symbol_table, reg_pool)
    }

    let mut target_reg = reg_pool.allocate().ok_or("No registers available for binary operation")?;
    let left_reg = generate_expression(left, var_table, result_vec, type_map, symbol_table, reg_pool)?;
    let right_reg = generate_expression(right, var_table, result_vec, type_map, symbol_table, reg_pool)?;

    // Move values to target registers if needed
    if let Some(left_reg) = left_reg {
        if has_suffix(&left_reg) {
            if get_suffix_char(&left_reg) == Some('d') {
                result_vec.push(format!("    mov {}d, {}", &target_reg, left_reg));
                //target_reg.push('d');
            } else {
                result_vec.push(format!("    movzx {}, {}", &target_reg, left_reg));
            }
        } else {
            result_vec.push(format!("    mov {}, {}", &target_reg, left_reg));
        }
        reg_pool.release(left_reg);
    } else {
        result_vec.push(format!("    pop {}", &target_reg));
    }

    let temp_reg = right_reg.unwrap_or_else(|| {
        let reg = reg_pool.allocate().expect("No registers available");
        result_vec.push(format!("    pop {}", reg));
        reg
    });

    if is_logical_operator(&operator) {
        handle_logical_operation(&target_reg, &temp_reg, operator, result_vec)?;
    } else {
        let instruction = get_instruction_by_operator(&operator)?;
        result_vec.push(format!("    {} {}, {}", instruction, &target_reg, &temp_reg));
    }

    reg_pool.release(temp_reg);
    Ok(Some(target_reg.clone()))
}

fn handle_logical_operation(target_reg: &String, temp_reg: &String, operator: Operator, result_vec: &mut Vec<String>) -> Result<(), String> {
    result_vec.push(format!("    cmp {}, {}", target_reg, temp_reg));

    let set_instr = match operator {
        Operator::Less => "setl",
        Operator::More => "setg",
        Operator::LessEqual => "setle",
        Operator::MoreEqual => "setge",
        Operator::Equal => "sete",
        Operator::NotEqual => "setne",
        _ => return Err(format!("Unsupported logical operator: {:?}", operator)),
    };

    result_vec.push(format!("    {} al", set_instr));
    result_vec.push(format!("    movzx {}, al", target_reg));
    Ok(())
}

fn handle_assignment(
    left: &Expr,
    right: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    let rhs_reg = generate_expression(right, var_table, result_vec, type_map, symbol_table, reg_pool)?;
    match left {
        Expr::VarUsage(name) => {
            let var = var_table.get(name).ok_or_else(|| format!("Undefined variable: {}", name))?;
            let size = get_size(&var.var_type, type_map, symbol_table)?;
            let (instruction, suffix) = get_type_specifiers(size)?;

            if let Some(reg) = rhs_reg {
                result_vec.push(format!("    mov {}[rbp - {}], {}{}", instruction, var.memory_offset, reg, suffix));
                reg_pool.release(reg);
            } else {
                result_vec.push(format!("    pop {}[rbp - {}]", instruction, var.memory_offset));
            }
        },
        Expr::ArrayAccess(var_expr, indexing_expr) => {
            let mut size ;
            let array_name = get_array_name_from_access_expr(&left)?;
            let array_var = var_table.get(&array_name);
            if array_var.is_none() {
                return Err(format!("No arrray var as {array_name} defined"))
            }
            let array_type = array_var.unwrap().var_type.clone();
            let underlying_size =get_array_underlying_size(&array_type, type_map, symbol_table)?;
            let dimensions = get_array_dimensions(&array_type);
            let depth = get_array_access_depth(&left)?;
            if depth + 1 == dimensions.len() as i64 {
                size = underlying_size;
            } else {
                size = 8;
            }
            let (instruction, suffix) = get_type_specifiers(size)?;
            let addr_reg = generate_array_access_instructions(
                left, &mut 0, dimensions.len() as i64, underlying_size, var_table, result_vec, type_map, symbol_table, reg_pool)?;
            if addr_reg.is_none() {

            } else {

            }
            let addr_reg_str = addr_reg.unwrap();
            if let  Some(reg) = rhs_reg {
                result_vec.push(format!("    mov {}[{}], {}{}", instruction, addr_reg_str, reg, suffix));
                reg_pool.release(reg);
            } else {
                let temp_reg = reg_pool.allocate()
                    .ok_or("No available registers for temporary storage")?;
                result_vec.push(format!("    pop {}", temp_reg));  // Pop value into temporary register
                result_vec.push(format!("    mov {}[{}], {}", instruction, addr_reg_str, temp_reg));
            }
            reg_pool.release(addr_reg_str.clone());
        },
        _ => return Err("Invalid left-hand side in assignment".to_string())
    };

    Ok(None)
}

fn generate_unary_expression(
    operand: &Expr,
    operator: Operator,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    match operator {
        Operator::Deref => handle_dereference(operand, var_table, result_vec, type_map, symbol_table, reg_pool),
        Operator::Ref => handle_address_of(operand, var_table, result_vec, type_map, symbol_table, reg_pool),
        _ => handle_arithmetic_unary(operand, operator, var_table, result_vec, type_map, symbol_table, reg_pool)
    }
}

fn handle_address_of(
    operand: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    let var_name = match operand {
        Expr::VarUsage(name) => name,
        _ => return Err("Address-of operator requires lvalue".to_string())
    };

    let var = var_table.get(var_name)
        .ok_or_else(|| format!("Undefined variable: {}", var_name))?;

    let target_reg = reg_pool.allocate()
        .ok_or("No registers available for address operation")?;

    result_vec.push(format!("    lea {}, [rbp - {}]", target_reg, var.memory_offset));
    Ok(Some(target_reg))
}

fn handle_dereference(
    operand: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    let addr_reg = generate_expression(operand, var_table, result_vec, type_map, symbol_table, reg_pool)?
        .ok_or("Dereference requires loaded address")?;

    let (size, instruction, suffix) = match get_pointer_base_type(operand, var_table, type_map, symbol_table)? {
        Some(t) => {
            let size = get_size(&t, type_map, symbol_table)?;
            let instr = crate::codegen::get_type_instruction(size)?;
            let suf = crate::codegen::get_register_suffix(size)?;
            (size, instr, suf)
        }
        None => return Err("Dereferencing non-pointer type".to_string())
    };

    let value_reg = reg_pool.allocate()
        .ok_or("No registers available for dereference")?;

    result_vec.push(format!("    mov {}{}, {}[{}]", value_reg, suffix, instruction, addr_reg));
    reg_pool.release(addr_reg);

    Ok(Some(value_reg + suffix.as_str()))
}

fn handle_arithmetic_unary(
    operand: &Expr,
    operator: Operator,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
) -> Result<Option<String>, String> {
    let operand_reg = generate_expression(operand, var_table, result_vec, type_map, symbol_table, reg_pool)?
        .ok_or("Need register for unary operation")?;

    match operator {
        Operator::Not => {
            result_vec.push(format!("    test {}, {}", operand_reg, operand_reg));
            result_vec.push("    setz al".to_string());
            result_vec.push(format!("    movzx {}, al", operand_reg));
        }
        Operator::Tilde => result_vec.push(format!("    not {}", operand_reg)),
        Operator::Minus => result_vec.push(format!("    neg {}", operand_reg)),
        _ => return Err(format!("Unsupported unary operator: {:?}", operator))
    }

    Ok(Some(operand_reg))
}

fn get_pointer_base_type(
    expr: &Expr,
    var_table: &HashMap<String, crate::codegen::Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, SymbolTableEntry>,
) -> Result<Option<Type>, String> {
    match expr {
        Expr::VarUsage(name) => {
            let var = var_table.get(name)
                .ok_or_else(|| format!("Undefined variable: {}", name))?;

            if let Type::Pointer(inner) = &var.var_type {
                Ok(Some(*inner.clone()))
            } else {
                Ok(None)
            }
        }
        Expr::UnaryExpr(UnaryExpr { operator: Operator::Ref, operand }) => {
            get_expression_type(operand, var_table, type_map, symbol_table)
        }
        _ => Err("Complex pointer expressions not supported".to_string())
    }
}

fn get_expression_type(
    expr: &Box<Expr>,
    var_table: &HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, SymbolTableEntry>
) -> Result<Option<Type>, String> {
    match expr.as_ref() {
        Expr::VarUsage(name) => {
            let var = var_table.get(name);
            if var.is_none() {
                return Err(format!("Var {name} is not defined"));
            }
            Ok(Some(var.unwrap().var_type.clone()))
        }
        //Expr::ArrayAccess(_, _) => {}
        _ => Err(format!("{:?} is not pointer type.", expr))
    }
}

/// Tracks register state
#[derive(Clone)]
struct RegisterPool {
    available: Vec<String>,
}

impl RegisterPool {
    fn new(free_regs: &[String]) -> Self {
        RegisterPool {
            available: free_regs.to_vec(),
        }
    }

    fn allocate(&mut self) -> Option<String> {
        self.available.pop()
    }

    fn release(&mut self, mut reg: String) {
        if has_suffix(&reg) {
            reg.pop();
        }
        self.available.push(reg);
    }
}


fn get_instruction_by_operator(operator: &Operator) -> Result<&str, String> {
    return match operator {
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

fn get_type_specifiers(size: i64) -> Result<(&'static str, &'static str), String> {
    match size {
        1 => Ok(("byte ", "b")),
        2 => Ok(("word ", "w")),
        4 => Ok(("dword ", "d")),
        8 => Ok(("qword ptr ", "")),
        _ => Err(format!("Unsupported data size: {}", size)),
    }
}
