use std::collections::HashMap;
use std::iter;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::codegen::{get_args_total_size, get_array_access_depth, get_array_dimensions, get_array_underlying_size, get_size, get_suffix_char, has_suffix, Variable};
use crate::lexer::{Constant, FunDef, Operator, StructDef, StructField, SymbolTableEntry, Type};

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
    cur_stack: &mut i64,
) -> Result<Option<String>, String> {
    let free_regs = vec![String::from("r8"), String::from("r9"), String::from("r10"), String::from("r11"),
    ];
    let mut reg_pool = RegisterPool::new(&*free_regs);
    let mut stack = *cur_stack;
    let res = generate_expression(
        expression_root,
        var_table,
        result_vec,
        type_map,
        symbol_table,
        &mut reg_pool,
        &mut stack,
    );
    if stack != *cur_stack {
        return Err(String::from("Bad stack manipulation occured during expression codegen"))
    }
    res
}

fn generate_expression(
    expression_root: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64,
) -> Result<Option<String>, String> {
    let current_node = expression_root;
    return match current_node {
        Expr::BinaryExpr(BinaryExpression{left, right, operator })=> {
            Ok(generate_binary_instruction(left, right, operator.clone(), var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?)
        },
        Expr::UnaryExpr(UnaryExpr{ operator, operand }) => {
            Ok(generate_unary_expression(operand, operator.clone(), var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?)
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
            if var.register.is_some() {
                return Ok(var.register.clone());
            }
            let instruction = crate::codegen::get_type_instruction(size)?;
            let suffix = crate::codegen::get_register_suffix(size)?;
            let neg_offset = var.memory_offset < 0;
            if let Some(reg) = reg_pool.allocate() {
                if neg_offset {
                    result_vec.push(format!("    mov {}{}, {}[rbp + {}]", reg, suffix, instruction, var.memory_offset.abs()));
                }  else {
                    result_vec.push(format!("    mov {}{}, {}[rbp - {}]", reg, suffix, instruction, var.memory_offset));
                }
                Ok(Some(reg + suffix.as_str()))
            } else {
                if neg_offset {
                    result_vec.push(format!("    push {}[rbp + {}]", instruction, var.memory_offset.abs()));
                }  else {
                    result_vec.push(format!("    push {}[rbp - {}]", instruction, var.memory_offset));
                }
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
                cur_stack
            )?.ok_or("Array access failed")?;

            let (instr, suffix) = get_type_specifiers(underlying_size)?;
            let value_reg = reg_pool.allocate()
                .ok_or("No registers available for array element")?;

            result_vec.push(format!("    mov {}{}, {}[{}]",
                                    value_reg, suffix, instr, addr_reg
            ));
            reg_pool.release(addr_reg);

            Ok(Some(value_reg))
        },
        Expr::StructAccess(inner, field) => {
            let res = handle_struct_access(current_node, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack).unwrap().ok_or("")?;
            let target_reg = reg_pool.allocate().ok_or("No registers available")?;
            let mut struct_def = get_struct_def(inner, var_table, type_map, symbol_table)?;
            let field_offset =  struct_def.get_field_offset(field);
            let size = get_size(&struct_def.get_field_type(field).unwrap(), type_map, symbol_table)?;
            let (w, s) = get_type_specifiers(size)?;
            result_vec.push(format!("    mov {target_reg}{s}, {w}[{res}]"));
            Ok(Some(target_reg))
        },
        Expr::Function_Call { name, args } => {
            handle_fn_call(name, args, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)
        }
        _ => Err("Invalid node type during expression codgen".to_string())
    }
}


fn handle_fn_call(
    name: &String,
    args: &Vec<Expr>,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64  // current additional bytes allocated on the stack
) -> Result<Option<String>, String> {
    // Retrieve the function definition from the symbol table.
    let entry = symbol_table.get(name)
        .ok_or_else(|| format!("Function {} not found", name))?;
    let (fun_args, fun_type) = if let SymbolTableEntry::FunDef(def) = entry {
        // We assume def.args is an Option<Vec<Stmt>> where each statement is a FnParam.
        (def.args.clone().unwrap_or_else(|| vec![]), def.funType.clone())
    } else {
        return Err(format!("{} is not a function", name));
    };

    // For simplicity, assume every argument is passed on the stack as 8 bytes.
    let num_args = fun_args.len() as i64;
    let total_args_size = num_args * 8; // each push uses 8 bytes

    // Compute padding needed so that BEFORE the call, rsp is 16-byte aligned.
    // When call _fun is executed, the call instruction will push an 8-byte return address.
    // We want: (cur_stack + total_args_size + 8) % 16 == 0.
    // let mut padding = 0;
    // if (*cur_stack + total_args_size + 8) % 16 != 0 {
    //     padding = 16 - ((*cur_stack + total_args_size + 8) % 16);
    // }
    //
    // // Reserve space for the padding.
    // if padding > 0 {
    //     result_vec.push(format!("    sub rsp, {}", padding));
    //     *cur_stack += padding;
    // }

    // Evaluate and push the arguments in reverse order.
    // This way, after pushing, the first argument is closest to the return address.
    // (That is, inside the callee after "push rbp; mov rbp, rsp", the first argument will be at [rbp+16].)
    let param_regs = vec!["rdi", "rsi", "rdx", "rcx"];
    for (index, expr) in args.iter().enumerate() {
        let mut arg_reg = generate_expression(expr, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?
            .ok_or_else(|| format!("Failed to generate argument for expression :{:?}", expr))?;
        let suffix = get_suffix_char(&arg_reg);
        if suffix.is_some() {
            arg_reg.pop();
            result_vec.push(format!("    movsx {arg_reg}, {arg_reg}{}", suffix.unwrap()))
        }
        let res_reg = param_regs.get(index).unwrap();
        result_vec.push(format!("    mov {res_reg}, {}", arg_reg));
        //*cur_stack += 8; // each push decrements rsp by 8 bytes
    }

    // Call the function. The call will push the return address (8 bytes).
    result_vec.push(format!("    call _{}", name));

    // After the call, the pushed arguments (and any padding) remain on the stack.
    // Remove them by adding the total size back to rsp.
    // let total_to_remove = total_args_size + padding;
    // if total_to_remove > 0 {
    //     result_vec.push(format!("    add rsp, {}", total_to_remove));
    //     *cur_stack -= total_to_remove;
    // }
    if let Type::Void =  fun_type {
        return Ok(None)
    }
    // Retrieve the return value from rax.
    if let Some(ret_reg) = reg_pool.allocate() {
        result_vec.push(format!("    mov {}, rax", ret_reg));
        Ok(Some(ret_reg))
    } else {
        // If no register is available, simply pop rax.
        result_vec.push("    pop rax".to_string());
        Ok(None)
    }
}
fn handle_struct_access(
    expression: &Expr,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64
) -> Result<Option<String>, String> {
    if let Expr::StructAccess(inner, field) = expression {
        // calculate inner offset
        let base = get_address(inner, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?
            .ok_or("Failed to generate struct base address")?;
        // get offset of the field
        let target_reg = reg_pool.allocate().ok_or("No registers available")?;
        let mut struct_def = get_struct_def(inner, var_table, type_map, symbol_table)?;
        let field_offset =  struct_def.get_field_offset(field);
        result_vec.push(format!("    lea {}, [{} + {}]", target_reg, base, field_offset));
        reg_pool.release(base);
        Ok(Some(target_reg))
    } else {
        Err("Not a struct access expression".to_string())
    }
}

fn get_address(
    expression: &Expr,
    var_table: &mut HashMap<String, Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64,
) -> Result<Option<String>, String> {
    match expression {
        Expr::VarUsage(name) => {
            let var = var_table.get(name).unwrap();
            let target_reg = reg_pool.allocate().ok_or("")?;
            let is_neg = var.memory_offset < 0;
            if is_neg {
                result_vec.push(format!("    lea {target_reg}, [rbp + {}]", var.memory_offset.abs()));
            } else {
                result_vec.push(format!("    lea {target_reg}, [rbp - {}]", var.memory_offset));
            }
            Ok(Some(target_reg))
        },
        Expr::ArrayAccess(inner, index) => {
            let array_name  = get_array_name_from_access_expr(&expression)?;
            let array_type = &var_table.get(&array_name).unwrap().var_type;
            let underlying_size = get_array_underlying_size(&array_type, type_map, symbol_table)?;
            let dimensions = get_array_dimensions(array_type);
            let addr_reg = generate_array_access_instructions(
                expression,
                &mut 0,
                dimensions.len() as i64,
                underlying_size,
                var_table,
                result_vec,
                type_map,
                symbol_table,
                reg_pool,
                cur_stack,
            )?.ok_or("Array access failed")?;

            Ok(Some(addr_reg))
        },
        Expr::StructAccess(inner, field) => {
            let inner_reg = get_address(inner, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?.ok_or("")?;
            let mut def = get_struct_def(expression, var_table, type_map, symbol_table)?;
            let offset = def.get_field_offset(field);
            result_vec.push(format!("    add {inner_reg}, {offset}"));
            return Ok(Some(inner_reg))
        },
        Expr::UnaryExpr(un) => {
            if un.operator == Operator::Deref {
                get_address(&un.operand.clone(), var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)
            } else {
                return Err(format!("Invalid expression type: {:?}, in codgen of address", expression))
            }
        }
        _ => return Err(format!("Invalid expression type: {:?}, in codgen of address", expression))
    }
}

fn get_struct_def(
    expression: &Expr,
    var_table: &mut HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<StructDef, String> {
    match expression {
        Expr::VarUsage(name) => {
            let var = var_table.get(name)
                .ok_or_else(|| format!("Variable '{}' not found", name))?;
            let struct_def = get_struct_def_by_type(&var.var_type.clone(), var_table, type_map, symbol_table)?;
            Ok(struct_def)
        }
        // Expr::ArrayAccess(inner, _) => {
        //     let inner_type = get_struct_def(inner, var_table, type_map, symbol_table)?;
        //     if let Type::Array(of_type, _) = inner_type {
        //         Ok(*of_type)
        //     } else {
        //         Err("Array access on non-array type".to_string())
        //     }
        // }
        Expr::StructAccess(inner, field) => {
            let parent_def = get_struct_def(inner, var_table, type_map, symbol_table)?;
            let struct_name = parent_def.name;
                let symbol_table_entry = symbol_table.get(&struct_name)
                    .ok_or_else(|| format!("Struct '{}' not defined", struct_name))?;
                let def = match symbol_table_entry {
                    SymbolTableEntry::StructDef(def_str) => {
                        def_str.clone()
                    }
                    _ => return Err("lol".to_string())
                };
                let struct_fields = def.fields.clone();
                struct_fields.iter()
                        .find(|(cur_field)| &cur_field.name == field)
                        .map(|(cur_field)| def.clone())
                        .ok_or_else(|| format!("Field '{}' not found in struct '{}'", field, struct_name))
        }
        _ => Err("Invalid expression type for struct access".to_string())
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
    cur_stack: &mut i64
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
                var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack
            )?.ok_or("Missing base address")?;
            println!("{:?} and {:?}", inner, index_expr);
            // Evaluate index to register
            let index_reg = generate_expression(
                index_expr, var_table, result_vec,
                type_map, symbol_table, reg_pool, cur_stack
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
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64
) -> Result<Option<String>, String> {

    if  let Operator::Assign = operator {
        return handle_assignment(left, right, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)
    }

    let mut target_reg = reg_pool.allocate().ok_or("No registers available for binary operation")?;
    let left_reg = generate_expression(left, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?;
    let right_reg = generate_expression(right, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?;

    let mut left_suf = String::from("");
    // Move values to target registers if needed
    if let Some(left_reg) = left_reg {
        if has_suffix(&left_reg) {
            if get_suffix_char(&left_reg) == Some('d') {
                target_reg.push('d');
                left_suf.push('d');
                result_vec.push(format!("    mov {}, {}", &target_reg, left_reg));
                //target_reg.push('d');
            } else {

                left_suf.push(get_suffix_char(&left_reg).unwrap());
                result_vec.push(format!("    movzx {}, {}", &target_reg, left_reg));
                target_reg.push(get_suffix_char(&left_reg).unwrap());
            }
        } else {
            result_vec.push(format!("    mov {}, {}", &target_reg, left_reg));
        }
        reg_pool.release(left_reg);
    } else {
        result_vec.push(format!("    pop {}", &target_reg));
    }

    let mut temp_reg = right_reg.unwrap_or_else(|| {
        let reg = reg_pool.allocate().expect("No registers available");
        result_vec.push(format!("    pop {}", reg));
        reg
    });
    if is_arg_register(&*temp_reg) && left_suf != "" {
        temp_reg = get_subregister(&temp_reg, &*left_suf).unwrap().parse().unwrap();
    } else if !has_suffix(&temp_reg) {
        temp_reg += &left_suf;
    }
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
    cur_stack: &mut i64
) -> Result<Option<String>, String> {
    let rhs_reg = generate_expression(right, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?;
    match left {
        Expr::VarUsage(name) => {
            let var = var_table.get(name).ok_or_else(|| format!("Undefined variable: {}", name))?;
            let size = get_size(&var.var_type, type_map, symbol_table)?;
            let (instruction, suffix) = get_type_specifiers(size)?;
            if var.register.is_some() {
                return Ok(var.register.clone());
            }
            if let Some(reg) = rhs_reg {
                result_vec.push(format!("    mov {}[rbp - {}], {}{suffix}", instruction, var.memory_offset, reg));
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
                left, &mut 0, dimensions.len() as i64, underlying_size, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?;
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
        Expr::StructAccess(inner, field) => {
            let address = handle_struct_access(left, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?.unwrap();
            let mut def = get_struct_def(left, var_table, type_map, symbol_table)?;
            let field_type = def.get_field_type(field).unwrap();
            let size = get_size(&field_type, type_map, symbol_table)?;
            let (w, s) = get_type_specifiers(size)?;
            if let Some(reg) = rhs_reg {
                result_vec.push(format!("    mov {w}[{}], {}{s}", address, reg));
                reg_pool.release(reg);
            } else {
            }

        }
        // handle *x = ...
        Expr::UnaryExpr(un) => {
            //let reg = get_address(&left,var_table, result_vec, type_map, symbol_table,reg_pool, cur_stack)?.unwrap();
            let var_name = match un.operand.as_ref() {
                Expr::VarUsage(name) => name,
                _ => return Err("Cannot deref anything except one level pointer".to_string())
            };
            let pointer_var = var_table.get(var_name).unwrap();
            let cur_type = get_pointer_base_type(&un.operand.clone(), var_table, type_map, symbol_table)?.unwrap();
            let size = get_size(&cur_type, type_map, symbol_table)?;
            let (w, s) = get_type_specifiers(size)?;
            let is_neg = pointer_var.memory_offset < 0;
            let reg = reg_pool.allocate().unwrap();
            if pointer_var.register.is_some() {
                let reg = pointer_var.clone().register.unwrap().clone();
                let rhs = rhs_reg.unwrap();
                result_vec.push(format!("    mov {w}[{reg}], {rhs}",));
                return Ok(None)
            }
            if is_neg {
                result_vec.push(format!("    mov {reg}, [rbp + {}]",  pointer_var.memory_offset.abs()));
            } else {
                result_vec.push(format!("    mov {reg}, [rbp - {}]",  pointer_var.memory_offset));
            }
            let mut rhs = rhs_reg.unwrap();
            if !has_suffix(&rhs) {
                rhs += s;
            }
            result_vec.push(format!("    mov {w}[{reg}], {}", rhs))
        }
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
    cur_stack: &mut i64,
) -> Result<Option<String>, String> {
    match operator {
        Operator::Deref => handle_dereference(operand, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack),
        Operator::Ref => handle_address_of(operand, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack),
        _ => handle_arithmetic_unary(operand, operator, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)
    }
}

fn handle_address_of(
    operand: &Expr,
    var_table: &mut HashMap<String, crate::codegen::Variable>,
    result_vec: &mut Vec<String>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    cur_stack: &mut i64,
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
    cur_stack: &mut i64
) -> Result<Option<String>, String> {
    let addr_reg = generate_expression(operand, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?
        .ok_or("Dereference requires loaded address")?;
    let base_type = get_pointer_base_type(operand, var_table, type_map, symbol_table)?;
    let (size, instruction, suffix) = match  base_type {
        Some(t) => {
            let size = get_size(&t, type_map, symbol_table)?;
            let instr = crate::codegen::get_type_instruction(size)?;
            let suf = crate::codegen::get_register_suffix(size)?;
            (size, instr, suf)
        }
        None => return Err(format!("invalid deref of: {:?}, operand: {:?},", base_type, operand))
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
    cur_stack: &mut i64
) -> Result<Option<String>, String> {
    let operand_reg = generate_expression(operand, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?
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
        _ => Err(format!("Complex pointer expressions not supported, {:?}", expr))
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

fn get_struct_def_by_type(
    cur_type: &Type,
    var_table: &mut HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<StructDef, String> {
    return match cur_type {
        // Type::Pointer(_) => {
        //
        // }
        // Type::Array(_, _) => {
        //
        // }
        Type::Struct(name) => {
            let st_entry = symbol_table.get(name).unwrap();
            if let SymbolTableEntry::StructDef(def) = st_entry {
                Ok(def.clone())
            } else {
                Err(format!("Expected struct type, but got: {:?}", cur_type))
            }
        }
        _ => Err(format!("Expected struct type, but got: {:?}", cur_type)),
    }
}

fn get_subregister(register: &str, suffix: &str) -> Option<&'static str> {
    match (register, suffix) {
        ("rdi", "b") => Some("dil"),  // Lower 8 bits of rdi
        ("rdi", "w") => Some("di"),   // Lower 16 bits of rdi
        ("rdi", "d") => Some("edi"),  // Lower 32 bits of rdi
        ("rdi", "r") => Some("rdi"),  // Full 64 bits of rdi

        ("rsi", "b") => Some("sil"),  // Lower 8 bits of rsi
        ("rsi", "w") => Some("si"),   // Lower 16 bits of rsi
        ("rsi", "d") => Some("esi"),  // Lower 32 bits of rsi
        ("rsi", "r") => Some("rsi"),  // Full 64 bits of rsi

        ("rdx", "b") => Some("dl"),   // Lower 8 bits of rdx
        ("rdx", "w") => Some("dx"),   // Lower 16 bits of rdx
        ("rdx", "d") => Some("edx"),  // Lower 32 bits of rdx
        ("rdx", "r") => Some("rdx"),  // Full 64 bits of rdx

        ("rcx", "b") => Some("cl"),   // Lower 8 bits of rcx
        ("rcx", "w") => Some("cx"),   // Lower 16 bits of rcx
        ("rcx", "d") => Some("ecx"),  // Lower 32 bits of rcx
        ("rcx", "r") => Some("rcx"),  // Full 64 bits of rcx

        _ => None, // Return None if the combination doesn't match
    }
}
fn is_arg_register(register: &str) -> bool {
    match register {
        "rdi" | "rsi" | "rdx" | "rcx"  => true,
        _ => false,
    }
}
