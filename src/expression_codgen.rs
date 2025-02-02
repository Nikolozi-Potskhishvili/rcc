use std::collections::{HashMap, HashSet};
use std::env::set_current_dir;
use std::ops::Deref;
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
                         String::from("r12"), String::from("r13"),
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
            let (w, s) = get_type_specifiers(size)?;
            let neg_offset = var.memory_offset < 0;
            load_var_in_reg(var.memory_offset, w, s, result_vec, neg_offset, reg_pool)
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
            let (res, cur_type) = generate_struct_address(current_node, var_table,  type_map, symbol_table, reg_pool, result_vec, cur_stack)?;
            let target_reg = reg_pool.allocate().ok_or("No registers available")?;
            let size = get_size(&cur_type, type_map, symbol_table)?;
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

fn load_var_in_reg(
    offset: i64,
    w: &str,
    s: &str,
    result_vec: &mut Vec<String>,
    is_neg: bool,
    reg_pool: &mut RegisterPool
) -> Result<Option<String>, String> {
    if let Some(reg) = reg_pool.allocate() {
        if s == "" {
            result_vec.push(format!("    mov {reg}, {w}[rbp - {offset}]"))
        } else if s == "d" {
            result_vec.push(format!("    movsxd {reg}, {w}[rbp - {offset}]"))
        } else {
            result_vec.push(format!("    movsx {reg}, {w}[rbp - {offset}]"))
        }
        Ok(Some(reg))
    } else {
        // if is_neg {
        //     result_vec.push(format!("    push {}[rbp + {}]", instruction, var.memory_offset.abs()));
        // }  else {
        //     result_vec.push(format!("    push {}[rbp - {}]", instruction, var.memory_offset));
        // }
        Ok(None)
    }
}

fn store_var_on_stack(
    offset: i64,
    w: &str,
    s: &str,
    result_vec: &mut Vec<String>,
    is_neg: bool,
    reg: &String,
)  {
    if s == "" {
        result_vec.push(format!("    mov {}[rbp - {}], {}", w, offset, reg));
    } else if s == "d" {
        result_vec.push(format!("    mov {}[rbp - {}], {}{s}", w, offset, reg));
    } else {
        result_vec.push(format!("    mov {}[rbp - {}], {}{s}", w, offset, reg));
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
            let var = var_table.get(name)
                .ok_or_else(|| format!("Variable '{}' not found", name))?;
            let target_reg = reg_pool.allocate().ok_or("No register available")?;
            // Generate code that loads the variable’s address from rbp,
            // taking care of whether the offset is negative or not.
            if var.memory_offset < 0 {
                result_vec.push(format!("    lea {}, [rbp + {}]", target_reg, var.memory_offset.abs()));
            } else {
                result_vec.push(format!("    lea {}, [rbp - {}]", target_reg, var.memory_offset));
            }
            Ok(Some(target_reg))
        },
        Expr::ArrayAccess(inner, _index) => {
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
            // Get the base address for the inner struct expression.
            let base_reg = get_address(inner, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)?
                .ok_or("Failed to get base address for struct access")?;
            // Get the struct definition for the inner expression
            let mut def = get_struct_def(inner, var_table, type_map, symbol_table)?.unwrap();
            // Obtain the offset for the desired field.
            let offset = def.get_field_offset(field);
            // Generate code that adds the field offset to the base address.
            result_vec.push(format!("    add {}, {}", base_reg, offset));
            // Return the register holding the address of the field.
            Ok(Some(base_reg))
        },
        Expr::UnaryExpr(un) => {
            if un.operator == Operator::Deref {
                get_address(&un.operand, var_table, result_vec, type_map, symbol_table, reg_pool, cur_stack)
            } else {
                Err(format!("Invalid expression type in get_address: {:?}", expression))
            }
        },
        _ => Err(format!("Invalid expression type in get_address: {:?}", expression)),
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
            let is_neg = var.memory_offset < 0;
            let right_reg = rhs_reg.unwrap();
            store_var_on_stack(var.memory_offset, instruction, suffix, result_vec, is_neg, &right_reg);
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
            let (reg_cur, upper_type) = generate_struct_address(left, var_table, type_map, symbol_table, reg_pool, result_vec, cur_stack)?;
            let size = get_size(&upper_type, type_map, symbol_table)?;
            let (w, s) = get_type_specifiers(size)?;
            if let Some(reg) = rhs_reg {
                result_vec.push(format!("    mov {w}[{}], {}{s}", reg_cur, reg));
                reg_pool.release(reg);
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
            let reg = reg_pool.allocate().unwrap();
            if pointer_var.register.is_some() {
                let reg = pointer_var.clone().register.unwrap().clone();
                let rhs = rhs_reg.unwrap();
                result_vec.push(format!("    mov {w}[{reg}], {rhs}{s}",));
                return Ok(None)
            }

            result_vec.push(format!("    mov {reg}, [rbp - {}]",  pointer_var.memory_offset));
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
fn get_struct_def(
    expr: &Expr,
    var_table: &HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, SymbolTableEntry>,
) -> Result<Option<StructDef>, String> {
    /// Resolves type through pointers, arrays, and type aliases
    fn resolve_type(mut ty: Type, type_map: &HashMap<String, Type>) -> Type {
        let mut visited_names = HashSet::new(); // Track visited type names

        loop {
            match &ty {
                Type::Pointer(inner) => ty = *inner.clone(),
                Type::Array(inner, _) => ty = *inner.clone(),
                Type::Struct(name) | Type::Primitive(name) => {
                    // Check for cyclic type definitions
                    if !visited_names.insert(name.clone()) {
                        break; // Break if we've already seen this type
                    }

                    if let Some(aliased) = type_map.get(name) {
                        ty = aliased.clone();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        ty
    }
    /// Recursively gets the base type of an expression
    fn get_base_type(
        expr: &Expr,
        var_table: &HashMap<String, Variable>,
        type_map: &HashMap<String, Type>,
    ) -> Result<Type, String> {
        match expr {
            Expr::VarUsage(name) => {
                let var = var_table.get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                Ok(resolve_type(var.var_type.clone(), type_map))
            }

            Expr::StructAccess(inner, field) => {
                let base_type = get_base_type(inner, var_table, type_map)?;
                if let Type::Struct(name) = base_type {
                    Ok(Type::Struct(name))  // Return parent struct type
                } else {
                    Err(format!("Cannot access field '{}' on non-struct type", field))
                }
            }

            Expr::ArrayAccess(inner, _) => {
                let base_type = get_base_type(inner, var_table, type_map)?;
                if let Type::Array(element_type, _) = base_type {
                    Ok(*element_type)
                } else {
                    Err("Array access on non-array type".into())
                }
            }

            Expr::UnaryExpr(unary) if unary.operator == Operator::Deref => {
                let base_type = get_base_type(&unary.operand, var_table, type_map)?;
                if let Type::Pointer(pointee_type) = base_type {
                    Ok(*pointee_type)
                } else {
                    Err("Dereferencing non-pointer type".into())
                }
            }

            _ => Err("Unsupported expression type for struct resolution".into()),
        }
    }

    // Main logic
    let base_type = get_base_type(expr, var_table, type_map)?;

    if let Type::Struct(struct_name) = base_type {
        symbol_table
            .get(&struct_name)
            .and_then(|entry| match entry {
                SymbolTableEntry::StructDef(def) => Some(def.clone()),
                _ => None,
            })
            .ok_or_else(|| format!("Undefined struct: {}", struct_name))
            .map(Some)
    } else {
        Ok(None)
    }
}
fn generate_struct_address(
    expr: &Expr,
    var_table: &HashMap<String, Variable>,
    type_map: &HashMap<String, Type>,
    symbol_table: &HashMap<String, SymbolTableEntry>,
    reg_pool: &mut RegisterPool,
    output: &mut Vec<String>,
    cur_stack: &mut i64
) -> Result<(String, Type), String> {
    fn inner_generator(
        expr: &Expr,
        var_table: &HashMap<String, Variable>,
        type_map: &HashMap<String, Type>,
        symbol_table: &HashMap<String, SymbolTableEntry>,
        reg_pool: &mut RegisterPool,
        output: &mut Vec<String>,
        cur_stack: &mut i64,
        base_offset: i64
    ) -> Result<(String, Type, i64), String> {
        match expr {
            Expr::StructAccess(inner, field_name) => {
                // Recursively process inner expression
                let (mut base_reg, mut inner_type, offset) = inner_generator(
                    inner,
                    var_table,
                    type_map,
                    symbol_table,
                    reg_pool,
                    output,
                    cur_stack,
                    base_offset
                )?;

                // Find field offset
                let struct_name = match inner_type {
                    Type::Struct(name) => name,
                    Type::Pointer(inner) => {
                        match inner.as_ref() {
                            Type::Struct(inner_name) => inner_name.clone(),
                            _ => return Err(format!("Inner inner {:?}", inner.clone())),

                        }
                    }
                    _ => return Err(format!("Inner {:?}", inner_type))
                };
                let struct_def = symbol_table.get(&struct_name).unwrap();
                let def = match struct_def {
                    SymbolTableEntry::StructDef(d) => d,
                    _ => return Err("dfdfsf".to_string()),
                };

                Ok((base_reg, def.clone().get_field_type(field_name).unwrap(), offset + def.clone().get_field_offset(field_name)))
            }
            Expr::UnaryExpr(UnaryExpr { operator, operand }) => {
                if !matches!(operator, Operator::Deref){
                    return Err("Only deref can be in struct".to_string())
                }
                let (mut base_reg, mut inner_type, offset) = inner_generator(
                    operand.as_ref(),
                    var_table,
                    type_map,
                    symbol_table,
                    reg_pool,
                    output,
                    cur_stack,
                    base_offset
                )?;
                if offset != 0 {
                    output.push(format!("    lea {base_reg}, [{base_reg} + {offset}]"));
                }
                let inner = match inner_type {
                    Type::Pointer(inner_access) => {
                        inner_access.clone()
                    }
                    _ => return Err("Invalid var type in deref in struct".to_string())
                };
                output.push(format!("    mov {base_reg}, [{base_reg}]"));
                Ok((base_reg, inner.deref().clone(), 0))
            },
            Expr::VarUsage(name) => {
                // Base case - get initial address
                let base_reg = reg_pool.allocate().ok_or("No registers available")?;
                let var = var_table.get(name).unwrap();

                output.push(format!("    lea {base_reg}, [rbp - {}]", var.memory_offset));
                Ok((base_reg, var.var_type.clone(), 0))
            },
            _ => return Err("ivalid expr in struct access chain".to_string())
        }
    }

    let (base_reg, upper_type,total_offset) = inner_generator(
        expr,
        var_table,
        type_map,
        symbol_table,
        reg_pool,
        output,
        cur_stack,
        0
    )?;

    if total_offset != 0 {
        output.push(format!("    lea {}, [{}{:+}]", base_reg, base_reg, total_offset));
    }

    Ok((base_reg, upper_type))
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
    let size = match  base_type {
        Some(t) => {
          get_size(&t, type_map, symbol_table)?
        }
        None => return Err(format!("invalid deref of: {:?}, operand: {:?},", base_type, operand))
    };
    let (w, s) = get_type_specifiers(size)?;
    let value_reg = reg_pool.allocate()
        .ok_or("No registers available for dereference")?;
    if s == "" {
        result_vec.push(format!("    mov {value_reg}, {w}[{addr_reg}]"))
    } else if s == "d" {
        result_vec.push(format!("    movsxd {value_reg}, {w}[{addr_reg}]"))
    } else {
        result_vec.push(format!("    movsx {value_reg}, {w}[{addr_reg}]"))
    }
    reg_pool.release(addr_reg);

    Ok(Some(value_reg))
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
        1 => Ok(("BYTE PTR ", "b")),
        2 => Ok(("WORD PTR ", "w")),
        4 => Ok(("DWORD PTR ", "d")),
        8 => Ok(("QWORD PTR ", "")),
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
