use std::cell::{ RefCell};
use std::collections::HashMap;
use std::fmt::{Pointer};
use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;
use crate::ast_types::{BinaryExpression, Expr, Stmt, UnaryExpr};
use crate::ast_types::Stmt::{Block, For, VarAssignment};
use crate::codegen::get_size;
use crate::expression_parser::ExpressionParser;
use crate::lexer::{Operator, SpecialCharacter, Keyword, Token, Constant, Type, SymbolTableEntry, StructDef, StructField};
use crate::lexer::SpecialCharacter::{LeftCurlyBracket, RightCurlyBracket};
use crate::lexer::SymbolTableEntry::FunDef;


// current grammar:
//Expression    ::= Assignment
//Assignment ::= LogicalOr
//LogicalOr        ::= LogicalAnd ( '|' LogicalAnd )*
//LogicalAnd       ::= Relational ( '&' Relational )*
//Relational       ::= Additive ( ('<' | '>' | '<=' | '>=') Additive )*
//Additive      ::= Multiplicative ( '+' | '-' Multiplicative )*
//Multiplicative ::= Unary ( ('*' | '/') Unary )*
// Unary(Prefix)         ::= ('~' | '!') Unary | Primary
// Postfix       ::= Primary ( '++' | '--' | '[' Expression ']')*
// Primary       ::= NUMBER | Var

fn parse_expression(
    tokens: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<Expr, String> {
    let mut extracted_tokens: Vec<Token> = Vec::new();
    let mut open_paren_count = 0;
    for token in tokens {
        match token {
            Token::Identifier(_) | Token::Constant(_) | Token::Operator(_)
                | Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket)
            | Token::SpecialCharacter(SpecialCharacter::RightSquareBracket)
            | Token::SpecialCharacter(SpecialCharacter::Comma)=> {
                extracted_tokens.push(token);
            },
            Token::SpecialCharacter(SpecialCharacter::LeftParenthesis) => {
                open_paren_count += 1;
                extracted_tokens.push(token);
            },
            Token::SpecialCharacter(SpecialCharacter::RightParenthesis) => {
                open_paren_count -= 1;
                if open_paren_count < 0 {
                    break;
                }
                extracted_tokens.push(token);
            }
            Token::SpecialCharacter(SpecialCharacter::SemiColon) | Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket) => break,
            _ => return Err(format!("Unexpected token {:?}, during expression paring", token)),
        }
    }
    println!("Extracted tokens: {:?}", extracted_tokens);
    let mut expression_parser = ExpressionParser::new(extracted_tokens, type_map.clone(), symbol_table.clone());
    if let Ok(expression_root) = expression_parser.parse_expression() {
        return Ok(expression_root);
    };
    Err(String::from("Unexpected expression parse error"))
}

/// Gets Vec of tokens as input and returns AST tree or error Result
///
pub fn generate_ast_tree(
    tokens: Vec<Token>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<Vec<Rc<RefCell<Stmt>>>, String> {
    let mut token_iter = tokens.into_iter().peekable();
    let mut result = Vec::new();
    while let Some(token) = token_iter.next() {
        match token {
            Token::Keyword(keyword) => {
            // expect some type keyword at global scope
                match keyword {
                    Keyword::Type(type_var) => {
                        let type_key = match type_var {
                            Type::Primitive(name) => name,
                            Type::Struct(name) => name,
                            _ => return Err(format!("Unexpected type at global scope: {:?}", type_var))
                        };
                        if !type_map.contains_key(&type_key) {
                            return Err(format!("Unknown type: {:?}", type_key))
                        }
                        let mut cur_type = type_map.get(&type_key).unwrap().clone();
                        while let Some(Token::Operator(Operator::Multiplication)) = token_iter.peek().cloned() {
                            let pointer_type = Type::Pointer(Box::from(cur_type.clone()));
                            cur_type = pointer_type;
                            token_iter.next();
                        }
                        result.push(parse_type_declaration(&mut token_iter, type_map, symbol_table, &mut cur_type)?);
                    }
                    Keyword::TypeDef => {
                        parse_type_def(&mut token_iter, type_map, symbol_table)?;
                    }
                    _ => return Err(format!("Unexpected keyword {:?}, at global scope", keyword))
                }
            },
            // must be a struct type global var/function
            Token::Identifier(some) => {

            },
            _ => return Err("Unexpected token at global scope".to_string())
        }
    }
    Ok(result)
}

fn parse_type_def(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<(), String> {
    expect_token(token_iter, Token::Keyword(Keyword::Struct))?;
    //consume struct name:
    let name;
    if let Some(Token::Identifier(identifier)) = token_iter.next() {
       name = identifier;
    } else {
        return Err(String::from("No identifier after struct keyword"))
    }
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket))?;
    // while scope does not end parse struct fields
    let struct_type = Type::Struct(name.clone());
    type_map.insert(name.clone(), struct_type.clone());
    let mut fields: Vec<StructField> = Vec::new();
    let mut cur_offset = 0;
    while !matches!(token_iter.peek(), Some(Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket)) | None) {
        let cur_type = token_iter.next().unwrap();
        let mut field_name;
        let mut cur_field_type : Type = match cur_type {
            Token::Identifier(identifier) => {
                let type_op = type_map.get(&identifier);
                if type_op.is_none() {
                    return Err(format!("Undefined type: {identifier} during parsing of struct: {name}"))
                }
                type_op.unwrap().clone()
            },
            Token::Keyword(Keyword::Type(Type)) => {

                Type.clone()
            },
            _ => return Err(format!("Invalid token instead of type in field of:{name} struct"))
        };
        //check for *
        while let Some(Token::Operator(Operator::Multiplication)) = token_iter.peek().cloned() {
            let pointer_type = Type::Pointer(Box::from(cur_field_type.clone()));
            cur_field_type= pointer_type;
            token_iter.next();
        }
        //consume name
        let next = token_iter.next();
        if let Some(Token::Identifier(identifier)) = next {
            field_name = identifier;
        } else {
            return Err(format!("Invalid token: {:?}, instead of field field_name in struct: {:?}", next, name))
        };
        // check for []
        while let Some(Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket)) = token_iter.peek() {
            expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket))?;
            let size;
            let next_token = token_iter.next();
            if let Some(Token::Constant(number)) = next_token {
                size = number;
            } else {
                return Err(format!("Unexpected token: {:?} found after parsing [] in struct: {name}, field: {field_name}", next_token))
            }
            cur_field_type = Type::Array(Box::from(cur_field_type), size);
            expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::RightSquareBracket))?;
        }
        if cur_field_type == struct_type {
            return Err(format!("You cannot have field of same type inside try: {:?}*{name}", struct_type))
        }
        let size = get_size(&cur_field_type, type_map, symbol_table)?;
        let field_str = StructField {
            name: field_name,
            field_type: cur_field_type,
            offset: cur_offset,
        };
        cur_offset += size;
        fields.push(field_str);
        expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::SemiColon))?;
    }
    let total_size = calculate_struct_total_size(&fields, type_map, symbol_table)?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket))?;
    expect_token(token_iter, Token::Identifier(name.clone()))?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::SemiColon))?;
    let def = StructDef {
        name: name.clone(),
        fields,
        size: total_size + 1,
    };
    symbol_table.insert(name, SymbolTableEntry::StructDef(def));
    Ok(())
}

fn calculate_struct_total_size(
    fields: &Vec<StructField>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<i64, String> {
    let mut acum = 0;
    for  field in fields {
        let f_type = &field.field_type;
        acum += get_size(f_type, type_map, symbol_table)?;
    }
    Ok(acum)
}

/// Parses scope and returns statements, that belong to it.
///
///
fn parse_scope_tokens(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Vec<Rc<RefCell<Stmt>>>, String> {
    let mut scope_statements: Vec<Rc<RefCell<Stmt>>> = Vec::new();
    // stack which tracks the latest scopes block variant reference
    let mut parent_stack : Vec<Rc<RefCell<Stmt>>> = Vec::new();
    // Parse the tokens
    while let Some(token) = token_iter.peek().cloned() {
        let mut stmt = match token {
            // parse type keyword keyword
            Token::Keyword(Keyword::Type(some_type)) => {
                token_iter.next();
                let mut cur_type = some_type.clone();
                while let Some(Token::Operator(Operator::Multiplication)) = token_iter.peek().cloned() {
                    let pointer_type = Type::Pointer(Box::from(cur_type));
                    cur_type = pointer_type;
                    token_iter.next();
                }
                let name_string = match token_iter.peek() {
                    Some(Token::Identifier(name)) => name.clone(),
                    _ => return Err(format!("Illegal token after integer keyword: {:?}", token_iter.peek()))
                };
                parse_variable_declaration(token_iter, &name_string, type_map, symbol_table, &mut cur_type)?
            },
            Token::Keyword(Keyword::If) => {
                token_iter.next();
                parse_conditional(token_iter, type_map, symbol_table)?
            },
            // Match return statement `return <int>;`
            Token::Keyword(Keyword::Return) => {
                token_iter.next();
                parse_return_statement(token_iter, type_map, symbol_table)?
            },
            // Handle block end `}`
            Token::SpecialCharacter(SpecialCharacter::RightCurlyBracket) => {
                token_iter.next();
                return Ok(scope_statements);
            },
            Token::Identifier(identifier) => {
                handle_identifier_usage(token_iter, &identifier, type_map, symbol_table)?
            },
            Token::Keyword(Keyword::While) => {
                token_iter.next();
                handle_while_keyword(token_iter, type_map, symbol_table)?
            },
            Token::Keyword(Keyword::For) => {
                token_iter.next();
                handle_for_keyword(token_iter, type_map, symbol_table)?
            },
            // Skip any other tokens or syntax we don't support
            _ => continue,
        };
        scope_statements.push(stmt);
    }
    for statement in &scope_statements {
        println!("{:?}", statement);
    }
    Ok(scope_statements)
}

fn handle_for_keyword(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    //Expect open parenthesis
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    // parse tokens after it to the first semicolon(initialisation)
    // three possible variants: 1) declaration of new var, 2) assignment to already declared one, 3) just semicolon
    let init_root = match token_iter.peek().cloned() {
        Some(Token::Identifier(identifier)) => {
            // Handle identifier usage (e.g., assignment to an already declared variable)
            Some(handle_identifier_usage(token_iter, &identifier, type_map, symbol_table)?)
        }
        Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) => {
            // No initialization, just a semicolon
            token_iter.next();
            None
        }
        Some(Token::Keyword(Keyword::Type(Type))) => {
            // Handle variable declaration
            token_iter.next();
            match token_iter.clone().peek() {
                Some(Token::Identifier(identifier)) => {
                    Some(parse_variable_declaration(token_iter, &identifier, type_map, symbol_table, &mut Type.clone())?)
                }
                _ => {
                    return Err("Expected identifier after type keyword before first semicolon in for".to_string());
                }
            }
        }
        Some(other) => {
            // Invalid token
            return Err(format!("Invalid token before first semicolon in for: {:?}", other));
        }
        None => {
            // No token present
            return Err("No token after for loop parenthesis opened".to_string());
        }
    };
    // parse tokens between first and second semicolon(condition)
    // either expression or semicolon
    let mut is_semicolon = match token_iter.peek() {
        None => return Err("No token after second semicolon in for loop".to_string()),
        Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) => true,
        _=> false
    };
    let mut condition_root;
    if is_semicolon {
        condition_root = None
    } else {
        condition_root = Some(parse_expression(token_iter, type_map, symbol_table)?);
    }

    // parse tokens after second semicolon and right parenthesis(increment)
    // either expression(s) or right parenthesis
    is_semicolon = match token_iter.peek() {
        None => return Err("No token after second semicolon in for loop".to_string()),
        Some(Token::SpecialCharacter(SpecialCharacter::SemiColon)) => true,
        _=> false
    };
    let mut increment_root;
    if is_semicolon {
        increment_root = None
    } else {
        increment_root = Some(parse_expression(token_iter, type_map, symbol_table)?);
    }
    // parse loop body, if exists
    let body_option : Option<Rc<RefCell<Stmt>>> = match token_iter.next() {
        None => return Err("No token after parsing for loop conditions".to_string()),
        Some(token) => {
            match token {
                Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket) => {
                    let body_vec = parse_scope_tokens(token_iter, type_map, symbol_table)?;
                    Some(Rc::new(RefCell::new(Block(body_vec))))
                },
                Token::SpecialCharacter(SpecialCharacter::SemiColon)=> None,
                _ => return Err("Invalid token after parsing for loop conditions".to_string())
            }
        }
    };

    let for_loop_body = Rc::new(RefCell::new(For {
        initialization: init_root,
        condition: condition_root,
        increment: increment_root,
        body: body_option,
    }));

    Ok(for_loop_body)
}

fn handle_while_keyword(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    //consume while keyword
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let expression_root = parse_expression(token_iter, type_map, symbol_table)?;
    expect_token(token_iter, Token::SpecialCharacter(LeftCurlyBracket))?;
    let cycle_body = parse_scope_tokens(token_iter, type_map, symbol_table)?;
    let body_block = Rc::new(RefCell::new(Block(cycle_body)));
    let while_node = Rc::new(RefCell::new( Stmt::While {
        condition: expression_root,
        body: body_block
    }));

    Ok(while_node)
}

/// Parses tokens after else keyword
///
fn parse_else_keyword(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    match token_iter.peek().ok_or_else(|| "No token after parsing else keyword")? {
        Token::Keyword(Keyword::If) => {
            // consume if keyword
            token_iter.next();
            let if_else_node = parse_conditional(token_iter, type_map, symbol_table)?;
            Ok(if_else_node)
        }
        Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket) => {
            token_iter.next();
            let statements = parse_scope_tokens(token_iter, type_map, symbol_table)?;

            let condition_node = Rc::new(RefCell::new(Block(statements)));
            Ok(condition_node)
        }
        _ => {
            Err(format!("Invalid token after else keyword {:?}", token_iter.peek()))
        }
    }
}

/// Parsers if keyword
///
///
fn parse_conditional(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    println!("Parsing conditional");
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let bool_expression_root = parse_expression(token_iter, type_map, symbol_table)?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket))?;
    let statements = parse_scope_tokens(token_iter, type_map, symbol_table)?;
    let else_branch = match token_iter.peek() {
        None => None,
        Some(token) => {
            match token {
                Token::Keyword(Keyword::Else) => {
                    println!("We are parsing else keyword!!!!!");
                    token_iter.next();
                    Some(parse_else_keyword(token_iter, type_map, symbol_table)?)
                },
                _ => None,
            }
        }
    };

    let if_statement = Rc::new(RefCell::new(Stmt::If {
        condition: bool_expression_root,
        then_branch: Rc::new(RefCell::new(Stmt::Block(statements))),
        else_branch,
    }));

    Ok(if_statement)
}

/// Handles identifier usage
///
fn handle_identifier_usage(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    let entry_option = symbol_table.get(name);
    if entry_option.is_none() {
        return Err(format!("Invalid identifier:{name} during parsing"))
    }
    let entry = entry_option.unwrap().clone();
    match entry {
        SymbolTableEntry::Variable(Type) => {
            match Type {
                Type::Primitive(_) | Type::Pointer(_) => parse_primitive_identifier(token_iter, name, type_map, symbol_table),
                Type::Array(inner, size) => parse_array_identifier(token_iter, name, type_map, symbol_table, inner.clone(), size),
                Type::Struct(struct_name) => parse_struct_identifier(token_iter, &struct_name, type_map, symbol_table),
                // Type::Function => {}
                Type::Void => return Err("Invalid use of keyword void".to_string()),
                _ => Err("dff".to_string()),
            }
        },
        SymbolTableEntry::StructDef(str_def) => {
            let name = &str_def.name;
            let cur_type = type_map.get(name).ok_or(format!("No type as {name} declared"))?;
            token_iter.next();

            parse_type_declaration(token_iter, type_map, symbol_table, &mut cur_type.clone())
        }
        _ => return Err(format!("Invalid type of identifier:{:?} during parsing", entry)),
    }


}

fn parse_struct_identifier(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<Rc<RefCell<Stmt>>, String> {
    let next_token = token_iter.peek().cloned();
    if let Some(Token::Identifier(name)) = next_token {
        let expression_root = parse_expression(token_iter, type_map, symbol_table)?;
        Ok(Rc::new(RefCell::new(VarAssignment { name: name.clone(), expr: Some(expression_root) })))
    } else  {
        return Err(format!("Expected identifier token, but got {:?}", next_token))
    }
}

fn parse_array_identifier(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    inner_type: Box<Type>,
    dimension: i64,
) -> Result<Rc<RefCell<Stmt>>, String> {
    // parse identifier;
    let expression = parse_expression(token_iter, type_map, symbol_table)?;
    Ok(Rc::new(RefCell::new(Stmt::VarAssignment {
        name: name.clone(),
        expr: Some(expression),
    })))
}

fn parse_primitive_identifier(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Rc<RefCell<Stmt>>, String> {
    let mut iter_clone = token_iter.clone();
    //consume identifier and peek token after
    iter_clone.next();
    match iter_clone.peek() {
        // var assignment
        Some(Token::Operator(Operator::Assign)) => {
            let expression_root = parse_expression(token_iter, type_map, symbol_table)?;
            let var_node = Rc::new(RefCell::new(Stmt::VarAssignment {
                name: name.to_string(),
                expr: Some(expression_root),
            }));
            Ok(var_node)
        },
        Some(Token::SpecialCharacter(SpecialCharacter::LeftParenthesis)) => {

            Err("Function call not supported yet".to_string())
        },
        _ => Err(format!("Invalid symbol after identifier {:?}", token_iter.peek())),
    }
}
/// Parses integer keyword at global scope, returns VarDec Node or FuncDec Node,
/// All the scopes inside are parsed recursively
///
fn parse_type_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    cur_type: &mut Type
) -> Result<Rc<RefCell<Stmt>>, String> {
    if let Some(Token::Identifier(name)) = token_iter.peek().cloned() {
        let mut cloned_iter = token_iter.clone();
        let after_identifier = {
            cloned_iter.next();
            cloned_iter.peek()
        };
        if let Some(Token::SpecialCharacter(character)) = after_identifier {
            return match character {
                SpecialCharacter::LeftParenthesis => {
                    //consume name;
                    token_iter.next();
                    parse_function_declaration(token_iter, &name, type_map, symbol_table, cur_type)
                },
                SpecialCharacter::SemiColon => {
                    parse_variable_declaration(token_iter, &name, type_map, symbol_table, cur_type)
                },
                _ => Err(format!("Unexpected special character after {:?}, integer declaration", character))
            }
        } else if let Some(Token::Operator(Operator::Assign)) = after_identifier {
            return parse_variable_declaration(
                token_iter,
                &name,
                type_map,
                symbol_table,
                cur_type
            )
        }
    }
    Err(format!("Unexpected token while parsing integer declaration: {:?}", token_iter.peek()))
}

fn parse_function_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    cur_type: &mut Type
) -> Result<Rc<RefCell<Stmt>>, String> {
    // Expect parentheses `()` and '{'
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftParenthesis))?;
    let args = parse_args(token_iter, type_map, symbol_table)?;
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::LeftCurlyBracket))?;

    let function_statements = parse_scope_tokens(token_iter, type_map, symbol_table)?;
    symbol_table.insert(name.to_string(), FunDef(crate::lexer::FunDef{
        funType: cur_type.clone(),
        args: args.clone(),
    }
    ));
    let function_node = Rc::new(RefCell::new(Stmt::FnDecl {
        name: name.to_string(),
        return_type: cur_type.clone(),
        args,
        body: Rc::new(RefCell::new(Stmt::Block(function_statements))),
    }));
    Ok(function_node)
}

fn parse_args(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
) -> Result<Option<Vec<Stmt>>, String> {
    let mut args = Vec::new();

    while !matches!(
        token_iter.peek(),
        Some(Token::SpecialCharacter(SpecialCharacter::RightParenthesis)) | None
    ) {
        // Get the next token safely
        let next = match token_iter.next() {
            Some(token) => token,
            None => return Err("Unexpected end of input while parsing function parameters.".to_string()),
        };

        let cur_type = match next {
            Token::Identifier(ref ident) => {
                if !type_map.contains_key(ident) {
                    return Err(format!("Type: {:?} was not defined", ident));
                }
                type_map.get(ident).unwrap().clone()
            }
            Token::Keyword(Keyword::Type(arg_type)) => arg_type.clone(),
            token => return Err(format!("Unexpected token: {:?} instead of type of param", token)),
        };

        // Get the next token, expecting an identifier (parameter name)
        let name = match token_iter.next() {
            Some(Token::Identifier(ident)) => ident,
            Some(token) => return Err(format!("Unexpected token: {:?} instead of name of param", token)),
            None => return Err("Unexpected end of input after parameter type.".to_string()),
        };

        let size = get_size(&cur_type, type_map, symbol_table)?;

        let cur_arg = Stmt::FnParam {
            param_type: cur_type,
            param_name: name,
            param_size: size,
        };
        args.push(cur_arg);

        // Get the next token and check if it's a comma or a closing parenthesis
        match token_iter.peek() {
            Some(Token::SpecialCharacter(SpecialCharacter::Comma)) => {
                token_iter.next(); // Consume the comma and continue
            }
            Some(Token::SpecialCharacter(SpecialCharacter::RightParenthesis)) => {
                token_iter.next(); // Consume the closing parenthesis and break
                return Ok(Some(args))
            }
            Some(token) => {
                return Err(format!("Invalid token: {:?}, after arg name in the func decl", token))
            }
            None => {
                return Err("Unexpected end of input while parsing function parameters.".to_string())
            }
        }
    }
    expect_token(token_iter, Token::SpecialCharacter(SpecialCharacter::RightParenthesis))?;
    Ok(None)
}
///
/// Gets 3 parameters: token iterator, variable name and type. However, name of variable should not
/// be consumed, so the current element in iter must be identifier.
///
///
///
fn parse_variable_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    var_name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    cur_type: &mut Type
) -> Result<Rc<RefCell<Stmt>>, String> {
    // check for array decl
    let mut clone_iter = token_iter.clone();
    clone_iter.next();
    if let Some(Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket)) = clone_iter.peek().cloned() {
        return parse_array_declaration(token_iter, var_name, type_map, symbol_table, cur_type);
    }
    symbol_table.insert(var_name.clone(), SymbolTableEntry::Variable(cur_type.clone()));
    let expression = parse_expression(token_iter, type_map, symbol_table)?;
    if let Type::Struct(_) = cur_type {
        return Ok(Rc::new(RefCell::new(Stmt::VarDecl {
            name: var_name.to_string(),
            var_type: cur_type.clone(),
            expr: None,
        })))
    }
    Ok(Rc::new(RefCell::new(Stmt::VarDecl {
        name: var_name.to_string(),
        var_type: cur_type.clone(),
        expr: Some(expression),
    })))
}

fn parse_array_declaration(
    token_iter: &mut Peekable<IntoIter<Token>>,
    var_name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    arr_type: &mut Type
) -> Result<Rc<RefCell<Stmt>>, String> {
    //consume identifier
    token_iter.next();
    let mut cur_arr_type = arr_type.clone();
    while !matches!(token_iter.peek(), Some(Token::Operator(Operator::Assign)) | Some(Token::SpecialCharacter(SpecialCharacter::SemiColon))) {
        //parse square bracket
        let nex_token = token_iter.next();
        if let Some(Token::SpecialCharacter(SpecialCharacter::LeftSquareBracket)) = nex_token {
            match token_iter.next().unwrap() {
                Token::Identifier(name) => {
                    let var_op = symbol_table.get(&name);
                    if var_op.is_none() {
                        return Err(format!("No variable {name} is defined in array declaration {var_name}"))
                    }
                    return Err("Variable type arrays are not supported".to_string())
                }
                Token::Constant(constant) => {
                    let cur_type = Type::Array(Box::from(cur_arr_type.clone()), constant);
                    cur_arr_type = cur_type;
                }
                Token::SpecialCharacter(SpecialCharacter::RightSquareBracket) => {
                    let cur_type = Type::Array(Box::from(cur_arr_type.clone()), -1);
                    cur_arr_type = cur_type;
                }
                _ => return Err(format!("Invalid token after [ in arry declaration: {:?}", token_iter.peek().cloned()))
            }
        } else {
            break;
        }
    }
    match token_iter.peek().unwrap() {
        Token::Operator(Operator::Assign) => {
            //either all length given or none
            token_iter.next();
            let mut tem_type = &cur_arr_type;
            let mut dimensions = Vec::new();
            while let Type::Array(nested, size) = tem_type {
                dimensions.push(*size);
                tem_type = nested;
            }
            let all_filled = dimensions.iter().all(|&size| size != -1);

            if !all_filled {
                return Err("Mixed array dimensions: some are filled, others are incomplete.".to_string());
            }

            parse_array_decl_expression(token_iter, var_name, type_map, symbol_table, arr_type, &dimensions)
        }
        Token::SpecialCharacter(SpecialCharacter::SemiColon) => {
            token_iter.next();
            // every level should be defined
            let mut temp_type = &cur_arr_type;
            while let Type::Array(nested, size) = temp_type {
               if(*size == -1) {
                   return Err("Incomplete array dimension specification".to_string());
               }
                temp_type = nested;
            }
            symbol_table.insert(var_name.clone(), SymbolTableEntry::Variable(cur_arr_type.clone()));
            Ok(Rc::new(RefCell::new(Stmt::VarDecl {
                name: var_name.clone(),
                var_type: cur_arr_type,
                expr: None,
            })))
        }
        _ => return Err(format!("Illegal token after array [] during declaration : {:?}", token_iter.peek()))
    }
}

fn parse_array_decl_expression(
    token_iter: &mut Peekable<IntoIter<Token>>,
    var_name: &String,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>,
    cur_type: &mut Type,
    dimensions: &Vec<i64>
) -> Result<Rc<RefCell<Stmt>>, String> {

    todo!()
}

fn parse_return_statement(
    token_iter: &mut Peekable<IntoIter<Token>>,
    type_map: &mut HashMap<String, Type>,
    symbol_table: &mut HashMap<String, SymbolTableEntry>
) -> Result<Rc<RefCell<Stmt>>, String> {
    let expression_root = parse_expression(token_iter, type_map, symbol_table)?;
    let return_node = Rc::new(RefCell::new(Stmt::Return(Some(expression_root))));
    Ok(return_node)
}


fn handle_end_of_block(
    token_iter: &mut Peekable<IntoIter<Token>>,
    parent_stack: &mut Vec<Rc<RefCell<Stmt>>>
) -> Result <(), String> {
    let res = parent_stack.pop();
    if res.is_none() {
        return Err("There is } but no {".to_string());
    }
    Ok(())
}

/// consumes next if it is expected token and returns () or returns error if not
fn expect_token(
    token_iter: &mut Peekable<std::vec::IntoIter<Token>>,
    expected_token: Token
) -> Result<(), String> {
    if token_iter.peek() == Some(&expected_token) {
        token_iter.next();
        Ok(())
    } else {
        Err(format!("Expected {:?}, found {:?}", expected_token, token_iter.peek()))
    }
}


pub fn print_ast(root_nodes: &Vec<Rc<RefCell<Stmt>>>) {
    for node in root_nodes {
        println!("{:?}\n", node);
    }
}


fn get_array_total_size(array_type: &Type) -> i64 {
    0
}