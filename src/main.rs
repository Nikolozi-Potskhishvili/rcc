#![recursion_limit = "512"]
use std::{env, fs};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::process::{Command, ExitStatus};
use crate::codegen::generate_assembly;
use crate::lexer::{Lexer, Type};
use crate::parser::{generate_ast_tree, print_ast};

mod lexer;
mod parser;
mod semantic_analysis;
mod codegen;
mod ast_types;

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("You must specify file name in order to compile it");
    }
    let file_name = &args[1];
    let source_code = get_source_code(file_name);
    compile_source_code(source_code);
}

fn get_source_code(file_name: &String) -> String {
     fs::read_to_string(file_name).expect(format!("Problem with reading file {}", file_name).as_str())
}

fn compile_source_code(source_code: String) -> ExitStatus {
    let tokens = Lexer::tokenize(&source_code);
    let mut type_map = init_type_map();
    let mut symbol_table = HashMap::new();
    let mut ast_tree = generate_ast_tree(tokens, &mut type_map, &mut symbol_table).expect("Problem creating AST tree");
    print_ast(&ast_tree);
    let code = generate_assembly(&ast_tree, &mut type_map, &mut symbol_table).expect("expected no errors in codegen");
    for line in code.lines() {
        println!("{}", line);
    }
    let mut assembly_file = File::create("test.s").expect("Problem creating assembly file");
    assembly_file.write_all(code.as_ref()).expect("Problem writing assembly file");

    let compile_status = Command::new("gcc")
        .args(&["-o", "test", "test.s"])
        .status()
        .expect("Failed to compile assembly");
    compile_status
}

fn init_type_map() -> HashMap<String, Type> {
    HashMap::from([
        ("int".to_string(), Type::Primitive("int".to_string())),
        ("short".to_string(), Type::Primitive("short".to_string())),
        ("long".to_string(), Type::Primitive("long".to_string())),
        ("bool".to_string(), Type::Primitive("bool".to_string())),
        ("char".to_string(), Type::Primitive("char".to_string())),
    ])
}

#[cfg(test)]
mod tests {
    use std::{fs, panic};
    use std::process::Command;
    use crate::{compile_source_code, get_source_code};

    fn test_helper(file_name: &str, expected_value: i32) -> std::thread::Result<()> {
        let file_name = String::from(file_name);
        let source_code = get_source_code(&file_name);
        let result = panic::catch_unwind(|| {
            let compile_status = compile_source_code(source_code);
            assert!(compile_status.success());
            let run_asm_status = Command::new("./test")
                .status()
                .expect("Failed to run assembly");
            if expected_value == 0 {
                assert!(run_asm_status.success());
            } else {
                assert!(!run_asm_status.success());
                let exit_code = run_asm_status.code().unwrap();
                assert_eq!(exit_code, expected_value);
            }
        });
        result
    }

    fn clean_up_tests_files() {
        let _ = fs::remove_file("test").ok();
        let _ = fs::remove_file("test.s").ok();
    }

    #[test]
    fn for_cycle_tests() {
        let file_name = "./test_files/for_loop_tests/all_fields.c";
        let result = test_helper(file_name, 10);
        clean_up_tests_files();
        result.expect("tests panicked");
    }

    #[test]
    fn only_integer_return() {
        let result = test_helper("test.c", 2);
        clean_up_tests_files();

        result.expect("tests panicked");
    }

    #[test]
    fn simple_vars() {
        let result = test_helper("./test_files/test_simple_int_var.c", 7);
        clean_up_tests_files();

        result.expect("tests panicked");
    }

    #[test]
    fn simple_conditionals() {
        let result = test_helper("./test_files/test_conditionals.c", 3);
        clean_up_tests_files();

        result.expect("tests panicked");
    }
    #[test]
    fn unary_operators_only_integers() {
        let inputs = vec!(
            String::from("./test_files/test_minus.c"),
            String::from("./test_files/test_not_on_int.c"),
            String::from("./test_files/test_tilde_on_int.c"),
            String::from("./test_files/test_multi_layered_unary.c"));
        let expected_max_output = 2_i32.pow(8);
        let expected_outputs = vec!(
            (expected_max_output - 10),
            0,
            (expected_max_output - 11),
            (expected_max_output - 1)
        );
        for (index, input) in inputs.iter().enumerate() {
            println!("current test index: {}", index);
            let expected_output = expected_outputs[index];
            let result = test_helper(input, expected_output);
            clean_up_tests_files();
            result.expect("failed");
        }

    }

    #[test]
    fn test_sum() {
        let file_name = String::from("./test_files/test_sum_of_two.c");
        let result = test_helper(&file_name, 6);
        assert!(result.is_ok());
        clean_up_tests_files();
        result.expect("failed");
    }

    #[test]
    fn test_binary_and_unary_ops() {
        let file_name = String::from("./test_files/test_unary_and_binary_ops.c");
        let result = test_helper(&file_name, 156);
        assert!(result.is_ok());
        clean_up_tests_files();
        result.expect("failed");
    }

    #[test]
    fn test_simple_while() {
        let file_name = String::from("./test_files/test_simple_while.c");
        let result = test_helper(&file_name, 16);
        assert!(result.is_ok());
        clean_up_tests_files();
        result.expect("failed");
    }

    #[test]
    fn test_mixed_ar_expressions() {
        let file_name = String::from("./test_files/test_mult_of_sums.c");
        let expected_val = 45;

        let result = test_helper(&file_name, expected_val);
        clean_up_tests_files();
        result.expect("failed");
    }

    #[test]
    fn test_refs() {
        let file_name = String::from("./test_files/ref_deref_tests/simple_ref.c");
        let expected_val = 69;

        let result = test_helper(&file_name, expected_val);
        clean_up_tests_files();
        result.expect("failed");
    }
}