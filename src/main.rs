#![recursion_limit = "512"]
use std::{env, fs};
use std::fs::File;
use std::io::Write;
use std::process::{Command, ExitStatus};
use crate::codegen::generate_assembly;
use crate::lexer::Lexer;
use crate::parser::{generate_ast_tree, print_ast};

mod lexer;
mod parser;
mod semantic_analysis;
mod codegen;

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
     fs::read_to_string(file_name).expect("Problem with reading file")
}

fn compile_source_code(source_code: String) ->  ExitStatus {
    let tokens = Lexer::tokenize(&source_code);
    let ast_tree = generate_ast_tree(tokens).expect("Problem creating AST tree");
    print_ast(&ast_tree, 0);
    let code = generate_assembly(ast_tree);
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

#[cfg(test)]
mod tests {
    use std::{fs, panic};
    use std::fs::File;
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
            assert!(!run_asm_status.success());
            let exit_code = run_asm_status.code().unwrap();
            assert_eq!(exit_code, expected_value);
        });
        result
    }

    fn clean_up_tests_files() {
        let _ = fs::remove_file("test").ok();
        let _ = fs::remove_file("test.s").ok();
    }

    #[test]
    fn only_integer_return() {
        let result = test_helper("test.c", 2);
        clean_up_tests_files();

        result.expect("tests panicked");
    }

    #[test]
    fn unary_operators_only_integers() {
        let inputs = vec!(
            String::from("./test_files/test_minus.c"),
            String::from("./test_files/test_not_on_int.c"),
            String::from("./test_files/test_tilde_on.c.c"),
            String::from("./test_files/test_multi_layered_unary.c"));
        let expected_outputs = vec!(
            246,
            0,
            -11,
            -1
        );
        for (index, input) in inputs.iter().enumerate() {
            let expected_output = expected_outputs[index];
            let result = test_helper(input, expected_output);
            clean_up_tests_files();
            result.expect("failed");
        }

    }
}