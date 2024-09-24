use std::{env, fs};
use crate::lexer::Lexer;

mod lexer;
mod parser;
mod ast;
mod semantic_analysis;
mod codegen;

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("You must specify file name in order to compile it");
    }
    let file_name = &args[1];
    let source_code = fs::read_to_string(file_name).expect("Problem with reading file");

    let _tokens = Lexer::tokenize(&source_code);


}
