use crate::ast::{AST, Decl, Expr, Program, Stmt};
use crate::lexer::{Keyword, Token, TokenKind};
use std::collections::HashSet;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    typedefs: Vec<HashSet<String>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens,
            index: 0,
            typedefs: Vec::new(),
        }
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.get(self.index).map(|token| &token.kind)
    }

    fn peek_n(&self, n: usize) -> Option<&TokenKind> {
        self.tokens.get(self.index + n).map(|token| &token.kind)
    }

    fn next(&mut self) {
        self.index += 1
    }

    fn is_typdef(&self) -> bool {
        if let Some(token) = self.peek() {
            return token.clone() == TokenKind::Keyword(Keyword::TypeDef);
        }
        false
    }

    fn is_type_specifier(&self) -> bool {
        match self.peek() {
            Some(TokenKind::Keyword(keyword)) => keyword.is_type(),
            Some(TokenKind::Identifier(identifier)) => self
                .typedefs
                .last()
                .map(|cur_types| cur_types.contains(identifier))
                .unwrap_or(false),
            _ => false,
        }
    }

    fn is_eof(&self) -> bool {
        if let Some(token) = self.peek() {
            return token.clone() == TokenKind::EOF;
        }
        false
    }

    pub fn parse(&mut self) -> Result<AST, String> {
        let global = self
            .translation_unit()
            .map_err(|err| format!("Error during parsing: {:?}", err))?;
        Ok(AST {
            root: Program { decls: global },
        })
    }

    fn translation_unit(&mut self) -> Result<Vec<Decl>, String> {
        let mut decls = Vec::new();
        while !self.is_eof() {
            let decl = self
                .external_decl()
                .map_err(|err| format!("Error during translation unit: {:?}", err))?;
            decls.push(decl);
        }
        Ok(Vec::new())
    }

    fn external_decl(&mut self) -> Result<Decl, String> {
        // } else if  {
        //
        // } else {
        //     Err(format!(
        //         "Error in parsing external declaration, unexpected token: {:?}",
        //         self.peek()
        //     ))
        // }
        todo!()
    }

    //
    // Parser treats both function definitions and prototypes as same type: Decl::Func, for
    // prototypes body(Option<Box<Stmt>>) is None.
    //
    fn func_def(&mut self) -> Result<Vec<Decl>, String> {
        todo!()
    }

    fn obj_decl(&mut self) -> Result<Decl, String> {
        if self.is_typdef() {
            let decl = self
                .typedef_decl()
                .map_err(|err| format!("Error during parsing external declaration: {:?}", err))?;

            return Ok(decl);
        }
        Err("Hello".to_string())
    }

    fn typedef_decl(&mut self) -> Result<Decl, String> {
        todo!()
    }

    fn global_var_decl(&mut self) -> Result<Decl, String> {
        todo!()
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        todo!()
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        todo!()
    }
}
