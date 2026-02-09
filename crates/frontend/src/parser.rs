use crate::ast::{AST, Decl, Expr, Program, Stmt};
use crate::token::{Keyword, Token, TokenKind};
use std::collections::HashSet;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    typedefs: Vec<HashSet<String>>,
}

pub struct ParserErr {
    token: Option<Token>,
    msg: String,
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
            // Some(TokenKind::Keyword(keyword)) => keyword.(),
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

    pub fn parse(&mut self) -> Result<AST, ParserErr> {
        let global = self.translation_unit()?;
        Ok(AST {
            root: Program { decls: global },
        })
    }

    fn translation_unit(&mut self) -> Result<Vec<Decl>, ParserErr> {
        let mut decls = Vec::new();
        while !self.is_eof() {
            let decl = self.external_decl()?;
            decls.push(decl);
        }
        Ok(Vec::new())
    }

    fn external_decl(&mut self) -> Result<Decl, ParserErr> {
        // match self.peek() {
        //     Some(token) => match token {
        //         Keyword(Keyword::TypeDef) => return self.obj_decl(),
        //     },
        //     None => return Err(self.pars_err(None, "expected Some in external_decl, found None")),
        // }
        todo!()
    }

    //
    // Parser treats both function definitions and prototypes as same type: Decl::Func, for
    // prototypes body(Option<Box<Stmt>>) is None.
    //
    fn func_def(&mut self) -> Result<Vec<Decl>, ParserErr> {
        todo!()
    }

    fn obj_decl(&mut self) -> Result<Decl, ParserErr> {
        if self.is_typdef() {
            let decl = self.typedef_decl()?;

            return Ok(decl);
        }
        todo!()
    }

    fn typedef_decl(&mut self) -> Result<Decl, ParserErr> {
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

    fn pars_err(&self, token: Option<Token>, msg: &str) -> ParserErr {
        ParserErr {
            token: token,
            msg: msg.to_string(),
        }
    }
}
