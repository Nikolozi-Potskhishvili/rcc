use std::cell::RefCell;
use std::rc::Rc;
use crate::lexer::{Constant, Lexer, Operator, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    DummyExpr(Expr),
    VarDecl { name: String, var_type: String, expr: Option<Expr>},
    VarAssignment { name: String, expr: Option<Expr>},
    FnDecl {
        name: String,
        return_type: String,
        args: Option<Vec<Expr>>,
        body: Rc<RefCell<Stmt>>
    },
    If {
        condition: Expr,
        then_branch: Rc<RefCell<Stmt>>,
        else_branch: Option<Rc<RefCell<Stmt>>>
    },
    While { condition: Expr, body: Box<Stmt>},
    Block(Vec<Rc<RefCell<Stmt>>>),
    Return(Option<Expr>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    BinaryExpr(BinaryExpression),
    UnaryExpr(UnaryExpr),
    Const(Constant),
    VarUsage(String),
    Function_Call {
        name: String,
        args: Vec<Expr>
    },
}



#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Operator,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub operator: Operator,
    pub operand: Box<Expr>,
}
