use std::cell::RefCell;
use std::rc::Rc;
use crate::lexer::{Constant, Lexer, Operator, Token, Type};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    DummyExpr(Expr),
    VarDecl {name: String, var_type: Type, expr: Option<Expr>},
    VarAssignment {name: String, expr: Option<Expr>},
    FnDecl {
        name: String,
        return_type: Type,
        args: Option<Vec<Stmt>>,
        body: Rc<RefCell<Stmt>>
    },
    If {
        condition: Expr,
        then_branch: Rc<RefCell<Stmt>>,
        else_branch: Option<Rc<RefCell<Stmt>>>
    },
    While { condition: Expr, body: Rc<RefCell<Stmt>>},
    For {
        initialization: Option<Rc<RefCell<Stmt>>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Option<Rc<RefCell<Stmt>>>
    },
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
