// use std::f32::consts::E;
//
// #[derive(Debug)]
// pub struct HIR {
//     pub program: Program,
//     pub statments: Vec<Stmt>,
//     pub declarations: Vec<Decl>,
//     pub expressions: Vec<Expr>,
// }
//
// impl IR {
//     pub fn print(&self) {
//         for node in self.program.decls.iter() {}
//     }
// }
//
// #[derive(Debug)]
// pub enum IRNode<'a> {
//     Program(&'a Program),
//     Stmt(&'a Stmt),
//     Decl(&'a Decl),
//     Expr(&'a Expr),
// }
//
// #[derive(Copy, Clone, Debug)]
// pub struct ExprId(pub usize);
//
// #[derive(Copy, Clone, Debug)]
// pub struct StmtId(pub usize);
//
// #[derive(Copy, Clone, Debug)]
// pub struct DeclId(pub usize);
//
// #[derive(Debug)]
// pub struct Program {
//     pub decls: Vec<DeclId>,
// }
//
// #[derive(Debug)]
// pub enum Type {
//     Int,
//     Short,
//     Long,
//     Char,
//     Bool,
//     Struct {
//         name: String,
//         fields: Vec<(Type, String)>,
//     },
//     Alias(Box<Type>),
// }
//
// #[derive(Debug)]
// pub enum Stmt {
//     Expr(ExprId),
//     Return(Option<ExprId>),
//     If {
//         cond: Option<ExprId>,
//         if_block: Option<Vec<StmtId>>,
//         else_block: Option<Vec<StmtId>>,
//     },
//     While {
//         cond: Option<ExprId>,
//         while_block: Option<Vec<StmtId>>,
//     },
//     For {
//         init: Option<ExprId>,
//         condition: Option<ExprId>,
//         step: Option<ExprId>,
//         for_body: Option<StmtId>,
//     },
//     Block(Vec<StmtId>),
// }
//
// #[derive(Debug)]
// pub enum Decl {
//     Var {
//         name: String,
//         var_type: Type,
//         val: Option<ExprId>,
//     },
//     Func {
//         name: String,
//         return_type: Type,
//         args: Option<StmtId>,
//     },
// }
//
// #[derive(Debug)]
// pub enum Operator {
//     Plus,
//     Minus,
//     Multiplication,
//     Devision,
//     BitOr,
//     BitAnd,
//     BitNot,
// }
//
// #[derive(Debug)]
// pub enum LiteralValue {
//     Int(i64),
//     Short(i64),
//     Long(i64),
//
//     Char(char),
//     String(String),
//     Bool(bool),
// }
//
// #[derive(Debug)]
// pub enum Expr {
//     ConstLiteral {
//         literal_type: Type,
//         val: LiteralValue,
//     },
//     Var {
//         name: String,
//     },
//     UnaryExpression {
//         operand: ExprId,
//         operator: Operator,
//     },
//     BinaryExpression {
//         left: ExprId,
//         right: ExprId,
//         operator: Operator,
//     },
//     FunctionCall {
//         name: String,
//         args: Vec<ExprId>,
//     },
// }
