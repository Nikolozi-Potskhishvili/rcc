#[derive(Debug)]
pub struct AST {
    root: Program,
}

impl AST {
    pub fn print(&self) {
        println!("Program AST:");
        self.root.print(0);
    }
}

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
}

impl Program {
    fn print(&self, ident: usize) {
        let prefix = "  ".repeat(ident);
        for decl in &self.decls {
            decl.print(ident + 1);
        }
    }
}

#[derive(Debug)]
pub enum TypeSyntax {
    Named(String),
    Pointer(Box<TypeSyntax>),
    Array(Box<TypeSyntax>, Option<Box<Expr>>),
}

#[derive(Debug)]
pub enum LiteralValue {
    Int(String),
    Float(String),
    Char(String),
    String(String),
    Bool(String),
}

#[derive(Debug)]
pub enum Stmt {
    Empty,
    Expr(Box<Expr>),
    Return(Option<Box<Expr>>),
    If {
        cond: Box<Expr>,
        if_block: Vec<Box<Stmt>>,
        else_block: Option<Vec<Box<Stmt>>>,
    },
    While {
        cond: Box<Expr>,
        while_block: Vec<Box<Stmt>>,
    },
    For {
        init: Option<Box<Expr>>,
        cond: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
        for_block: Vec<Box<Stmt>>,
    },
    Block(Vec<Box<Stmt>>),
}

impl Stmt {
    fn print(&self, mut ident: usize) {
        let mut prefix = "  ".repeat(ident);
        match self {
            Stmt::Empty => println!("{}Empty", prefix),
            Stmt::Expr(expr) => {
                println!("{}Expression:", prefix);
                expr.print(ident + 1);
            }
            Stmt::Return(expr) => {
                println!("{}Return:", prefix);
                if let Some(expr) = expr {
                    expr.print(ident + 1);
                }
            }
            Stmt::If {
                cond,
                if_block,
                else_block,
            } => {
                println!("{}If:", prefix);
                ident += 1;
                prefix = "  ".repeat(ident);
                println!("{}Cond:", prefix);
                cond.print(ident + 1);
                println!("{}Then: ", prefix);
                for s in if_block {
                    s.print(ident + 1);
                }

                if let Some(else_block) = else_block {
                    println!("{}Else:", ident);
                    for s in else_block {
                        s.print(ident + 1);
                    }
                }
            }
            Stmt::While { cond, while_block } => {
                println!("{}While:", prefix);
                ident += 1;
                prefix = "  ".repeat(ident);
                println!("{}Cond:", prefix);
                cond.print(ident + 1);
                println!("{}Then: ", prefix);
                for s in while_block {
                    s.print(ident + 1);
                }
            }
            Stmt::For {
                init,
                cond,
                step,
                for_block,
            } => {
                println!("{}For:", prefix);
                ident += 1;
                prefix = "  ".repeat(ident);
                println!("{}Init:", prefix);
                if let Some(init) = init {
                    init.print(ident + 1);
                }
                println!("{}Cond:", prefix);
                if let Some(cond) = cond {
                    cond.print(ident + 1);
                }
                println!("{}Step:", prefix);
                if let Some(step) = step {
                    step.print(ident + 1);
                }
                println!("{}Then: ", prefix);
                for s in for_block {
                    s.print(ident + 1);
                }
            }
            Stmt::Block(statements) => {
                println!("{}Block:", prefix);
                for s in statements {
                    s.print(ident + 1);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    Var {
        name: String,
        var_type: TypeSyntax,
        val: Option<Box<Expr>>,
    },
    Func {
        name: String,
        return_type: TypeSyntax,
        args: Vec<(String, Box<TypeSyntax>)>,
    },
}

impl Decl {
    fn print(&self, ident: usize) {
        let prefix = "  ".repeat(ident);
        match self {
            Decl::Var {
                name,
                var_type,
                val,
            } => {
                println!("{}Var {}: {:?}", prefix, name, var_type);
                if let Some(v) = val {
                    println!("{}Value: ", prefix);
                    v.print(ident + 1);
                }
            }
            Decl::Func {
                name,
                return_type,
                args,
            } => {
                println!("{}Function {} -> {:?}", prefix, name, return_type);
                println!("{}Args: ", prefix);
                for (arg_name, arg_type) in args {
                    println!("{} {} {:?}", prefix, arg_name, arg_type);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiplication,
    Division,
    BitOr,
    BitAnd,
    BitNot,
}

#[derive(Debug)]
pub enum Expr {
    Literal {
        val: LiteralValue,
    },
    Var {
        name: String,
    },
    UnaryExpression {
        operand: Box<Expr>,
        operator: Operator,
    },
    BinaryExpression {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Operator,
    },
    FunctionCall {
        name: String,
        args: Vec<Box<Expr>>,
    },
}

impl Expr {
    fn print(&self, ident: usize) {
        let prefix = "  ".repeat(ident);
        match self {
            Expr::Literal { val } => println!("{}Literal: {:?}", prefix, val),
            Expr::Var { name } => println!("{}Var: {}", prefix, name),
            Expr::UnaryExpression { operand, operator } => {
                println!("{}Unary: {:?}", prefix, operator);
                operand.print(ident + 1);
            }
            Expr::BinaryExpression {
                left,
                right,
                operator,
            } => {
                println!("{}Binary: {:?}", prefix, operator);
                left.print(ident + 1);
                right.print(ident + 1);
            }
            Expr::FunctionCall { name, args } => {
                println!("{}Call: {:?}", prefix, name);
                println!("{}Args:", prefix);
                for e in args {
                    e.print(ident + 1);
                }
            }
        }
    }
}
