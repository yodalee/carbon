use std::collections::HashMap;

#[derive(Debug,Hash,Eq,PartialEq)]
pub enum CType {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Custom(String)
}

#[derive(Debug)]
pub enum CastTop {
    VarDecList(Vec<CastDecl>),
    FuncDeclList(Vec<FuncDecl>)
}

#[derive(Debug)]
pub enum CastDecl {
    TypeDecl(String, CType),
    VarDecl(String, CType, Option<CastStmt>)
}

pub enum CastExpr {
}

#[derive(Debug)]
pub enum CastOperator {
    Add, Sub, Mul, Div, Negate,
    LT, GT, LEQ, GEQ, EQ, NEQ,
    AND, OR, Not, BAND, BOR,
    BXOR, BNOT
}

#[derive(Debug)]
pub enum CastStmt {
    Block(Vec<CastStmt>, Vec<CastDecl>),
    Compound(Vec<CastStmt>),
    Return(Option<Box<CastStmt>>),
    Identifier(String),
    Expression(CastOperator, Box<CastStmt>, Box<CastStmt>),
    If(Box<CastStmt>, Box<CastStmt>, Box<CastStmt>),
    None
}

#[derive(Debug)]
pub struct FuncDecl {
    FuncName: String,
    FuncArgs: HashMap<CType, String>,
    FuncCode: CastStmt,
    FuncRetType: CType
}

impl FuncDecl {
    pub fn new(FuncName: &str,
           FuncArgs: HashMap<CType, String>,
           FuncCode: CastStmt,
           FuncRetType: CType) -> FuncDecl {
        FuncDecl {
            FuncName: FuncName.to_string(),
            FuncArgs: FuncArgs,
            FuncCode: FuncCode,
            FuncRetType: FuncRetType
        }
    }
}
