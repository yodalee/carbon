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

#[derive(Debug)]
pub enum CastOperator {
    COMMA,
    ASSIGN, ASSIGNADD, ASSIGNSUB, ASSIGNMUL, ASSIGNDIV,
    ASSIGNMOD, ASSIGNLSH, ASSIGNRSH, ASSIGNBAND,
    ASSIGNBOR, ASSIGNBXOR,
    ADD, SUB, MUL, DIV, MOD,
    LE, GE, LT, GT, NE, EQ,
    LSH, RSH,
    AND, OR, BAND, BOR, BXOR
}

#[derive(Debug)]
pub enum CastLiteral {
    IntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
}

#[derive(Debug)]
pub enum CastStmt {
    Block(Vec<CastStmt>, Vec<CastDecl>),
    Compound(Vec<CastStmt>),
    Return(Option<Box<CastStmt>>),
    Identifier(String),
    Literal(CastLiteral),
    Expression(CastOperator, Box<CastStmt>, Box<CastStmt>),
    If(Box<CastStmt>, Box<CastStmt>, Box<CastStmt>),
    Call(Box<CastStmt>, Vec<CastStmt>),     // foo(a1, a2, ...)
    ArrayRef(Box<CastStmt>, Box<CastStmt>), // ArrayRef (Identifier("a")) (Literal(IntLiteral(10)))
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
