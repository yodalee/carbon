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
    Identifier(String),
    Literal(CastLiteral),
    Expression(CastOperator, Box<CastStmt>, Box<CastStmt>),
    If(Box<CastStmt>, Box<CastStmt>, Box<Option<CastStmt>>),
    While { whileCond: Box<CastStmt>, whileCode: Box<CastStmt> },
    Do    { doCode: Box<CastStmt>, doCond: Box<CastStmt> },
    For { forInit: Box<CastStmt>,
          forCond: Box<CastStmt>,
          forIter: Box<CastStmt>,
          forCode: Box<CastStmt> },
    Call(Box<CastStmt>, Vec<CastStmt>),     // foo(a1, a2, ...)
    ArrayRef(Box<CastStmt>, Box<CastStmt>), // ArrayRef (Identifier("a")) (Literal(IntLiteral(10)))
    Return(Box<Option<CastStmt>>),
    Break,
    Continue,
    None
}

impl CastStmt {
    pub fn new_for(forInit: CastStmt,
               forCond: CastStmt,
               forIter: CastStmt,
               forCode: CastStmt) -> CastStmt {
        CastStmt::For {
            forInit: Box::new(forInit),
            forCond: Box::new(forCond),
            forIter: Box::new(forIter),
            forCode: Box::new(forCode)
        }
    }
    pub fn new_while(whileCond: CastStmt,
                     whileCode: CastStmt) -> CastStmt {
        CastStmt::While {
            whileCond: Box::new(whileCond),
            whileCode: Box::new(whileCode)
        }
    }
    pub fn new_do(doCode: CastStmt,
                  doCond: CastStmt) -> CastStmt {
        CastStmt::Do {
            doCode: Box::new(doCode),
            doCond: Box::new(doCond)
        }
    }
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
