use std::collections::HashMap;

#[derive(Debug)]
pub enum Pos {
    NoPos,
    Pos { line_no: usize, col_no: usize, s: String }
}

#[derive(Debug,Hash,Eq,PartialEq,Clone)]
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
    Identifier(Pos, String),
    Literal(Pos, CastLiteral),
    Expression(Pos, CastOperator, Box<CastStmt>, Box<CastStmt>),
    If(Pos, Box<CastStmt>, Box<CastStmt>, Box<Option<CastStmt>>),
    While { pos: Pos, whileCond: Box<CastStmt>, whileCode: Box<CastStmt> },
    Do    { pos: Pos, doCode: Box<CastStmt>, doCond: Box<CastStmt> },
    For { pos: Pos,
          forInit: Box<Option<CastStmt>>,
          forCond: Box<Option<CastStmt>>,
          forIter: Box<Option<CastStmt>>,
          forCode: Box<CastStmt> },
    Call(Pos, Box<CastStmt>, Vec<CastStmt>),     // foo(a1, a2, ...)
    ArrayRef(Pos, Box<CastStmt>, Box<CastStmt>), // ArrayRef (Identifier("a")) (Literal(IntLiteral(10)))
    Return(Pos, Box<Option<CastStmt>>),
    Break(Pos),
    Continue(Pos),
    None
}

impl CastStmt {
    pub fn new_for(pos: Pos,
                   forInit: Option<CastStmt>,
                   forCond: Option<CastStmt>,
                   forIter: Option<CastStmt>,
                   forCode: CastStmt) -> CastStmt {
        CastStmt::For {
            pos: pos,
            forInit: Box::new(forInit),
            forCond: Box::new(forCond),
            forIter: Box::new(forIter),
            forCode: Box::new(forCode)
        }
    }
    pub fn new_while(pos: Pos,
                     whileCond: CastStmt,
                     whileCode: CastStmt) -> CastStmt {
        CastStmt::While {
            pos: pos,
            whileCond: Box::new(whileCond),
            whileCode: Box::new(whileCode)
        }
    }
    pub fn new_do(pos: Pos,
                  doCode: CastStmt,
                  doCond: CastStmt) -> CastStmt {
        CastStmt::Do {
            pos: pos,
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
