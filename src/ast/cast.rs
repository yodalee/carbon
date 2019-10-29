use super::ctype::CType;

#[derive(Debug,Clone)]
pub enum Pos {
    NoPos,
    Pos { line_no: usize, col_no: usize, s: String }
}

#[derive(Debug)]
pub enum CastTop {
    VarDecList(Vec<CastDecl>),
    FuncDeclList(Vec<FuncDecl>)
}

#[derive(Debug,Clone)]
pub enum CastDecl {
    TypeDecl(String, CType),
    VarDecl(String, CType, Option<CastStmt>)
}

#[derive(Debug,Clone)]
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

#[derive(Debug,Clone)]
pub enum CastLiteral {
    IntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
}

#[derive(Debug,Clone)]
pub enum CastStmt {
    Block(Vec<CastStmt>, Vec<CastDecl>),
    Compound(Vec<CastStmt>),
    Identifier(Pos, String),
    Literal(Pos, CastLiteral),
    Expression(Pos, CastOperator, Box<CastStmt>, Box<CastStmt>),
    If(Pos, Box<CastStmt>, Box<CastStmt>, Box<Option<CastStmt>>),
    While { pos: Pos,
            while_cond: Box<CastStmt>,
            while_code: Box<CastStmt> },
    Do    { pos: Pos,
            do_code: Box<CastStmt>,
            do_cond: Box<CastStmt> },
    For { pos: Pos,
          for_init: Box<Option<CastStmt>>,
          for_cond: Box<Option<CastStmt>>,
          for_iter: Box<Option<CastStmt>>,
          for_code: Box<CastStmt> },
    Call(Pos, Box<CastStmt>, Vec<CastStmt>),     // foo(a1, a2, ...)
    ArrayRef(Pos, Box<CastStmt>, Box<CastStmt>), // ArrayRef (Identifier("a")) (Literal(IntLiteral(10)))
    Return(Pos, Box<Option<CastStmt>>),
    Break(Pos),
    Continue(Pos),
    None
}

impl CastStmt {
    pub fn new_for(pos: Pos,
                   for_init: Option<CastStmt>,
                   for_cond: Option<CastStmt>,
                   for_iter: Option<CastStmt>,
                   for_code: CastStmt) -> CastStmt {
        CastStmt::For {
            pos: pos,
            for_init: Box::new(for_init),
            for_cond: Box::new(for_cond),
            for_iter: Box::new(for_iter),
            for_code: Box::new(for_code)
        }
    }
    pub fn new_while(pos: Pos,
                     while_cond: CastStmt,
                     while_code: CastStmt) -> CastStmt {
        CastStmt::While {
            pos: pos,
            while_cond: Box::new(while_cond),
            while_code: Box::new(while_code)
        }
    }
    pub fn new_do(pos: Pos,
                  do_code: CastStmt,
                  do_cond: CastStmt) -> CastStmt {
        CastStmt::Do {
            pos: pos,
            do_code: Box::new(do_code),
            do_cond: Box::new(do_cond)
        }
    }
}

#[derive(Debug)]
pub struct FuncDecl {
    fun_type: CType,
    fun_name: String,
    fun_args: Vec<String>,
    fun_code: CastStmt,
}

impl FuncDecl {
    pub fn new(fun_type: CType,
               fun_name: String,
               fun_args: Vec<String>,
               fun_code: CastStmt) -> FuncDecl {
        FuncDecl {
            fun_type: fun_type,
            fun_name: fun_name.to_string(),
            fun_args: fun_args,
            fun_code: fun_code,
        }
    }
}
