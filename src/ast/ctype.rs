use super::cast::CastStmt;

#[derive(Debug,Clone)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Debug,Clone)]
pub enum CType {
    Void,
    Char(Sign),
    Short(Sign),
    Int(Sign),
    Long(Sign),
    LongLong(Sign),
    Float,
    Double,
    Array(Box<CType>, Box<CastStmt>),
    Func(Box<CType>, Vec<CType>), // return type, param type
    Custom(String)
}
