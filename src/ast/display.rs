use super::ctype::{CType, Sign};
use super::cast::*;
use super::helper::stringfy;
use std::fmt;

impl fmt::Display for CastTop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CastTop::VarDecList(_v) => write!(f, ""),
            CastTop::FuncDeclList(v) => write!(f, "{}", stringfy(v, "\n")),
        }
    }
}

impl fmt::Display for FuncDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.fun_name, self.fun_type)
    }
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sign::Signed => write!(f, "i"),
            Sign::Unsigned => write!(f, "u"),
        }
    }
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CType::Void => write!(f, "()"),
            CType::Char(sign)  => write!(f, "{}{}", sign, 8),
            CType::Short(sign) => write!(f, "{}{}", sign, 16),
            CType::Int(sign)   => write!(f, "{}{}", sign, 32),
            CType::Long(sign)  => write!(f, "{}{}", sign, 64),
            CType::LongLong(sign) => write!(f, "{}{}", sign, 64),
            CType::Float      => write!(f, "f32"),
            CType::Double     => write!(f, "f64"),
            CType::Array(typ, _) => write!(f, "{}[]", typ),
            CType::Func(ret, args)  => {
                write!(f, "fun ({}) -> {}", stringfy(args, "\n"), ret)
            },
            _ => panic!(),
        }
    }
}
