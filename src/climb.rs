use pest::prec_climber::{Assoc, PrecClimber, Operator};
use pest::iterators::{Pair};
use super::grammar::{Rule};

use super::ast::cast::*;
use super::parse::ASTBuilder;

lazy_static! [
    static ref PREC_CLIMBER : PrecClimber<Rule> = build_precedence_climb();
];

fn build_precedence_climb() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::op_assign,      Assoc::Right) |
        Operator::new(Rule::op_assign_add,  Assoc::Right) |
        Operator::new(Rule::op_assign_sub,  Assoc::Right) |
        Operator::new(Rule::op_assign_mul,  Assoc::Right) |
        Operator::new(Rule::op_assign_div,  Assoc::Right) |
        Operator::new(Rule::op_assign_mod,  Assoc::Right) |
        Operator::new(Rule::op_assign_lsh,  Assoc::Right) |
        Operator::new(Rule::op_assign_rsh,  Assoc::Right) |
        Operator::new(Rule::op_assign_band, Assoc::Right) |
        Operator::new(Rule::op_assign_bor,  Assoc::Right) |
        Operator::new(Rule::op_assign_bxor, Assoc::Right),
        Operator::new(Rule::op_or,   Assoc::Left),
        Operator::new(Rule::op_and,  Assoc::Left),
        Operator::new(Rule::op_bor,  Assoc::Left),
        Operator::new(Rule::op_bxor, Assoc::Left),
        Operator::new(Rule::op_band, Assoc::Left),
        Operator::new(Rule::op_eq,   Assoc::Left) |
        Operator::new(Rule::op_ne,   Assoc::Left),
        Operator::new(Rule::op_gt,   Assoc::Left) |
        Operator::new(Rule::op_lt,   Assoc::Left) |
        Operator::new(Rule::op_ge,   Assoc::Left) |
        Operator::new(Rule::op_le,   Assoc::Left),
        Operator::new(Rule::op_lsh,  Assoc::Left) |
        Operator::new(Rule::op_rsh,  Assoc::Left),
        Operator::new(Rule::op_add,  Assoc::Left) |
        Operator::new(Rule::op_sub,  Assoc::Left),
        Operator::new(Rule::op_mul,  Assoc::Left) |
        Operator::new(Rule::op_div,  Assoc::Left) |
        Operator::new(Rule::op_mod,  Assoc::Left),
    ])
}

impl ASTBuilder {
    fn infix_rule(&self, lhs: CastStmt, pair: Pair<Rule>, rhs: CastStmt) -> CastStmt {
        let pos = self.derive_pos(&pair);
        let op = match pair.as_rule() {
            Rule::op_comma       => CastOperator::COMMA,
            Rule::op_assign      => CastOperator::ASSIGN,
            Rule::op_assign_add  => CastOperator::ASSIGNADD,
            Rule::op_assign_sub  => CastOperator::ASSIGNSUB,
            Rule::op_assign_mul  => CastOperator::ASSIGNMUL,
            Rule::op_assign_div  => CastOperator::ASSIGNDIV,
            Rule::op_assign_mod  => CastOperator::ASSIGNMOD,
            Rule::op_assign_lsh  => CastOperator::ASSIGNLSH,
            Rule::op_assign_rsh  => CastOperator::ASSIGNRSH,
            Rule::op_assign_band => CastOperator::ASSIGNBAND,
            Rule::op_assign_bor  => CastOperator::ASSIGNBOR,
            Rule::op_assign_bxor => CastOperator::ASSIGNBXOR,
            Rule::op_or          => CastOperator::OR,
            Rule::op_and         => CastOperator::AND,
            Rule::op_bor         => CastOperator::BOR,
            Rule::op_bxor        => CastOperator::BXOR,
            Rule::op_band        => CastOperator::BAND,
            Rule::op_eq          => CastOperator::EQ,
            Rule::op_ne          => CastOperator::NE,
            Rule::op_gt          => CastOperator::GT,
            Rule::op_lt          => CastOperator::LT,
            Rule::op_ge          => CastOperator::GE,
            Rule::op_le          => CastOperator::LE,
            Rule::op_lsh         => CastOperator::LSH,
            Rule::op_rsh         => CastOperator::RSH,
            Rule::op_add         => CastOperator::ADD,
            Rule::op_sub         => CastOperator::SUB,
            Rule::op_mul         => CastOperator::MUL,
            Rule::op_div         => CastOperator::DIV,
            Rule::op_mod         => CastOperator::MOD,
            _ => parse_fail!(pair),
        };
        CastStmt::Expression(pos, op, Box::new(lhs), Box::new(rhs))
    }

    pub fn climb(&self, pair: Pair<Rule>) -> CastStmt {
        PREC_CLIMBER.climb(pair.into_inner(),
                           |pair| self.build_postfix(pair),
                           |lhs, pair, rhs| self.infix_rule(lhs, pair, rhs))
    }
}
