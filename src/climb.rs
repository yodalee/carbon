use pest::prec_climber::{Assoc, PrecClimber, Operator};
use pest::iterators::{Pair};
use super::grammar::{Rule};

use super::ast::*;
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
        match pair.as_rule() {
            Rule::op_comma       => CastStmt::Expression(CastOperator::COMMA      , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign      => CastStmt::Expression(CastOperator::ASSIGN     , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_add  => CastStmt::Expression(CastOperator::ASSIGNADD  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_sub  => CastStmt::Expression(CastOperator::ASSIGNSUB  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_mul  => CastStmt::Expression(CastOperator::ASSIGNMUL  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_div  => CastStmt::Expression(CastOperator::ASSIGNDIV  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_mod  => CastStmt::Expression(CastOperator::ASSIGNMOD  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_lsh  => CastStmt::Expression(CastOperator::ASSIGNLSH  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_rsh  => CastStmt::Expression(CastOperator::ASSIGNRSH  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_band => CastStmt::Expression(CastOperator::ASSIGNBAND , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_bor  => CastStmt::Expression(CastOperator::ASSIGNBOR  , Box::new(lhs), Box::new(rhs)),
            Rule::op_assign_bxor => CastStmt::Expression(CastOperator::ASSIGNBXOR , Box::new(lhs), Box::new(rhs)),
            Rule::op_or   => CastStmt::Expression(CastOperator::OR   , Box::new(lhs), Box::new(rhs)),
            Rule::op_and  => CastStmt::Expression(CastOperator::AND  , Box::new(lhs), Box::new(rhs)),
            Rule::op_bor  => CastStmt::Expression(CastOperator::BOR  , Box::new(lhs), Box::new(rhs)),
            Rule::op_bxor => CastStmt::Expression(CastOperator::BXOR , Box::new(lhs), Box::new(rhs)),
            Rule::op_band => CastStmt::Expression(CastOperator::BAND , Box::new(lhs), Box::new(rhs)),
            Rule::op_eq   => CastStmt::Expression(CastOperator::EQ   , Box::new(lhs), Box::new(rhs)),
            Rule::op_ne   => CastStmt::Expression(CastOperator::NE   , Box::new(lhs), Box::new(rhs)),
            Rule::op_gt   => CastStmt::Expression(CastOperator::GT   , Box::new(lhs), Box::new(rhs)),
            Rule::op_lt   => CastStmt::Expression(CastOperator::LT   , Box::new(lhs), Box::new(rhs)),
            Rule::op_ge   => CastStmt::Expression(CastOperator::GE   , Box::new(lhs), Box::new(rhs)),
            Rule::op_le   => CastStmt::Expression(CastOperator::LE   , Box::new(lhs), Box::new(rhs)),
            Rule::op_lsh  => CastStmt::Expression(CastOperator::LSH  , Box::new(lhs), Box::new(rhs)),
            Rule::op_rsh  => CastStmt::Expression(CastOperator::RSH  , Box::new(lhs), Box::new(rhs)),
            Rule::op_add  => CastStmt::Expression(CastOperator::ADD  , Box::new(lhs), Box::new(rhs)),
            Rule::op_sub  => CastStmt::Expression(CastOperator::SUB  , Box::new(lhs), Box::new(rhs)),
            Rule::op_mul  => CastStmt::Expression(CastOperator::MUL  , Box::new(lhs), Box::new(rhs)),
            Rule::op_div  => CastStmt::Expression(CastOperator::DIV  , Box::new(lhs), Box::new(rhs)),
            Rule::op_mod  => CastStmt::Expression(CastOperator::MOD  , Box::new(lhs), Box::new(rhs)),
            _ => parse_fail!(pair),
        }
    }

    pub fn climb(&self, pair: Pair<Rule>) -> CastStmt {
        PREC_CLIMBER.climb(pair.into_inner(),
                           |pair| self.build_postfix(pair),
                           |lhs, pair, rhs| self.infix_rule(lhs, pair, rhs))
    }
}
