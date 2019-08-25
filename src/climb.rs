use pest::prec_climber::{Assoc, PrecClimber, Operator};
use pest::iterators::{Pair};
use super::grammar::{Rule};

use super::ast::*;
use super::parse::{build_primary};

fn infix_rule(lhs: CastStmt, pair: Pair<Rule>, rhs: CastStmt) -> CastStmt {
    match pair.as_rule() {
        Rule::op_or   => CastStmt::Expression(CastBinaryOperator::OR   , Box::new(lhs), Box::new(rhs)),
        Rule::op_and  => CastStmt::Expression(CastBinaryOperator::AND  , Box::new(lhs), Box::new(rhs)),
        Rule::op_bor  => CastStmt::Expression(CastBinaryOperator::BOR  , Box::new(lhs), Box::new(rhs)),
        Rule::op_bxor => CastStmt::Expression(CastBinaryOperator::BXOR , Box::new(lhs), Box::new(rhs)),
        Rule::op_band => CastStmt::Expression(CastBinaryOperator::BAND , Box::new(lhs), Box::new(rhs)),
        Rule::op_eq   => CastStmt::Expression(CastBinaryOperator::EQ   , Box::new(lhs), Box::new(rhs)),
        Rule::op_ne   => CastStmt::Expression(CastBinaryOperator::NE   , Box::new(lhs), Box::new(rhs)),
        Rule::op_gt   => CastStmt::Expression(CastBinaryOperator::GT   , Box::new(lhs), Box::new(rhs)),
        Rule::op_lt   => CastStmt::Expression(CastBinaryOperator::LT   , Box::new(lhs), Box::new(rhs)),
        Rule::op_ge   => CastStmt::Expression(CastBinaryOperator::GE   , Box::new(lhs), Box::new(rhs)),
        Rule::op_le   => CastStmt::Expression(CastBinaryOperator::LE   , Box::new(lhs), Box::new(rhs)),
        Rule::op_lsh  => CastStmt::Expression(CastBinaryOperator::LSH  , Box::new(lhs), Box::new(rhs)),
        Rule::op_rsh  => CastStmt::Expression(CastBinaryOperator::RSH  , Box::new(lhs), Box::new(rhs)),
        Rule::op_add  => CastStmt::Expression(CastBinaryOperator::ADD  , Box::new(lhs), Box::new(rhs)),
        Rule::op_sub  => CastStmt::Expression(CastBinaryOperator::SUB  , Box::new(lhs), Box::new(rhs)),
        Rule::op_mul  => CastStmt::Expression(CastBinaryOperator::MUL  , Box::new(lhs), Box::new(rhs)),
        Rule::op_div  => CastStmt::Expression(CastBinaryOperator::DIV  , Box::new(lhs), Box::new(rhs)),
        Rule::op_mod  => CastStmt::Expression(CastBinaryOperator::MOD  , Box::new(lhs), Box::new(rhs)),
        _ => parse_fail!(pair),
    }
}

fn build_precedence_climb() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
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

lazy_static! [
    static ref PREC_CLIMBER : PrecClimber<Rule> = build_precedence_climb();
];

pub fn climb(pair: Pair<Rule>) -> CastStmt {
    PREC_CLIMBER.climb(pair.into_inner(), build_primary, infix_rule)
}
