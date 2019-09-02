extern crate pest;

use pest::Parser;
use pest::iterators::{Pair};

use super::helper::{iterate_rules};
use super::grammar::{CParser, Rule};
use super::ast::*;
use super::climb::climb;

use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;

pub fn parse_program_file(path: &str) {
    let mut f = File::open(path).expect(&format!("file {} not found", path));
    let mut content = String::new();
    f.read_to_string(&mut content).expect(&format!("Error in reading file {}", path));
    let ast = parse_program_text(&content);
    println!("{:?}", ast);
}

fn parse_program_text(content: &str) -> CastTop {
    let pairs = CParser::parse(Rule::program, content)
                    .unwrap_or_else(|e| panic!("{}", e))
                    .next().unwrap();
    iterate_rules(pairs.clone(), 0);
    build_program(pairs)
}

fn build_program(pair: Pair<Rule>) -> CastTop {
    let funcdels = pair.into_inner()
                       .filter(|pair| pair.as_rule() == Rule::function_decl)
                       .map(build_function)
                       .collect();
    CastTop::FuncDeclList(funcdels)
}

fn build_function(pair: Pair<Rule>) -> FuncDecl {
    let mut inner = pair.into_inner();
    let rettype = build_type(inner.next().unwrap());
    let name = inner.next().unwrap().as_str();
    inner.next().unwrap();
    let args = HashMap::new();
    let body = build_compound_statement(inner.next().unwrap());
    FuncDecl::new(name, args, body, rettype)
}

fn build_type(pair: Pair<Rule>) -> CType {
    let typename = pair.as_str();
    match typename.as_ref() {
        "int" => CType::Int,
        "float" => CType::Float,
        &_ => panic!(format!("Unknown Type {}", typename)),
    }
}

fn build_statement(pair: Pair<Rule>) -> CastStmt {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::compound_stat => build_compound_statement(pair),
        Rule::if_stat => {
            let mut inner = pair.into_inner();
            let condition = Box::new(climb(inner.next().unwrap()));
            let ifcode = Box::new(build_statement(inner.next().unwrap()));
            let elsecode = Box::new(match inner.next() {
                Some(stmt) => Some(build_statement(stmt)),
                None => None,
            });
            CastStmt::If(condition, ifcode, elsecode)
        }
        Rule::while_stat => {
            let mut inner = pair.into_inner();
            let whilecond = climb(inner.next().unwrap());
            let whilecode = build_statement(inner.next().unwrap());
            CastStmt::new_while(whilecond, whilecode)
        }
        Rule::do_stat => {
            let mut inner = pair.into_inner();
            let docode = build_statement(inner.next().unwrap());
            let docond = climb(inner.next().unwrap());
            CastStmt::new_do(docond, docode)
        }
        Rule::return_stat => {
            let expr = Box::new(match pair.into_inner().next() {
                Some(expr) => Some(climb(expr)),
                None => None
            });
            CastStmt::Return(expr)
        }
        Rule::break_stat => CastStmt::Break,
        Rule::cont_stat => CastStmt::Continue,
        Rule::expression_stat => {
            match pair.into_inner().next() {
                Some(expr) => climb(expr),
                None => CastStmt::None,
            }
        },
        _ => parse_fail!(pair),
    }
}

fn build_compound_statement(pair: Pair<Rule>) -> CastStmt {
    CastStmt::Compound(pair.into_inner().map(|pair| build_block(pair)).collect())
}

fn build_block(pair: Pair<Rule>) -> CastStmt {
    let mut decls = vec![];
    let mut stmts = vec![];
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::declaration => decls.push(build_declaration(pair)),
            Rule::statement   => stmts.push(build_statement(pair)),
            _ => parse_fail!(pair),
        }
    }
    CastStmt::Block(stmts, decls)
}

fn build_declaration(pair: Pair<Rule>) -> CastDecl {
    let mut inner = pair.into_inner();
    let typ  = build_type(inner.next().unwrap());
    let id   = inner.next().unwrap().as_str();
    let expr = match inner.next() {
        Some(expr) => Some(climb(expr)),
        None => None,
    };
    CastDecl::VarDecl(id.to_string(), typ, expr)
}

pub fn helper_fold_postfix(primary: CastStmt, pair: Pair<Rule>) -> CastStmt {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::postfix_array => {
            let expr = pair.into_inner().next().unwrap();
            CastStmt::ArrayRef(Box::new(primary), Box::new(climb(expr)))
        }
        Rule::postfix_call => CastStmt::Call(Box::new(primary), Vec::new()),
        _ => parse_fail!(pair),
    }
}

pub fn build_postfix(pair: Pair<Rule>) -> CastStmt {
    let mut inner = pair.into_inner();
    let primary = build_primary_expr(inner.next().unwrap());
    inner.fold(primary, |primary, postfix| helper_fold_postfix(primary, postfix))
}

pub fn build_primary_expr(pair: Pair<Rule>) -> CastStmt {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::identifier => CastStmt::Identifier(pair.as_str().to_string()),
        Rule::constant   => CastStmt::Literal(build_constant(pair)),
        Rule::string_lit => CastStmt::Literal(build_constant(pair)),
        Rule::expression => climb(pair),
        _ => parse_fail!(pair),
    }
}

fn build_constant(pair: Pair<Rule>) -> CastLiteral {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::integer_constant => build_integer(pair),
        Rule::string_lit => CastLiteral::StringLiteral(pair.as_str().to_string()),
        _ => parse_fail!(pair),
    }
}

fn build_integer(pair: Pair<Rule>) -> CastLiteral {
    let pair = pair.into_inner().next().unwrap();
    let radix = match pair.as_rule() {
        Rule::octal_constant => 8,
        Rule::decimal_constant => 10,
        Rule::hexadecimal_constant => 16,
        _ => parse_fail!(pair),
    };
    let s = pair.as_str();
    CastLiteral::IntLiteral(u64::from_str_radix(s, radix)
                                .expect(&format!("Parse integere failed on string {} with radix {}", s, radix)))
}

#[cfg(test)]
mod tests {
    use super::*;

    // test rule can match content exactly
    fn can_parse(rule: Rule, content: &str) -> bool {
        match CParser::parse(rule, content) {
            Err(_) => false,
            Ok(mut pair) => {
                let parse_str = pair.next().unwrap().as_str();
                println!("{:?} match {}", rule, parse_str);
                parse_str == content
            },
        }
    }

    #[test]
    fn test_identifier() {
        assert!(can_parse(Rule::identifier, "a123_"));
        assert!(!can_parse(Rule::identifier, "123"));
        assert!(can_parse(Rule::identifier, "_039"));
    }
}
