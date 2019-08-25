extern crate pest;

use pest::Parser;
use pest::iterators::{Pair};

use super::helper::{iterate_rules};
use super::grammar::{CParser, Rule};
use super::ast::*;

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
    let body = build_statement(inner.next().unwrap());
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
    match pair.as_rule() {
        Rule::compound_stat => {
            CastStmt::Compound(pair.into_inner().map(|pair| build_block(pair)).collect())
        }
        _ => parse_fail!(pair),
    }
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
    let expr = None;
    CastDecl::VarDecl(id.to_string(), typ, expr)
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
