extern crate pest;
extern crate carbon;

use pest::Parser;
use pest::iterators::{Pair};

use carbon::helper::{iterate_rules};
use carbon::grammar::{CParser, Rule};
use carbon::ast::*;
use carbon::parse_fail;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;
use std::collections::HashMap;

fn main() {
    let args : Vec<_> = env::args().collect();
    if args.len() == 0 {
        eprintln!("Usage: carbon <file> ...");
        process::exit(1);
    }
    for arg in args.iter().skip(1) {
        parse_program_file(&arg)
    }
}

fn parse_program_file(path: &str) {
    let mut f = File::open(path).expect(&format!("file {} not found", path));
    let mut content = String::new();
    f.read_to_string(&mut content).expect(&format!("Error in reading file {}", path));
    parse_program_text(&content);
}

fn parse_program_text(content: &str) {
    let pairs = CParser::parse(Rule::program, content)
                    .unwrap_or_else(|e| panic!("{}", e))
                    .next().unwrap();
    iterate_rules(pairs.clone(), 0);
    let cast = build_program(pairs);
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
    let args = HashMap::new();
    let body = CastStmt::None;
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
