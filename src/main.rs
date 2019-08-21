extern crate pest;
extern crate carbon;

use pest::Parser;

use carbon::helper::{iterate_rules};
use carbon::grammar::{CParser, Rule};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;

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
