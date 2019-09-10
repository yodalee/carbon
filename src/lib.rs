extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

#[macro_use]
mod helper;
pub mod parse;
mod grammar;
mod ast;
mod climb;

#[cfg(test)]
mod tests {

    use super::grammar::{CParser, Rule};
    use pest::{Parser};

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

    #[test]
    fn test_number() {
        assert!(can_parse(Rule::integer_constant, "123"));
        assert!(!can_parse(Rule::integer_constant, "11a"));
        assert!(can_parse(Rule::integer_constant, "067"));
        assert!(!can_parse(Rule::integer_constant, "080"));
        assert!(can_parse(Rule::integer_constant, "0x1234567890abcdef"));
        assert!(!can_parse(Rule::integer_constant, "0x"));
        assert!(!can_parse(Rule::integer_constant, "0xxx"));
    }
}
