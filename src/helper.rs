use pest::iterators::{Pair};
use grammar::Rule;

pub fn iterate_rules(pair: Pair<Rule>, indent: usize) {
    let indentstr = " ".repeat(indent);
    println!("{}Rule: {:?}", indentstr, pair.as_rule());
    println!("{}span: {}", indentstr, pair.as_str().replace("\n", " "));
    for innerpair in pair.into_inner() {
        iterate_rules(innerpair, indent+2);
    }
}

#[macro_export]
macro_rules! parse_fail {
  ( $x:expr ) => {
    {
      let rule = $x.as_rule();
      let s = $x.as_str();
      unreachable!("unexpected rule {:?} with content {}", rule, s)
    }
  }
}

