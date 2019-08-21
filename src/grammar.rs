#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("c.pest");

#[derive(Parser)]
#[grammar = "c.pest"]
pub struct CParser;
