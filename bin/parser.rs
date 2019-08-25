extern crate carbon;

use carbon::parse::{parse_program_file};

use std::process;
use std::env;

fn main() {
    let args : Vec<_> = env::args().collect();
    if args.len() == 0 {
        eprintln!("Usage: carbon <file> ...");
        process::exit(1);
    }
    for arg in args.iter().skip(1) {
        parse_program_file(&arg);
    }
}

