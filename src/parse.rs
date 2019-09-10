extern crate pest;

use pest::{Parser};
use pest::iterators::{Pair};

use super::helper::{iterate_rules};
use super::grammar::{CParser, Rule};
use super::ast::*;

use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;

pub struct ASTBuilder {
}

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
    let mut builder = ASTBuilder{};
    builder.build_program(pairs)
}

impl ASTBuilder {
    pub fn build_program(&mut self, pair: Pair<Rule>) -> CastTop {
        let funcdels = pair.into_inner()
                           .filter(|pair| pair.as_rule() == Rule::function_decl)
                           .map(|pair| self.build_function(pair))
                           .collect();
        CastTop::FuncDeclList(funcdels)
    }

    fn build_function(&mut self, pair: Pair<Rule>) -> FuncDecl {
        let mut inner = pair.into_inner();
        let rettype = self.build_type(inner.next().unwrap());
        let name = inner.next().unwrap().as_str();
        inner.next().unwrap();
        let args = HashMap::new();
        let body = self.build_compound_statement(inner.next().unwrap());
        FuncDecl::new(name, args, body, rettype)
    }

    fn build_type(&self, pair: Pair<Rule>) -> CType {
        let typename = pair.as_str();
        match typename.as_ref() {
            "int" => CType::Int,
            "float" => CType::Float,
            &_ => panic!(format!("Unknown Type {}", typename)),
        }
    }

    fn build_statement(&mut self, pair: Pair<Rule>) -> CastStmt {
        let pair = pair.into_inner().next().unwrap();
        let pos = self.derive_pos(&pair);
        match pair.as_rule() {
            Rule::compound_stat => self.build_compound_statement(pair),
            Rule::if_stat => {
                let mut inner = pair.into_inner();
                let condition = Box::new(self.climb(inner.next().unwrap()));
                let ifcode = Box::new(self.build_statement(inner.next().unwrap()));
                let elsecode = Box::new(match inner.next() {
                    Some(stmt) => Some(self.build_statement(stmt)),
                    None => None,
                });
                CastStmt::If(pos, condition, ifcode, elsecode)
            }
            Rule::while_stat => {
                let mut inner = pair.into_inner();
                let whilecond = self.climb(inner.next().unwrap());
                let whilecode = self.build_statement(inner.next().unwrap());
                CastStmt::new_while(pos, whilecond, whilecode)
            }
            Rule::do_stat => {
                let mut inner = pair.into_inner();
                let docode = self.build_statement(inner.next().unwrap());
                let docond = self.climb(inner.next().unwrap());
                CastStmt::new_do(pos, docond, docode)
            }
            Rule::for_stat => {
                let build_opt_expr = |pair: Pair<Rule>| -> Option<CastStmt> {
                    match pair.as_rule() {
                        Rule::expression => Some(self.climb(pair)),
                        _ => None,
                    }
                };
                let mut inner = pair.into_inner();
                let forInit = build_opt_expr(inner.next().unwrap());
                let forCond = build_opt_expr(inner.next().unwrap());
                let forIter = build_opt_expr(inner.next().unwrap());
                let forCode = self.build_statement(inner.next().unwrap());
                CastStmt::new_for(pos, forInit, forCond, forIter, forCode)
            }
            Rule::return_stat => {
                let expr = Box::new(match pair.into_inner().next() {
                    Some(expr) => Some(self.climb(expr)),
                    None => None
                });
                CastStmt::Return(pos, expr)
            }
            Rule::break_stat => CastStmt::Break(pos),
            Rule::cont_stat => CastStmt::Continue(pos),
            Rule::expression_stat => {
                match pair.into_inner().next() {
                    Some(expr) => self.climb(expr),
                    None => CastStmt::None,
                }
            },
            _ => parse_fail!(pair),
        }
    }

    fn build_compound_statement(&mut self, pair: Pair<Rule>) -> CastStmt {
        CastStmt::Compound(pair.into_inner()
                               .map(|pair| self.build_block(pair))
                               .collect())
    }

    fn build_block(&mut self, pair: Pair<Rule>) -> CastStmt {
        let mut decls = vec![];
        let mut stmts = vec![];
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::declaration => decls.append(&mut self.build_declaration(pair)),
                Rule::statement   => stmts.push(self.build_statement(pair)),
                _ => parse_fail!(pair),
            }
        }
        CastStmt::Block(stmts, decls)
    }

    fn helper_create_vardecl(&self, typ: CType, pair: Pair<Rule>) -> CastDecl {
        let mut inner = pair.into_inner();
        let id   = inner.next().unwrap().as_str();
        let expr = match inner.next() {
            Some(expr) => Some(self.climb(expr)),
            None => None,
        };
        CastDecl::VarDecl(id.to_string(), typ, expr)
    }
    fn build_declaration(&self, pair: Pair<Rule>) -> Vec<CastDecl> {
        let mut inner = pair.into_inner();
        let typ  = self.build_type(inner.next().unwrap());
        inner.map(|pair| self.helper_create_vardecl(typ.clone(), pair))
             .collect()
    }

    fn helper_fold_postfix(&self, primary: CastStmt, pair: Pair<Rule>) -> CastStmt {
        let pair = pair.into_inner().next().unwrap();
        let pos  = self.derive_pos(&pair);
        match pair.as_rule() {
            Rule::postfix_array => {
                let expr = pair.into_inner().next().unwrap();
                CastStmt::ArrayRef(pos, Box::new(primary), Box::new(self.climb(expr)))
            }
            Rule::postfix_call => CastStmt::Call(pos, Box::new(primary), Vec::new()),
            _ => parse_fail!(pair),
        }
    }

    pub fn build_postfix(&self, pair: Pair<Rule>) -> CastStmt {
        let mut inner = pair.into_inner();
        let primary = self.build_primary_expr(inner.next().unwrap());
        inner.fold(primary, |primary, postfix| self.helper_fold_postfix(primary, postfix))
    }

    fn build_primary_expr(&self, pair: Pair<Rule>) -> CastStmt {
        let pair = pair.into_inner().next().unwrap();
        let pos  = self.derive_pos(&pair);
        match pair.as_rule() {
            Rule::identifier => CastStmt::Identifier(pos, pair.as_str().to_string()),
            Rule::constant   => CastStmt::Literal(pos, self.build_constant(pair)),
            Rule::string_lit => CastStmt::Literal(pos, self.build_constant(pair)),
            Rule::expression => self.climb(pair),
            _ => parse_fail!(pair),
        }
    }

    fn build_constant(&self, pair: Pair<Rule>) -> CastLiteral {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::integer_constant => self.build_integer(pair),
            Rule::string_lit => CastLiteral::StringLiteral(pair.as_str().to_string()),
            _ => parse_fail!(pair),
        }
    }

    fn build_integer(&self, pair: Pair<Rule>) -> CastLiteral {
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

    pub fn derive_pos(&self, pair: &Pair<Rule>) -> Pos {
        let (line_no, col_no) = pair.as_span().start_pos().line_col();
        Pos::Pos {
            line_no : line_no,
            col_no : col_no,
            s : pair.as_str().to_string()
        }
    }
}

