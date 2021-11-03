mod lexer;
mod parser;

use std::{fs::File, io::Read};

use clap::Parser;
use parser::Parser as IoParser;

use crate::lexer::{Lexer, token::Token};

#[derive(Parser, Debug)]
#[clap(name = "iodine", version = "1.0")]
struct Opts {
    /// Name of the person to greet
    #[clap(short, long)]
    script: String,
}

fn main() -> std::io::Result<()> {
    let opts = Opts::parse();
    let mut file = File::open(opts.script)?;
    let mut script_content = String::new();
    file.read_to_string(&mut script_content)?;
    
    let mut p = IoParser::new(Lexer::new(&script_content)).map_err(|err| err.to_string()).unwrap();
    let prog = p.parse().map_err(|err| err.to_string()).unwrap();
    for s in &prog {
        println!("{:#?}", s);
    }

    Ok(())
}

fn lex(input: &str) -> Result<(), String> {
    let mut l = Lexer::new(input);

    let tokens = l.lex().map_err(|err| err.to_string())?;

    for t in &tokens {
        match t {
            Token::Eof => {
                break;
            }
            Token::Illegal(ill) => {
                return Err(format!("illegal token: {}", ill));
            }
            _ => {
                println!("  - {:?}", t);
            }
        };
    }

    println!();
    Ok(())
}