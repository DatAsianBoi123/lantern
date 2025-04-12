use std::{fs::File, io::Read, time::Instant};

use clap::Parser;
use parse::{ast::LanternFile, lex::lex, Parse};
use runtime::LanternRuntime;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    file: String,

    #[arg(short, long)]
    verbose: bool,
    #[arg(short, long)]
    no_run: bool,
}

fn main() {
    let Args { file: file_name, verbose, no_run } = Args::parse();

    let Ok(mut file) = File::open(&file_name) else {
        eprintln!("file not found: `{file_name}`");
        return;
    };

    let mut content = String::new();
    if file.read_to_string(&mut content).is_err() {
        eprintln!("file contains invalid UTF-8");
        return;
    };
    let mut input = content.chars().peekable();
    let before_compile = Instant::now();
    let before = Instant::now();
    let tokens = match lex(&mut input) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    if let Some(next) = input.next() {
        eprintln!("invalid token {next}");
        return;
    };

    if verbose {
        println!("{tokens:#?}");
        println!("took {:?}", Instant::now().duration_since(before));
    }

    let before = Instant::now();
    let lantern_file = match LanternFile::parse(&mut tokens.into_iter().peekable()) {
        Ok(lantern_file) => lantern_file,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    if verbose {
        println!("{lantern_file:#?}");
        println!("took {:?}", Instant::now().duration_since(before));
    }

    let before = Instant::now();
    let Ok(instructions) = flame::ignite(lantern_file) else {
        eprintln!("instructions don't fit in txt space!");
        return;
    };

    if verbose {
        println!("{instructions:?}");
        println!("took {:?}", Instant::now().duration_since(before));
    }

    println!("finished compiling in {:?}", Instant::now().duration_since(before_compile));

    if no_run { return; }

    let runtime: LanternRuntime<256, 512> = LanternRuntime::new(instructions);
    println!("running {file_name}");
    let before = Instant::now();
    match runtime.exec() {
        Ok(stack) => {
            println!("Finished executing in {:?}", Instant::now().duration_since(before));
            if verbose {
                println!("stack dump:\n{stack}");
            }
        },
        Err(err) => eprintln!("{err}"),
    }
}

