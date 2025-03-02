use std::{env::args, fs::File, io::Read, time::Instant};

use parse::{ast::LanternFile, error::InvalidTokenError, lex::lex, Parse};
use runtime::LanternRuntime;

fn main() {
    let Some(file) = args().nth(1) else {
        eprint!("missing file");
        return;
    };

    let Ok(mut file) = File::open(&file) else {
        eprintln!("file not found: `{file}`");
        return;
    };

    let mut content = String::new();
    if file.read_to_string(&mut content).is_err() {
        eprintln!("file contains invalid UTF-8");
        return;
    };
    let mut input = content.chars().peekable();
    let before = Instant::now();
    let tokens = match lex(&mut input) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };
    let elapsed = Instant::now().duration_since(before);
    
    if let Some(next) = input.next() {
        eprintln!("{}", InvalidTokenError(next));
        return;
    }

    println!("{tokens:#?}");
    println!("took {elapsed:?}");

    let before = Instant::now();
    let lantern_file = match LanternFile::parse(&mut tokens.into_iter().peekable()) {
        Ok(lantern_file) => lantern_file,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };
    let elapsed = Instant::now().duration_since(before);

    println!("{lantern_file:#?}");
    println!("took {elapsed:?}");

    let before = Instant::now();
    let Ok(instructions) = flame::ignite(lantern_file) else {
        eprintln!("instructions don't fit in txt space!");
        return;
    };
    let elapsed = Instant::now().duration_since(before);

    println!("{instructions:?}");
    println!("took {elapsed:?}");

    let before = Instant::now();
    let runtime: LanternRuntime<128, 512> = LanternRuntime::new(instructions);
    match runtime.exec() {
        Ok(stack) => {
            println!("Finished executing in {:?}", Instant::now().duration_since(before));
            println!("stack dump:\n{stack}");
        },
        Err(err) => eprintln!("{err}"),
    }
}

