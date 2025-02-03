use std::{env::args, fs::File, io::Read, time::Instant};

use parse::lex::{error::InvalidTokenError, lex};

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
    let tokens = lex(&mut input);
    let elapsed = Instant::now().duration_since(before);
    
    if let Some(next) = input.next() {
        eprintln!("{}", InvalidTokenError(next));
        return;
    }

    println!("{tokens:#?}");
    println!("took {elapsed:?}");
}

