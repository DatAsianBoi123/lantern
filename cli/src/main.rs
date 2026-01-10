use std::{fs::File, io::Read, time::Instant};

use clap::Parser;
use runtime::VM;

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
    let before_compile = Instant::now();
    let lantern_file = match parse::parse(content.trim()) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    if verbose {
        let took = Instant::now().duration_since(before_compile);
        println!("{lantern_file:#?}");
        println!("Parsed in {took:?}");
    }

    let before = Instant::now();
    let vm = match VM::new(lantern_file) {
        Ok(vm) => vm,
        Err(err) => {
            eprintln!("{err}");
            return;
        },
    };

    if verbose {
        vm.funs().iter().enumerate().for_each(|(i, fun)| {
            println!("Generated {i}:\n{fun}");
        });
        println!("Compiled in {:?}", Instant::now().duration_since(before));
    }

    println!("finished compiling in {:?}", Instant::now().duration_since(before_compile));

    if no_run { return; }

    println!("running {file_name}");
    match vm.exec() {
        Ok(_) => {},
        Err(err) => eprintln!("{err}"),
    }
}

