use std::{fs::File, io::Read, time::Instant};

use clap::Parser;
use runtime::LanternRuntime;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    file: String,

    #[arg(short, long)]
    verbose: bool,
    #[arg(short, long)]
    no_run: bool,
    #[arg(long)]
    stack_dump: Option<String>,
}

fn main() {
    let Args { file: file_name, verbose, no_run, stack_dump } = Args::parse();

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
        println!("took {took:?}");
    }

    let before = Instant::now();
    let instructions = match flame::ignite(lantern_file) {
        Ok(instructions) => instructions,
        Err(err) => {
            eprintln!("{err}");
            return;
        },
    };

    if verbose {
        println!("{instructions:?}");
        println!("took {:?}", Instant::now().duration_since(before));
    }

    println!("finished compiling in {:?}", Instant::now().duration_since(before_compile));

    if no_run { return; }

    let runtime: LanternRuntime = LanternRuntime::new(32, instructions);
    println!("running {file_name}");
    let before = Instant::now();
    match runtime.exec() {
        Ok(stack) => {
            println!("Finished executing in {:?}", Instant::now().duration_since(before));
            if verbose {
                println!("stack dump:\n{stack}");
            }

            if let Some(stack_dump) = stack_dump {
                if let Err(err) = std::fs::write(&stack_dump, stack) {
                    eprintln!("{err}");
                }
            }
        },
        Err(err) => eprintln!("{err}"),
    }
}

