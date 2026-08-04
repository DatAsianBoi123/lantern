use std::{fs::File, io::Read, time::Instant};

use clap::Parser;
use diagnostic::DiagnosticSink;
use runtime::{VM, flame::FunctionKind};

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

    let mut sink = DiagnosticSink::new();
    let before = Instant::now();
    let vm = VM::new(lantern_file, &mut sink);

    for err in sink.into_emitted() {
        eprintln!("{err}");
    }

    let Some(vm) = vm else { return; };

    if verbose {
        vm.funs().iter().enumerate().for_each(|(i, fun)| {
            println!("Generated {i} ({}):", fun.name);
            match &fun.kind {
                FunctionKind::Instructions(instructions, locals) => println!("{locals} locals\n{instructions}"),
                FunctionKind::Native(native) => println!("<native function {native:?}>"),
            }
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

