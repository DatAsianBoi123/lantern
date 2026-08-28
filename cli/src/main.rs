use std::{fs::File, io::Read, process::ExitCode, time::Instant};

use clap::Parser;
use diagnostic::{DiagnosticSink, symbol::SymbolTable};
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

fn main() -> ExitCode {
    let Args { file: file_name, verbose, no_run } = Args::parse();

    let Ok(mut file) = File::open(&file_name) else {
        eprintln!("file not found: `{file_name}`");
        return ExitCode::from(3);
    };

    let mut content = String::new();
    if file.read_to_string(&mut content).is_err() {
        eprintln!("file contains invalid UTF-8");
        return ExitCode::from(3);
    };
    let mut symbol_table = SymbolTable::new();
    let before_compile = Instant::now();
    let lantern_file = match parse::parse(content.trim(), &mut symbol_table) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::from(80);
        }
    };

    if verbose {
        let took = Instant::now().duration_since(before_compile);
        println!("{lantern_file:#?}");
        println!("Parsed in {took:?}");
    }

    let mut sink = DiagnosticSink::new();
    let vm = VM::new(lantern_file, &mut sink, &symbol_table);

    for err in sink.into_emitted() {
        eprintln!("{err}");
    }

    let Some(vm) = vm else { return ExitCode::from(101); };

    if verbose {
        vm.funs().iter().enumerate().for_each(|(i, fun)| {
            println!("Generated {i} ({}):", fun.name);
            match &fun.kind {
                FunctionKind::Instructions(instructions, locals) => println!("{locals} locals\n{instructions}"),
                FunctionKind::Native(native) => println!("<native function {native:?}>"),
            }
        });
    }

    println!("finished compiling in {:?}", Instant::now().duration_since(before_compile));

    if no_run { return ExitCode::SUCCESS; }

    println!("running {file_name}");
    match vm.exec() {
        Ok(_) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::FAILURE
        },
    }
}

