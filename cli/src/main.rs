use std::{path::Path, process::ExitCode, time::Instant};

use arena::Arena;
use clap::Parser;
use diagnostic::{SourceMap, symbol::SymbolTable};
use runtime::{VM, flame::FunctionKind};

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    file: String,

    #[arg(long)]
    print_ast: bool,
    #[arg(long)]
    print_codegen: bool,
    #[arg(short, long)]
    no_run: bool,
}

fn main() -> ExitCode {
    let Args { file: file_name, print_ast, print_codegen, no_run } = Args::parse();

    let path: &Path = file_name.as_ref();
    let Some(parent) = path.parent() else {
        eprintln!("invalid file path");
        return ExitCode::from(11);
    };
    let Some(filename) = path.file_name() else {
        eprintln!("invalid file path");
        return ExitCode::from(12);
    };

    let mut source_map = SourceMap::new(parent.into());
    let root = match source_map.load(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::from(50);
        }
    };

    let mut symbol_table = SymbolTable::new();

    let before_compile = Instant::now();
    let lantern_file = match parse::parse(root, &source_map.get_source(root).1, &mut symbol_table) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{}", err.display(&source_map));
            return ExitCode::from(80);
        }
    };

    if print_ast {
        println!("{lantern_file:#?}");
    }

    let type_arena = Arena::new(64);
    let (spark, sink) = spark::lower(lantern_file, &type_arena, &mut symbol_table, &mut source_map);
    let errors = sink.display_errors(&source_map);
    if !errors.is_empty() {
        eprintln!("{}", errors);
    }

    let Some(spark) = spark else { return ExitCode::from(101); };

    let vm = VM::new(spark);
    if print_codegen {
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

