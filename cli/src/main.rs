use std::path::PathBuf;

use clap::{Parser, Subcommand};
use thiserror::Error;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::{CompileError, CompileErrors};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

#[derive(Debug, Error)]
enum RunError {
    #[error("I/O error")]
    Io(#[from] std::io::Error),

    #[error("Compilation failed with {0} error(s)")]
    Compile(#[from] CompileErrors),

    #[error("Runtime error")]
    Runtime(#[from] CompileError),
}

#[derive(Parser)]
#[command(name = "fossil")]
#[command(about = "Fossil - A small language for living data")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Fossil script
    Run {
        /// Path to the Fossil script to run
        #[arg(short, long)]
        path: PathBuf,
    },
}

fn main() -> Result<(), RunError> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { path } => run(path),
    }
}

fn run(path: PathBuf) -> Result<(), RunError> {
    let mut gcx = GlobalContext::default();
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    gcx.register_provider("shex", fossil_providers::shapes::shex::ShexProvider);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let program = compiler.compile(CompilerInput::File(path))?;
    IrExecutor::execute(program)?;

    Ok(())
}
