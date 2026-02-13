use std::fs::read_to_string;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, NamedSource, Report};

use fossil_lang::compiler::{CompileResult, Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

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
    Compile {
        /// Path to the Fossil script to compile
        #[arg(short, long)]
        path: PathBuf,
    },
    Run {
        /// Path to the Fossil script to run
        #[arg(short, long)]
        path: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { path } => {
            let _ = compile(path)?;
        }
        Commands::Run { path } => {
            let _ = run(path)?;
        }
    };

    Ok(())
}

fn compile(path: PathBuf) -> miette::Result<CompileResult> {
    let mut gcx = GlobalContext::default();
    fossil_providers::init(&mut gcx);
    fossil_ifc::init(&mut gcx);
    fossil_stdlib::init(&mut gcx);

    let source_content = read_to_string(&path).into_diagnostic()?;

    let compiler = Compiler::with_context(gcx);
    let result = compiler.compile(CompilerInput::File(path.clone())).map_err(|errors| {
        let source = NamedSource::new(path.display().to_string(), source_content.clone());
        if let Some(first_error) = errors.0.into_iter().next() {
            Report::new(first_error).with_source_code(source)
        } else {
            Report::msg("Compilation failed with unknown errors")
        }
    })?;

    if !result.warnings.is_empty() {
        for warning in result.warnings.0.iter() {
            eprintln!("warning: {:?}", warning);
        }
    }

    Ok(result)
}

fn run(path: PathBuf) -> miette::Result<()> {
    let result = compile(path.clone())?;

    let source_content = read_to_string(&path).into_diagnostic()?;
    let source = NamedSource::new(path.display().to_string(), source_content);

    IrExecutor::execute(result.program).map_err(|e| {
        Report::new(e).with_source_code(source)
    })?;

    Ok(())
}
